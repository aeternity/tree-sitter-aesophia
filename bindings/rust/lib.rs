//! This crate provides aesophia language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [language][language func] function to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse Ok code:
//!
//! ```
//! let code = "";
//! let mut parser = tree_sitter::Parser::new();
//! parser.set_language(tree_sitter_aesophia::language()).expect("Error loading aesophia grammar");
//! let tree = parser.parse(code, None).unwrap();
//! ```
//!
//! [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
//! [language func]: fn.language.html
//! [Parser]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

extern crate tree_sitter;

use tree_sitter::*;

mod ast;

extern "C" {
    fn tree_sitter_aesophia() -> Language;
}

/// Get the tree-sitter [Language][] for this grammar.
///
/// [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
pub fn language() -> Language {
    unsafe { tree_sitter_aesophia() }
}

/// The content of the [`node-types.json`][] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

// Uncomment these to include any queries that this grammar contains

// pub const HIGHLIGHTS_QUERY: &'static str = include_str!("../../queries/highlights.scm");
// pub const INJECTIONS_QUERY: &'static str = include_str!("../../queries/injections.scm");
// pub const LOCALS_QUERY: &'static str = include_str!("../../queries/locals.scm");
// pub const TAGS_QUERY: &'static str = include_str!("../../queries/tags.scm");


#[derive(Clone, Debug)]
pub enum ParseError {
    InvalidToken(String),
    InvalidUtf16(std::ops::Range<usize>),
    InvalidEscape,
    UnexpectedNode(String),
    MissingField(String),
    MissingChild(usize),
    MissingNode(String),
    NodeError(String),
}

type ParseErrors = Vec<ParseError>;
type SubtypeMap = std::collections::HashMap<String, String>;

#[derive(Clone, Debug)]
pub struct ParseEnv<'a> {
    src: &'a [u16],
    subtypes: SubtypeMap,
    errs: ParseErrors,
}

type ParseResult<T> = Option<T>;
type ParseResultN<T> = ParseResult<ast::Node<T>>;

fn node_content(node: &Node, env: &mut ParseEnv) -> ParseResult<String> {
    let as_utf = node.utf16_text(env.src);
    match String::from_utf16(as_utf) {
        Ok(content) => Some(content),
        Err(_) => {
            let range = node.byte_range();
            env.errs.push(ParseError::InvalidUtf16(range.start..range.end));
            None
        }
    }
}

fn parse_char(token: &str) -> ParseResult<char> {
    let mut chars = token[1..token.len() - 1].chars();
    let c = unescape('\'', &mut chars)?;
    Some(c)
}

fn parse_str(token: &str) -> ParseResult<String> {
    let mut chars = token[1..token.len() - 1].chars().peekable();
    let mut out = String::with_capacity(token.len());
    while *(chars.peek()?) != '"' {
        let c = unescape('"', &mut chars)?;
        out.push(c)
    }
    Some(out)
}

fn parse_int(token: &str) -> ParseResult<i64> {
    let (chars_trimmed, radix) = if token.len() > 2 && &token[0..2] == "0x" {
        (token[2..].chars(), 16)
    } else {
        (token.chars(), 10)
    };

    let mut chars = chars_trimmed.filter(|x| *x != '_' && *x != 'x');

    let mut neg = false;

    let mut out: i64 = match chars.next().unwrap() {
        '-' => {
            neg = true;
            -(chars.next()?.to_digit(radix)? as i64)
        }
        c => c.to_digit(radix).unwrap() as i64,
    };

    for c in chars {
        if c == '_' {
            continue;
        }
        if c == '-' {
            neg = true
        }

        out *= radix as i64;

        if neg {
            out = out.checked_sub(c.to_digit(radix).unwrap() as i64).unwrap();
        } else {
            out = out.checked_add(c.to_digit(radix).unwrap() as i64).unwrap();
        }
    }
    Some(out)
}

fn parse_bytes(token: &str) -> ParseResult<Vec<u8>> {
    let mut chars = token.chars();
    let mut out: Vec<u8> = Vec::with_capacity(token.len() / 2);
    while let Ok(c0) = chars
        .next()
        .ok_or(ParseError::InvalidToken(token.to_string()))
    {
        if c0 == '_' {
            continue;
        }

        let c1 = chars.next()?;

        let c0d = c0.to_digit(16)?;
        let c1d = c1.to_digit(16)?;

        let byte = c0d * 16 + c1d;

        out.push(byte as u8)
    }
    Some(out)
}

fn parse_qual(token: &str) -> ParseResult<(Vec<String>, String)> {
    let path = token.split('.').map(String::from).collect::<Vec<_>>();
    let name = path.last()?.to_owned();
    let path_prefix = &path[0..path.len() - 1];
    Some((path_prefix.to_vec(), name))
}

fn unescape<T>(close: char, chars: &mut T) -> ParseResult<char>
where
    T: Iterator<Item = char>,
{
    let c0 = chars.next()?;
    if c0 == '\\' {
        match chars.next()? {
            '\\' => Some('\\'),
            'b' => Some('\u{8}'),
            'e' => Some('\u{27}'),
            'f' => Some('\u{12}'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            'v' => Some('\u{11}'),
            // 'x' => _,
            c if c == close => Some(c),
            _ => None,
        }
    } else {
        Some(c0)
    }
}

fn parse_bin_op(token: &str) -> ParseResult<ast::BinOp> {
    match token {
        "+" => Some(ast::BinOp::Add),
        "-" => Some(ast::BinOp::Sub),
        "*" => Some(ast::BinOp::Mul),
        "/" => Some(ast::BinOp::Div),
        "mod" => Some(ast::BinOp::Mod),
        "^" => Some(ast::BinOp::Pow),
        "&&" => Some(ast::BinOp::And),
        "||" => Some(ast::BinOp::Or),
        "<" => Some(ast::BinOp::LT),
        "=<" => Some(ast::BinOp::LE),
        ">" => Some(ast::BinOp::GT),
        ">=" => Some(ast::BinOp::GE),
        "==" => Some(ast::BinOp::EQ),
        "!=" => Some(ast::BinOp::NE),
        "::" => Some(ast::BinOp::Cons),
        "++" => Some(ast::BinOp::Concat),
        _ => None,
    }
}

fn parse_literal(
    tc: &mut TreeCursor,
    env: &mut ParseEnv
) -> ParseResultN<ast::Literal> {
    use ast::Literal;
    let node = &tc.node();
    let content = node_content(node, env)?;
    let lit = match node.kind() {
        // "lit_constructor" => Literal::Constructor { val: content },
        "lit_bytes" => {
            let lit = parse_bytes(&content);

            if lit.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
            }

            Literal::Bytes { val: lit? }
        }
        "lit_lambda_op" => {
            let op = parse_bin_op(&content);

            if op.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
            }

            Literal::LambdaBinOp { val: op? }
        }
        "lit_integer" => {
            let lit = parse_int(&content);

            if lit.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
            }

            Literal::Int { val: lit? }
        }
        "lit_bool" => {
            if content == "true" {
                Literal::Bool { val: true }
            } else if content == "false" {
                Literal::Bool { val: false }
            } else {
                env.errs.push(ParseError::InvalidToken(content));
                None?
            }
        }
        "lit_empty_map_or_record" => Literal::EmptyMapOrRecord,
        "lit_string" => {
            let lit = parse_str(&content);

            if lit.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
            }

            Literal::String { val: lit? }
        }
        "lit_char" => {
            let lit = parse_char(&content);

            if lit.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
                None?
            } else {
                Literal::Char {
                    val: parse_char(&content)?,
                }
            }
        }
        "lit_wildcard" => Literal::Wildcard,
        _ => {
            env.errs.push(ParseError::InvalidToken(content));
            None?
        }
    };
    Some(mk_node(&node, lit))
}

fn ann(_node: &tree_sitter::Node) -> ast::Ann {
    ast::Ann {}
}

fn mk_node<T: Clone>(node: &tree_sitter::Node, value: T) -> ast::Node<T> {
    ast::Node {
        node: value,
        ann: ann(node),
    }
}

fn mk_node_many<T: Clone>(node: &tree_sitter::Node, values: Vec<ast::Node<T>>) -> ast::NodeMany<T> {
    ast::NodeMany {
        nodes: values,
        ann: ann(node),
    }
}

fn parse_children<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    mut filter: F,
) -> ParseResult<Vec<ast::Node<T>>>
where
    F: FnMut(&tree_sitter::TreeCursor) -> bool,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>,
{
    let mut children = Vec::with_capacity(tc.node().child_count());
    tc.goto_first_child();
    while {
        let node = tc.node();
        if node.is_missing() {
            env.errs.push(ParseError::MissingNode(node.kind().to_string()));
        }
        if node.is_error() {
            match node_content(&node, env) {
                Some(content) => env.errs.push(ParseError::NodeError(content)),
                None => (),
            };
            parse_error(tc, env);
        }
        if filter(tc) {
            children.push(parse(tc, env));
        }
        tc.goto_next_sibling()
    } {}
    tc.goto_parent();
    let nodes: Option<Vec<_>> = children.into_iter().collect();
    nodes
}

fn parse_child<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    filter: F,
) -> ParseResultN<T>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>,
    F: FnMut(&tree_sitter::TreeCursor) -> bool
{
    let nodes = parse_children(tc, env, &parse, filter);

    let node_vec = nodes?;

    if !node_vec.is_empty() {
        let node = node_vec[0].clone();
        Some(node)
    } else {
        None
    }
}

fn parse_child_by_idx<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    idx: usize,
) -> ParseResultN<T>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>,
{
    let mut i = 0;
    parse_child(tc, env, &parse, |_| {
        i += 1;
        idx == i - 1
    })
}

fn parse_child_by_field<T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor,
    env: &mut ParseEnv,
    parse: P,
    name: &str,
) -> ParseResultN<T>
where P: Fn(&mut tree_sitter::TreeCursor, &mut ParseEnv) -> ParseResultN<T>,
{
    parse_child(tc, env, &parse, |c| {
        c.field_name() == Some(name)
    })
}

fn parse_children_by_field<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    name: &str,
) -> ParseResult<Vec<ast::Node<T>>>
where
    T: Clone,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>,
{
    parse_children(tc, env, parse, |c| c.field_name() == Some(name))
}

fn parse_children_by_kind<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    name: &str,
) -> ParseResult<Vec<ast::Node<T>>>
where
    T: Clone,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>,
{
    parse_children(tc, env, parse, |c| c.node().kind() == name)
}

fn parse_expr<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Expr> {
    use ast::Expr;
    let node = &tc.node();
    let expr = match node.kind() {
        "expr_variable" => {
            let content = node_content(node, env)?;
            let qual = parse_qual(&content);

            if qual.is_none() {
                env.errs.push(ParseError::InvalidToken(content));
            }

            let (path, name) = qual?;
            Expr::Var {
                var: ast::QName {
                    path: mk_node_many(node, vec![]), // TODO fix path
                    name: mk_node(node, name),
                },
            }
        }
        "expr_literal" => {
            let lit = parse_child_by_idx(tc, env, parse_literal, 0);

            Expr::Literal { val: lit? }
        }
        "expr_tuple" => {
            let elems = parse_children_by_field(tc, env, parse_expr, "elem");
            Expr::Tuple {
                elems: mk_node_many(node, elems?),
            }
        }
        "expr_list_literal" => {
            let elems = parse_children_by_field(tc, env, parse_expr, "elem");
            Expr::List {
                elems: mk_node_many(node, elems?),
            }
        }
        "expr_lambda" => {
            unimplemented!()
        }
        "expr_typed" => {
            unimplemented!()
        }
        "expr_op" => {
            unimplemented!()
        }
        "expr_application" => {
            unimplemented!()
        }
        "expr_record" => {
            unimplemented!()
        }
        "expr_record_update" => {
            unimplemented!()
        }
        "expr_map" => {
            unimplemented!()
        }
        "expr_map_update" => {
            unimplemented!()
        }
        "expr_map_access" => {
            unimplemented!()
        }
        "expr_projection" => {
            unimplemented!()
        }
        "expr_switch" => {
            unimplemented!()
        }
        "expr_if" => {
            unimplemented!()
        }
        "expr_block" => {
            unimplemented!()
        }
        _ => {
            let content = node_content(node, env)?;
            env.errs.push(ParseError::InvalidToken(content));
            None?
        }
    };
    Some(mk_node(node, expr))
}

/// Checks if the node is an error (or missing) node and if so, runs a fallback parser to collect
/// errors. Returns true if The node was ineed an error, false otherwise.
fn parse_error<'a>(tc: &mut TreeCursor<'a>, env: &mut ParseEnv) -> bool {
    let node = &tc.node();
    if node.is_missing() {
        env.errs.push(ParseError::MissingNode(node.kind().to_string()));
        return true;
    }

    if node.is_error() {
        parse_children(tc, env, parse_any, |_| true);
        return true;
    }

    false
}

fn parse_any<'a>(
    tc: &mut TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<bool> {
    let node = &tc.node();
    let kind = node.kind();

    let sub_err = match env.subtypes.get(kind)?.as_str() {
        "_expression" => parse_expr(tc, env).is_none(),
        "_literal" => parse_expr(tc, env).is_none(),
        _ => false
    };

    None
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast::Literal;

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(super::language())
            .expect("Error loading aesophia language");
    }

    #[test]
    fn test_simple_file() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(super::language())
            .expect("Error loading aesophia language");


        let nodes_json_src =  std::fs::read_to_string("src/node-types.json").expect("Unable to read file");
        let nodes_json: serde_json::Value = serde_json::from_str(&nodes_json_src).unwrap();
        let mut subtypes: HashMap<String, String> = HashMap::new();
        match nodes_json {
            serde_json::Value::Array(arr) =>
                for v in arr {
                    match (&v["subtypes"], &v["type"]) {
                        (serde_json::Value::Array(subts),
                         serde_json::Value::String(t)
                         )=> {
                            for subt in subts {
                                match &subt["type"] {
                                    serde_json::Value::String(s) => {
                                        subtypes.insert(s.clone(), t.clone());
                                    },
                                    _ => panic!("wtf sub type")
                                }
                            }
                        },
                        _ => ()
                    };
                },
            _ => panic!("wtf json")
        }

        println!("SUBTYPES: {:?}\n\n", subtypes);


        // let src = "contract C = function f(x, y) = 123";
        let src = "@ts.parse(expression)\n[1, 2, [3, ), 4] ";
        let ast = parser.parse(src, None).unwrap();
        let root = ast.root_node();
        println!("ROOT SEXP: {}", root.to_sexp());
        println!("ROOT KIND: {}", root.kind());

        let mut errs = vec![];
        let src_data: Vec<u16> = src.encode_utf16().collect();

        let mut env = ParseEnv {
            src: &src_data,
            errs: errs,
            subtypes: subtypes
        };

        let node =
            parse_child_by_field(&mut ast.walk(), &mut env, parse_expr, "parsed");

        println!();
        println!("\n\nERRORS: {:?}", env.errs);
        println!("\n\nOUTPUT: {:?}", node);
        println!();

        panic!("done :)");
    }
}
