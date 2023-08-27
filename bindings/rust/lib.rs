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
use std::sync::TryLockResult;

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
}

type ParseErrors = Vec<ParseError>;

type ParseResult<T> = Option<T>;
type ParseResultN<T> = ParseResult<ast::Node<T>>;

fn node_content(node: &Node, src: &[u16], errs: &mut ParseErrors)
                -> ParseResult<String> {
    let as_utf = node.utf16_text(src);
    match String::from_utf16(as_utf) {
        Ok(content) => Some(content),
        Err(_) => {
            let range = node.byte_range();
            errs.push(ParseError::InvalidUtf16(range.start..range.end));
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
    while *(chars.peek()?) != '"'
    {
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

fn parse_literal(tc: &mut TreeCursor, src: &[u16], errs: &mut ParseErrors)
                 -> ParseResultN<ast::Literal> {
    use ast::Literal;
    let node = &tc.node();
    let content = node_content(node, src, errs)?;
    let lit = match node.kind() {
        // "lit_constructor" => Literal::Constructor { val: content },
        "lit_bytes" => {
            let lit = parse_bytes(&content);

            if lit.is_none() {
                errs.push(ParseError::InvalidToken(content));
            }

            Literal::Bytes {
                val: lit?,
            }
        },
        "lit_lambda_op" => {
            let op = parse_bin_op(&content);

            if op.is_none() {
                errs.push(ParseError::InvalidToken(content));
            }

            Literal::LambdaBinOp {
                val: op?
            }
        },
        "lit_integer" => {
            let lit = parse_int(&content);

            if lit.is_none() {
                errs.push(ParseError::InvalidToken(content));
            }

            Literal::Int {
                val: lit?,
            }
        },
        "lit_bool" => {
            if content == "true" {
                Literal::Bool { val: true }
            } else if content == "false" {
                Literal::Bool { val: false }
            } else {
                errs.push(ParseError::InvalidToken(content));
                None?
            }
        }
        "lit_empty_map_or_record" =>
            Literal::EmptyMapOrRecord,
        "lit_string" => {
            let lit = parse_str(&content);

            if lit.is_none() {
                errs.push(ParseError::InvalidToken(content));
            }

            Literal::String {
                val: lit?,
            }
        },
        "lit_char" => {
            let lit = parse_char(&content);

            if lit.is_none() {
                errs.push(ParseError::InvalidToken(content));
                None?
            } else {
                Literal::Char {
                    val: parse_char(&content)?,
                }
            }
        },
        "lit_wildcard" =>
            Literal::Wildcard,
        _ => panic!("bad node"),
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


fn parse_child_by_idx<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    src: &'a [u16],
    errs: &mut ParseErrors,
    parse: P,
    idx: usize
) -> ParseResultN<T>
where P: Fn(&mut tree_sitter::TreeCursor<'a>, &'a[u16], &mut ParseErrors) -> ParseResultN<T>
{   let child = tc.node().child(idx)?;
    let node = parse(&mut child.walk(), src, errs);
    node
}

fn parse_child_by_name<'a, T: Clone, F : Fn(&tree_sitter::TreeCursor) -> bool>(
    tc: &mut tree_sitter::TreeCursor,
    src: &'a [u16],
    errs: &mut ParseErrors,
    parse: fn(&mut tree_sitter::TreeCursor, src: &'a [u16], errs: &mut ParseErrors) -> ParseResultN<T>,
    name: &str
) ->
    ParseResultN<T>
{
    println!("NAMED AT node: {}", tc.node().id());
    let child = tc.node().child_by_field_name(name)?;
    tc.reset(child);
    println!("NAMED CHILD AT node: {}", tc.node().id());
    let node = parse(tc, src, errs);
    let moved = tc.goto_parent();
    println!("NAMED QUIT AT node: {}, moved: {}", tc.node().id(), moved);
    node
}

fn parse_children<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    src: &'a [u16],
    errs: &mut ParseErrors,
    parse: P,
    filter: F
) ->
    ParseResult<Vec<ast::Node<T>>>
where F: Fn(& tree_sitter::TreeCursor) -> bool,
      P: Fn(&mut tree_sitter::TreeCursor<'a>, &'a [u16], &mut ParseErrors) -> ParseResultN<T>
{
    let mut children = Vec::with_capacity(tc.node().child_count());
    tc.goto_first_child();
    while {
        if filter(tc) {
            children.push(parse(tc, src, errs));
        }
        tc.goto_next_sibling()
    } {}
    tc.goto_parent();
    let nodes: Option<Vec<_>> = children.into_iter().collect();
    nodes
}

fn parse_children_by_field<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    src: &'a [u16],
    errs: &mut ParseErrors,
    parse: P,
    name: &str) ->
    ParseResult<Vec<ast::Node<T>>>
where T: Clone,
      P: Fn(&mut tree_sitter::TreeCursor<'a>, &'a [u16], &mut ParseErrors) -> ParseResultN<T>
{
    parse_children(tc, src, errs, parse, |c| c.field_name() == Some(name))
}


fn parse_expr<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    src: &'a [u16],
    errs: &mut ParseErrors
) -> ParseResultN<ast::Expr> {
    use ast::Expr;
    let node = &tc.node();
    let expr = match node.kind() {
        "expr_variable" => {
            let content = node_content(node, src, errs)?;
            let qual = parse_qual(&content);

            if qual.is_none() {
                errs.push(ParseError::InvalidToken(content));
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
            let lit = parse_child_by_idx(tc, src, errs, parse_literal, 0);

            Expr::Literal {
                val: lit?,
            }
        },
        "expr_tuple" => {
            let elems = parse_children_by_field(tc, src, errs, parse_expr, "elem");
            Expr::Tuple{
                elems: mk_node_many(&node, elems?)
            }
        }
        "expr_list" => {
            let elems = parse_children_by_field(tc, src, errs, parse_expr, "elem");
            Expr::List{
                elems: mk_node_many(&node, elems?)
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
            panic!("bad node")
        }
    };
    Some(mk_node(node, expr))
}

#[cfg(test)]
mod tests {
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
        // let src = "contract C = function f(x, y) = 123";
        let src = "@ts.parse(expression)\n((1111, 22222), (3333, 444))";
        let ast = parser.parse(src, None).unwrap();
        let node = ast.root_node().child_by_field_name("parsed").unwrap();

        println!("SEXP: {}", node.to_sexp());
        println!("NAMED: {}", node.named_child_count());
        println!("CHILDR: {}", node.child_count());
        println!(
            "CN: {:?}",
            node.named_children(&mut node.walk())
                .map(|x| x.to_sexp())
                .collect::<Vec<_>>()
        );
        println!(
            "C: {:?}",
            node.children(&mut node.walk())
                .map(|x| x.to_sexp())
                .collect::<Vec<_>>()
        );

        let src_data: Vec<u16> = src.encode_utf16().collect();
        let mut errs = vec![];
        let mut tc = node.walk();
        panic!("{:?}", parse_expr(&mut tc, &src_data, &mut errs))
    }
}
