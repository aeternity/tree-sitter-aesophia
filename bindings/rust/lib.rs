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

use tree_sitter::Language;

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
    InvalidEscape,
    UnexpectedNode(String),
    MissingField(String),
    MissingChild(usize),
}

type ParseResult<T> = Result<T, ParseError>;
type ParseResultN<T> = ParseResult<ast::Node<T>>;

fn node_content<'a>(node: &'a tree_sitter::Node<'a>, src: &'a [u16]) -> ParseResult<String> {
    Ok(String::from_utf16(node.utf16_text(src)).unwrap())
}

fn parse_char(token: &str) -> ParseResult<char> {
    let mut chars = token[1..token.len() - 1].chars();
    let c = unescape('\'', &mut chars)?;
    Ok(c)
}

fn parse_str(token: &str) -> ParseResult<String> {
    let mut chars = token[1..token.len() - 1].chars().peekable();
    let mut out = String::with_capacity(token.len());
    while *(chars
        .peek()
        .ok_or(ParseError::InvalidToken(token.to_string()))?)
        != '"'
    {
        let c = unescape('"', &mut chars)?;
        out.push(c)
    }
    Ok(out)
}

fn parse_int(token: &str) -> ParseResult<i64> {
    let (chars_trimmed, radix) = if &token[0..2] == "0x" {
        (token[2..].chars(), 16)
    } else {
        (token.chars(), 10)
    };

    let mut chars = chars_trimmed.filter(|x| *x != '_' && *x != 'x');

    let mut neg = false;

    let mut out: i64 = match chars.next().unwrap() {
        '-' => {
            neg = true;
            -(chars.next().unwrap().to_digit(radix).unwrap() as i64)
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
    Ok(out)
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

        let c1 = chars
            .next()
            .ok_or(ParseError::InvalidToken(token.to_string()))?;

        let c0d = c0
            .to_digit(16)
            .ok_or(ParseError::InvalidToken(token.to_string()))?;
        let c1d = c1
            .to_digit(16)
            .ok_or(ParseError::InvalidToken(token.to_string()))?;

        let byte = c0d * 16 + c1d;

        out.push(byte as u8)
    }
    Ok(out)
}

fn parse_qual(token: &str) -> ParseResult<(Vec<String>, String)> {
    let path = token.split('.').map(String::from).collect::<Vec<_>>();
    let name = path[path.len() - 1].to_owned();
    let path_prefix = &path[0..path.len() - 1];
    Ok((path_prefix.to_vec(), name))
}

fn unescape<T>(close: char, chars: &mut T) -> ParseResult<char>
where
    T: Iterator<Item = char>,
{
    let c0 = chars.next().ok_or(ParseError::InvalidEscape)?;
    if c0 == '\\' {
        match chars.next().ok_or(ParseError::InvalidEscape)? {
            '\\' => Ok('\\'),
            'b' => Ok('\u{8}'),
            'e' => Ok('\u{27}'),
            'f' => Ok('\u{12}'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            'v' => Ok('\u{11}'),
            // 'x' => _,
            c if c == close => Ok(c),
            _ => Err(ParseError::InvalidEscape),
        }
    } else {
        Ok(c0)
    }
}

fn parse_bin_op(token: &str) -> ParseResult<ast::BinOp> {
    match token {
        "+" => Ok(ast::BinOp::Add),
        "-" => Ok(ast::BinOp::Sub),
        "*" => Ok(ast::BinOp::Mul),
        "/" => Ok(ast::BinOp::Div),
        "mod" => Ok(ast::BinOp::Mod),
        "^" => Ok(ast::BinOp::Pow),
        "&&" => Ok(ast::BinOp::And),
        "||" => Ok(ast::BinOp::Or),
        "<" => Ok(ast::BinOp::LT),
        "=<" => Ok(ast::BinOp::LE),
        ">" => Ok(ast::BinOp::GT),
        ">=" => Ok(ast::BinOp::GE),
        "==" => Ok(ast::BinOp::EQ),
        "!=" => Ok(ast::BinOp::NE),
        "::" => Ok(ast::BinOp::Cons),
        "++" => Ok(ast::BinOp::Concat),
        _ => Err(ParseError::InvalidToken(token.to_string())),
    }
}

fn parse_literal<'a>(node: &tree_sitter::Node, src: &'a [u16]) -> ParseResultN<ast::Literal> {
    use ast::Literal;
    let content = node_content(node, src)?;
    let lit = match node.kind() {
        // "lit_constructor" => Literal::Constructor { val: content },
        "lit_bytes" => Literal::Bytes {
            val: parse_bytes(&content)?,
        },
        "lit_lambda_op" => {
            let op_node = node.child_by_field_name("op").unwrap();
            Literal::LambdaBinOp {
                val: parse_bin_op(&node_content(&op_node, src)?)?,
            }
        },
        "lit_integer" => Literal::Int {
            val: parse_int(&content)?,
        },
        "lit_bool" => {
            if content == "true" {
                Literal::Bool { val: true }
            } else if content == "false" {
                Literal::Bool { val: false }
            } else {
                Err(ParseError::InvalidToken(content))?
            }
        }
        "lit_empty_map_or_record" => Literal::EmptyMapOrRecord,
        "lit_string" => Literal::String {
            val: parse_str(&content)?,
        },
        "lit_char" => Literal::Char {
            val: parse_char(&content)?,
        },
        "lit_wildcard" => Literal::Wildcard,
        _ => panic!("bad node"),
    };
    Ok(mk_node(node, lit))
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

fn get_child<'a>(node: &'a tree_sitter::Node, idx: usize) -> ParseResult<tree_sitter::Node<'a>> {
    node.child(idx).ok_or(ParseError::MissingChild(idx))
}

fn get_field<'a>(node: &'a tree_sitter::Node, field: &str) -> ParseResult<tree_sitter::Node<'a>> {
    node.child_by_field_name(field).ok_or(ParseError::MissingField(field.to_string()))
}

fn parse_expr<'a>(node: &tree_sitter::Node, src: &'a [u16]) -> ParseResultN<ast::Expr> {
    use ast::Expr;
    let expr = match node.kind() {
        "expr_variable" => {
            let content = node_content(node, src)?;
            let (path, name) = parse_qual(&content)?;
            Expr::Var {
                var: ast::QName {
                    path: mk_node_many(node, vec![]), // TODO fix path
                    name: mk_node(node, name),
                },
            }
        }
        "expr_literal" => Expr::Literal {
            val: parse_literal(&get_child(node, 0)?, src)?,
        },
        "expr_tuple" => {
            let mut coursor = node.walk();
            let kids = node.children(&mut coursor).map(|e| parse_expr(&e, src));
            let nodes = mk_node_many(node, kids.collect::<ParseResult<Vec<_>>>()?);
            Expr::Tuple{
                elems: nodes
            }
        }
        "expr_list" => {
            let mut coursor = node.walk();
            let kids = node.children(&mut coursor).map(|e| parse_expr(&e, src));
            let nodes = mk_node_many(node, kids.collect::<ParseResult<Vec<_>>>()?);
            Expr::Tuple{
                elems: nodes
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
    Ok(mk_node(node, expr))
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
        let src = "@!EXPRESSION\n(123, 321)";
        let ast = parser.parse(src, None).unwrap();
        let node = ast.root_node().child(1).unwrap();

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
        panic!("{:?}", parse_expr(&node, &src_data))
    }
}
