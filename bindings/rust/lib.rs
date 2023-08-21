//! This crate provides aesophia language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [language][language func] function to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse some code:
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

use crate::ast::Constructor;
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

type ParseResult<T> = Result<T, ()>;

trait Ast<'a, T> {
    fn parse(&self, src: &'a [u16]) -> ParseResult<T>;
}

fn node_content<'a>(node: &'a tree_sitter::Node<'a>, src: &'a [u16]) -> ParseResult<String> {
    Ok(String::from_utf16(node.utf16_text(src)).unwrap())
}

fn parse_char(token: &str) -> Option<char> {
    let mut chars = token.chars();
    let c = unescape('\'', &mut chars)?;
    Some(c)
}

fn parse_str(token: &str) -> Option<String> {
    let mut chars = token.chars().peekable();
    let mut out = String::with_capacity(token.len());
    while *(chars.peek()?) != '"' {
        let c = unescape('"', &mut chars)?;
        out.push(c)
    }
    Some(out)
}

fn parse_int(token: &str) -> Option<i64> {
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
    Some(out)
}

fn parse_bytes(token: &str) -> Option<Vec<u8>> {
    let mut chars = token.chars();
    let mut out: Vec<u8> = Vec::with_capacity(token.len() / 2);
    while let Some(c0) = chars.next() {
        if c0 == '_' {
            continue;
        }

        let c1 = chars.next()?;
        let byte = c0.to_digit(16)? * 16 + c1.to_digit(16)?;

        out.push(byte as u8)
    }
    Some(out)
}

fn parse_qual(token: &str) -> Option<Vec<String>> {
    Some(token.split('.').map(String::from).collect())
}

// fn unescape(close: char, chars: &[char]) -> Option<(char, &[char])> {
fn unescape<T>(close: char, chars: &mut T) -> Option<char>
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
        _ => Err(())
    }
}

impl<'a> Ast<'a, ast::Literal> for tree_sitter::Node<'a> {
    fn parse(&self, src: &'a [u16]) -> ParseResult<ast::Literal> {
        use ast::Literal;
        match self.kind() {
            "lit_constructor" => Ok(Literal::Constructor {
                name: node_content(self, src)?,
            }),
            "lit_bytes" => Ok(Literal::Bytes {
                val: parse_bytes(&node_content(self, src)?).unwrap(),
            }),
            "lit_lambda_op" =>
                Ok(Literal::LambdaBinOp {
                    val: parse_bin_op(&node_content(self, src)?)?
                }),
            "lit_integer" => Ok(Literal::Int {
                val: parse_int(&node_content(self, src)?).unwrap(),
            }),
            "lit_bool" => {
                if node_content(self, src)? == "true" {
                    Ok(Literal::Bool { val: true })
                } else if node_content(self, src)? == "false" {
                    Ok(Literal::Bool { val: false })
                } else {
                    Err(())
                }
            }
            "lit_empty_map_or_record" => Ok(Literal::EmptyMapOrRecord),
            "lit_string" => Ok(Literal::String {
                val: node_content(self, src)?,
            }),
            "lit_char" => Ok(Literal::Char {
                val: parse_char(&node_content(self, src)?).unwrap(),
            }),
            "lit_wildcard" => Ok(Literal::Wildcard),
            _ => panic!("bad node"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Literal;
    use super::*;

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
        let src = "@!LITERAL\n123";
        let ast = parser.parse(src, None).unwrap();
        let node = ast.root_node().child(1).unwrap();
        println!("{}", node.to_sexp());
        let src_data: Vec<u16> = src.encode_utf16().collect();
        panic!("{:?}", node.parse(&src_data))
    }
}
