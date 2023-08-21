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

impl<'a> Ast<'a, ast::Literal> for tree_sitter::Node<'a> {
    fn parse(&self, src: &'a [u16]) -> ParseResult<ast::Literal> {
        use ast::Literal;
        match self.kind() {
            "lit_constructor" =>
                Ok(Literal::Constructor {
                    name: node_content(self, src)?
                }),
            _ => panic!("bad node")
        }
    }
}

#[cfg(test)]
mod tests {
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
        let src = "contract C = function f(x, y) = 123";
        let ast = parser.parse(src, None).unwrap();
        let node = ast.root_node();
        <node as Ast<Literal>>::parse()
    }
}
