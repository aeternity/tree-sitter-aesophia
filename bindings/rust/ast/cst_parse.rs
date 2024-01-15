//! Fundamental tree-sitter parsing utilities.

use crate::ast::ast;
use tree_sitter;

/// Errors related to lexing
#[derive(Clone, Debug)]
pub enum TokenError {
    InvalidEscape(char),
    UnclosedEscape,
    UnclosedChar,
    EmptyChar,
    UnclosedString,
    InvalidInteger,
    InvalidBool,
    InvalidByteChar,
    UnevenBytes,
}

impl TokenError {
    pub fn to_str(&self) -> String {
        match self {
            Self::InvalidEscape(_c) => "Illegal character escape",
            Self::UnclosedEscape => "Unclosed character escape",
            Self::UnclosedChar => "Unclosed char",
            Self::EmptyChar => "Empty char",
            Self::UnclosedString => "Unclosed string",
            Self::InvalidInteger => "Invalid integer",
            Self::InvalidBool => "Invalid boolean",
            Self::InvalidByteChar => "Invalid hex character",
            Self::UnevenBytes => "Byte sequence must have even digits",
        }.to_string()
    }
}

/// Precise parse errors. To be wrapped in annotated nodes for location info.
#[derive(Clone, Debug)]
pub enum ParseError {
    TokenError(TokenError),
    InvalidUtf16,
    MissingField(String),
    MissingChild(usize),
    MissingNode,
    NodeError,
    BlockNoExpr,
    LonelyElIf,
    LonelyElse,
    InvalidModifier,
    NamespaceImpl,
    InconsistentName,
    DuplicateSig,
}

impl ParseError {
    pub fn to_str(&self) -> String {
        match self {
            Self::TokenError(e) => e.to_str(),
            Self::InvalidUtf16 => "Invalid UTF-16 character".to_string(),
            Self::MissingField(s) => vec!["Missing field: ", s].concat(),
            Self::MissingChild(_i) => "Missing index".to_string(),
            Self::MissingNode => "Missing node".to_string(),
            Self::NodeError => "Parse error".to_string(),
            Self::BlockNoExpr => "Empty block".to_string(),
            Self::LonelyElIf => "Missing `if` for `elif`".to_string(),
            Self::LonelyElse => "Missing `if` for `else`".to_string(),
            Self::InvalidModifier => "Invalid modifier".to_string(),
            Self::NamespaceImpl => "Cannot implement a namespace".to_string(),
            Self::InconsistentName => "Inconsistent naming".to_string(),
            Self::DuplicateSig => "Duplicated signature".to_string(),
        }
    }
}

/// Data structure to store parse errors
pub type ParseErrors = ast::Nodes<ParseError>;

/// Information about CST node subtyping. Maps subtype to its supertype. This says for example that
/// `expr_if` is a special case of `expr`.
pub type SubtypeMap = std::collections::HashMap<String, String>;

/// Parsing environment for CST processing.
#[derive(Debug)]
pub struct ParseEnv {
    /// Source file reference
    pub src: Vec<u16>,

    /// Map of node subtypes
    pub subtypes: SubtypeMap,

    /// Error feed
    pub errs: ParseErrors,

    /// Whether the CST contains error nodes. `false` does not indicate that this CST represents a
    /// correct AST, but only that tree-sitter managed to build the CST itself.
    pub has_ts_errors: bool,
}

impl ParseEnv {
    pub fn new(src: Vec<u16>, subtypes: SubtypeMap, has_ts_errors: bool) -> Self {
        ParseEnv {
            src,
            errs: vec![],
            subtypes,
            has_ts_errors
        }
    }

    /// Add an error at node's location
    pub fn err(&mut self, node: &TsNode, err: ParseError) {
        self.errs.push(mk_node(node, err));
        self.has_ts_errors = true;
    }

    /// Add a lexing error at node's location
    pub fn tok_err(&mut self, node: &TsNode, err: TokenError) {
        self.err(node, ParseError::TokenError(err))
    }

    /// Runs error node parser if the node has errors and returns `None`. Does nothing otherwise.
    pub fn check_error(&mut self, tc: &mut TsCursor) -> ParseResult<()> {
        if self.has_ts_errors && parse_error(tc, self) {
            None
        } else {
            Some(())
        }
    }
}


/// Default parsing result type
pub type ParseResult<T> = Option<T>;

/// Parsing result with location info
pub type ParseResultN<T> = ParseResult<ast::Node<T>>;

/// Alias for tree-sitter node
pub type TsNode<'a> = tree_sitter::Node<'a>;

/// Alias for tree-sitter coursor
pub type TsCursor<'a> = tree_sitter::TreeCursor<'a>;


/// Extracts annotation from a tree-sitter node
pub fn ann(node: &TsNode) -> ast::Ann {
    let tree_sitter::Point{row: start_line, column: start_col} = node.start_position();
    let tree_sitter::Point{row: end_line, column: end_col} = node.end_position();
    ast::Ann {
        start_line: start_line as u32,
        start_col: start_col as u32,
        start_byte: node.start_byte() as u32,
        end_line: end_line as u32,
        end_col: end_col as u32,
        end_byte: node.end_byte() as u32,
        filename: "<filename mock>".to_string(),
        root_node: node.id()
    }
}


/// Wraps an item with an AST node based on a tree-sitter node
pub fn mk_node<T: Clone>(node: &TsNode, value: T) -> ast::Node<T> {
    ast::Node {
        node: value,
        ann: ann(node),
    }
}


/// Extracts raw string content of a tree-sitter node.
pub fn node_content(node: &TsNode, env: &mut ParseEnv) -> ParseResult<String> {
    let as_utf = node.utf16_text(&env.src);
    match String::from_utf16(as_utf) {
        Ok(content) => Some(content),
        Err(_) => {
            env.err(node, ParseError::InvalidUtf16);
            None
        }
    }
}


/// Checks if the node is an error (or missing) node and if so, runs a fallback parser to collect
/// tree-sitter errors. Returns true if The node was ineed an error, false otherwise.
pub fn parse_error<'a>(tc: &mut TsCursor<'a>, env: &mut ParseEnv) -> bool {
    let node = &tc.node();
    if node.is_missing() {
        env.err(node, ParseError::MissingNode);
        return true;
    }

    if node.is_error() {
        env.err(node, ParseError::NodeError);
    }

    parse_children(tc, env, &|tc, env| {Some(parse_error(tc, env))}, |_| true);
    false
}


/// Parses all children with the given parser if they fulfill the predicate
pub fn parse_children<'a, T: Clone, F, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    mut filter: F,
) -> Vec<T>
where
    F: FnMut(&TsCursor<'a>) -> bool,
    P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let mut children = Vec::with_capacity(tc.node().child_count());
    let mut child_found = tc.goto_first_child();

    if !child_found {
        return vec![];
    }

    while child_found {
        if filter(tc) {
            children.push(parse(tc, env));
        }
        child_found = tc.goto_next_sibling();
    }

    tc.goto_parent();

    children.into_iter().flatten().collect()
}


/// Parses the first child of using the given parser which fulfills the predicate
pub fn parse_child<'a, T: Clone, F, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    filter: F,
) -> ParseResult<T>
where
    P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
    F: FnMut(&TsCursor) -> bool
{
    let nodes = parse_children(tc, env, &parse, filter);

    let node_vec = nodes;

    if !node_vec.is_empty() {
        let node = node_vec[0].clone();
        Some(node)
    } else {
        None
    }
}

/// Parses nth child using the given parser
pub fn parse_idx<'a, T: Clone, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    idx: usize,
) -> ParseResult<T>
where
    P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let mut i = 0;
    let child = parse_child(tc, env, &parse, |_| {
        i += 1;
        idx == i - 1
    });

    if child.is_none() {
        // todo: optional fields!
        env.err(&tc.node(), ParseError::MissingChild(idx));
    }

    child
}

/// Parses child of the given name using the given parser
pub fn parse_field<'a, T: Clone, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> ParseResult<T>
where P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let child = parse_child(tc, env, &parse, |c| {
        c.field_name() == Some(name)
    });

    if child.is_none() {
        env.err(&tc.node(), ParseError::MissingField(name.to_string()));
    }

    child
}

/// Parses all fields of the given name using the given parser
pub fn parse_fields<'a, T, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> Vec<T>
where
    T: Clone,
    P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    parse_children(tc, env, parse, |c| c.field_name() == Some(name))
}

/// Parses all children of the given kind using the given parser
pub fn parse_kinds<'a, T, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> Vec<T>
where
    T: Clone,
    P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    parse_children(tc, env, parse, |c| c.node().kind() == name)
}

/// Within the field parses all children on the given inner field using the given parser. Useful
/// when a node has multiple subnodes of the same kind hidden under a single wrapping field.
pub fn parse_fields_in_field<'a, T: Clone, P>(
    tc: &mut TsCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
    name_in: &str,
) -> ParseResult<ast::NodeMany<T>>
where P: Fn(&mut TsCursor<'a>, &mut ParseEnv) -> ParseResultN<T>
{
    parse_field(tc, env, &|tc, env| {
        let children = parse_fields(tc, env, parse, name_in);
        Some(mk_node(&tc.node(), children))
    }, name)
}
