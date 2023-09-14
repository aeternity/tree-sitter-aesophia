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

use num_bigint::BigInt;
use num_traits::Num;
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

type ParseErrors = ast::Nodes<ParseError>;
type SubtypeMap = std::collections::HashMap<String, String>;

#[derive(Debug)]
pub struct ParseEnv<'a> {
    src: &'a [u16],
    subtypes: SubtypeMap,
    errs: ParseErrors,
    has_errors: bool,
}

impl ParseEnv<'_> {
    fn err(&mut self, node: &Node, err: ParseError) {
        self.errs.push(mk_node(node, err));
        self.has_errors = true;
    }

    fn tok_err(&mut self, node: &Node, err: TokenError) {
        self.err(node, ParseError::TokenError(err))
    }

    fn check_error(&mut self, tc: &mut TreeCursor) -> ParseResult<()> {
        if self.has_errors && parse_error(tc, self) {
            None
        } else {
            Some(())
        }
    }
}

type ParseResult<T> = Option<T>;
type ParseResultN<T> = ParseResult<ast::Node<T>>;

fn node_content(node: &Node, env: &mut ParseEnv) -> ParseResult<String> {
    let as_utf = node.utf16_text(env.src);
    match String::from_utf16(as_utf) {
        Ok(content) => Some(content),
        Err(_) => {
            env.err(node, ParseError::InvalidUtf16);
            None
        }
    }
}

fn parse_char(
    tc: &TreeCursor,
    env: &mut ParseEnv
) -> ParseResult<char>
{
    let node = &tc.node();
    let token = node_content(node, env)?;
    let mut chars = token[1..].chars();
    let c = match chars.next() {
        Some('\\') => unescape('\'', &mut chars, node, env)?,
        Some('\'') => {
            env.tok_err(node, TokenError::EmptyChar);
            None?
        }
        Some(c) => c,
        None => {
            env.tok_err(node, TokenError::UnclosedChar);
            None?
        }
    };
    Some(c)
}

fn parse_str<'a>(
    tc: &mut TreeCursor<'a>,
    env: &mut ParseEnv
) -> ParseResult<String>
{
    let node = &tc.node();
    let token = node_content(node, env)?;
    let mut chars = token[1..token.len() - 1].chars();
    let mut out = String::with_capacity(token.len());
    loop {
        match chars.next() {
            None => {
                env.tok_err(node, TokenError::UnclosedString);
                None?
            }
            Some('"') => {
                break;
            }
            Some('\\') => {
                let c = unescape('"', &mut chars, node, env)?;
                out.push(c)
            }
            Some(c) => {
                out.push(c)
            }
        }
    }
    Some(out)
}

fn unescape<T>(close: char, chars: &mut T, node: &Node, env: &mut ParseEnv) -> ParseResult<char>
where
    T: Iterator<Item = char>,
{
    let c = match chars.next() {
        Some('\\') =>'\\',
        Some('b') => '\u{8}',
        Some('e') => '\u{27}',
        Some('f') => '\u{12}',
        Some('n') => '\n',
        Some('r') => '\r',
        Some('t') => '\t',
        Some('v') => '\u{11}',
        // 'x' => _,
        Some(c) if c == close => c,
        Some(c) => {
            env.tok_err(node, TokenError::InvalidEscape(c));
            None?
        }
        None => {
            env.tok_err(node, TokenError::UnclosedEscape);
            None?
        }
    };
    Some(c)
}

fn parse_int(tc: &TreeCursor, env: &mut ParseEnv) -> ParseResult<BigInt> {
    let token = node_content(&tc.node(), env)?;
    let (chars_trimmed, radix) = if token.len() > 2 && &token[0..2] == "0x" {
        (token[2..].chars(), 16)
    } else {
        (token.chars(), 10)
    };

    let chars = chars_trimmed.filter(|x| *x != '_');

    match BigInt::from_str_radix(chars.collect::<String>().as_str(), radix) {
        Ok(int) => Some(int),
        Err(_) => {
            env.tok_err(&tc.node(), TokenError::InvalidInteger);
            None
        }
    }
}

fn parse_bool(
    tc: &TreeCursor,
    env: &mut ParseEnv
) -> ParseResult<bool> {
    let node = &tc.node();
    let content = node_content(node, env)?;

    if content == "true" { Some(true) }
    else if content == "false" { Some(false) }
    else {
        env.tok_err(node, TokenError::InvalidBool);
        None
    }
}

fn parse_bytes(
    tc: &TreeCursor,
    env: &mut ParseEnv
) -> ParseResult<Vec<u8>>
{
    let node = &tc.node();
    let token = node_content(node, env)?;
    let mut chars = token.chars();
    let mut out: Vec<u8> = Vec::with_capacity(token.len() / 2);
    while let Some(c0) = chars.next() {
        if c0 == '_' {
            continue;
        }

        let c1 = match chars.next() {
            Some(c1) => c1,
            None => {
                env.tok_err(node, TokenError::UnevenBytes);
                None?
            }
        };

        let c0d = c0.to_digit(16)?;
        let c1d = c1.to_digit(16)?;

        let byte = c0d * 16 + c1d;

        out.push(byte as u8)
    }
    Some(out)
}

fn parse_qual(
    tc: &mut TreeCursor,
    env: &mut ParseEnv) ->
ParseResultN<ast::QName> {
        let node = &tc.node();
        let path = parse_fields(tc, env, &parse_name, "path");
        let name = parse_field(tc, env, &parse_name, "name");
        Some(mk_node(node, ast::QName {
            path: mk_node(node, path),
            name: name?,
        }))
}

fn parse_literal(
    tc: &mut TreeCursor,
    env: &mut ParseEnv
) -> ParseResultN<ast::Literal> {
    use ast::Literal;
    let node = &tc.node();
    let lit = match node.kind() {
        "lit_constructor" => {
            let lit = parse_qual(tc, env);
            Literal::Constructor { val: lit?.node }
        }
        "lit_bytes" => {
            let lit = parse_bytes(tc, env);
            Literal::Bytes { val: lit? }
        }
        "lit_lambda_op" => {
            let op = parse_binop(tc, env);
            Literal::LambdaBinOp { val: op? }
        }
        "lit_integer" => {
            let lit = parse_int(tc, env);
            Literal::Int { val: lit? }
        }
        "lit_bool" => {
            let lit = parse_bool(tc, env);
            Literal::Bool { val: lit? }
        }
        "lit_empty_map_or_record" => {
            Literal::EmptyMapOrRecord
        }
        "lit_string" => {
            let lit = parse_str(tc, env);
            Literal::String { val: lit? }
        }
        "lit_char" => {
            let lit = parse_char(tc, env);
            Literal::Char { val: lit? }
        }
        e => {
            panic!("Unknown literal node: {}", e)
        }
    };
    Some(mk_node(node, lit))
}

fn ann(node: &tree_sitter::Node) -> ast::Ann {
    let Point{row: start_line, column: start_col} = node.start_position();
    let Point{row: end_line, column: end_col} = node.end_position();
    ast::Ann {
        start_line: start_line as u32,
        start_col: start_col as u32,
        start_byte: node.start_byte() as u32,
        end_line: end_line as u32,
        end_col: end_col as u32,
        end_byte: node.end_byte() as u32,
        filename: "<filename mock>".to_string(),
    }
}

fn mk_node<T: Clone>(node: &tree_sitter::Node, value: T) -> ast::Node<T> {
    ast::Node {
        node: value,
        ann: ann(node),
    }
}

fn parse_children<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    mut filter: F,
) -> Vec<T>
where
    F: FnMut(&tree_sitter::TreeCursor<'a>) -> bool,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
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

fn parse_child<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    filter: F,
) -> ParseResult<T>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
    F: FnMut(&tree_sitter::TreeCursor) -> bool
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

fn parse_idx<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    idx: usize,
) -> ParseResult<T>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
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

fn parse_field<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> ParseResult<T>
where P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let child = parse_child(tc, env, &parse, |c| {
        c.field_name() == Some(name)
    });

    if child.is_none() {
        env.err(&tc.node(), ParseError::MissingField(name.to_string()));
    }

    child
}

fn parse_fields<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> Vec<T>
where
    T: Clone,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    parse_children(tc, env, parse, |c| c.field_name() == Some(name))
}

fn parse_kinds<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> Vec<T>
where
    T: Clone,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    parse_children(tc, env, parse, |c| c.node().kind() == name)
}

fn parse_fields_in_field<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
    name_in: &str,
) -> ParseResult<ast::NodeMany<T>>
where P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>
{
    parse_field(tc, env, &|tc, env| {
        let children = parse_fields(tc, env, parse, name_in);
        Some(mk_node(&tc.node(), children))
    }, name)
}

fn parse_module<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Module> {
    let node = &tc.node();
    let pragmas = parse_kinds(tc, env, &parse_pragma, "top_pragma");
    let includes = parse_kinds(tc, env, &parse_include, "include");
    let usings = parse_kinds(tc, env, &parse_using, "using");
    let scopes = parse_kinds(tc, env, &parse_scope_decl, "scope_declaration");
    Some(mk_node(node, ast::Module {
        pragmas,
        includes,
        usings,
        scopes,
    }))
}

fn parse_using<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Using> {
    let node = &tc.node();
    let scope = parse_field(tc, env, &parse_qual, "scope");
    let select = parse_field(tc, env, &parse_using_select, "select");
    Some(mk_node(node, ast::Using {
        scope: scope?,
        select: select.unwrap_or(mk_node(node, ast::UsingSelect::All)),
    }))
}

fn parse_using_select<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::UsingSelect> {
    let node = &tc.node();
    let select = match node.kind() {
        "using_as" => {
            let name = parse_field(tc, env, &parse_name, "name");
            ast::UsingSelect::Rename(name?)
        }
        "using_hiding" => {
            let names = parse_fields_in_field(tc, env, &parse_name, "names", "name");
            ast::UsingSelect::Exclude(names?)

        }
        "using_for" => {
            let names = parse_fields_in_field(tc, env, &parse_name, "names", "name");
            ast::UsingSelect::Include(names?)
        }
        e => panic!("Unknown using select: {}", e)
    };
    Some(mk_node(node, select))
}

fn parse_pragma<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Pragma> {
    let node = &tc.node();
    let op = parse_field(tc, env, &parse_binop, "op");
    let vsn = parse_fields_in_field(tc, env, &parse_name, "version", "subver");
    Some(mk_node(node, ast::Pragma::CompilerVsn {
        op: op?,
        vsn: vsn?
    }))
}

fn parse_include<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Include> {
    let node = &tc.node();
    let path = parse_field(tc, env, &parse_str, "path");
    Some(mk_node(node, ast::Include{
        path: path?,
    }))
}

fn parse_scope_decl<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::ScopeDecl> {
    use ast::ScopeDecl;
    let node = &tc.node();

    let modifiers = parse_fields(tc, env, &parse_name, "modifier");
    let head = parse_field(tc, env, &parse_name, "head");
    let is_interface = node.child_by_field_name("interface").is_some();
    let name = parse_field(tc, env, &parse_name, "name");
    let impls = parse_fields(tc, env, &parse_qual, "implements");

    let Modifiers {
        is_payable,
        is_stateful,
        is_private,
        is_main,
    } = parse_modifiers(env, &modifiers);

    let sdecl = match head?.node.as_str() {
        "contract" if !is_interface => {
            if is_private || is_stateful {
                env.err(node, ParseError::InvalidModifier);
            }

            let decls = parse_fields(tc, env, &parse_decl_in_contract, "decl");

            ScopeDecl::Contract {
                name: name?,
                main: is_main,
                payable: is_payable,
                implements: impls,
                decls
            }
        }
        "contract" if is_interface => {
            if is_payable || is_private || is_stateful || is_main {
                env.err(node, ParseError::InvalidModifier);
            }

            unimplemented!("contract interface")
        }
        "namespace" => {
            if is_payable || is_private || is_stateful || is_main || is_interface {
                env.err(node, ParseError::InvalidModifier);
            }

            if !impls.is_empty() {
                env.err(node, ParseError::NamespaceImpl)
            }

            unimplemented!("namespace")
        }
        e => {
            panic!("Unknown scope header: {}", e)
        }
    };
    Some(mk_node(node, sdecl))
}

struct Modifiers {
    is_payable: bool,
    is_stateful: bool,
    is_private: bool,
    is_main: bool,
}

fn parse_modifiers(
    env: &mut ParseEnv,
    modifiers: &Vec<ast::Node<String>>,
) -> Modifiers {
    let mut is_payable = false;
    let mut is_stateful = false;
    let mut is_private = false;
    let mut is_main = false;

    for m in modifiers {
        match m.node.as_str() {
            "payable" => is_payable = true,
            "stateful" => is_stateful = true,
            "private" => is_private = true,
            "main" => is_main = true,
            e => panic!("Unknown modifier: {}", e)
        }
    }

    Modifiers {
        is_payable,
        is_stateful,
        is_private,
        is_main,
    }
}

fn parse_decl_in_contract<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::InContractDecl> {
    use ast::InContractDecl::*;
    let node = &tc.node();
    match node.kind() {
        "function_declaration" => {
            let decl = parse_function_declaration(tc, env)?;
            Some(decl.map(FunDef))
        }
        _ => {
            unimplemented!("fun decl in ct")
        }
    }
}

fn parse_function_declaration<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::FunDef>
{
    let node = &tc.node();
    let modifiers = parse_fields(tc, env, &parse_name, "modifier");
    let head = parse_field(tc, env, &parse_name, "head");
    let clauses = parse_fields(tc, env, &parse_function_clause, "clause");

    let Modifiers {
        is_payable,
        is_stateful,
        is_private,
        is_main,
    } = parse_modifiers(env, &modifiers);

    if is_private || is_main {
        env.err(node, ParseError::InvalidModifier);
    }

    let is_entrypoint = match head?.node.as_str() {
        "entrypoint" => true,
        "function" => false,
        _ => {
            env.err(node, ParseError::InvalidModifier);
            false
        }
    };

    let mut fun_name: Option<ast::Node<ast::Name>> = None;

    let mut fun_clauses = Vec::with_capacity(clauses.len());
    let mut fun_signature = None;

    for (clause_name, clause) in clauses {
        match fun_name {
            None => fun_name = Some(clause_name.clone()),
            Some(n) => {
                if n.node != clause_name.node {
                    env.err(node, ParseError::InconsistentName);
                }
                fun_name = Some(n)
            }
        }

        match clause {
            either::Left(clause_def) => {
                fun_clauses.push(clause_def)
            }
            either::Right(clause_sig) => {
                if fun_signature.is_some() {
                    env.err(node, ParseError::DuplicateSig);
                } else {
                    fun_signature = Some(clause_sig.node.signature);
                }
            }

        }
    };

    Some(mk_node(node, ast::FunDef {
        stateful: is_stateful,
        payable: is_payable,
        public: is_entrypoint,
        name: fun_name?,
        clauses: fun_clauses,
        signature: fun_signature
    }))
}

fn parse_function_clause(
    tc: &mut tree_sitter::TreeCursor,
    env: &mut ParseEnv,
) -> ParseResult<(ast::Node<ast::Name>, either::Either<ast::Node<ast::FunClause>, ast::Node<ast::FunSig>>)> {
    let node = &tc.node();
    let name = parse_field(tc, env, &parse_name, "name");
    let fun = match node.kind() {
        "function_clause" => {
            let args = parse_fields_in_field(tc, env, &parse_pattern, "args", "arg");
            let typ = parse_field(tc, env, &parse_type, "type");
            let body = parse_field(tc, env, &parse_expr, "body");
            either::Left(mk_node(node, ast::FunClause {
                args: args?,
                ret_type: typ,
                body: body?,
            }))
        }
        "function_signature" => {
            let typ = parse_field(tc, env, &parse_type, "type");
            either::Right(mk_node(node, ast::FunSig {
                signature: typ?,
            }))
        }
        _ => panic!("bad fun clause")
    };
    Some((name?, fun))
}


// fn parse_decl_in_contract_interface<'a>(
//     tc: &mut tree_sitter::TreeCursor<'a>,
//     env: &mut ParseEnv,
// ) -> ParseResultN<ast::InInterfaceDecl> {
//     use ast::InInterfaceDecl;
//     let node = &tc.node();
// }

// fn parse_decl_in_namespace<'a>(
//     tc: &mut tree_sitter::TreeCursor<'a>,
//     env: &mut ParseEnv,
// ) -> ParseResultN<ast::InNamespaceDecl> {
//     use ast::InNamespaceDecl;
//     let node = &tc.node();
// }

fn parse_type_def<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::TypeDef> {
    use ast::TypeDef;
    let node = &tc.node();
    let td = match node.kind() {
        "type_alias" => {
            let name = parse_field(tc, env, &parse_name, "name");
            let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
            let def = parse_field(tc, env, &parse_type, "type");
            TypeDef::Alias {
                name: name?,
                params: params?,
                def: def?,
            }
        }
        "record_declaration" => {
            let name = parse_field(tc, env, &parse_name, "name");
            let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
            let fields = parse_fields_in_field(tc, env, &parse_field_decl, "fields", "field");
            TypeDef::Record {
                name: name?,
                params: params?,
                fields: fields?,
            }
        }
        "variant_declaration" => {
            let name = parse_field(tc, env, &parse_name, "name");
            let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
            let constrs = parse_fields(tc, env, &parse_constructor_decl, "constructor");
            TypeDef::Variant {
                name: name?,
                params: params?,
                constructors: constrs,
            }
        }
        bad => panic!("Bad type def {}", bad)
    };

    Some(mk_node(node, td))
}

fn parse_field_decl<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::FieldDecl> {
    let node = &tc.node();
    let name = parse_field(tc, env, &parse_name, "name");
    let typ = parse_field(tc, env, &parse_type, "type");
    Some(mk_node(node, ast::FieldDecl {
        name: name?,
        typedecl: typ?,
    }))

}

fn parse_constructor_decl<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Constructor> {
    let node = &tc.node();
    let name = parse_field(tc, env, &parse_name, "name");
    let params = parse_fields_in_field(tc, env, &parse_type, "params", "param");
    Some(mk_node(node, ast::Constructor {
        name: name?,
        params: params?,
    }))
}


fn parse_expr<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Expr> {
    use ast::Expr;
    let node = &tc.node();
    let expr = match node.kind() {
        "expr_variable" => {
            let qname = parse_qual(tc, env);

            Expr::Var {
                var: qname?.node
            }
        }
        "expr_literal" => {
            let lit = parse_field(tc, env, &parse_literal, "literal");
            Expr::Literal { val: lit? }
        }
        "expr_tuple" => {
            let elems = parse_fields(tc, env, &parse_expr, "elem");
            Expr::Tuple {
                elems,
            }
        }
        "expr_list_literal" => {
            let elems = parse_fields(tc, env, &parse_expr, "elem");
            Expr::List {
                elems,
            }
        }
        "expr_lambda" => {
            let node_args = parse_fields_in_field(tc, env, &parse_pattern, "args", "arg");
            let body = parse_field(tc, env, &parse_expr, "body");

            Expr::Lambda {
                args: node_args?,
                body: body?.rec()
            }
        }
        "expr_typed" => {
            let e = parse_field(tc, env, &parse_expr, "expr");
            let t = parse_field(tc, env, &parse_type, "type");
            Expr::Typed {
                expr: e?.rec(),
                t: t?
            }
        }
        "expr_op" => {
            let op_l = parse_field(tc, env, &parse_expr, "op_l");
            let op_r_m = parse_field(tc, env, &parse_expr, "op_r");
            match op_r_m {
                Some(op_r) => {
                    let op = parse_field(tc, env, &parse_binop, "op");
                    Expr::BinOp {
                        op_l: op_l?.rec(),
                        op: op?,
                        op_r: op_r.rec(),
                    }
                }
                None => {
                    let op = parse_field(tc, env, &parse_unop, "op");
                    Expr::UnOp {
                        op_l: op_l?.rec(),
                        op: op?,
                    }
                }
            }
        }
        "expr_application" => {
            let fun = parse_field(tc, env, &parse_expr, "fun");
            let args = parse_fields_in_field(tc, env, &parse_expr_arg, "args", "arg");
            Expr::App {
                fun: fun?.rec(),
                args: args?,
            }
        }
        "expr_record" => {
            let fields = parse_fields(tc, env, &parse_record_field_assign, "field");
            Expr::Record {
                fields,
            }
        }
        "expr_record_update" => {
            let updates = parse_fields_in_field(tc, env, &parse_record_field_update, "updates", "update");

            let record = parse_field(tc, env, &parse_expr, "record");

            Expr::RecordUpdate {
                record: record?.rec(),
                updates: updates?,
            }
        }
        "expr_map" => {
            let assigns = parse_fields(tc, env, &parse_map_field_assign, "field");
            Expr::Map {
                assigns,
            }
        }

        "expr_map_update" => {
            let map = parse_field(tc, env, &parse_expr, "map");
            let updates = parse_fields_in_field(tc, env, &parse_map_field_update, "fields", "field");
            Expr::MapUpdate {
                map: map?.rec(),
                updates: updates?,
            }
        }
        "expr_map_access" => {
            let map = parse_field(tc, env, &parse_expr, "map");
            let key = parse_field(tc, env, &parse_expr, "key");
            Expr::MapAccess {
                map: map?.rec(),
                key: key?.rec(),
                default: None, //todo
            }
        }
        "expr_projection" => {
            let expr = parse_field(tc, env, &parse_expr, "record");
            let field = parse_field(tc, env, &parse_name, "field");
            Expr::Proj {
                expr: expr?.rec(),
                field: field?,
            }
        }
        "expr_switch" => {
            let node_exprs = parse_fields_in_field(tc, env, &parse_expr, "exprs", "expr");
            let node_cases = parse_fields_in_field(tc, env, &parse_case, "cases", "case");

            Expr::Switch {
                exprs: node_exprs?,
                cases: node_cases?.node,
            }
        }
        "expr_if" => {
            let conds = parse_fields(tc, env, &parse_expr, "cond");
            let thens = parse_fields(tc, env, &parse_expr, "then");
            let neg = parse_field(tc, env, &parse_expr, "neg");
            let zipped: Vec<_> = conds.iter().zip(thens.iter()).map(|(c, t)| {
                mk_node(node, ast::ExprCond {
                    cond: c.to_owned(),
                    pos: t.to_owned(),
                })
            }).collect();
            Expr::If {
                conds: zipped,
                neg: neg?.rec()
            }
        }
        "expr_block" => {
            use ast::Statement;
            let mut all_stmts = parse_stmts(tc, env)?;
            let last = all_stmts.pop()?;

            match last.node {
                Statement::Expr{expr: expr_last} => {
                    let stmts = &all_stmts[..all_stmts.len() - 1];
                    Expr::Block {
                        stmts: stmts.to_vec(),
                        value: expr_last.rec()
                    }
                }
                _ => {
                    env.err(node, ParseError::BlockNoExpr);
                    None?
                }
            }
        }
        "expr_hole" => {
            Expr::Hole
        }
        e => panic!("Unknown expression node: {}", e)
    };
    Some(mk_node(node, expr))
}

fn parse_case<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Case> {
    let node = &tc.node();
    let pattern = parse_field(tc, env, &parse_pattern, "pattern");
    let branches = parse_fields(tc, env, &parse_case_branch, "branch");
    Some(mk_node(node, ast::Case {
        pattern: pattern?,
        branches,
    }))
}

fn parse_case_branch<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::CaseBranch> {
    let node = &tc.node();
    let guards = parse_fields(tc, env, &parse_expr, "guard");
    let body = parse_field(tc, env, &parse_expr, "body");
    Some(mk_node(node, ast::CaseBranch {
        guards: mk_node(node, guards),
        body: body?,
    }))
}


fn parse_binop<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::BinOp> {
    use ast::BinOp::*;
    let node = &tc.node();
    let content = node_content(node, env)?;
    let op = match content.as_str() {
        "+" => Add,
        "-" => Sub,
        "*" => Mul,
        "/" => Div,
        "mod" => Mod,
        "^" => Pow,
        "<" => LT,
        ">" => GT,
        "=<" => LE,
        ">=" => GE,
        "==" => EQ,
        "!=" => NE,
        "&&" => And,
        "||" => Or,
        "::" => Cons,
        "++" => Concat,
        e => panic!("Unknown binary op: {}", e)
    };
    Some(mk_node(node, op))
}

fn parse_unop<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::UnOp> {
    use ast::UnOp::*;
    let node = &tc.node();
    let content = node_content(node, env)?;
    let op = match content.as_str() {
        "-" => Neg,
        "!" => Not,
        e => panic!("Unknown unary op: {}", e)
    };
    Some(mk_node(node, op))
}

fn parse_expr_arg<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::ExprArg> {
    let node = &tc.node();
    let arg = match node.kind() {
        "expr_named_argument" => {
            let name = parse_field(tc, env, &parse_name, "name");
            let value = parse_field(tc, env, &parse_expr, "value");
            ast::ExprArg::NamedArg {
                name: name?,
                val: value?,
            }
        },
        _ => {
            let value = parse_expr(tc, env);
            ast::ExprArg::Arg {
                val: value?
            }
        }
    };
    Some(mk_node(node, arg))
}

fn parse_record_field_assign<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::RecordFieldAssign> {
    let node = &tc.node();
                let field = parse_field(tc, env, &parse_name, "field");
                let value = parse_field(tc, env, &parse_expr, "value");
                Some(mk_node(node, ast::RecordFieldAssign{
                    field: field?,
                    value: value?,
                }))
}

fn parse_record_field_update<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::RecordFieldUpdate> {
    let node = &tc.node();
    let new_value = parse_field(tc, env, &parse_expr, "new_value");
    let old_value = parse_field(tc, env, &parse_name, "old_value");
    let path = parse_fields_in_field(tc, env, &parse_name, "path", "field");
    Some(mk_node(node, ast::RecordFieldUpdate{
        new_value: new_value?,
        old_value,
        path: path?,
    }))
}

fn parse_map_field_assign<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::MapFieldAssign> {
    let node = &tc.node();
    let key = parse_field(tc, env, &parse_expr, "key");
                let value = parse_field(tc, env, &parse_expr, "value");
                Some(mk_node(node, ast::MapFieldAssign{
                    key: key?,
                    value: value?,
                }))
}

fn parse_map_field_update<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::MapFieldUpdate> {
    let node = &tc.node();
    let key = parse_field(tc, env, &parse_expr, "key");
    let old_value = parse_field(tc, env, &parse_name, "old_value");
    let new_value = parse_field(tc, env, &parse_expr, "new_value");
    Some(mk_node(node, ast::MapFieldUpdate{
        key: key?,
        old_value,
        new_value: new_value?,
    }))
}

fn parse_name<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Name> {
    let node = &tc.node();
    Some(mk_node(node, node_content(node, env)?))
}

fn parse_type<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Type>{
    use ast::Type;
    env.check_error(tc)?;
    let node = &tc.node();
    let t = match node.kind() {
        "type_application" => {
            let fun = parse_field(tc, env, &parse_type, "fun");
            let args = parse_fields(tc, env, &parse_type, "params");
            Type::App {
                fun: fun?.rec(),
                args: mk_node(node, args),
            }
        },
        "type_function" => {
            let args = parse_fields(tc, env, &parse_type, "domain");
            let ret = parse_field(tc, env, &parse_type, "codomain");
            Type::Fun {
                named_args: vec![],
                args: mk_node(node, args),
                ret: ret?.rec()
            }
        },
        "type_paren" => {
            parse_field(tc, env, &parse_type, "type")?.node
        },
        "type_tuple" => {
            let elems = parse_fields(tc, env, &parse_type, "elem");
            Type::Tuple {
                elems,
            }
        },
        "type_variable" => {
            let name = node_content(node, env);
            Type::Var {
                name: mk_node(node, name?),
            }
        },
        "type_variable_poly" => {
            let name = node_content(node, env);
            Type::PolyVar {
                name: mk_node(node, name?),
            }
        },
        e => panic!("Unknown type node: {}", e)
    };
    Some(mk_node(node, t))
}

fn parse_pattern<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Pattern> {
    use ast::Pattern;
    env.check_error(tc)?;
    let node = &tc.node();
    let pat = match node.kind() {
        "expr_application" => {
            let fun = parse_field(tc, env, &parse_name, "fun");
            let args = parse_fields_in_field(tc, env, &parse_pattern, "args", "arg");
            Pattern::App {
                fun: fun?,
                args: args?,
            }
        },
        "expr_match" => {
            let name = parse_field(tc, env, &parse_name, "lvalue");
            let pat = parse_field(tc, env, &parse_pattern, "rvalue");
            Pattern::Let {
                name: name?,
                pat: pat?.rec(),
            }
        },
        "expr_list_literal" => {
            let elems = parse_fields(tc, env, &parse_pattern, "elem");
            Pattern::List {
                elems,
            }
        },
        "expr_literal" => {
            let lit = parse_field(tc, env, &parse_literal, "literal");
            Pattern::Lit {
                value: lit?
            }
        },
        "expr_op" => {
            let op_l = parse_field(tc, env, &parse_pattern, "op_l");
            let op_r = parse_field(tc, env, &parse_pattern, "op_r");
            let op = parse_field(tc, env, &parse_binop, "op");
            Pattern::Op {
                op_l: op_l?.rec(),
                op: op?,
                op_r: op_r?.rec(),
            }
        },
        "expr_record" => {
            let fields = parse_fields(tc, env, &parse_pat_field, "field");
            Pattern::Record {
                fields,
            }
        },
        "expr_tuple" => {
            let elems = parse_fields(tc, env, &parse_pattern, "elem");
            Pattern::Tuple {
                elems,
            }
        },
        "expr_typed" => {
            let p = parse_field(tc, env, &parse_pattern, "pattern");
            let t = parse_field(tc, env, &parse_type, "type");
            Pattern::Typed {
                pat: p?.rec(),
                t: t?
            }
        },
        "expr_variable" => {
            let name = node_content(node, env);
            Pattern::Var {
                name: mk_node(node, name?),
            }
        },
        "expr_wildcard" => {
            Pattern::Wildcard
        },
        e => panic!("Unknown pattern node: {}", e)
    };
    Some(mk_node(node, pat))
}

fn parse_pat_field<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::PatternRecordField> {
    let node = &tc.node();
    let path = parse_fields_in_field(tc, env, &parse_name, "fields", "field");
    let pattern = parse_field(tc, env, &parse_pattern, "pattern");
    Some(mk_node(node, ast::PatternRecordField{
        path: path?,
        pattern: pattern?,
    }))
}


fn parse_stmts<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResult<ast::Nodes<ast::Statement>> {
    use ast::Statement;
    let node = &tc.node();
    let mut part_stmts = parse_fields(tc, env, &parse_split_stmt, "stmt");
    part_stmts.reverse();

    let mut stmts: Vec<ast::Node<ast::Statement>> = Vec::with_capacity(part_stmts.len());
    while let Some(psn) = part_stmts.pop() {
        let ps = psn.node;
        let ann = psn.ann;
        match ps {
            SplitStmt::Full(s) => {
                stmts.push(ast::Node{node: s, ann});
            }
            SplitStmt::PartElIf(cond) => {
                let last = stmts.last_mut();
                match last {
                    Some(ast::Node{node: Statement::If{conds: ref mut conds, neg: None}, ..}) => {
                        conds.push(cond);
                    }
                    _ => {
                        env.err(node, ParseError::LonelyElIf)
                    }
                }
            }
            SplitStmt::PartElse(expr) => {
                match stmts.pop() {
                    Some(ast::Node{node: Statement::If{neg: ref mut neg@None, ..}, ..}) => {
                        *neg = Some(expr)
                    }
                    _ => {
                        env.err(node, ParseError::LonelyElse)
                    }
                }
            }
        }
    }
    Some(stmts)
}

#[derive(Clone, Debug)]
enum SplitStmt {
    Full(ast::Statement),
    PartElIf(ast::Node<ast::ExprCond>),
    PartElse(ast::Node<ast::Expr>),
}

fn parse_split_stmt<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<SplitStmt> {
    use ast::Statement;
    env.check_error(tc)?;
    let node = &tc.node();
    let stmt = match node.kind() {
        "stmt_expr" => {
            let expr = parse_field(tc, env, &parse_expr, "expr");
            SplitStmt::Full(Statement::Expr {
                expr: expr?
            })
        }
        "stmt_letval" => {
            let pattern = parse_field(tc, env, &parse_pattern, "pattern");
            let value = parse_field(tc, env, &parse_expr, "value");
            SplitStmt::Full(Statement::Let {
                pattern: pattern?,
                body: value?,
            })
        }
        "stmt_if" => {
            let cond = parse_field(tc, env, &parse_expr, "cond");
            let then = parse_field(tc, env, &parse_expr, "then");
            let conds = vec![mk_node(node, ast::ExprCond {
                cond: cond?,
                pos: then?,
            })];
            SplitStmt::Full(Statement::If {
                conds,
                neg: None,
            })
        }
        "stmt_elif" => {
            let cond = parse_field(tc, env, &parse_expr, "cond");
            let then = parse_field(tc, env, &parse_expr, "then");
            SplitStmt::PartElIf(mk_node(node, ast::ExprCond {
                cond: cond?,
                pos: then?,
            }))
        }
        "stmt_else" => {
            let expr = parse_field(tc, env, &parse_expr, "else");
            SplitStmt::PartElse(expr?)
        }
        e => panic!("Unknown statement node: {}", e)
    };
    Some(mk_node(node, stmt))
}

/// Checks if the node is an error (or missing) node and if so, runs a fallback parser to collect
/// errors. Returns true if The node was ineed an error, false otherwise.
fn parse_error<'a>(tc: &mut TreeCursor<'a>, env: &mut ParseEnv) -> bool {
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

fn parse_any<'a>(
    tc: &mut TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<bool> {
    let node = &tc.node();
    let kind = node.kind();

    let sub_err = match env.subtypes.get(kind) {
        Some(sub) => {
            match sub.as_str() {
                // "_expression" => parse_expr(tc, env).is_some(),
                // "_pattern" => parse_pattern(tc, env).is_some(),
                // "_type" => parse_type(tc, env).is_some(),
                // "_literal" => parse_literal(tc, env).is_some(),
                // // "_list_comprehension_filter" => parse_list_comp(tc, env).is_some(),
                // "_operator" => parse_binop(tc, env).is_some(),
                // // "_scoped_declaration" => parse_scoped_declaration(tc, env).is_some(),
                // "_statement" => parse_split_stmt(tc, env).is_some(),
                // // "_top_decl" => parse_top_decl(tc, env).is_some(),
                // "_type_definition" => parse_type_def(tc, env).is_some(),
                _ => {
                    parse_children(tc, env, &parse_any, |_| true);
                    false
                }
            }
        }
        None => {
            parse_children(tc, env, &parse_any, |_| true);
            false
        }
    };

    None
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

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

        let src = std::fs::read_to_string("C.aes").unwrap();
        let ast = parser.parse(&src, None).unwrap();
        let root = ast.root_node();

        println!("SEXP: {}\n\n", root.to_sexp());

        let errs = vec![];
        let src_data: Vec<u16> = src.encode_utf16().collect();

        let mut env = ParseEnv {
            src: &src_data,
            errs,
            subtypes,
            has_errors: root.has_error(),
        };

        let mut tc = ast.walk();

        let node = if root.has_error() {
            println!("PARSING ERROR...");
            parse_error(&mut tc, &mut env); None
        } else {
            let kind = parse_field(&mut tc, &mut env, &|tc, env| node_content(&tc.node(), env), "kind").unwrap_or("module".to_string());
            println!("PARSING {}", kind);
            parse_field(&mut tc, &mut env, &parse_expr, &kind)
        };

        println!("ERRORS:");
        for en in env.errs {
            println!("ERROR: {:?} at", en.node);
            println!("---\n{}\n---", String::from_utf16(&src_data[en.ann.start_byte as usize ..en.ann.end_byte as usize]).unwrap_or("idk".to_string()));
        }

        println!("\nOUTPUT:");
        if let Some(out) = node {
            println!("SUCCESS {:#?}", out);
        } else {
            println!("FAILED PARSE");

        }

        panic!("done :)");
    }
}
