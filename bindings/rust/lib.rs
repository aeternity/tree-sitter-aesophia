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
    BlockNoExpr(ast::Ann),
    LonelyElIf,
    LonelyElse,
}

type ParseErrors = Vec<ParseError>;
type SubtypeMap = std::collections::HashMap<String, String>;

#[derive(Clone, Debug)]
pub struct ParseEnv<'a> {
    src: &'a [u16],
    subtypes: SubtypeMap,
    errs: ParseErrors,
    has_errors: bool,
}

impl ParseEnv<'_> {
    fn err(&mut self, err: ParseError) {
        self.errs.push(err);
        self.has_errors = true;
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
            let range = node.byte_range();
            env.err(ParseError::InvalidUtf16(range.start..range.end));
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
    env.check_error(tc)?;
    let node = &tc.node();
    let content = node_content(node, env)?;
    let lit = match node.kind() {
        // "lit_constructor" => Literal::Constructor { val: content },
        "lit_bytes" => {
            let lit = parse_bytes(&content);

            if lit.is_none() {
                env.err(ParseError::InvalidToken(content));
            }

            Literal::Bytes { val: lit? }
        }
        "lit_lambda_op" => {
            let op = parse_bin_op(&content);

            if op.is_none() {
                env.err(ParseError::InvalidToken(content));
            }

            Literal::LambdaBinOp { val: op? }
        }
        "lit_integer" => {
            let lit = parse_int(&content);

            if lit.is_none() {
                env.err(ParseError::InvalidToken(content));
            }

            Literal::Int { val: lit? }
        }
        "lit_bool" => {
            if content == "true" {
                Literal::Bool { val: true }
            } else if content == "false" {
                Literal::Bool { val: false }
            } else {
                env.err(ParseError::InvalidToken(content));
                None?
            }
        }
        "lit_empty_map_or_record" => Literal::EmptyMapOrRecord,
        "lit_string" => {
            let lit = parse_str(&content);

            if lit.is_none() {
                env.err(ParseError::InvalidToken(content));
            }

            Literal::String { val: lit? }
        }
        "lit_char" => {
            let lit = parse_char(&content);

            if lit.is_none() {
                env.err(ParseError::InvalidToken(content));
                None?
            } else {
                Literal::Char {
                    val: parse_char(&content)?,
                }
            }
        }
        _ => {
            env.err(ParseError::InvalidToken(content));
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

fn node_box<T: Clone>(node: ast::Node<T>) -> ast::NodeBox<T> {
    ast::Node {
        node: Box::new(node.node),
        ann: node.ann,
    }
}

fn mk_node_box<T: Clone>(node: &tree_sitter::Node, value: T) -> ast::NodeBox<T> {
    ast::Node {
        node: Box::new(value),
        ann: ann(node),
    }
}

fn mk_node_many<T: Clone>(node: &tree_sitter::Node, values: Vec<ast::Node<T>>) -> ast::NodeMany<T> {
    ast::NodeMany {
        nodes: values,
        ann: ann(node),
    }
}

fn re_node<T1: Clone, T2: Clone>(n1: ast::Node<T1>, t: T2) -> ast::Node<T2> {
    ast::Node {
        node: t,
        ann: n1.ann,
    }
}

fn parse_children<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    mut filter: F,
) -> ParseResult<Vec<T>>
where
    F: FnMut(&tree_sitter::TreeCursor) -> bool,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let mut children = Vec::with_capacity(tc.node().child_count());
    let mut child_found = tc.goto_first_child();

    if !child_found {
        return Some(vec![]);
    }

    while child_found {
        if env.has_errors && !parse_error(tc, env) && filter(tc) {
            children.push(parse(tc, env));
        }
        child_found = tc.goto_next_sibling();
    }

    tc.goto_parent();

    let nodes: Option<Vec<_>> = children.into_iter().collect();
    nodes
}

fn parse_child<'a, T: Clone, F, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    filter: F,
) -> Option<ParseResult<T>>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
    F: FnMut(&tree_sitter::TreeCursor) -> bool
{
    let nodes = parse_children(tc, env, &parse, filter);

    let node_vec = nodes?;

    if !node_vec.is_empty() {
        let node = node_vec[0].clone();
        Some(Some(node))
    } else {
        None
    }
}

fn parse_opt_child_by_idx<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: P,
    idx: usize,
) -> Option<ParseResult<T>>
where
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
{
    let mut i = 0;
    parse_child(tc, env, &parse, |_| {
        i += 1;
        idx == i - 1
    })
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
    match parse_opt_child_by_idx(tc, env, parse, idx) {
        None => {
            env.err(ParseError::MissingChild(idx));
            None
        },
        Some(v) => v
    }
}

fn parse_opt_child_by_field<T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor,
    env: &mut ParseEnv,
    parse: P,
    name: &str,
) -> Option<ParseResult<T>>
where P: Fn(&mut tree_sitter::TreeCursor, &mut ParseEnv) -> ParseResult<T>,
{
    parse_child(tc, env, &parse, |c| {
        c.field_name() == Some(name)
    })
}

fn parse_child_by_field<T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor,
    env: &mut ParseEnv,
    parse: P,
    name: &str,
) -> ParseResult<T>
where P: Fn(&mut tree_sitter::TreeCursor, &mut ParseEnv) -> ParseResult<T>,
{
    match parse_opt_child_by_field(tc, env, parse, name) {
        None => {
            env.err(ParseError::MissingField(name.to_string()));
            None
        },
        Some(v) => v
    }
}

fn parse_children_by_field<'a, T, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
) -> ParseResult<Vec<T>>
where
    T: Clone,
    P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResult<T>,
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

fn parse_fields_in_field<'a, T: Clone, P>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
    parse: &P,
    name: &str,
    name_in: &str,
) -> ParseResult<ast::NodeMany<T>>
where P: Fn(&mut tree_sitter::TreeCursor<'a>, &mut ParseEnv) -> ParseResultN<T>
{
    parse_child_by_field(tc, env, |tc, env| {
        let children = parse_children_by_field(tc, env, parse, name_in)?;
        Some(mk_node_many(&tc.node(), children))
    }, name)
}

fn parse_expr<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Expr> {
    use ast::Expr;
    env.check_error(tc)?;
    let node = &tc.node();
    let expr = match node.kind() {
        "expr_variable" => {
            let content = node_content(node, env)?;
            let qual = parse_qual(&content);

            if qual.is_none() {
                env.err(ParseError::InvalidToken(content));
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
            let node_args = parse_fields_in_field(tc, env, parse_pattern, "args", "arg");
            let body = parse_child_by_field(tc, env, parse_expr, "body");

            Expr::Lambda {
                args: node_args?,
                body: node_box(body?)
            }
        }
        "expr_typed" => {
            let e = parse_child_by_field(tc, env, parse_expr, "expr");
            let t = parse_child_by_field(tc, env, parse_type, "type");
            Expr::Typed {
                expr: node_box(e?),
                t: t?
            }
        }
        "expr_op" => {
            let op_l = parse_child_by_field(tc, env, parse_expr, "op_l");
            let op_r_m = parse_opt_child_by_field(tc, env, parse_expr, "op_r");
            match op_r_m {
                Some(op_r) => {
                    let op = parse_child_by_field(tc, env, parse_binop, "op");
                    Expr::BinOp {
                        op_l: node_box(op_l?),
                        op: op?,
                        op_r: node_box(op_r?),
                    }
                }
                None => {
                    let op = parse_child_by_field(tc, env, parse_unop, "op");
                    Expr::UnOp {
                        op_l: node_box(op_l?),
                        op: op?,
                    }
                }
            }
        }
        "expr_application" => {
            let fun = parse_child_by_field(tc, env, parse_expr, "fun");
            let args = parse_fields_in_field(tc, env, parse_expr_arg, "args", "arg");
            Expr::App {
                fun: node_box(fun?),
                args: args?,
            }
        }
        "expr_record" => {
            let fields = parse_fields_in_field(tc, env, parse_record_field_assign, "fields", "field");
            Expr::Record {
                fields: fields?,
            }
        }
        "expr_record_update" => {
            let updates = parse_fields_in_field(tc, env, parse_record_field_update, "updates", "update");

            let record = parse_child_by_field(tc, env, parse_expr, "record");

            Expr::RecordUpdate {
                record: node_box(record?),
                updates: updates?,
            }
        }
        "expr_map" => {
            let assigns = parse_children_by_field(tc, env, parse_map_field_assign, "field");
            Expr::Map {
                assigns: mk_node_many(node, assigns?),
            }
        }

        "expr_map_update" => {
            let map = parse_child_by_field(tc, env, parse_expr, "map");
            let updates = parse_fields_in_field(tc, env, parse_map_field_update, "fields", "field");
            Expr::MapUpdate {
                map: node_box(map?),
                updates: updates?,
            }
        }
        "expr_map_access" => {
            let map = parse_child_by_field(tc, env, parse_expr, "map");
            let key = parse_child_by_field(tc, env, parse_expr, "key");
            Expr::MapAccess {
                map: node_box(map?),
                key: node_box(key?),
                default: None, //todo
            }
        }
        "expr_projection" => {
            let expr = parse_child_by_field(tc, env, parse_expr, "record");
            let field = parse_child_by_field(tc, env, parse_name, "field");
            Expr::Proj {
                expr: node_box(expr?),
                field: field?,
            }
        }
        "expr_switch" => {
            let node_exprs = parse_fields_in_field(tc, env, parse_expr, "exprs", "expr");
            let node_cases = parse_fields_in_field(tc, env, parse_case, "cases", "case");

            Expr::Switch {
                exprs: node_exprs?,
                cases: node_cases?,
            }
        }
        "expr_if" => {
            let conds = parse_children_by_field(tc, env, parse_expr, "cond");
            let thens = parse_children_by_field(tc, env, parse_expr, "then");
            let neg = parse_child_by_field(tc, env, parse_expr, "neg");
            let zipped: Vec<_> = conds?.iter().zip(thens?.iter()).map(|(c, t)| {
                mk_node(node, ast::ExprCond {
                    cond: c.to_owned(),
                    pos: t.to_owned(),
                })
            }).collect();
            Expr::If {
                conds: mk_node_many(node, zipped),
                neg: node_box(neg?)
            }
        }
        "expr_block" => {
            use ast::Statement;
            let all_stmts_n = parse_stmts(tc, env)?;
            let mut all_stmts = all_stmts_n.nodes;
            let last = all_stmts.pop()?;

            match last.node {
                Statement::Expr{expr: expr_last} => {
                    let stmts = &all_stmts[..all_stmts.len() - 1];
                    Expr::Block {
                        stmts: mk_node_many(node, stmts.to_vec()),
                        value: node_box(expr_last)
                    }
                }
                _ => {
                    env.err(ParseError::BlockNoExpr(last.ann));
                    None?
                }
            }
        }
        "expr_hole" => {
            Expr::Hole
        }
        _ => {
            let content = node_content(node, env)?;
            env.err(ParseError::InvalidToken(content));
            None?
        }
    };
    Some(mk_node(node, expr))
}

fn parse_case<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::Case> {
    let node = &tc.node();
    let pattern = parse_child_by_field(tc, env, parse_pattern, "pattern");
    let branches = parse_children_by_field(tc, env, parse_case_branch, "branch");
    Some(mk_node(node, ast::Case {
        pattern: pattern?,
        branches: mk_node_many(node, branches?),
    }))
}

fn parse_case_branch<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::CaseBranch> {
    let node = &tc.node();
    let guards = parse_children_by_field(tc, env, parse_expr, "guard");
    let body = parse_child_by_field(tc, env, parse_expr, "body");
    Some(mk_node(node, ast::CaseBranch {
        guards: mk_node_many(node, guards?),
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
        _ => {
            env.err(ParseError::InvalidToken(content));
            None?
        }
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
        _ => {
            env.err(ParseError::InvalidToken(content));
            None?
        }
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
            let name = parse_child_by_field(tc, env, parse_name, "name");
            let value = parse_child_by_field(tc, env, parse_expr, "value");
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
                let field = parse_child_by_field(tc, env, parse_name, "field");
                let value = parse_child_by_field(tc, env, parse_expr, "value");
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
    let new_value = parse_child_by_field(tc, env, parse_expr, "new_value");
                let old_value = parse_opt_child_by_field(tc, env, parse_name, "old_value");
                let path = parse_fields_in_field(tc, env, parse_name, "path", "field");
                Some(mk_node(node, ast::RecordFieldUpdate{
                    new_value: new_value?,
                    old_value: old_value?,
                    path: path?,
                }))
}

fn parse_map_field_assign<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::MapFieldAssign> {
    let node = &tc.node();
    let key = parse_child_by_field(tc, env, parse_expr, "key");
                let value = parse_child_by_field(tc, env, parse_expr, "value");
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
    let key = parse_child_by_field(tc, env, parse_expr, "key");
    let old_value = parse_opt_child_by_field(tc, env, parse_name, "old_value");
    let new_value = parse_child_by_field(tc, env, parse_expr, "new_value");
    Some(mk_node(node, ast::MapFieldUpdate{
        key: key?,
        old_value: old_value?,
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
            let fun = parse_child_by_field(tc, env, parse_type, "fun");
            let args = parse_children_by_field(tc, env, parse_type, "params");
            Type::App {
                fun: node_box(fun?),
                args: mk_node_many(node, args?),
            }
        },
        "type_function" => {
            let args = parse_children_by_field(tc, env, parse_type, "domain");
            let ret = parse_child_by_field(tc, env, parse_type, "codomain");
            Type::Fun {
                named_args: mk_node_many(node, vec![]),
                args: mk_node_many(node, args?),
                ret: node_box(ret?)
            }
        },
        "type_paren" => {
            parse_child_by_field(tc, env, parse_type, "type")?.node
        },
        "type_tuple" => {
            let elems = parse_children_by_field(tc, env, parse_type, "elem");
            Type::Tuple {
                elems: mk_node_many(node, elems?),
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
        _ => {
            let content = node_content(node, env)?;
            env.err(ParseError::InvalidToken(content));
            None?
        }
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
        "pat_application" => {
            let fun = parse_child_by_field(tc, env, parse_name, "fun");
            let args = parse_fields_in_field(tc, env, parse_pattern, "args", "arg");
            Pattern::App {
                fun: fun?,
                args: args?,
            }
        },
        "pat_let" => {
            let name = parse_child_by_field(tc, env, parse_name, "name");
            let pat = parse_child_by_field(tc, env, parse_pattern, "pattern");
            Pattern::Let {
                name: name?,
                pat: node_box(pat?),
            }
        },
        "pat_list" => {
            let elems = parse_children_by_field(tc, env, parse_pattern, "elem");
            Pattern::List {
                elems: mk_node_many(node, elems?),
            }
        },
        "pat_literal" => {
            let lit = parse_child_by_idx(tc, env, parse_literal, 0);
            Pattern::Lit {
                value: lit?
            }
        },
        "pat_operator" => {
            let op_l = parse_child_by_field(tc, env, parse_pattern, "op_l");
            let op_r = parse_child_by_field(tc, env, parse_pattern, "op_r");
            let op = parse_child_by_field(tc, env, parse_binop, "op");
            Pattern::Op {
                op_l: node_box(op_l?),
                op: op?,
                op_r: node_box(op_r?),
            }
        },
        "pat_record" => {
            let fields = parse_fields_in_field(tc, env, parse_pat_field, "fields", "field");
            Pattern::Record {
                fields: fields?,
            }
        },
        "pat_tuple" => {
            let elems = parse_children_by_field(tc, env, parse_pattern, "elem");
            Pattern::Tuple {
                elems: mk_node_many(node, elems?),
            }
        },
        "pat_typed" => {
            let p = parse_child_by_field(tc, env, parse_pattern, "pattern");
            let t = parse_child_by_field(tc, env, parse_type, "type");
            Pattern::Typed {
                pat: node_box(p?),
                t: t?
            }
        },
        "pat_variable" => {
            let name = node_content(node, env);
            Pattern::Var {
                name: mk_node(node, name?),
            }
        },
        "pat_wildcard" => {
            Pattern::Wildcard
        },
        _ => {
            let content = node_content(node, env)?;
            env.err(ParseError::InvalidToken(content));
            None?
        }
    };
    Some(mk_node(node, pat))
}

fn parse_pat_field<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResultN<ast::PatternRecordField> {
    let node = &tc.node();
    let path = parse_fields_in_field(tc, env, parse_name, "fields", "field");
    let pattern = parse_child_by_field(tc, env, parse_pattern, "pattern");
    Some(mk_node(node, ast::PatternRecordField{
        path: path?,
        pattern: pattern?,
    }))
}


fn parse_stmts<'a>(
    tc: &mut tree_sitter::TreeCursor<'a>,
    env: &mut ParseEnv,
) -> ParseResult<ast::NodeMany<ast::Statement>> {
    use ast::Statement;
    let node = &tc.node();
    let part_stmts = parse_children_by_field(tc, env, parse_split_stmt, "stmt")?;

    let mut stmts = Vec::with_capacity(part_stmts.len());
    for ps in part_stmts {
        match ps.node {
            SplitStmt::Full(s) => {
                stmts.push(re_node(ps.to_owned(), s));
            }
            SplitStmt::PartElIf(cond) => {
                match stmts.last() {
                    Some(ast::Node{node: Statement::If{conds: mut conds, neg: None}, ..}) => {
                        conds.nodes.push(cond);
                    }
                    _ => {
                        env.err(ParseError::LonelyElIf)
                    }
                }
            }
            SplitStmt::PartElse(expr) => {
                match stmts.pop() {
                    Some(ast::Node{node: Statement::If{neg: ref mut neg@None, ..}, ..}) => {
                        *neg = Some(expr)
                    }
                    _ => {
                        env.err(ParseError::LonelyElse)
                    }
                }
            }
        }
    }
    Some(mk_node_many(node, stmts))
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
            let expr = parse_child_by_field(tc, env, parse_expr, "expr");
            SplitStmt::Full(Statement::Expr {
                expr: expr?
            })
        }
        "stmt_letval" => {
            let pattern = parse_child_by_field(tc, env, parse_pattern, "pattern");
            let value = parse_child_by_field(tc, env, parse_expr, "value");
            SplitStmt::Full(Statement::Let {
                pattern: pattern?,
                body: value?,
            })
        }
        "stmt_if" => {
            let cond = parse_child_by_field(tc, env, parse_expr, "cond");
            let then = parse_child_by_field(tc, env, parse_expr, "then");
            let conds = vec![mk_node(node, ast::ExprCond {
                cond: cond?,
                pos: then?,
            })];
            SplitStmt::Full(Statement::If {
                conds: mk_node_many(node, conds),
                neg: None,
            })
        }
        "stmt_elif" => {
            let cond = parse_child_by_field(tc, env, parse_expr, "cond");
            let then = parse_child_by_field(tc, env, parse_expr, "then");
            SplitStmt::PartElIf(mk_node(node, ast::ExprCond {
                cond: cond?,
                pos: then?,
            }))
        }
        "stmt_else" => {
            let expr = parse_child_by_field(tc, env, parse_expr, "else");
            SplitStmt::PartElse(expr?)
        }
        _ => {
            let content = node_content(node, env)?;
            env.err(ParseError::InvalidToken(content));
            None?
        }
    };
    Some(mk_node(node, stmt))
}

/// Checks if the node is an error (or missing) node and if so, runs a fallback parser to collect
/// errors. Returns true if The node was ineed an error, false otherwise.
fn parse_error<'a>(tc: &mut TreeCursor<'a>, env: &mut ParseEnv) -> bool {
    let node = &tc.node();
    if node.is_missing() {
        env.err(ParseError::MissingNode(node.kind().to_string()));
        return true;
    }

    if node.is_error() {
        if let Some(content) = node_content(node, env) {
            env.err(ParseError::NodeError(content))
        };
        parse_any(tc, env);
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

    let sub_err = match env.subtypes.get(kind) {
        Some(sub) => {
            match sub.as_str() {
                "_expression" => parse_expr(tc, env).is_some(),
                "_literal" => parse_literal(tc, env).is_some(),
                _ => {
                    parse_children(tc, env, parse_any, |_| true);
                    false
                }
            }
        }
        None => {
            parse_children(tc, env, parse_any, |_| true);
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

        let src = "@ts.parse(expression)\n(1, XD) => 123)";
        let ast = parser.parse(src, None).unwrap();
        let root = ast.root_node();

        let errs = vec![];
        let src_data: Vec<u16> = src.encode_utf16().collect();

        let mut env = ParseEnv {
            src: &src_data,
            errs: errs,
            subtypes: subtypes,
            has_errors: root.has_error(),
        };

        let mut tc = ast.walk();

        let node = parse_any(&mut tc, &mut env);

        for e in env.errs {
            println!("ERROR: {:?}", e);
        }

        if let Some(out) = node {
            println!("OUTPUT {:?}", out);
        } else {
            println!("FAILED PARSE");
        }

        panic!("done :)");
    }
}
