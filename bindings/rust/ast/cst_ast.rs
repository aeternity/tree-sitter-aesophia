//! Parsers of Concrete Syntax Tree

use num_bigint::BigInt;
use num_traits::Num;

use crate::cst::*;

use crate::ast::ast;
use crate::ast::ast::QName;
use crate::ast::cst_parse::*;

pub trait CstNode: Sized {
    fn ts_dispatch() -> Option<String> {
        None
    }

    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<Self>;
}

pub trait Cst: Sized {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResult<Self>;
}

impl CstNode for ast::Module {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Module> {
        let pragmas = parse_kinds_in_field(tc, env, &<ast::Pragma as CstNode>::parse, "module", "top_pragma");
        let includes = parse_kinds_in_field(tc, env, &<ast::Include as CstNode>::parse, "module", "include");
        let usings = parse_kinds_in_field(tc, env, &<ast::Using as CstNode>::parse, "module", "using");
        let scopes = parse_kinds_in_field(tc, env, &<ast::ScopeDecl as CstNode>::parse, "module", "scope_declaration");

        Some(mk_node_tc(
            tc,
            ast::Module {
                pragmas: pragmas?.node,
                includes: includes?.node,
                usings: usings?.node,
                scopes: scopes?.node,
            },
        ))
    }
}

impl CstNode for ast::Using {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Using> {
        let node = &tc.node();
        let scope = parse_field(tc, env, &parse_name, "scope");
        let alias = parse_field(tc, env, &parse_name, "alias");
        let select = parse_field(tc, env, &<ast::UsingSelect as CstNode>::parse, "select");
        Some(mk_node(
            node,
            ast::Using {
                scope: scope?,
                alias,
                select: select.unwrap_or(mk_node(node, ast::UsingSelect::All)),
            },
        ))
    }
}

impl CstNode for ast::UsingSelect {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::UsingSelect> {
        let node = &tc.node();
        let select = match node.kind() {
            "using_for" => {
                let names = parse_fields_in_field(tc, env, &parse_name, "names", "name");
                ast::UsingSelect::Include(names?)
            }
            "using_hiding" => {
                let names = parse_fields_in_field(tc, env, &parse_name, "names", "name");
                ast::UsingSelect::Exclude(names?)
            }
            e => panic!("Unknown using select: {}", e),
        };
        Some(mk_node(node, select))
    }
}

impl CstNode for ast::Pragma {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Pragma> {
        parse_field(tc, env, &parse_top_pragma, "pragma")
    }
}

fn parse_top_pragma(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Pragma> {
    let node = &tc.node();
    match node.kind() {
        "pragma_compiler_vsn" => {
            let op = parse_field(tc, env, &<ast::BinOp as CstNode>::parse, "op");
            let vsn = parse_fields_in_field(tc, env, &parse_name, "version", "subver");
            Some(mk_node(
                node,
                ast::Pragma::CompilerVsn { op: op?, vsn: vsn? },
            ))
        }
        e => panic!("Unknown pragma: {}", e),
    }
}

impl CstNode for ast::Include {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Include> {
        let node = &tc.node();
        let path = parse_field(tc, env, &parse_str, "path");
        Some(mk_node(node, ast::Include { path: path? }))
    }
}

impl CstNode for ast::ScopeDecl {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<Self> {
        let node = &tc.node();

        let head = <ast::ScopeHead as CstNode>::parse(tc, env);
        let name = parse_field(tc, env, &parse_name, "name");

        let funs = parse_kinds(tc, env, &<ast::FunDef as CstNode>::parse, "function_declaration");
        let types = parse_kinds(tc, env, &<ast::TypeDef as CstNode>::parse, "type_declaration");

        let scope = Self {
            head: head?,
            name: name?,
            types,
            funs,
        };
        Some(mk_node(node, scope))
    }
}


impl CstNode for ast::ScopeHead {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<Self> {
        let node = &tc.node();

        let modifiers = parse_fields(tc, env, &parse_name, "modifier");
        let head_type = parse_field(tc, env, &parse_name, "head");
        let is_interface = node.child_by_field_name("interface").is_some();
        let name = parse_field(tc, env, &parse_name, "name");
        let impls = parse_fields(tc, env, &parse_qual, "implements");

        let Modifiers {
            is_payable,
            is_stateful,
            is_private,
            is_main,
        } = parse_modifiers(env, &modifiers);

        let scope_head = match head_type?.node.as_str() {
            "contract" if !is_interface => {
                if is_private || is_stateful {
                    env.err(node, ParseError::InvalidModifier);
                }

                Self::Contract {
                    main: is_main,
                    payable: is_payable,
                    implements: impls,
                }
            }
            "contract" if is_interface => {
                if is_payable || is_private || is_stateful || is_main {
                    env.err(node, ParseError::InvalidModifier);
                }

                Self::Interface {
                    payable: is_payable,
                    extends: impls,
                }
            }
            "namespace" => {
                if is_payable || is_private || is_stateful || is_main || is_interface {
                    env.err(node, ParseError::InvalidModifier);
                }

                if !impls.is_empty() {
                    env.err(node, ParseError::NamespaceImpl)
                }

                Self::Namespace{}
            }
            e => {
                panic!("Unknown scope header: {}", e)
            }
        };
        Some(mk_node(node, scope_head))
    }
}
struct Modifiers {
    is_payable: bool,
    is_stateful: bool,
    is_private: bool,
    is_main: bool,
}

fn parse_modifiers(_env: &mut ParseEnv, modifiers: &Vec<ast::Node<String>>) -> Modifiers {
    let mut is_payable = false;
    let mut is_stateful = false;
    let mut is_private = false;
    let mut is_main = false;

    // TODO: ban repeated modifiers
    for m in modifiers {
        match m.node.as_str() {
            "payable" => is_payable = true,
            "stateful" => is_stateful = true,
            "private" => is_private = true,
            "main" => is_main = true,
            e => panic!("Unknown modifier: {}", e),
        }
    }

    Modifiers {
        is_payable,
        is_stateful,
        is_private,
        is_main,
    }
}

impl CstNode for ast::FunDef {
    fn ts_dispatch() -> Option<String> {
        Some("function_declaration".to_string())
    }

    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::FunDef> {
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
                either::Left(clause_def) => fun_clauses.push(clause_def),
                either::Right(clause_sig) => {
                    if fun_signature.is_some() {
                        env.err(node, ParseError::DuplicateSig);
                    } else {
                        fun_signature = Some(clause_sig.node.signature);
                    }
                }
            }
        }

        Some(mk_node(
            node,
            ast::FunDef {
                stateful: is_stateful,
                payable: is_payable,
                public: is_entrypoint,
                name: fun_name?,
                clauses: fun_clauses,
                signature: fun_signature,
            },
        ))
    }
}

fn parse_function_clause(
    tc: &mut TsCursor,
    env: &mut ParseEnv,
) -> ParseResult<(
    ast::Node<ast::Name>,
    either::Either<ast::Node<ast::FunClause>, ast::Node<ast::FunSig>>,
)> {
    let node = &tc.node();
    let name = parse_field(tc, env, &parse_name, "name");
    let fun = match node.kind() {
        "function_clause" => {
            let args =
                parse_fields_in_field(tc, env, &<ast::Pattern as CstNode>::parse, "args", "arg");
            let typ = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
            let body = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "body");
            either::Left(mk_node(
                node,
                ast::FunClause {
                    args: args?,
                    ret_type: typ,
                    body: body?,
                },
            ))
        }
        "function_signature" => {
            let typ = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
            either::Right(mk_node(node, ast::FunSig { signature: typ? }))
        }
        _ => panic!("bad fun clause"),
    };
    Some((name?, fun))
}

// fn parse_decl_in_contract_interface(
//     tc: &mut TsCursor,
//     env: &mut ParseEnv,
// ) -> ParseResultN<ast::InInterfaceDecl> {
//     use ast::InInterfaceDecl;
//     let node = &tc.node();
// }

// fn parse_decl_in_namespace(
//     tc: &mut TsCursor,
//     env: &mut ParseEnv,
// ) -> ParseResultN<ast::InNamespaceDecl> {
//     use ast::InNamespaceDecl;
//     let node = &tc.node();
// }

impl CstNode for ast::TypeDef {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::TypeDef> {
        use ast::TypeDef;
        let node = &tc.node();
        let td = match node.kind() {
            "type_alias" => {
                let name = parse_field(tc, env, &parse_name, "name");
                let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
                let def = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
                TypeDef::Alias {
                    name: name?,
                    params: params?,
                    def: def?,
                }
            }
            "record_declaration" => {
                let name = parse_field(tc, env, &parse_name, "name");
                let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
                let fields = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::FieldDecl as CstNode>::parse,
                    "fields",
                    "field",
                );
                TypeDef::Record {
                    name: name?,
                    params: params?,
                    fields: fields?,
                }
            }
            "variant_declaration" => {
                let name = parse_field(tc, env, &parse_name, "name");
                let params = parse_fields_in_field(tc, env, &parse_name, "params", "param");
                let constrs = parse_fields(
                    tc,
                    env,
                    &<ast::Constructor as CstNode>::parse,
                    "constructor",
                );
                TypeDef::Variant {
                    name: name?,
                    params: params?,
                    constructors: constrs,
                }
            }
            bad => panic!("Bad type def {}", bad),
        };

        Some(mk_node(node, td))
    }
}

impl CstNode for ast::FieldDecl {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::FieldDecl> {
        let node = &tc.node();
        let name = parse_field(tc, env, &parse_name, "name");
        let typ = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
        Some(mk_node(
            node,
            ast::FieldDecl {
                name: name?,
                typedecl: typ?,
            },
        ))
    }
}

impl CstNode for ast::Constructor {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Constructor> {
        let node = &tc.node();
        let name = parse_field(tc, env, &parse_name, "name");
        let params =
            parse_fields_in_field(tc, env, &<ast::Type as CstNode>::parse, "params", "param");
        Some(mk_node(
            node,
            ast::Constructor {
                name: name?,
                params: params?,
            },
        ))
    }
}

impl CstNode for ast::Expr {
    fn ts_dispatch() -> Option<String> {
        Some("expression".to_string())
    }

    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Expr> {
        use ast::Expr;
        let node = &tc.node();
        let expr = match node.kind() {
            "identifier" => {
                let name = parse_name(tc, env);
                let qname = QName {
                    path: mk_node(node, vec![]),
                    name: name?,
                };

                Expr::Var { var: qname }
            }
            "qual_identifier" => {
                let qname = parse_qual(tc, env);

                Expr::Var { var: qname?.node }
            }
            "expr_literal" => {
                let lit = parse_field(tc, env, &parse_literal, "literal");
                Expr::Literal { val: lit? }
            }
            "expr_tuple" => {
                let elems = parse_fields(tc, env, &<ast::Expr as CstNode>::parse, "elem");
                Expr::Tuple { elems }
            }
            "expr_list_literal" => {
                let elems = parse_fields(tc, env, &<ast::Expr as CstNode>::parse, "elem");
                Expr::List { elems }
            }
            "expr_list_range" => {
                let start = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "start")?.rec();
                let end = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "end")?.rec();
                Expr::ListRange { start, end }
            }
            "expr_lambda" => {
                let node_args = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::Pattern as CstNode>::parse,
                    "args",
                    "arg",
                );
                let body = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "body");

                Expr::Lambda {
                    args: node_args?,
                    body: body?.rec(),
                }
            }
            "expr_typed" => {
                let e = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "expr");
                let t = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
                Expr::Typed {
                    expr: e?.rec(),
                    t: t?,
                }
            }
            "expr_op" => {
                let op_l_m = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "op_l");
                let op_r = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "op_r");
                match op_l_m {
                    Some(op_l) => {
                        let op = parse_field(tc, env, &<ast::BinOp as CstNode>::parse, "op");
                        Expr::BinOp {
                            op_l: op_l.rec(),
                            op: op?,
                            op_r: op_r?.rec(),
                        }
                    }
                    None => {
                        let op = parse_field(tc, env, &<ast::UnOp as CstNode>::parse, "op");
                        Expr::UnOp {
                            op: op?,
                            op_r: op_r?.rec(),
                        }
                    }
                }
            }
            "expr_application" => {
                let fun = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "fun");
                let args = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::ExprArg as CstNode>::parse,
                    "args",
                    "arg",
                );
                Expr::App {
                    fun: fun?.rec(),
                    args: args?,
                }
            }
            "expr_record" => {
                let fields = parse_fields(
                    tc,
                    env,
                    &<ast::RecordFieldAssign as CstNode>::parse,
                    "field",
                );
                Expr::Record { fields }
            }
            "expr_record_update" => {
                let updates = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::RecordFieldUpdate as CstNode>::parse,
                    "updates",
                    "update",
                );

                let record = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "record");

                Expr::RecordUpdate {
                    record: record?.rec(),
                    updates: updates?,
                }
            }
            "expr_map" => {
                let assigns =
                    parse_fields(tc, env, &<ast::MapFieldAssign as CstNode>::parse, "field");
                Expr::Map { assigns }
            }

            "expr_map_update" => {
                let map = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "map");
                let updates = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::MapFieldUpdate as CstNode>::parse,
                    "fields",
                    "field",
                );
                Expr::MapUpdate {
                    map: map?.rec(),
                    updates: updates?,
                }
            }
            "expr_map_access" => {
                let map = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "map");
                let key = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "key");
                Expr::MapAccess {
                    map: map?.rec(),
                    key: key?.rec(),
                    default: None, //todo
                }
            }
            "expr_projection" => {
                let expr = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "record");
                let field = parse_field(tc, env, &parse_name, "field");
                Expr::Proj {
                    expr: expr?.rec(),
                    field: field?,
                }
            }
            "expr_switch" => {
                let node_exprs =
                    parse_fields_in_field(tc, env, &<ast::Expr as CstNode>::parse, "exprs", "expr");
                let node_cases =
                    parse_fields_in_field(tc, env, &<ast::Case as CstNode>::parse, "cases", "case");

                Expr::Switch {
                    exprs: node_exprs?,
                    cases: node_cases?.node,
                }
            }
            "expr_if" => {
                let conds = parse_fields(tc, env, &<ast::Expr as CstNode>::parse, "cond");
                let thens = parse_fields(tc, env, &<ast::Expr as CstNode>::parse, "then");
                let neg = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "neg");
                let zipped: Vec<_> = conds
                    .iter()
                    .zip(thens.iter())
                    .map(|(c, t)| {
                        mk_node(
                            node,
                            ast::ExprCond {
                                cond: c.to_owned(),
                                pos: t.to_owned(),
                            },
                        )
                    })
                    .collect();
                Expr::If {
                    conds: zipped,
                    neg: neg?.rec(),
                }
            }
            "expr_block" => {
                use ast::Statement;
                let mut all_stmts = parse_stmts(tc, env)?;
                let last = all_stmts.pop()?;

                match last.node {
                    Statement::Expr { expr: expr_last } => {
                        let stmts = &all_stmts[..];
                        Expr::Block {
                            stmts: stmts.to_vec(),
                            value: expr_last.rec(),
                        }
                    }
                    _ => {
                        env.err(node, ParseError::BlockNoExpr);
                        None?
                    }
                }
            }
            "expr_hole" => Expr::Hole,
            e => panic!("Unknown expression node: {}", e),
        };
        Some(mk_node(node, expr))
    }
}

impl CstNode for ast::Case {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Case> {
        let node = &tc.node();
        let pattern = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "pattern");
        let branches = parse_fields(tc, env, &<ast::CaseBranch as CstNode>::parse, "branch");
        Some(mk_node(
            node,
            ast::Case {
                pattern: pattern?,
                branches,
            },
        ))
    }
}

impl CstNode for ast::CaseBranch {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::CaseBranch> {
        let node = &tc.node();
        let guards = parse_fields(tc, env, &<ast::Expr as CstNode>::parse, "guard");
        let body = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "body");
        Some(mk_node(
            node,
            ast::CaseBranch {
                guards: mk_node(node, guards),
                body: body?,
            },
        ))
    }
}

impl CstNode for ast::BinOp {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::BinOp> {
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
            "|>" => Pipe,
            e => panic!("Unknown binary op: {}", e),
        };
        Some(mk_node(node, op))
    }
}

impl CstNode for ast::UnOp {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::UnOp> {
        use ast::UnOp::*;
        let node = &tc.node();
        let content = node_content(node, env)?;
        let op = match content.as_str() {
            "-" => Neg,
            "!" => Not,
            e => panic!("Unknown unary op: {}", e),
        };
        Some(mk_node(node, op))
    }
}

impl CstNode for ast::ExprArg {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::ExprArg> {
        let node = &tc.node();
        let arg = match node.kind() {
            "expr_named_argument" => {
                let name = parse_field(tc, env, &parse_name, "name");
                let value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "value");
                ast::ExprArg {
                    name: Some(name?),
                    value: value?,
                }
            }
            "expr_argument" => {
                let value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "value");
                ast::ExprArg {
                    name: None,
                    value: value?,
                }
            }
            e => panic!("Unknown arg node: {}", e)
        };
        Some(mk_node(node, arg))
    }
}

impl CstNode for ast::RecordFieldAssign {
    fn parse(
        tc: &mut TsCursor,
        env: &mut ParseEnv,
    ) -> ParseResultN<ast::RecordFieldAssign> {
        let node = &tc.node();
        let field = parse_field(tc, env, &parse_name, "field");
        let value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "value");
        Some(mk_node(
            node,
            ast::RecordFieldAssign {
                field: field?,
                value: value?,
            },
        ))
    }
}

impl CstNode for ast::RecordFieldUpdate {
    fn parse(
        tc: &mut TsCursor,
        env: &mut ParseEnv,
    ) -> ParseResultN<ast::RecordFieldUpdate> {
        let node = &tc.node();
        let new_value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "new_value");
        let old_value = parse_field(tc, env, &parse_name, "old_value");
        let path = parse_fields_in_field(tc, env, &parse_name, "path", "field");
        Some(mk_node(
            node,
            ast::RecordFieldUpdate {
                new_value: new_value?,
                old_value,
                path: path?,
            },
        ))
    }
}

impl CstNode for ast::MapFieldAssign {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::MapFieldAssign> {
        let node = &tc.node();
        let key = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "key");
        let value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "value");
        Some(mk_node(
            node,
            ast::MapFieldAssign {
                key: key?,
                value: value?,
            },
        ))
    }
}

impl CstNode for ast::MapFieldUpdate {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::MapFieldUpdate> {
        let node = &tc.node();
        let key = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "key");
        let old_value = parse_field(tc, env, &parse_name, "old_value");
        let new_value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "new_value");
        Some(mk_node(
            node,
            ast::MapFieldUpdate {
                key: key?,
                old_value,
                new_value: new_value?,
            },
        ))
    }
}

fn parse_name(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Name> {
    let node = &tc.node();
    Some(mk_node(node, node_content(node, env)?))
}

impl CstNode for ast::Type {
    fn ts_dispatch() -> Option<String> {
        Some("type".to_string())
    }

    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Type> {
        use ast::Type;
        env.check_error(tc)?;
        let node = &tc.node();
        let t = match node.kind() {
            "type_application" => {
                let fun = parse_field(tc, env, &<ast::Type as CstNode>::parse, "fun");
                let args = parse_fields_in_field(tc, env, &<ast::Type as CstNode>::parse, "params", "param");
                Type::App {
                    fun: fun?.rec(),
                    args: args?,
                }
            }
            "type_function" => {
                let args = parse_field(tc, env, &parse_fun_domain, "domain");
                let ret = parse_field(tc, env, &<ast::Type as CstNode>::parse, "codomain");
                Type::Fun {
                    // named_args: vec![], // TODO
                    args: args?,
                    ret: ret?.rec(),
                }
            }
            "type_paren" => parse_field(tc, env, &<ast::Type as CstNode>::parse, "type")?.node,
            "type_tuple" => {
                let elems = parse_fields(tc, env, &<ast::Type as CstNode>::parse, "elem");
                Type::Tuple { elems }
            }
            "type_variable" => {
                let name = node_content(node, env);
                Type::Var {
                    name: mk_node(node, name?),
                }
            }
            "type_variable_poly" => {
                let name = node_content(node, env);
                Type::PolyVar {
                    name: mk_node(node, name?),
                }
            }
            e => panic!("Unknown type node: {}", e),
        };
        Some(mk_node(node, t))
    }
}

fn parse_fun_domain(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Nodes<ast::Type>> {
    env.check_error(tc)?;
    let node = &tc.node();
    let args = match node.kind() {
        "type_domain_zero" => vec![],
        "type_domain_one" => {
            parse_fields(tc, env, &<ast::Type as CstNode>::parse, "param")
        }
        "type_domain_many" => {
            parse_fields(tc, env, &<ast::Type as CstNode>::parse, "param")
        }
        e => panic!("Unknown type node: {}", e),
    };
    Some(mk_node(node, args))
}

impl CstNode for ast::Pattern {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Pattern> {
        use ast::Pattern;
        env.check_error(tc)?;
        let node = &tc.node();
        let pat = match node.kind() {
            "expr_application" => {
                let fun = parse_field(tc, env, &parse_name, "fun");
                let args = parse_fields_in_field(
                    tc,
                    env,
                    &<ast::Pattern as CstNode>::parse, // TODO this is broken
                    "args",
                    "arg",
                );
                Pattern::App {
                    fun: fun?,
                    args: args?,
                }
            }
            "expr_match" => {
                let name = parse_field(tc, env, &parse_name, "lvalue");
                let pat = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "rvalue");
                Pattern::Let {
                    name: name?,
                    pat: pat?.rec(),
                }
            }
            "expr_list_literal" => {
                let elems = parse_fields(tc, env, &<ast::Pattern as CstNode>::parse, "elem");
                Pattern::List { elems }
            }
            "expr_literal" => {
                let lit = parse_field(tc, env, &parse_literal, "literal");
                Pattern::Lit { value: lit? }
            }
            "expr_op" => {
                let op_l = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "op_l");
                let op_r = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "op_r");
                let op = parse_field(tc, env, &<ast::BinOp as CstNode>::parse, "op");
                Pattern::Op {
                    op_l: op_l?.rec(),
                    op: op?,
                    op_r: op_r?.rec(),
                }
            }
            "expr_record" => {
                let fields = parse_fields(
                    tc,
                    env,
                    &<ast::PatternRecordField as CstNode>::parse,
                    "field",
                );
                Pattern::Record { fields }
            }
            "expr_tuple" => {
                let elems = parse_fields(tc, env, &<ast::Pattern as CstNode>::parse, "elem");
                Pattern::Tuple { elems }
            }
            "expr_typed" => {
                let p = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "pattern");
                let t = parse_field(tc, env, &<ast::Type as CstNode>::parse, "type");
                Pattern::Typed {
                    pat: p?.rec(),
                    t: t?,
                }
            }
            "identifier" => {
                let name = node_content(node, env);
                Pattern::Var {
                    name: mk_node(node, name?),
                }
            }
            "expr_wildcard" => Pattern::Wildcard,
            e => panic!("Unknown pattern node: {}", e),
        };
        Some(mk_node(node, pat))
    }
}

impl CstNode for ast::PatternRecordField {
    fn parse(
        tc: &mut TsCursor,
        env: &mut ParseEnv,
    ) -> ParseResultN<ast::PatternRecordField> {
        let node = &tc.node();
        let path = parse_fields_in_field(tc, env, &parse_name, "fields", "field");
        let pattern = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "pattern");
        Some(mk_node(
            node,
            ast::PatternRecordField {
                path: path?,
                pattern: pattern?,
            },
        ))
    }
}

fn parse_stmts(
    tc: &mut TsCursor,
    env: &mut ParseEnv,
) -> ParseResult<ast::Nodes<ast::Statement>> {
    use ast::Statement;
    let node = &tc.node();
    let mut part_stmts = parse_fields(tc, env, &<SplitStmt as CstNode>::parse, "stmt");
    part_stmts.reverse();

    let mut stmts: Vec<ast::Node<ast::Statement>> = Vec::with_capacity(part_stmts.len());
    while let Some(psn) = part_stmts.pop() {
        let ps = psn.node;
        let node_id = psn.id;
        match ps {
            SplitStmt::Full(s) => {
                stmts.push(ast::Node {
                    node: s,
                    id: node_id,
                });
            }
            SplitStmt::PartElIf(cond) => {
                let last = stmts.last_mut();
                match last {
                    Some(ast::Node {
                        node:
                            Statement::If {
                                ref mut conds,
                                neg: None,
                            },
                        ..
                    }) => {
                        conds.push(cond);
                    }
                    _ => env.err(node, ParseError::LonelyElIf),
                }
            }
            SplitStmt::PartElse(expr) => match stmts.pop() {
                Some(ast::Node {
                    node:
                        Statement::If {
                            neg: ref mut neg @ None,
                            ..
                        },
                    ..
                }) => *neg = Some(expr),
                _ => env.err(node, ParseError::LonelyElse),
            },
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

impl CstNode for SplitStmt {
    fn parse(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<SplitStmt> {
        use ast::Statement;
        env.check_error(tc)?;
        let node = &tc.node();
        let stmt = match node.kind() {
            "stmt_expr" => {
                let expr = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "expr");
                SplitStmt::Full(Statement::Expr { expr: expr? })
            }
            "stmt_letval" => {
                let pattern = parse_field(tc, env, &<ast::Pattern as CstNode>::parse, "pattern");
                let value = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "value");
                SplitStmt::Full(Statement::Let {
                    pattern: pattern?,
                    body: value?,
                })
            }
            "stmt_if" => {
                let cond = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "cond");
                let then = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "then");
                let conds = vec![mk_node(
                    node,
                    ast::ExprCond {
                        cond: cond?,
                        pos: then?,
                    },
                )];
                SplitStmt::Full(Statement::If { conds, neg: None })
            }
            "stmt_elif" => {
                let cond = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "cond");
                let then = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "then");
                SplitStmt::PartElIf(mk_node(
                    node,
                    ast::ExprCond {
                        cond: cond?,
                        pos: then?,
                    },
                ))
            }
            "stmt_else" => {
                let expr = parse_field(tc, env, &<ast::Expr as CstNode>::parse, "else");
                SplitStmt::PartElse(expr?)
            }
            e => panic!("Unknown statement node: {}", e),
        };
        Some(mk_node(node, stmt))
    }
}

fn parse_char(tc: &TsCursor, env: &mut ParseEnv) -> ParseResult<char> {
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

fn parse_str(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResult<String> {
    let node = &tc.node();
    let token = node_content(node, env)?;
    let mut chars = token[1..].chars();
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
            Some(c) => out.push(c),
        }
    }
    Some(out)
}

fn unescape<T>(close: char, chars: &mut T, node: &TsNode, env: &mut ParseEnv) -> ParseResult<char>
where
    T: Iterator<Item = char>,
{
    let c = match chars.next() {
        Some('\\') => '\\',
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

fn parse_int(tc: &TsCursor, env: &mut ParseEnv) -> ParseResult<BigInt> {
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

fn parse_bool(tc: &TsCursor, env: &mut ParseEnv) -> ParseResult<bool> {
    let node = &tc.node();
    let content = node_content(node, env)?;

    if content == "true" {
        Some(true)
    } else if content == "false" {
        Some(false)
    } else {
        env.tok_err(node, TokenError::InvalidBool);
        None
    }
}

fn parse_bytes(tc: &TsCursor, env: &mut ParseEnv) -> ParseResult<Vec<u8>> {
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

fn parse_qual(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::QName> {
    let node = &tc.node();
    let path = parse_fields(tc, env, &parse_name, "path");
    let name = parse_field(tc, env, &parse_name, "name");
    Some(mk_node(
        node,
        ast::QName {
            path: mk_node(node, path),
            name: name?,
        },
    ))
}

fn parse_literal(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResultN<ast::Literal> {
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
            let op = <ast::BinOp as CstNode>::parse(tc, env);
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
        "lit_empty_map_or_record" => Literal::EmptyMapOrRecord,
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

fn parse_any_with<T, P>(tc: &mut TsCursor, env: &mut ParseEnv, parse: P) -> ParseResult<()>
where
    P: FnOnce(&mut TsCursor, &mut ParseEnv) -> ParseResult<T>,
{
    let _ = parse(tc, env)?;
    Some(())
}

/// Parses anything based on its kind and the subtype map. Discards the result.
pub fn parse_any(tc: &mut TsCursor, env: &mut ParseEnv) -> ParseResult<()> {
    let node = &tc.node();
    let kind = node.kind();

    match env.subtypes.get(kind) {
        Some(sub) => {
            let sub_str = sub.as_str();
            match sub_str {
                "_expression" => parse_any_with(tc, env, <ast::Expr as CstNode>::parse),
                "_type" => parse_any_with(tc, env, <ast::Type as CstNode>::parse),
                "_literal" => parse_any_with(tc, env, parse_literal),
                "_operator" => parse_any_with(tc, env, <ast::BinOp as CstNode>::parse),
                "_type_definition" => parse_any_with(tc, env, <ast::TypeDef as CstNode>::parse),
                _ => {
                    parse_children(tc, env, &parse_any, |_| true);
                    Some(())
                }
            }
        }
        None => match kind {
            "module" => parse_any_with(tc, env, <ast::Module as CstNode>::parse),
            _ => {
                parse_children(tc, env, &parse_any, |_| true);
                None
            }
        },
    }
}


#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use self::ast::{Module, Node};

    use super::*;

    fn load_test_file(good: bool, test_name: &'static str) -> String {
        let prefix = "bindings/rust/ast/tests/";
        let filename = vec![
            prefix,
            if good { "good/" } else { "bad/" },
            test_name,
            ".aes",
        ]
        .concat();

        println!("Loading test file: {}", filename);
        match std::fs::read_to_string(filename) {
            Err(e) => panic!("Failed to load file: {}", e),
            Ok(src) => { src }
        }
    }

    fn load_good_test_file(test_name: &'static str) -> String {
        load_test_file(true, test_name)
    }

    fn load_subtypes() -> SubtypeMap {
        print!("Loading subtype map... ");

        let nodes_json_src =
            std::fs::read_to_string("src/node-types.json").expect("Unable to read file");
        let nodes_json: serde_json::Value = serde_json::from_str(&nodes_json_src).unwrap();
        let mut subtypes: HashMap<String, String> = HashMap::new();
        match nodes_json {
            serde_json::Value::Array(arr) => {
                for v in arr {
                    match (&v["subtypes"], &v["type"]) {
                        (serde_json::Value::Array(subts), serde_json::Value::String(t)) => {
                            for subt in subts {
                                match &subt["type"] {
                                    serde_json::Value::String(s) => {
                                        subtypes.insert(s.clone(), t.clone());
                                    }
                                    _ => panic!("wtf sub type"),
                                }
                            }
                        }
                        _ => (),
                    };
                }
            }
            _ => panic!("wtf json"),
        }

        println!("Done");
        subtypes
    }

    fn prepare_test(src: &str) -> (ParseEnv, tree_sitter::Tree) {
        let mut parser = load_lang();
        let src_data: Vec<u16> = src.encode_utf16().collect();

        let tree = parser.parse(src, None).expect("Unable to parse");

        let subtypes = load_subtypes();
        let env = ParseEnv::new(src_data, subtypes, tree.root_node().has_error());

        (env, tree)
    }

    fn parse_source_module(src: &str) -> Node<Module> {
        let (mut env, tree) = prepare_test(src);
        let mut tc = tree.walk();

        println!("Parsed {}", tree.root_node().to_sexp());

        if tc.node().has_error() {
            panic!("Error node")
        }

        match <ast::Module as CstNode>::parse(&mut tc, &mut env) {
            Some(ast) => ast,
            None => parsing_errors(&env),
        }
    }

    fn parse_source_content(src: &str) {
        let (mut env, tree) = prepare_test(src);
        let mut tc = tree.walk();

        println!("Parsed {}", tree.root_node().to_sexp());

        if tc.node().has_error() {
            panic!("Error node")
        }

        tc.reset(tc.node().child_by_field_name("content").expect("No source"));

        if parse_any(&mut tc, &mut env).is_none() {
            parsing_errors(&env)
        }
    }

    fn run_dispatch_good(test_name: &'static str) {
        let src = load_good_test_file(test_name);
        parse_source_content(&src);
    }

    fn run_roundtrip_test(test_name: &'static str) {
        let src1 = load_good_test_file(test_name);

        let ast1 = parse_source_module(&src1);
        println!("Roundtrip: parsed as: {:?}", ast1);

        let src2 = &ast1.to_string();
        println!("Roundtrip: re-rendered: {}", src2);

        let ast2 = parse_source_module(src2);
        println!("Roundtrip: re-parsed: {}", ast2);

        assert_eq!(ast1.to_string(), ast2.to_string());
    }

    fn parsing_errors(env: &ParseEnv) -> ! {
        println!("PARSE ERRORS:\n\n");

        for e in &env.errs {
            println!("PARSE ERROR: {}", e.node.to_str())
        }
        panic!("CST->AST conversion failed");
    }

    #[test]
    fn test_roundtrip_usings() {
        run_roundtrip_test("usings");
    }

    #[test]
    fn test_roundtrips_includes() {
        run_roundtrip_test("includes");
    }

    #[test]
    fn test_roundtrips_pragmas() {
        run_roundtrip_test("pragmas");
    }

    #[test]
    fn test_roundtrips_block_open() {
        run_roundtrip_test("simple_contract_block_open");
        run_roundtrip_test("simple_contract_block_open_inline");
    }

    #[test]
    fn test_dispatches() {
        run_dispatch_good("expr_int");
        run_dispatch_good("expr_op_simple");
        run_dispatch_good("expr_application");
        run_dispatch_good("qual_expr_application");
        run_dispatch_good("lit_int");
    }
}
