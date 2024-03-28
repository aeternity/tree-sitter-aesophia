use crate::ast::ast::{self, InContractDecl};
use crate::tc::*;
use type_env::*;
use scope::*;
use utype::*;
use crate::code_table::HasCodeRef;

trait Infer<Env: HasTEnv> {
    fn infer(&self, env: &mut Env) -> Type;
    fn check(&self, env: &mut Env, t_self: &Type) {
        let t = self.infer(env);
        env.t_env_mut().unify(t_self, &t);
    }
}

impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for ast::Node<T> {
    fn infer(&self, env: &mut Env) -> Type {
        let u = infer_node(self, env);
        Type::Ref(u)
    }
}

fn infer_node<Env: HasTEnv, T: Infer<Env>>(node: &ast::Node<T>, env: &mut Env) -> TypeRef {
    in_node(env, node.id, |env_in| {
        let u = env_in.t_env().code_ref();
        node.node.check(env_in, &Type::Ref(u));
        u
    })
}

fn infer_nodes<Env: HasTEnv, T: Infer<Env>>(xs: &ast::Nodes<T>, env: &mut Env) -> Vec<TypeRef> {
    xs.iter().map(|x| infer_node(x, env)).collect()
}

fn type_ref_here<Env: HasTEnv>(env: &Env) -> TypeRef {
    env.t_env().code_ref()
}

fn type_here<Env: HasTEnv>(env: &Env) -> Type {
    let u = type_ref_here(env);
    Type::Ref(u)
}

fn infer_list<T: Infer<TEnv>>(env: &mut TEnv, elems: &[ast::Node<T>]) -> Type {
    let t_ref_list = builtin_list_ref();
    if let Some(elem0) = elems.first() {
        let t_ref_elem0 = infer_node(elem0, env);
        elems.iter().for_each(|e| e.check(env, &Type::Ref(t_ref_elem0)));

        Type::App { name: t_ref_list, args: vec![t_ref_elem0] }
    } else {
        Type::App { name: t_ref_list, args: vec![env.fresh_typeref()] }
    }
}

fn infer_typed<T: Infer<TEnv>>(env: &mut TEnv, typed: &ast::Node<T>, type_node: &ast::Node<ast::Type>) -> Type {
    let t_typed = typed.infer(env);
    type_node.check(env, &t_typed);
    t_typed
}

fn infer_binop<T: Infer<TEnv>>(env: &mut TEnv, op: &ast::Node<ast::BinOp>, op_l: &ast::Node<T>, op_r: &ast::Node<T>) -> Type {
    use ast::BinOp::*;
    match op.node {
        Add | Sub | Mul | Div | Mod | Pow => {
            op_l.check(env, &Type::int());
            op_r.check(env, &Type::int());

            let t_ref_op_l = infer_node(op_l, env);

            Type::Fun {
                args: vec![t_ref_op_l, t_ref_op_l],
                ret: t_ref_op_l
            }
        }
        And | Or => {
            op_l.check(env, &Type::bool());
            op_r.check(env, &Type::bool());

            let t_ref_op_l = infer_node(op_l, env);

            Type::Fun {
                args: vec![t_ref_op_l, t_ref_op_l],
                ret: t_ref_op_l
            }
        }
        EQ | NE | LT | GT | LE | GE => {
            let t_op_l = op_l.infer(env);
            op_r.check(env, &t_op_l);

            let t_ref_op = infer_node(op_l, env);

            Type::Fun { args: vec![t_ref_op, t_ref_op], ret: builtin_bool_ref() }
        }
        Cons => {
            let t_ref_elem = infer_node(op_l, env);
            let t_ref_list = infer_node(op_r, env);

            op_r.check(env, &Type::App { name: builtin_list_ref(), args: vec![t_ref_elem] });

            Type::Fun { args: vec![t_ref_elem, t_ref_list], ret: t_ref_list }
        }
        Concat => {
            let t_op_l = op_l.infer(env);
            if let Type::App { name, args: _ } = t_op_l.deref(env.type_table()) {
                env.unify(&Type::Ref(name), &Type::Ref(builtin_list_ref()));
                op_r.check(env, &t_op_l);

                let t_ref_op = infer_node(op_l, env);

                Type::Fun { args: vec![t_ref_op, t_ref_op], ret: t_ref_op }
            } else {
                panic!("TYPE ERROR: Both operands of operator '::' must be lists")
            }
        }
        Pipe => {
            if let Type::Fun { args, ret } = op_r.infer(env).deref(env.type_table()) {
                if args.len() != 1 {
                    panic!("TYPE ERROR: Right operand of `|>` is not a single argument function");
                }
                let t_ref_arg = args[0];
                let t_ref_fun = infer_node(op_r, env);
                op_l.check(env, &Type::Ref(t_ref_arg));

                Type::Fun { args: vec![t_ref_arg, t_ref_fun], ret }
            } else {
                panic!("TYPE ERROR: Right operand of `|>` is not a function")
            }
        }
    }
}

impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for Box<T> {
    fn infer(&self, env: &mut Env) -> Type {
        (**self).infer(env)
    }
}

impl Infer<TEnv> for ast::Literal {
    fn infer(&self, _env: &mut TEnv) -> Type {
        match self {
            ast::Literal::Int{..} => Type::int(),
            ast::Literal::Bool{..} => Type::bool(),
            ast::Literal::Char {..} => Type::char(),
            ast::Literal::String {..} => Type::string(),
            _ => todo!()
        }
    }
}

impl Infer<TEnv> for ast::Expr {
    fn infer(&self, env: &mut TEnv) -> Type {
        use ast::Expr::*;
        match self {
            Literal{val: lit} => lit.infer(env),
            Lambda {args,body} => {
                let t_args = args.node
                    .iter()
                    .map(|arg| infer_node(arg, env))
                    .collect();

                let t_body = env.with_local_vars(|env_in| infer_node(body, env_in));
                Type::Fun{
                    args: t_args,
                    ret: t_body,
                }
            }
            Typed {expr,t} => infer_typed(env, expr, t),
            BinOp {op,op_l,op_r} => infer_binop(env, op, op_l, op_r),
            UnOp {op,op_r} => {
                let t_expected = match op.node {
                    ast::UnOp::Neg => Type::int(),
                    ast::UnOp::Not => Type::bool()
                };
                op_r.check(env, &t_expected);

                let t_ref_op_r = infer_node(op_r, env);
                Type::Fun {
                    args: vec![t_ref_op_r],
                    ret: t_ref_op_r
                }
            }
            App {fun,args} => {
                let u_ret = type_ref_here(env);
                let t_args = infer_nodes(&args.node, env);

                let t_fun_exp = Type::Fun {
                    args: t_args,
                    ret: u_ret,
                };

                fun.check(env, &t_fun_exp);

                Type::Ref(u_ret)
            }
            Tuple {elems} if elems.is_empty() => Type::unit(),
            Tuple {elems} if elems.len() == 1 => elems[0].infer(env),
            Tuple {elems} => {
                let t_elems = elems.iter().map(|e| infer_node(e, env)).collect();
                Type::Tuple{elems: t_elems}
            }
            List { elems } => infer_list(env, elems),
            ListRange {start, end} => {
                start.check(env, &Type::int());
                end.check(env, &Type::int());

                Type::Fun { args: vec![builtin_int_ref(), builtin_int_ref()], ret: builtin_list_of_int_ref() }
            }
            ListComp {yield_expr,filters} => todo!(),
            Record {fields} => todo!(),
            RecordUpdate {record,updates} => todo!(),
            Map {assigns} => todo!(),
            MapUpdate{map,updates} => todo!(),
            MapAccess {map,key,default} => todo!(),
            Proj {expr,field} => todo!(),
            Switch {exprs,cases} => todo!(),
            If {conds, neg} => {
                let t_neg = neg.infer(env);
                for c in conds {
                    c.check(env, &t_neg);
                }
                t_neg
            }
            Var {var} => {
                match env.get_var(&var.name.node) {
                    None => panic!("UNDEFINED VAR {}", var.name.node),
                    Some(spec) => Type::Ref(spec.code_ref())
                }
            },
            Block {stmts,value} => todo!(),
            Hole => todo!()
        }
    }
}

impl Infer<TEnv> for ast::ExprArg {
    fn infer(&self, env: &mut TEnv) -> Type {
        // TODO Named args!
        self.value.infer(env)
    }
}

impl Infer<TEnv> for ast::ExprCond {
    fn infer(&self, env: &mut TEnv) -> Type {
        self.cond.check(env, &Type::bool());
        let t_e = self.pos.infer(env);
        t_e
    }
}

impl Infer<TEnv> for ast::Pattern {
    fn infer(&self, env: &mut TEnv) -> Type {
        use ast::Pattern::*;
        match self {
            Var {name} => {
                let loc = env.code_ref();
                let t = Type::Ref(loc);
                let spec = VarSpec::new(loc, true, t.clone());
                env.add_var(name.node.clone(), spec);
                t
            },
            Lit {value} => value.infer(env),
            List {elems} => infer_list(env, elems),
            Tuple {elems} => {
                let t_elems = infer_nodes(elems, env);
                Type::Tuple{elems: t_elems}
            },
            Record {fields} => todo!(),
            Op {op_l, op_r, op} => infer_binop(env, op, op_l, op_r),
            Let {name,pat} => {
                let t_name = Var{name: name.clone()}.infer(env);
                let t_pat = pat.infer(env);
                env.t_env_mut().unify(&t_name, &t_pat);
                t_name
            }
            Typed {pat,t} => infer_typed(env, pat, t),
            App {fun,args} => todo!(),
            Wildcard => type_here(env)
        }
    }
}

impl Infer<TEnv> for ast::Type {
    /// This essetially converts a syntax type into a runtime type
    fn infer(&self, env: &mut TEnv) -> Type {
        use ast::Type::*;
        println!("INFER TYPE {}", self);
        match self {
            Var {name} => Type::Var(name.node.clone()),
            PolyVar{name} => Type::Var(name.node.clone()), // TODO: does this make sense?
            Fun {args,ret} => {
                for a in &args.node {
                    a.infer(env);
                }
                ret.infer(env);
                Type::Fun {
                    args: infer_nodes(&args.node, env),
                    ret: infer_node(ret, env)
                }
            },
            Tuple {elems} => {
                for e in elems {
                    e.infer(env);
                }
                Type::Tuple {
                    elems: infer_nodes(elems, env)
                }
            },
            App {fun, args} => {
                fun.infer(env);
                for a in &args.node {
                    a.infer(env);
                }
                Type::App {
                    name: infer_node(fun, env),
                    args: infer_nodes(&args.node, env)
                }
            }
        }
    }
}

impl Infer<TEnv> for ast::FunDef {
    fn infer(&self, env: &mut TEnv) -> Type {
        let name = self.name.node.clone();
        env.in_local_scope(name, |env_in| {
            let t_sig = match &self.signature {
                None => type_here(env_in),
                Some(sig) => sig.infer(env_in)
            };

            for clause in &self.clauses {
                clause.check(env_in, &t_sig);
            }

            t_sig
        })
    }
}

impl Infer<TEnv> for ast::FunClause {
    fn infer(&self, env: &mut TEnv) -> Type {
        let t_args = self.args.node
            .iter().map(|arg| infer_node(arg, env)).collect();
        let t_body = env.with_local_vars(|env_in| infer_node(&self.body, env_in));
        match &self.ret_type {
            None => (),
            Some(ret_t) => {
                let t_ret = ret_t.infer(env);
                env.unify(&t_ret, &Type::Ref(t_body));
            }
        }
        Type::Fun{
            args: t_args,
            ret: t_body,
        }
    }
}

impl Infer<TEnv> for ast::InContractDecl {
    fn infer(&self, env: &mut TEnv) -> Type {
        let ast::InContractDecl::FunDef(fun_def) = self;
        fun_def.infer(env)
    }
}

pub fn check_scope(scope: &ast::ScopeDecl, env: &mut TEnv) -> () {
    match scope {
        ast::ScopeDecl::Contract {name, decls, ..} => {
            env.in_scope(name.node.clone(), |env_in| {
                for decl in decls {
                    decl.infer(env_in);
                }
            })
        }
        _ => todo!()
    }
}

pub fn check_module(module: &ast::Module, env: &mut TEnv) -> () {
    for scope in module.scopes.iter() {
        check_scope(&scope.node, env);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ast;

    fn parse_check_module(src: &str) {
        let module: ast::Node<ast::Module> =
            crate::ast::parse_str(src).expect("Parse error: module");
        // panic!("{}", module);
    }

    #[test]
    fn contract_test() {
        let src = r#"
            contract C =
              function f() = 123
            "#;
        parse_check_module(src);
    }

    fn check_in<T: crate::cst_ast::CstNode + Infer<TEnv>>(env: &mut TEnv, e: &str, t: &str) {
        let e: ast::Node<T> = crate::ast::parse_str(e).expect("Parse error: item");
        let t: ast::Node<ast::Type> = crate::ast::parse_str(t).expect("Parse error: type");

        let t = type_env::in_location(env, 1, 0, |env_in| infer_node(&t, env_in.t_env_mut()));

        println!("\nDEDUCTED TYPE: {}\n", env.t_env_mut().type_table().render_ast(t));

        let te = type_env::in_location(env, 0, 0, |env_in| infer_node(&e, env_in));

        println!("\nINFERRED TYPE: {}\n", env.t_env_mut().type_table().render_ast(te));

        env.t_env_mut().unify(&Type::Ref(te), &Type::Ref(t));
    }

    fn check_item<T: crate::cst_ast::CstNode + Infer<TEnv>>(local: bool, e: &str, t: &str) {
        let table = TypeTable::new(vec!["builtins".to_string(), "fresh_typerefs".to_string(), "item".to_string(), "type".to_string()]);
        let mut env = TEnv::new(table);

        if local {
            env.in_local_scope("local".to_string(), |env_in| check_in::<T>(env_in, e, t))
        } else {
            check_in::<T>(&mut env, e, t)
        }
    }

    fn check_local<T: crate::cst_ast::CstNode + Infer<TEnv>>(e: &str, t: &str) {
        check_item::<T>(true, e, t)
    }

    fn check_global<T: crate::cst_ast::CstNode + Infer<TEnv>>(e: &str, t: &str) {
        check_item::<T>(false, e, t)
    }


    #[test]
    fn check_literals() {
        check_local::<ast::Expr>("2137\n", "int");
        check_local::<ast::Expr>("0\n", "int");
        // check_local::<ast::Expr>("-100000\n", "int"); // FIXME (parser goes crazy)
        check_local::<ast::Expr>("true\n", "bool");
        check_local::<ast::Expr>("false\n", "bool");
    }

    #[test]
    fn check_typed() {
        check_local::<ast::Expr>("1 : int\n", "int");
        // TODO: add more tests
    }

    #[test]
    fn check_bin_op() {
        check_local::<ast::Expr>("1 + 1\n", "(int, int) => int");
        check_local::<ast::Expr>("2 - 2\n", "(int, int) => int");
        check_local::<ast::Expr>("80 / 8\n", "(int, int) => int");
        check_local::<ast::Expr>("10 * 10\n", "(int, int) => int");
        check_local::<ast::Expr>("14 mod 7\n", "(int, int) => int");
        check_local::<ast::Expr>("2 ^ 10\n", "(int, int) => int");

        check_local::<ast::Expr>("true || false\n", "(bool, bool) => bool");
        check_local::<ast::Expr>("false && false\n", "(bool, bool) => bool");

        check_local::<ast::Expr>("1 >= 2\n", "(int, int) => bool");
        check_local::<ast::Expr>("1 =< 2\n", "(int, int) => bool");
        check_local::<ast::Expr>("false != true\n", "(bool, bool) => bool");
        check_local::<ast::Expr>("'c' < 'a'\n", "(char, char) => bool");
        check_local::<ast::Expr>("'c' > 'a'\n", "(char, char) => bool");
        check_local::<ast::Expr>("\"str\" == \"str\"\n", "(string, string) => bool");

        check_local::<ast::Expr>("1::[1]\n", "(int, list(int)) => list(int)");
        check_local::<ast::Expr>("1::[1, 2, 3]\n", "(int, list(int)) => list(int)");
        check_local::<ast::Expr>("false::[true, false]\n", "(bool, list(bool)) => list(bool)");

        check_local::<ast::Expr>("[1] ++ [1]\n", "(list(int), list(int)) => list(int)");
        check_local::<ast::Expr>("['c'] ++ ['d']\n", "(list(char), list(char)) => list(char)");

        // TODO: This test should be fixed after fixing inference for lambdas
        //check_local::<ast::Expr>("1 |> ((x) => x + 1)\n", "(int, (int) => (int, int) => int) => (int, int) => int");
    }

    #[test]
    fn check_un_op() {
        check_local::<ast::Expr>("-1\n", "(int) => int");
        check_local::<ast::Expr>("!true\n", "(bool) => bool");
    }

    #[test]
    fn check_tuples() {
        check_local::<ast::Expr>("(21, 37)\n", "int * int");
        check_local::<ast::Expr>("(21, true, 37, false)\n", "int * bool * int * bool");

        // Nested
        check_local::<ast::Expr>("((21, true), (37, false))\n", "(int * bool) * (int * bool)");
        check_local::<ast::Expr>("((21, true), (false, 37))\n", "(int * bool) * (bool * int)");
        check_local::<ast::Expr>("((21, 37), (false, true))\n", "(int * int) * (bool * bool)");
        check_local::<ast::Expr>("(21, (true, 37, false))\n", "int * (bool * int * bool)");
        check_local::<ast::Expr>("((21, true, 37), false)\n", "(int * bool * int) * bool");

        // *THE* edge case
        check_local::<ast::Expr>("()", "unit");
    }

    #[test]
    fn check_lists() {
        check_local::<ast::Expr>("[1]\n", "list(int)");
        check_local::<ast::Expr>("[1, 2]\n", "list(int)");
        check_local::<ast::Expr>("[true, false]\n", "list(bool)");
        check_local::<ast::Expr>("[[1], [2, 3]]\n", "list(list(int))");

        // List range
        check_local::<ast::Expr>("[1..4]\n", "(int, int) => list(int)");

        // TODO: What is the type of an empty list?
        check_local::<ast::Expr>("[]\n", "list('a)");
    }

    #[test]
    fn check_lambdas() {
        check_local::<ast::Expr>("() => 2137", "() => int");
        check_local::<ast::Expr>("(x) => 2137", "(int) => int");
        check_local::<ast::Expr>("(x, y) => 2137", "(int, int) => int");

        // Applications
        check_local::<ast::Expr>("(() => 2137)()", "int");
        check_local::<ast::Expr>("((x) => 21)(37)", "int");
        check_local::<ast::Expr>("((x, y) => 21)(3, 7)", "int");

        // Poly-mono
        check_local::<ast::Expr>("(x) => x", "(int) => int");
        check_local::<ast::Expr>("(x, y) => (y, x)", "(int, int) => (int * int)");
        check_local::<ast::Expr>("(x, y) => (y, x)", "(int, bool) => (bool * int)");

        // Nested
        check_local::<ast::Expr>("(f, x) => f(x)", "((int) => bool, int) => bool");
        check_local::<ast::Expr>("(f) => (x) => f(x)", "((int) => bool) => (int) => bool");

        // Shadow
        check_local::<ast::Expr>("((x, y) => (x, ((x) => x)(y)))(123, true)", "(int * bool)");
    }

    #[test]
    fn check_fun_def() {
        check_global::<ast::FunDef>(
            "function f() = 123",
            "() => int"
        );

        // TODO: POLYMORPHISM!!! This should fail!
        // check_local::<ast::FunDef>(
        //     "function f(x) = 123",
        //     "(int) => int"
        // );

        check_global::<ast::FunDef>(
            r#"
function
  f : () => int
  f() = 123
                "#,
            "() => int"
                );
        check_global::<ast::FunDef>(
            r#"
function
  f : (int) => int
  f(0) = 321
  f(x) = x
                "#,
            "(int) => int"
        );


    }
}
