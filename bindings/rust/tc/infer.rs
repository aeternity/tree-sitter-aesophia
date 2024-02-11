use crate::ast::ast;
use crate::tc::*;
use type_env::*;
use utype::*;

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
        let u = env_in.t_env().location();
        node.node.check(env_in, &Type::Ref(u));
        u
    })
}

fn infer_nodes<Env: HasTEnv, T: Infer<Env>>(xs: &ast::Nodes<T>, env: &mut Env) -> Vec<TypeRef> {
    xs.iter().map(|x| infer_node(x, env)).collect()
}

fn type_ref_here<Env: HasTEnv>(env: &Env) -> TypeRef {
    env.t_env().location()
}

fn type_here<Env: HasTEnv>(env: &Env) -> Type {
    let u = type_ref_here(env);
    Type::Ref(u)
}

impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for Box<T> {
    fn infer(&self, env: &mut Env) -> Type {
        (**self).infer(env)
    }
}

impl Infer<LocalEnv> for ast::Literal {
    fn infer(&self, _env: &mut LocalEnv) -> Type {
        println!("INFER LIT {}", self);
        match self {
            ast::Literal::Int{..} => Type::int(),
            ast::Literal::Bool{..} => Type::bool(),
            _ => todo!()
        }
    }
}

impl Infer<LocalEnv> for ast::Expr {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        use ast::Expr::*;
        match self {
            Literal{val: lit} => lit.infer(env),
            Lambda {args,body} => {
                let mut vars =  LocalVars::new();
                let t_args = args.node.iter().map(|arg| infer_node(arg, &mut (env, &mut vars))).collect();

                let t_body = env.with_vars(vars, |env_in| infer_node(body, env_in));
                Type::Fun{
                    args: t_args,
                    ret: t_body,
                }
            }
            Typed {expr,t} => todo!(),
            BinOp {op,op_l,op_r} => todo!(),
            UnOp {op,op_r} => todo!(),
            App {fun,args} => {
                let u_ret = type_ref_here(env);
                let t_args = infer_nodes(&args.node, env);

                let t_fun_exp = Type::Fun {
                    args: t_args,
                    ret: u_ret,
                };

                let t_fun = fun.check(env, &t_fun_exp);

                Type::Ref(u_ret)
            }
            Tuple {elems} if elems.is_empty() => Type::unit(),
            Tuple {elems} if elems.len() == 1 => elems[0].infer(env),
            Tuple {elems} => {
                let t_elems = elems.iter().map(|e| infer_node(e, env)).collect();
                Type::Tuple{elems: t_elems}
            }
            List {elems} => todo!(),
            ListRange {start,end} => todo!(),
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
                    Some(u) => Type::Ref(u)
                }
            },
            Block {stmts,value} => todo!(),
            Hole => todo!()
        }
    }
}

impl Infer<LocalEnv> for ast::ExprArg {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        // TODO Named args!
        self.value.infer(env)
    }
}

impl Infer<LocalEnv> for ast::ExprCond {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        self.cond.check(env, &Type::bool());
        let t_e = self.pos.infer(env);
        t_e
    }
}

impl Infer<(&mut LocalEnv, &mut LocalVars)> for ast::Pattern {
    fn infer(&self, env: &mut (&mut LocalEnv, &mut LocalVars)) -> Type {
        use ast::Pattern::*;
        let l_env = &mut env.0;
        let vars = &mut env.1;
        match self {
            Var {name} => {
                match vars.get(&name.node) {
                    Some(u) => panic!("DUPL VAR IN PATTERN {} at {}", name, u),
                    None => {
                        vars.insert(name.node.clone(), name.id);
                        Type::Ref(l_env.t_env().node_ref(name.id))
                    }
                }
            },
            Lit {value} => value.infer(l_env),
            List {elems} => todo!(),
            Tuple {elems} => {
                let t_elems = infer_nodes(elems, env);
                Type::Tuple{elems: t_elems}
            },
            Record {fields} => todo!(),
            Op {op_l,op_r,op} => todo!(),
            Let {name,pat} => {
                let t_name = Var{name: name.clone()}.infer(env);
                let t_pat = pat.infer(env);
                env.t_env_mut().unify(&t_name, &t_pat);
                t_name
            }
            Typed {pat,t} => {
                let ut = t.infer(l_env);
                pat.check(env, &ut);
                ut
            }
            App {fun,args} => todo!(),
            Wildcard => type_here(env)
        }
    }
}

impl Infer<LocalEnv> for ast::Type {
    /// This essetially converts a syntax type into a runtime type
    fn infer(&self, env: &mut LocalEnv) -> Type {
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
            App {fun,args} => todo!(), // TODO type-app utype
            _ => todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ast;


    fn check_expr(e: &str, t: &str) {
        let e: ast::Node<ast::Expr> = crate::ast::parse_str(e).expect("Parse error: expr");
        let t: ast::Node<ast::Type> = crate::ast::parse_str(t).expect("Parse error: type");

        let table = TypeTable::new(vec!["expr".to_string(), "type".to_string()]);
        let t_env = TEnv::new(table);
        let mut env = LocalEnv::new(t_env);

        let t = type_env::in_location(&mut env, 1, 0, |env_in| infer_node(&t, env_in));

        println!("\nDEDUCTED TYPE: {}\n", env.t_env_mut().type_table().render_ast(t));

        let te = type_env::in_location(&mut env, 0, 0, |env_in| infer_node(&e, env_in));

        println!("\nINFERRED TYPE: {}\n", env.t_env_mut().type_table().render_ast(te));

        env.t_env_mut().unify(&Type::Ref(te), &Type::Ref(t));
    }

    #[test]
    fn check_literals() {
        check_expr("2137\n", "int");
        check_expr("0\n", "int");
        // check_expr("-100000\n", "int"); // FIXME (parser goes crazy)
        check_expr("true\n", "bool");
        check_expr("false\n", "bool");
    }

    #[test]
    fn check_tuples() {
        check_expr("(21, 37)\n", "int * int");
        check_expr("(21, true, 37, false)\n", "int * bool * int * bool");

        // Nested
        check_expr("((21, true), (37, false))\n", "(int * bool) * (int * bool)");
        check_expr("((21, true), (false, 37))\n", "(int * bool) * (bool * int)");
        check_expr("((21, 37), (false, true))\n", "(int * int) * (bool * bool)");
        check_expr("(21, (true, 37, false))\n", "int * (bool * int * bool)");
        check_expr("((21, true, 37), false)\n", "(int * bool * int) * bool");

        // *THE* edge case
        check_expr("()", "unit");
    }

    #[test]
    fn check_lambdas() {
        check_expr("() => 2137", "() => int");
        check_expr("(x) => 2137", "(int) => int");
        check_expr("(x, y) => 2137", "(int, int) => int");

        // Applications
        check_expr("(() => 2137)()", "int");
        check_expr("((x) => 21)(37)", "int");
        check_expr("((x, y) => 21)(3, 7)", "int");

        // Poly-mono
        check_expr("(x) => x", "(int) => int");
        check_expr("(x, y) => (y, x)", "(int, int) => (int * int)");
        check_expr("(x, y) => (y, x)", "(int, bool) => (bool * int)");

        // Nested
        check_expr("(f, x) => f(x)", "((int) => bool, int) => bool");
        check_expr("(f) => (x) => f(x)", "((int) => bool) => (int) => bool");

        // Shadow
        check_expr("((x, y) => (x, ((x) => x)(y)))(123, true)", "(int * bool)");
    }
}
