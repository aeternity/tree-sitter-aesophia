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

fn type_here<Env: HasTEnv>(env: &Env) -> Type {
    let u = env.t_env().location();
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
            _ => unimplemented!()
        }
    }
}

impl Infer<LocalEnv> for ast::Expr {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        use ast::Expr::*;
        match self {
            Literal{val: lit} => lit.infer(env),
            Lambda {args,body} => {
                env.save_vars(|env_in| {
                    let mut vars =  LocalVars::new();
                    let t_args = args.node.iter().map(|arg| infer_node(arg, &mut (env_in, &mut vars))).collect();
                    let t_body = infer_node(body, env_in);
                    Type::Fun{
                        args: t_args,
                        ret: t_body,
                    }
                })
            }
            Typed {expr,t} => unimplemented!(),
            BinOp {op,op_l,op_r} => unimplemented!(),
            UnOp {op,op_r} => unimplemented!(),
            App {fun,args} => unimplemented!(),
            Tuple {elems} => {
                let t_elems = elems.iter().map(|e| infer_node(e, env)).collect();
                Type::Tuple{elems: t_elems}
            }
            List {elems} => unimplemented!(),
            ListRange {start,end} => unimplemented!(),
            ListComp {yield_expr,filters} => unimplemented!(),
            Record {fields} => unimplemented!(),
            RecordUpdate {record,updates} => unimplemented!(),
            Map {assigns} => unimplemented!(),
            MapUpdate{map,updates} => unimplemented!(),
            MapAccess {map,key,default} => unimplemented!(),
            Proj {expr,field} => unimplemented!(),
            Switch {exprs,cases} => unimplemented!(),
            If {conds, neg} => {
                let t_neg = neg.infer(env);
                for c in conds {
                    c.check(env, &t_neg);
                }
                t_neg
            }
            Var {var} => unimplemented!(),
            Block {stmts,value} => unimplemented!(),
            Hole => unimplemented!()
        }
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
            List {elems} => unimplemented!(),
            Tuple {elems} => {
                let t_elems = infer_nodes(elems, env);
                Type::Tuple{elems: t_elems}
            },
            Record {fields} => unimplemented!(),
            Op {op_l,op_r,op} => unimplemented!(),
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
            App {fun,args} => unimplemented!(),
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
            App {fun,args} => unimplemented!(), // TODO type-app utype
            _ => unimplemented!()
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
        check_expr("true\n", "bool");
    }

    #[test]
    fn check_tuples() {
        check_expr("(21, 37)\n", "int * int");
        check_expr("(21, true, 37, false)\n", "int * bool * int * bool");
        check_expr("((21, true), (37, false))\n", "(int * bool) * (int * bool)");
        check_expr("(21, (true, 37, false))\n", "int * (bool * int * bool)");
        check_expr("((21, true, 37), false)\n", "(int * bool * int) * bool");
    }
}
