use crate::ast::ast;
use crate::tc::*;
use type_env::*;
use utype::*;

trait Infer<Env> {
    fn infer(&self, env: &mut Env) -> Type;
}

trait Check<Env> {
    fn check(&self, env: &mut Env, t: &Type);
}



impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for ast::Node<T> {
    fn infer(&self, env: &mut Env) -> Type {
        in_node(env, self.id, |env_in| {
            let t = self.node.infer(env_in);
            env_in.t_env_mut().unify_here(&t);
            t
        })
    }
}

impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for Box<T> {
    fn infer(&self, env: &mut Env) -> Type {
        (**self).infer(env)
    }
}

impl Infer<LocalEnv> for ast::Literal {
    fn infer(&self, _env: &mut LocalEnv) -> Type {
        match self {
            ast::Literal::Int{..} => Type::Var("int".to_string()),
            ast::Literal::Bool{..} => Type::Var("bool".to_string()),
            _ => unimplemented!()
        }
    }
}

impl Infer<LocalEnv> for ast::Expr {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        use ast::Expr::*;
        match self {
            Literal{val: lit} => lit.infer(env),
            If {conds, neg} => {
                let t_neg = neg.infer(env);
                for c in conds {
                    let t_c = c.infer(env);
                    env.t_env_mut().unify(&t_neg, &t_c);
                }

                t_neg
            }
            _ => unimplemented!()
        }
    }
}

impl Infer<LocalEnv> for ast::ExprCond {
    fn infer(&self, env: &mut LocalEnv) -> Type {
        let t_c = self.cond.infer(env);
        let t_e = self.pos.infer(env);

        env.t_env_mut().unify(&Type::Var("bool".to_string()), &t_c);
        t_e
    }
}
