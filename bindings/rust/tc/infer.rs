use crate::ast::ast;
use crate::tc::*;
use type_env::*;
use utype::*;

trait Infer<Env> {
    fn infer(&self, env: &mut Env) -> Type;
}


impl<Env: HasTEnv, T: Infer<Env>> Infer<Env> for ast::Node<T> {
    fn infer(&self, env: &mut Env) -> Type {
        in_node(env, self.id, |env_in| {
            let t = self.node.infer(env_in);
            env_in.t_env_mut().set_type_here(self.id, t.clone());
            t
        })
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
        match self {
            ast::Expr::Literal{val: lit} => lit.infer(env),

            _ => unimplemented!()
        }
    }
}
