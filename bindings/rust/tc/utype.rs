use crate::code_table::{CodeTable, CodeTableRef};
use std::collections::HashMap;

pub type TypeRef = CodeTableRef;

#[derive(Debug, Clone)]
pub enum Type {
    Ref(TypeRef),
    Var(String),
    Record(String),
    Variant(String),
    Fun { args: Vec<TypeRef>, ret: TypeRef },
    Tuple { elems: Vec<TypeRef> },
}

pub type TypeTable = crate::code_table::CodeTable<Type>;

pub trait Visitor<Env, Res> {
    fn visit_ref(env: Env, u: TypeRef) -> Res;
    fn visit_var(env: Env, var: &String) -> Res;
    fn visit_record(env: Env, name: &String) -> Res;
    fn visit_variant(env: Env, name: &String) -> Res;
    fn visit_fun(env: Env, args: Vec<Res>, ret: Res) -> Res;
    fn visit_tuple(env: Env, elems: Vec<Res>) -> Res;
}

struct DerefVisitor<Env, Res, Vis, Deref>
{
    env: std::marker::PhantomData<Env>,
    res: std::marker::PhantomData<Res>,
    vis: std::marker::PhantomData<Vis>,
    der: std::marker::PhantomData<Deref>,
}
impl<Env, Res, Vis, Deref> Visitor<(Deref, Env), Res> for DerefVisitor<Env, Res, Vis, Deref>
where
    Vis: Visitor<Env, Res>,
    Deref: Fn(TypeRef) -> Option<Type> + Copy,
    Env: Copy,
{
    fn visit_ref(env: (Deref, Env), u: TypeRef) -> Res {
        match env.0(u) {
            None => Vis::visit_ref(env.1, u),
            Some(t) => t.visit::<(Deref, Env), Res, DerefVisitor<Env, Res, Vis, Deref>>(env),
        }
    }
    fn visit_var(env: (Deref, Env), var: &String) -> Res {
        Vis::visit_var(env.1, var)
    }
    fn visit_record(env: (Deref, Env), name: &String) -> Res {
        unimplemented!()
    }
    fn visit_variant(env: (Deref, Env), name: &String) -> Res {
        unimplemented!()
    }
    fn visit_fun(env: (Deref, Env), args: Vec<Res>, ret: Res) -> Res {
        Vis::visit_fun(env.1, args, ret)
    }
    fn visit_tuple(env: (Deref, Env), elems: Vec<Res>) -> Res {
        Vis::visit_tuple(env.1, elems)
    }
}

impl Type {
    pub fn visit<Env, Res, Vis>(&self, env: Env) -> Res
    where
        Vis: Visitor<Env, Res>,
    Env: Copy,
    {
        match self {
            Self::Ref(u) => Vis::visit_ref(env, *u),
            Self::Var(v) => Vis::visit_var(env, v),
            Self::Fun { args, ret } => {
                let args_res = args.iter().map(|u| Vis::visit_ref(env, *u)).collect();
                let ret_res = Vis::visit_ref(env, *ret);
                Vis::visit_fun(env, args_res, ret_res)
            },
            Self::Tuple { elems } => {
                let elems_res = elems.iter().map(|u| Vis::visit_ref(env, *u)).collect();
                Vis::visit_tuple(env, elems_res)
            }
            _ => unimplemented!(),
        }
    }

    pub fn walk_f<Env, Res, Vis, Deref>(&self, deref: Deref, env: Env) -> Res
    where
        Vis: Visitor<Env, Res>,
        Deref: Fn(TypeRef) -> Option<Type> + Copy,
        Env: Copy,
    {
        self.visit::<(Deref, Env), _, DerefVisitor<Env, Res, Vis, Deref>>((deref, env))
    }

    pub fn walk<Env, Res, Vis>(&self, table: &TypeTable, env: Env) -> Res
    where
        Vis: Visitor<Env, Res>,
        Env: Copy
    {
        self.walk_f::<Env, Res, Vis, _>(|u| table.get(u), env)
    }

    pub fn unify(&self, table: &mut TypeTable, t: &Type) -> Vec<(Type, Type)> {
        let mut subst = HashMap::<TypeRef, Type>::new();
        let mut errs = Vec::<(Type, Type)>::new();

        self.mgu(table, t, &mut subst, &mut errs);

        errs
    }

    pub(crate) fn mgu(&self, table: &TypeTable, t: &Type, subst: &mut HashMap<TypeRef, Type>, errs: &mut Vec<(Type, Type)>) {
        match (self, t) {
            (Type::Ref(s_u), Type::Ref(t_u)) if s_u == t_u => (),
            (_, Type::Ref(t_u)) => {
                if self.occurs_check(table, subst, *t_u) {
                    errs.push((self.clone(), t.clone()));
                    return
                }
                let t1 = find_union_ref(table, subst, *t_u);
                self.mgu(table, &t1, subst, errs)
            }
            (Type::Ref(_), _) => {
                t.mgu(table, self, subst, errs)
            }
            (Type::Var(s_var), Type::Var(t_var)) if s_var == t_var => (),
            (Type::Fun{args: s_args, ret: s_ret}, Type::Fun{args: t_args, ret: t_ret}) => {
                if s_args.len() != t_args.len() {
                    errs.push((self.clone(), t.clone()))
                }

                for (s_a, t_a) in s_args.iter().zip(t_args) {
                    Type::Ref(*s_a).mgu(table, &Type::Ref(*t_a), subst, errs);
                }
                Type::Ref(*s_ret).mgu(table, &Type::Ref(*t_ret), subst, errs);
            }
            _ => {
                errs.push((self.clone(), t.clone()))
            }
        }
    }

    pub(crate) fn occurs_check(&self, table: &TypeTable, subst: &mut HashMap<TypeRef, Type>, u: TypeRef) -> bool {
        let lookup = |u0| subst.get(&u0).map(|t| t.clone()).or_else(|| table.get(u0));
        self.walk_f::<_, _, visitors::IsFv, _>(lookup, u)
    }
}


fn find_ref(table: &TypeTable, subst: &HashMap<TypeRef, Type>, u: TypeRef) -> Type {
    match subst.get(&u) {
        None => table.get(u).unwrap_or_else(|| Type::Ref(u)),
        Some(t) => t.clone()
    }
}

fn find_union_ref(table: &TypeTable, subst: &mut HashMap<TypeRef, Type>, u: TypeRef) -> Type {
    match subst.get(&u) {
        None => table.get(u).unwrap_or_else(|| Type::Ref(u)),
        Some(Type::Ref(u1)) => {
            let t = find_union_ref(table, subst, *u1);
            subst.insert(u, t.clone());
            t
        }
        Some(t) => t.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unify_test(table_init: &Vec<Type>, pairs: Vec<(Type, Type)>, errs: Vec<(Type, Type)>) {
        let mut table = TypeTable::new(vec![("test".to_string(), table_init.len() * 2)]);
        for i in 0..table_init.len() {
            table.set(CodeTableRef::new(0, i), table_init[i].clone())
        }

        let mut errs_acc = Vec::<(Type, Type)>::new();

        for (t1, t2) in pairs {
            let mut errs1 = t1.unify(&mut table, &t2);
            errs_acc.append(&mut errs1);
        }

        println!("ERRS: {:?} vs exp {:?}", errs_acc, errs);
        // println!("TABLE: {:?}", table);
        assert!(errs_acc.len() == errs.len())
    }

    mod t {
        use super::*;
        pub fn r(i: usize) -> Type {
            Type::Ref(CodeTableRef::new(0, i))
        }

        pub fn i() -> Type {
            Type::Var("int".to_string())
        }

        pub fn b() -> Type {
            Type::Var("bool".to_string())
        }
    }

    #[test]
    fn unify_test_int() {
        let table = vec![
            t::i(),  // 0
            t::i(),  // 1
            t::b(),  // 2
            t::b(),  // 3
            t::r(0), // 4  ->  i
            t::r(1), // 5  ->  i
            t::r(2), // 6  ->  b
            t::r(3), // 7  ->  b
            t::r(4), // 8  -> -> i
            t::r(5), // 9  -> -> i
            t::r(6), // 10 -> -> b
            t::r(7), // 11 -> -> b

            t::r(10), // 12 -> ?
            t::r(11), // 13 -> ?
            t::r(12), // 14 -> ?
        ];

        unify_test(&table, vec![(t::i(), t::i())], vec![]);
        unify_test(&table, vec![(t::r(0), t::i())], vec![]);
        unify_test(&table, vec![(t::i(), t::r(0))], vec![]);
        unify_test(&table, vec![(t::r(0), t::r(0))], vec![]);
        unify_test(&table, vec![(t::r(0), t::r(1))], vec![]);

        unify_test(&table, vec![(t::r(4), t::r(4))], vec![]);
        unify_test(&table, vec![(t::r(4), t::r(5))], vec![]);
        unify_test(&table, vec![(t::r(4), t::i())], vec![]);

        unify_test(&table, vec![
            // (t::r(12), t::r(4)),
            // (t::r(12), t::i())
        ], vec![]);

        // NEGATIVE
        unify_test(&table, vec![(t::i(), t::b())], vec![(t::i(), t::b())]);
        // unify_test(&table, vec![
        //     (t::r(12), t::r(4)),
        //     (t::r(13), t::r(14)),
        //     (t::r(14), t::r(6)),
        // ], vec![]);

    }
}

pub mod visitors {
    use super::*;
    pub struct IsFv {}
    impl Visitor<TypeRef, bool> for IsFv {
        fn visit_ref(env: TypeRef, u: TypeRef) -> bool {
            env == u
        }
        fn visit_var(env: TypeRef, var: &String) -> bool {
            false
        }
        fn visit_record(env: TypeRef, name: &String) -> bool {
            unimplemented!()
        }
        fn visit_variant(env: TypeRef, name: &String) -> bool {
            unimplemented!()
        }
        fn visit_fun(env: TypeRef, args: Vec<bool>, ret: bool) -> bool {
            ret || args.iter().any(|x| *x)
        }
        fn visit_tuple(env: TypeRef, elems: Vec<bool>) -> bool {
            elems.iter().any(|x| *x)
        }
    }

    pub struct Fvs {}
    impl Visitor<TypeRef, Vec<TypeRef>> for Fvs {
        fn visit_ref(env: TypeRef, u: TypeRef) -> Vec<TypeRef> {
            vec![u]
        }
        fn visit_var(env: TypeRef, var: &String) -> Vec<TypeRef> {
            Vec::new()
        }
        fn visit_record(env: TypeRef, name: &String) -> Vec<TypeRef> {
            unimplemented!()
        }
        fn visit_variant(env: TypeRef, name: &String) -> Vec<TypeRef> {
            unimplemented!()
        }
        fn visit_fun(
            _env: TypeRef,
            args: Vec<Vec<TypeRef>>,
            ret: Vec<TypeRef>,
        ) -> Vec<TypeRef> {
            args.into_iter().flatten().chain(ret).collect()
        }
        fn visit_tuple(env: TypeRef, elems: Vec<Vec<TypeRef>>) -> Vec<TypeRef> {
            elems.into_iter().flatten().collect()
        }
    }
}
