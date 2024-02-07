use crate::code_table::{CodeTable, CodeTableRef};

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

impl std::fmt::Display for Type {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Ref(u) => write!(f, "?{}", u),
            Type::Var(var) => write!(f, "{}", var),
            Type::Record(name) => write!(f, "{}", name),
            Type::Variant(name) => write!(f, "{}", name),
            Type::Fun{args, ret} => {
                if args.is_empty() {
                    write!(f, "()")?;
                } else {
                    write!(f, "(")?;
                    write!(f, "{}", args[0])?;
                    for a in &args[1..] {
                        write!(f, ", {}", a)?;
                    }
                    write!(f, ")")?;
                }
                write!(f, " => {}", ret)
            }
            Type::Tuple{elems} => {
                if elems.is_empty() {
                    write!(f, "()")
                } else {
                    write!(f, "(")?;
                    write!(f, "{}", elems[0])?;
                    for elem in &elems[1..] {
                        write!(f, " * {}", elem)?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

pub struct Subst {
    assigns: std::collections::HashMap<TypeRef, Type>,
}

impl Subst {
    pub fn new() -> Self {
        Self {
            assigns: std::collections::HashMap::new(),
        }
    }

    pub fn find(&mut self, u: TypeRef) -> Type {
        match self.assigns.get(&u) {
            None => Type::Ref(u),
            Some(Type::Ref(u1)) => {
                let t = self.find(*u1);
                self.assigns.insert(u, t.clone());
                t
            }
            Some(t) => t.clone()
        }
    }

    pub fn set(&mut self, u: TypeRef, t: Type) {
        match self.assigns.get(&u) {
            None => {
                self.assigns.insert(u, t);
            }
            _ => panic!("Subst overwrite!")
        }
    }
}

impl<'a> IntoIterator for Subst {
    type Item = (TypeRef, Type);
    type IntoIter = <std::collections::HashMap<TypeRef, Type> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.assigns.into_iter()
    }
}

pub type TypeTable = crate::code_table::CodeTable<Type>;

impl TypeTable {
    fn apply_subst(&mut self, subst: Subst) {
        for (u, t0) in subst {
            self.set(u, t0)
        }
    }

    fn find_with_subst(&self, subst: &mut Subst, u: TypeRef) -> Type {
        match self.get(u) {
            None => {
                match subst.find(u) {
                    Type::Ref(u0) => {
                        if u == u0 {
                            Type::Ref(u0)
                        } else {
                            self.find_with_subst(subst, u0)
                        }
                    }
                    t => t
                }
            }
            Some(Type::Ref(u0)) => self.find_with_subst(subst, u0),
            Some(t) => t
        }
    }
}

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
        let mut subst = Subst::new();
        let mut errs = Vec::<(Type, Type)>::new();

        self.mgu(table, t, &mut subst, &mut errs);

        table.apply_subst(subst);

        errs
    }

    /// Most General Unifier. Computes minimal substitution of uvars
    /// that makes two types match
    pub(crate) fn mgu(&self,
                      table: &TypeTable,
                      t: &Type,
                      subst: &mut Subst,
                      errs: &mut Vec<(Type, Type)>
    ) {
        let t = t.deref(table, subst);
        println!("MGU {} ~ {}", self, t);

        match (self, &t) {
            // Identical references, nothing to do
            (Type::Ref(s_u), Type::Ref(t_u)) if *s_u == *t_u => (),

            // Matching with a resolved reference
            (_, Type::Ref(t_u)) => {
                if self.occurs_check(table, subst, *t_u) {
                    // Illegal infinite type.  TODO: distinguish
                    // occurs check errors from plain unification
                    // errors
                    println!("OCCURS CHECK: {} ~ {}", t_u, self);
                    errs.push((self.clone(), t.clone()));
                } else {
                    // Substitute
                    subst.set(*t_u, self.clone());
                }
            }

            // MGU is symmetric up to isomorphism
            (Type::Ref(_), _) => {
                t.mgu(table, self, subst, errs)
            }

            // Rigid type vars must be the same
            (Type::Var(s_var), Type::Var(t_var)) if *s_var == *t_var => (),

            (Type::Fun{args: s_args, ret: s_ret}, Type::Fun{args: t_args, ret: t_ret}) => {
                // If the lengths do not match we report an error, but
                // proceed nevertheless for coverage. We assume
                // arguments are missing at the end. TODO: is this
                // helpful or confusing?
                if s_args.len() != t_args.len() {
                    errs.push((self.clone(), t.clone()))
                }

                for (s_a, t_a) in s_args.iter().zip(t_args) {
                    Type::Ref(*s_a).mgu(table, &Type::Ref(*t_a), subst, errs);
                }
                Type::Ref(*s_ret).mgu(table, &Type::Ref(*t_ret), subst, errs);
            }

            (Type::Tuple{elems: s_elems}, Type::Tuple{elems: t_elems}) => {
                // If the lengths do not match we report an error, and
                // not proceed, because it is seems more probable that
                // someone mistook variables, rather than forgot about
                // a tuple element.
                if s_elems.len() != s_elems.len() {
                    errs.push((self.clone(), t.clone()))
                } else {
                    for (s_e, t_e) in s_elems.iter().zip(t_elems) {
                        Type::Ref(*s_e).mgu(table, &Type::Ref(*t_e), subst, errs);
                    }
                }

            }
            _ => {

                println!("BAD UNIF: {} ~ {}", self, t);
                errs.push((self.clone(), t.clone()))
            }
        }
    }


    fn deref(&self, table: &TypeTable, subst: &mut Subst) -> Type {
        match self {
            Type::Ref(u) => table.find_with_subst(subst, *u),
            _ => self.clone()
        }
    }


    /// Checks if a unifiable variable is used in a type.
    pub(crate) fn occurs_check(&self, table: &TypeTable, subst: &mut Subst, u: TypeRef) -> bool {
        let t = self.deref(table, subst);
        match t {
            Type::Ref(u0) => u == u0,
            Type::Var(_) => false,
            Type::Record(_) => unimplemented!(),
            Type::Variant(_) => unimplemented!(),
            Type::Fun{args, ret} => {
                table.find_with_subst(subst, ret)
                    .occurs_check(table, subst, u)
                    || args.iter()
                    .any(|a| table.find_with_subst(subst, *a)
                         .occurs_check(table, subst, u)
                    )
            }
            Type::Tuple{elems} => {
                elems.iter()
                    .any(|e| table.find_with_subst(subst, *e)
                         .occurs_check(table, subst, u)
                    )
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn unify_test(table_init: &Vec<Type>, pairs: Vec<(Type, Type)>, errs_exp: Vec<(Type, Type)>) {
        let mut table = TypeTable::new(vec![("test".to_string(), table_init.len() * 2)]);
        for i in 0..table_init.len() {
            table.set(CodeTableRef::new(0, i), table_init[i].clone())
        }

        let mut errs_acc = Vec::<(Type, Type)>::new();

        for (t1, t2) in pairs {
            let mut errs1 = t1.unify(&mut table, &t2);
            errs_acc.append(&mut errs1);
        }

        println!("ERRS:");
        for (e1, e2) in &errs_acc {
            let mut s = Subst::new();
            println!("- {}\t~\t{}",  e1.deref(&table, &mut s), e2.deref(&table, &mut s))
        }
        assert!(errs_acc.len() == errs_exp.len())
    }

    mod t {
        use super::*;
        pub fn cc(i: usize) -> CodeTableRef {
            CodeTableRef::new(0, i)
        }

        pub fn r(i: usize) -> Type {
            Type::Ref(cc(i))
        }

        pub fn i() -> Type {
            Type::Var("int".to_string())
        }

        pub fn b() -> Type {
            Type::Var("bool".to_string())
        }

        pub fn t(es: Vec<usize>) -> Type {
            Type::Tuple{elems: es.iter().map(|e| cc(*e)).collect()}
        }

        pub fn f(aa: Vec<usize>, r: usize) -> Type {
            Type::Fun{
                args: aa.iter().map(|a| cc(*a)).collect(),
                ret: cc(r)
            }
        }
    }

    #[test]
    fn unify_int() {
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
            (t::r(12), t::r(4)),
            (t::r(12), t::i())
        ], vec![]);


        // NEGATIVE


        unify_test(&table, vec![(t::i(), t::b())], vec![(t::i(), t::b())]);
        unify_test(&table, vec![
            (t::r(12), t::r(4)),
            (t::r(12), t::b()),
        ], vec![(t::i(), t::b())]);

        unify_test(&table, vec![
            (t::r(12), t::r(4)), // 12 ~ i
            (t::r(13), t::r(14)),
            (t::r(14), t::r(6)), // 13 ~ 14 ~ b
            (t::r(12), t::r(14)), // i ~ b
        ], vec![]);
    }

    #[test]
    fn unify_complex() {
        let table = vec![
            t::i(),  // 0
            t::i(),  // 1
            t::b(),  // 2
            t::b(),  // 3
            t::r(0), // 4  ->  i
            t::r(1), // 5  ->  i
            t::r(2), // 6  ->  b
            t::r(3), // 7  ->  b

            t::t(vec![0,1,2,3]), // 8   (i,i,b,b)
            t::t(vec![0,1,2,3]), // 9   (i,i,b,b)
            t::t(vec![4,5,6,7]), // 10  (i,i,b,b)
            t::t(vec![4,3,6,7]), // 11  (i,b,b,b)
            t::t(vec![0,1]),     // 12  (i,i)

            t::t(vec![8, 9]),    // 13
            t::t(vec![9, 8]),    // 14
            t::t(vec![10, 11]),  // 15

            t::t(vec![20, 21, 22, 23]), // 16
        ];


        unify_test(&table, vec![(t::r(8), t::r(9))], vec![]);
        unify_test(&table, vec![(t::r(8), t::r(10))], vec![]);
        unify_test(&table, vec![(t::r(10), t::r(8))], vec![]);

        unify_test(&table, vec![(t::r(8), t::r(16)), (t::r(9), t::t(vec![0,1,2,3]))],
                   vec![]);

        unify_test(&table, vec![(t::r(8), t::r(11))], vec![]);
    }
}

pub mod visitors {
    use super::*;
    pub struct IsFv {}
    impl Visitor<TypeRef, bool> for IsFv {
        fn visit_ref(env: TypeRef, u: TypeRef) -> bool {
            env == u
        }
        fn visit_var(_env: TypeRef, _var: &String) -> bool {
            false
        }
        fn visit_record(_env: TypeRef, _name: &String) -> bool {
            unimplemented!()
        }
        fn visit_variant(_env: TypeRef, _name: &String) -> bool {
            unimplemented!()
        }
        fn visit_fun(_env: TypeRef, args: Vec<bool>, ret: bool) -> bool {
            ret || args.iter().any(|x| *x)
        }
        fn visit_tuple(_env: TypeRef, elems: Vec<bool>) -> bool {
            elems.iter().any(|x| *x)
        }
    }

    pub struct Fvs {}
    impl Visitor<&mut Vec<TypeRef>, ()> for Fvs {
        fn visit_ref(fvs: &mut Vec<TypeRef>, u: TypeRef) {
            fvs.push(u)
        }
        fn visit_var(_fvs: &mut Vec<TypeRef>, _var: &String) {}
        fn visit_record(_fvs: &mut Vec<TypeRef>, _name: &String) {
            unimplemented!()
        }
        fn visit_variant(_fvs: &mut Vec<TypeRef>, _name: &String) {
            unimplemented!()
        }
        fn visit_fun(
            _fvs: &mut Vec<TypeRef>,
            _args: Vec<()>,
            _ret: (),
        ) {}
        fn visit_tuple(_fvs: &mut Vec<TypeRef>, _elems: Vec<()>) {}
    }
}
