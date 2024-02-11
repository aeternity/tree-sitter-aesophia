use crate::code_table::{CodeTableRef};

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


pub type TypeTable = crate::code_table::CodeTable<Type>;

impl TypeTable {
    /// Finds type based on the reference. If the type is undefined, a
    /// reference is returned.
    pub fn find_const(&self, u: TypeRef) -> Type {
        let t = match self.get(u) {
            None => Type::Ref(u),
            Some(Type::Ref(u0)) => {
                let t = self.find_const(*u0);
                t
            }
            Some(t) => t.clone()
        };
        t
    }

    /// Finds type based on the reference. If the type is undefined, a
    /// reference is returned. Optimizes references on the way.
    pub fn find(&mut self, u: TypeRef) -> Type {
        let t = match self.get(u) {
            None => Type::Ref(u),
            Some(Type::Ref(u0)) => {
                let t = self.find(*u0);
                self.set(u, t.clone());
                t
            }
            Some(t) => t.clone()
        };
        println!("FIND {} -> {}", u, self.render_ast(u));
        t
    }

    pub fn unify(&mut self, t0: &Type, t1: &Type) -> Vec<(Type, Type)> {
        let mut errs = Vec::new();

        t0.mgu(self, &mut errs, t1);

        errs
    }

    pub fn render_ast(&self, u: TypeRef)
                      -> crate::ast::ast::Node<crate::ast::ast::Type> {
        use crate::ast::ast;
        use crate::ast::ast::Node;
        match self.find_const(u) {
            Type::Ref(u) => {
                Node::new(
                    u.node_id(),
                    ast::Type::PolyVar{
                        name: Node::new(u.node_id(), format!("{}", u))
                    })
            },
            Type::Var(v) => {
                Node::new(
                    u.node_id(),
                    ast::Type::Var{
                        name: Node::new(u.node_id(), v)
                    })
            },
            Type::Tuple{elems} => {
                let t = ast::Type::Tuple {
                    elems: elems.into_iter().map(|e| self.render_ast(e)).collect()
                };
                Node::new(u.node_id(), t)
            },
            Type::Fun{args, ret} => {
                let t = ast::Type::Fun {
                    args: Node::new(
                        u.node_id(),
                        args.into_iter().map(|a| self.render_ast(a)).collect()
                    ),
                    ret: self.render_ast(ret).rec()
                };
                Node::new(u.node_id(), t)
            },
            _ => unimplemented!()
        }
    }
}

impl Type {

    pub fn int() -> Self {
        Type::Var("int".to_string())
    }

    pub fn bool() -> Self {
        Type::Var("bool".to_string())
    }

    pub fn deref(&self, table: &mut TypeTable) -> Type {
        match self {
            Type::Ref(u) => table.find(*u),
            _ => self.clone()
        }
    }

    pub fn mgu(&self, table: &mut TypeTable, errs: &mut Vec<(Type, Type)>, t: &Type) {
        let t0 = self.deref(table);
        let t1 = t.deref(table);

        println!("\nMGU {} ~ {}", t0, t1);
        match (&t0, &t1) {
            // Identical references, nothing to do
            (Type::Ref(r0), Type::Ref(r1)) if r0 == r1 => (),

            // Matching with a resolved reference
            (_, Type::Ref(r1)) => {
                if t0.occurs_check(table, *r1) {
                    errs.push((t0.clone(), t1.clone()));
                } else {
                    table.set(*r1, t0);
                }
            }

            // MGU is symmetric up to isomorphism
            (Type::Ref(_), _) => {
                t1.mgu(table, errs, &t0)
            }

            // Rigid type vars must be the same
            (Type::Var(var0), Type::Var(var1)) if *var0 == *var1 => (),

            (Type::Fun{args: args0, ret: ret0}, Type::Fun{args: args1, ret: ret1}) => {
                // If the lengths do not match we report an error, but
                // proceed nevertheless for coverage. We assume
                // arguments are missing at the end. TODO: is this
                // helpful or confusing?
                if args0.len() != args1.len() {
                    errs.push((t0.clone(), t1.clone()))
                }

                for (a0, a1) in args0.into_iter().zip(args1) {
                    Type::Ref(*a0).mgu(table, errs, &Type::Ref(*a1));
                }

                Type::Ref(*ret0).mgu(table, errs, &Type::Ref(*ret1));
            }

            (Type::Tuple{elems: elems0}, Type::Tuple{elems: elems1}) => {
                // If the lengths do not match we report an error, and
                // not proceed, because it is seems more probable that
                // someone mistook variables, rather than forgot about
                // a tuple element.
                if elems0.len() != elems1.len() {
                    println!("LENGTH MISMATCH {} ~ {}", elems0.len(), elems1.len());
                    errs.push((t0.clone(), t1.clone()))
                } else {
                    for (e0, e1) in elems0.into_iter().zip(elems1) {
                        Type::Ref(*e0).mgu(table, errs, &Type::Ref(*e1));
                    }
                }

            }
            _ => {
                println!("TYPES TOTAL INCOMPATIBILITY");
                errs.push((t0.clone(), t1.clone()))
            }
        }
    }

    pub fn occurs_check(&self, table: &TypeTable, u: TypeRef) -> bool {
        match self {
            Type::Ref(u0) if u == *u0 => true,
            Type::Ref(u0) => {
                let tt = table.get(*u0);
                match tt {
                    None => false,
                    Some(t) => t.occurs_check(table, u)
                }
            }
            Type::Var(_) => false,
            Type::Fun{args, ret} => {
                std::iter::once(ret).chain(args.into_iter())
                    .any(|t| Type::Ref(*t).occurs_check(table, u))
            }
            Type::Tuple{elems} => {
                elems.into_iter()
                    .any(|t| Type::Ref(*t).occurs_check(table, u))
            }
            _ => unimplemented!()
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn unify_test(table_init: &Vec<Type>, pairs: Vec<(Type, Type)>, errs_exp: Vec<(Type, Type)>) {
        let mut table = TypeTable::new(vec!["test".to_string()]);
        for i in 0..table_init.len() {
            table.set(CodeTableRef::new(0, i), table_init[i].clone())
        }

        let mut errs_acc = Vec::<(Type, Type)>::new();

        for (t1, t2) in pairs {
            let mut errs1 = table.unify(&t1, &t2);
            errs_acc.append(&mut errs1);
        }

        println!("ERRS:");
        for (e1, e2) in &errs_acc {
            println!("- {}\t~\t{}",  e1.deref(&mut table), e2.deref(&mut table))
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

    // #[test]
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
        ], vec![(t::b(), t::i())]);

        unify_test(&table, vec![
            (t::r(12), t::r(4)), // 12 ~ i
            (t::r(13), t::r(14)),
            (t::r(14), t::r(6)), // 13 ~ 14 ~ b
            (t::r(12), t::r(14)), // i ~ b
        ], vec![]);
    }

    // #[test]
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
