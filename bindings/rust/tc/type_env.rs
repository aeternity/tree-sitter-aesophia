use std::collections::HashMap;

use crate::cst;
use crate::code_table::{CodeTable, CodeTableRef};
use crate::ast::ast::{QName, Name};
use crate::tc::utype::*;

pub struct FunSpec {
    args: Vec<Type>,
    ret_t: Type
}

impl FunSpec {
    pub fn new(args: Vec<Type>, ret_t: Type) -> FunSpec {
        FunSpec {args, ret_t}
    }
}

pub struct TypeSpec {
}

type ScopeName = Name;
type ScopePath = Vec<Name>;

struct Scope {
    subscopes: HashMap<ScopeName, Scope>,
    funs: HashMap<Name, FunSpec>,
    types: HashMap<Name, TypeSpec>,
}


#[derive(Clone, Debug)]
pub enum UsingSelect {
    Include(Vec<Name>),
    Exclude(Vec<Name>),
    Rename(Name),
    All
}

#[derive(Clone, Debug)]
pub struct Using {
    pub scope: ScopePath,
    pub select: UsingSelect,
}

pub struct TEnv {
    top_scope: Scope,
    current_scope: ScopePath,
    usings: Vec<Using>,
    current_node: cst::NodeId,
    current_file: cst::FileId,
    type_table: CodeTable<Type>,
}

pub trait HasTEnv {
    fn t_env(&self) -> &TEnv;
    fn t_env_mut(&mut self) -> &mut TEnv;
}

/// Execute function with another node set as the current location
pub fn in_node<Env: HasTEnv, F, R>(env: &mut Env, node_id: cst::NodeId, exec: F) -> R
where F: FnOnce(&mut Env) -> R,
{
    let previous = env.t_env().current_node;
    env.t_env_mut().current_node = node_id;
    let res = exec(env);
    env.t_env_mut().current_node = previous;
    res
}

impl HasTEnv for TEnv {
    fn t_env(&self) -> &TEnv {
        self
    }

    fn t_env_mut(&mut self) -> &mut TEnv {
        self
    }
}

impl cst::HasNodeId for TEnv {
    fn node_id(&self) -> cst::NodeId {
        self.current_node
    }
}

pub struct VarSpec {
    t: Type,
}

pub struct LocalEnv {
    global: TEnv,
    vars: HashMap<Name, VarSpec>
}

impl cst::HasNodeId for LocalEnv {
    fn node_id(&self) -> cst::NodeId {
        self.global.node_id()
    }
}

impl HasTEnv for LocalEnv {
    fn t_env(&self) -> &TEnv {
        &self.global
    }

    fn t_env_mut(&mut self) -> &mut TEnv {
        &mut self.global
    }
}

impl TEnv {
    pub fn switch_scope(&mut self, scope: ScopePath) {
        self.current_scope = scope;
    }

    pub fn push_scope(&mut self, scope: Name) {
        let current = &mut self.current_scope;
        current.push(scope);
    }

    pub fn pop_scope(&mut self) -> Option<Name> {
        self.current_scope.pop()
    }

    pub fn get_scope_abs<'a>(&'a self, path: ScopePath) -> &'a Scope {
        let mut scope = &self.top_scope;
        for name in path {
            let next_scope = &scope.subscopes[&name];
            scope = next_scope;
        }
        scope
    }

    pub fn get_scope_abs_mut<'a>(&'a mut self, path: ScopePath) -> &'a mut Scope {
        let mut scope = &mut self.top_scope;
        for name in path {
            let next_scope = scope.subscopes.get_mut(&name).expect("Not found scope");
            scope = next_scope;
        }
        scope
    }

    pub fn current_scope<'a>(&'a self) -> &'a Scope {
        let current = &self.current_scope.clone();
        self.get_scope_abs(current.clone())
    }

    pub fn current_scope_mut<'a>(&'a mut self) -> &'a mut Scope {
        let current = &self.current_scope.clone();
        self.get_scope_abs_mut(current.clone())
    }

    pub fn add_fun(&mut self, name: Name, args: Vec<Type>, ret_t: Type) {
        let scope = self.current_scope_mut();
        let spec = FunSpec::new(args, ret_t);
        let funs = &mut scope.funs;
        funs.insert(name, spec);
    }

    pub fn visible_scopes<'a>(&'a self) -> impl Iterator<Item=&'a Scope> {
        // TODO actually all scopes
        std::iter::once(self.current_scope())
    }

    pub fn get_fun<'a>(&'a self, name: Name) -> Option<&'a FunSpec> {
        self.visible_scopes().find_map(|s| s.funs.get(&name))
    }

    pub fn unify(&mut self, t0: &Type, t1: &Type) {
        let errs = self.type_table.unify(t0, t1);
        if !errs.is_empty() {
            panic!("TYPE ERRORS: {:?}", errs)
        }
    }

    pub fn unify_here(&mut self, t: &Type) {
        let file = self.current_file;
        let node = self.current_node;
        let tref = CodeTableRef::new(file, node);
        self.unify(&Type::Ref(tref), t);
    }
}
