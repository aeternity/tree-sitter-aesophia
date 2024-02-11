use std::collections::HashMap;

use crate::cst;
use crate::code_table::{CodeTable, CodeTableRef};
use crate::ast::ast::{Name};
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

impl Scope {
    pub fn new() -> Self {
        Self {
            subscopes: HashMap::new(),
            funs: HashMap::new(),
            types: HashMap::new(),
        }
    }
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

impl<Env: HasTEnv, T> HasTEnv for (&mut Env, T) {
    fn t_env(&self) -> &TEnv {
        self.0.t_env()
    }
    fn t_env_mut(&mut self) -> &mut TEnv {
        self.0.t_env_mut()
    }
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

/// Execute function with another file and node set as the current location
pub fn in_location<Env: HasTEnv, F, R>(env: &mut Env, file_id: cst::FileId, node_id: cst::NodeId, exec: F) -> R
where F: FnOnce(&mut Env) -> R,
{
    let previous_file = env.t_env().current_file;
    let previous_node = env.t_env().current_node;
    env.t_env_mut().current_file = file_id;
    env.t_env_mut().current_node = node_id;
    let res = exec(env);
    env.t_env_mut().current_file = previous_file;
    env.t_env_mut().current_node = previous_node;
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

impl TEnv {
    pub fn new(table: TypeTable) -> Self {
        Self {
            top_scope: Scope::new(),
            current_scope: vec![],
            usings: vec![],
            current_node: 0,
            current_file: 0,
            type_table: table,
        }
    }

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

    pub fn location(&self) -> CodeTableRef {
        let file = self.current_file;
        let node = self.current_node;
        CodeTableRef::new(file, node)
    }

    pub fn node_ref(&self, node_id: cst::NodeId) -> CodeTableRef {
        let file = self.current_file;
        CodeTableRef::new(file, node_id)
    }

    pub fn unify(&mut self, t0: &Type, t1: &Type) {
        let errs = self.type_table.unify(t0, t1);
        if !errs.is_empty() {
            for (t1, t2) in errs {
                println!("{} ~ {}", t1, t2);
            }
            panic!("TYPE ERRORS")
        }
    }

    pub fn unify_here(&mut self, t: &Type) {
        let tref = self.location();
        self.unify(&Type::Ref(tref), t);
    }

    pub fn type_table(&mut self) -> &mut TypeTable {
        &mut self.type_table
    }
}

pub type LocalVars = HashMap<Name, cst::NodeId>;

pub struct LocalEnv {
    global: TEnv,
    vars: LocalVars,
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

impl LocalEnv {
    pub fn new(t_env: TEnv) -> Self {
        LocalEnv {
            global: t_env,
            vars: LocalVars::new(),
        }
    }

    pub fn save_vars<F, Res>(&mut self, f: F) -> Res
    where F: FnOnce(&mut Self) -> Res
    {
        let old_vars = self.vars.clone();
        let res = f(self);
        self.vars = old_vars;
        res
    }

    pub fn with_vars<F, Res>(&mut self, vars: LocalVars, f: F) -> Res
    where F: FnOnce(&mut Self) -> Res
    {
        self.save_vars(|self_in| {
            self_in.vars.extend(vars);
            f(self_in)
        })
    }

    pub fn get_var(&self, name: &Name) -> Option<CodeTableRef> {
        self.vars.get(name).map(|i| self.global.node_ref(*i))
    }

    pub fn set_var(&mut self, name: Name, node_id: cst::NodeId) {
        self.vars.insert(name, node_id);
    }
}
