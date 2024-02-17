use crate::cst;
use crate::code_table::{CodeTable, CodeTableRef, HasCodeRef};
use crate::ast::ast::{Name};
use crate::tc::utype::*;
use crate::tc::scope::*;

/// In-function scope. Certain operations (such as typechecking
/// expressions) are only allowed when this special scope is defined.
#[derive(Clone)]
pub struct LocalScope {
    /// Local variables. Do not confuse with "global" variables
    /// defined outside the function.
    vars: VarEnv,
    /// Currently visited function's name.
    function_name: Name,
}

impl LocalScope {
    fn new(name: Name) -> Self {
        Self {
            vars: VarEnv::new(),
            function_name: name,
        }
    }
}

/// Type environment.
pub struct TEnv {
    /// The root of the scope tree.
    top_scope: Scope,
    /// Path to the currently visited scope in the scope tree.
    current_scope: ScopePath,
    /// If visiting a function, this defines the local
    /// scope. Otherwise, is None.
    local_scope: Option<LocalScope>,
    /// List of all currently open `using` directives for name lookup.
    usings: Vec<Using>,
    /// Currently visited node id
    current_node: cst::NodeId,
    /// Currently visited file id
    current_file: cst::FileId,
    /// Type table is dynamically built during the inference. It maps
    /// code references to assigned types, whenever it makes sense.
    type_table: CodeTable<Type>,
}

/// Constructors
impl TEnv {
    pub fn new(table: TypeTable) -> Self {
        let loc = CodeTableRef::new(0, 0);
        Self {
            top_scope: Scope::new(loc, ScopeKind::TopLevel),
            current_scope: vec![],
            usings: vec![],
            current_node: loc.node_id(),
            current_file: loc.file_id(),
            type_table: table,
            local_scope: None,
        }
    }
}

/// Scopes
impl TEnv {
    /// Adds a new scope within the current scope.
    pub fn create_scope(&mut self, name: Name, kind: ScopeKind) {
        let loc = self.code_ref();
        let current = &mut self.current_scope_mut();
        match current.subscopes.get(&name) {
            Some(_) => panic!("Duplicate scope"),
            None => {
                let scope = Scope::new(loc, kind);
                current.subscopes.insert(name, scope);
            }
        }
    }

    /// Opens scope under absolute path.
    pub fn open_scope_abs(&mut self, scope: ScopePath) {
        self.current_scope = scope;
    }

    /// Opens scope under relative path.
    pub fn open_scope(&mut self, scope: Name) {
        let current = &mut self.current_scope;
        current.push(scope);
    }

    /// Closes scope by moving to the superscope. Returns the name of
    /// the closed scope, or `None` if in already on the top level.
    pub fn pop_scope(&mut self) -> Option<Name> {
        self.current_scope.pop()
    }

    /// Finds reference to scope by absolute path without opening it.
    pub fn get_scope_abs<'a>(&'a self, path: ScopePath) -> &'a Scope {
        let mut scope = &self.top_scope;
        for name in path {
            let next_scope = &scope.subscopes[&name];
            scope = next_scope;
        }
        scope
    }

    /// Finds mutable reference to scope by absolute path without
    /// opening it.
    pub fn get_scope_abs_mut<'a>(&'a mut self, path: ScopePath) -> &'a mut Scope {
        let mut scope = &mut self.top_scope;
        for name in path {
            let next_scope = scope.subscopes.get_mut(&name).expect("Not found scope");
            scope = next_scope;
        }
        scope
    }

    /// Returns reference to the current scope.
    pub fn current_scope<'a>(&'a self) -> &'a Scope {
        let current = &self.current_scope.clone();
        self.get_scope_abs(current.clone())
    }

    /// Returns mutable reference to the current scope.
    pub fn current_scope_mut<'a>(&'a mut self) -> &'a mut Scope {
        let current = &self.current_scope.clone();
        self.get_scope_abs_mut(current.clone())
    }

    /// Checks whether local scope is open
    pub fn is_local(&self) -> bool {
        self.local_scope.is_some()
    }

    /// Executes a function in the given scope by relative path. The
    /// scope has to exist beforehand. Does not work if local scope is
    /// open.
    pub fn in_scope<F, R>(&mut self, name: Name, fun: F) -> R
    where F: FnOnce(&mut Self) -> R {
        if self.is_local() {
            panic!("Global in local");
        }

        self.open_scope(name);
        let res = fun(self);
        self.pop_scope();
        res
    }

    /// Executes a function in the given scope by relative path. Does
    /// not work if local scope is already open.
    pub fn in_local_scope<F, R>(&mut self, name: Name, fun: F) -> R
    where F: FnOnce(&mut Self) -> R {
        if self.is_local() {
            panic!("Reopen local");
        }

        self.local_scope = Some(LocalScope::new(name));
        let res = fun(self);
        self.local_scope = None;
        res
    }

    /// Lists currently visible scopes.
    pub fn visible_scopes<'a>(&'a self) -> impl Iterator<Item=&'a Scope> {
        // TODO actually all scopes
        std::iter::once(self.current_scope())
    }
}

/// Access
impl TEnv {
    /// Registers a function in the current scope. Fails in a local
    /// scope.
    pub fn add_fun(&mut self, name: Name, spec: FunSpec) {
        if self.is_local() {
            panic!("Fundef in local");
        }

        let scope = self.current_scope_mut();
        let funs = &mut scope.funs;
        funs.insert(name, spec);
    }

    /// Registers a variable in the current scope. If the scope is
    /// local, the variable is registered in the local
    /// scope. Otherwise the current global scope fathers the
    /// variable. If the variable was already defined, returns the
    /// previous definition's spec.
    pub fn add_var(&mut self, name: Name, spec: VarSpec) -> Option<VarSpec> {
        match self.local_scope.as_mut() {
            Some(ls) => {
                ls.vars.insert(name, spec)
            }
            None => {
                let scope = &mut self.current_scope_mut();
                if scope.vars.contains_key(&name) {
                    panic!("Redefinition of global var");
                }
                scope.vars.insert(name, spec)
            }
        }
    }

    /// Finds a function in the global scope.
    pub fn get_fun<'a>(&'a self, name: &Name) -> Option<&'a FunSpec> {
        self.visible_scopes().find_map(|s| s.funs.get(name))
    }

    /// Finds variable by name in the local scope. If not in local
    /// scope, returns None.
    pub fn get_local_var<'a>(&'a self, name: &Name) -> Option<&'a VarSpec> {
        match &self.local_scope {
            None => None,
            Some(ls) => ls.vars.get(name)
        }
    }

    /// Finds variable by name in the global scope. Ignores local scope.
    pub fn get_global_var<'a>(&'a self, name: &Name) -> Option<&'a VarSpec> {
        self.current_scope().vars.get(name)
    }

    /// Finds variable by name. First looks in the local scope, then
    /// in the global. TODO: look for functions too and return a
    /// FunSpec.
    pub fn get_var<'a>(&'a self, name: &Name) -> Option<&'a VarSpec> {
        self.get_local_var(name)
            .or_else(|| self.get_global_var(name))
    }

    /// Executes a function and rolls back all changes to the local
    /// variables after.
    pub fn with_local_vars<F, R>(&mut self, fun: F) -> R
    where F: FnOnce(&mut Self) -> R {
        // TODO: check for duplicate patterns
        match self.local_scope.clone() {
            None => panic!("Binder in non-local scope"),
            Some(ls) => {
                let res = fun(self);
                self.local_scope = Some(ls);
                res
            }
        }
    }
}

impl TEnv {
    /// Returns code table reference to a given node in the currently
    /// visited file
    pub fn node_ref(&self, node_id: cst::NodeId) -> CodeTableRef {
        let file = self.current_file;
        CodeTableRef::new(file, node_id)
    }

    /// Unifies two types in this environment
    pub fn unify(&mut self, t0: &Type, t1: &Type) {
        let errs = self.type_table.unify(t0, t1);
        if !errs.is_empty() {
            for (t1, t2) in errs {
                println!("{} ~ {}", t1, t2);
            }
            panic!("TYPE ERRORS")
        }
    }

    /// Unifies types with the currently visited node
    pub fn unify_here(&mut self, t: &Type) {
        let tref = self.code_ref();
        self.unify(&Type::Ref(tref), t);
    }

    /// Returns the internal type table
    pub fn type_table(&mut self) -> &mut TypeTable {
        &mut self.type_table
    }

}

impl HasCodeRef for TEnv {
    fn code_ref(&self) -> CodeTableRef {
        let file = self.current_file;
        let node = self.current_node;
        CodeTableRef::new(file, node)
    }
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
