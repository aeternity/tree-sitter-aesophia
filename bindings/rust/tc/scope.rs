use std::collections::HashMap;

use crate::code_table::{CodeTableRef, HasCodeRef};
use crate::ast::ast::{Name};
use crate::tc::utype::*;

/// Function specification for type checking
#[derive(Clone)]
pub struct FunSpec {
    location: CodeTableRef,
    args: Vec<Type>,
    ret_t: Type
}

impl FunSpec {
    pub fn new(location: CodeTableRef,
               args: Vec<Type>,
               ret_t: Type
    ) -> FunSpec {
        FunSpec {location, args, ret_t}
    }

    // pub fn type_params(&self) -> Vec<Name> {
    //     self.args.iter().chain(std::iter::once(&self.ret_t))
    //         .flat_map(|t| t.get_vars())
    // }
}

impl HasCodeRef for FunSpec {
    fn code_ref(&self) -> CodeTableRef {
        self.location
    }
}

/// Type definition specification for type checking
#[derive(Clone)]
pub struct TypeSpec {
    location: CodeTableRef,
}

impl TypeSpec {
    pub fn new(location: CodeTableRef) -> TypeSpec {
        TypeSpec {location}
    }
}

impl HasCodeRef for TypeSpec {
    fn code_ref(&self) -> CodeTableRef {
        self.location
    }
}

/// Variable specification for type checking. Used for both local and
/// global variables.
#[derive(Clone)]
pub struct VarSpec {
    location: CodeTableRef,
    local: bool,
    t: Type
}

impl HasCodeRef for VarSpec {
    fn code_ref(&self) -> CodeTableRef {
        self.location
    }
}

impl VarSpec {
    pub fn new(location: CodeTableRef, local: bool, t: Type) -> VarSpec {
        VarSpec {location, local, t}
    }
}


pub type VarEnv = HashMap<Name, VarSpec>;
pub type FunEnv = HashMap<Name, FunSpec>;
pub type TypedefEnv = HashMap<Name, TypeSpec>;


pub type ScopeName = Name;
pub type ScopePath = Vec<Name>;

/// Header of a scope. Used for example to distinguish between
/// contracts from namespaces.
pub enum ScopeKind {
    Contract {
        payable: bool,
        main: bool,
    },
    ContractInterface {
        payable: bool,
    },
    Namespace,
    TopLevel
}

/// Single scope instance. Can be for example a namespace or a contract.
pub struct Scope {
    pub location: CodeTableRef,
    pub subscopes: HashMap<ScopeName, Scope>,
    pub funs: FunEnv,
    pub types: TypedefEnv,
    pub vars: VarEnv,
    pub kind: ScopeKind,
}

impl Scope {
    pub fn new(location: CodeTableRef, kind: ScopeKind) -> Self {
        Self {
            location,
            subscopes: HashMap::new(),
            funs: HashMap::new(),
            types: HashMap::new(),
            vars: HashMap::new(),
            kind,
        }
    }
}

impl HasCodeRef for Scope {
    fn code_ref(&self) -> CodeTableRef {
        self.location
    }
}


/// Accessor for `using` directives
#[derive(Clone, Debug)]
pub enum UsingSelect {
    /// Blacklist imported names. Make empty to import everything.
    Exclude(Vec<Name>),
    /// Whitelist imported names
    Include(Vec<Name>),
    /// Alias the scope
    Rename(Name),
}

/// Specification of a `using` directive in context.
#[derive(Clone, Debug)]
pub struct Using {
    pub scope: ScopePath,
    pub select: UsingSelect,
}
