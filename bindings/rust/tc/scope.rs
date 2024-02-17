use std::collections::HashMap;

use crate::code_table::{CodeTableRef, HasCodeRef};
use crate::ast::ast::{Name};
use crate::tc::utype::*;

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


#[derive(Clone)]
pub struct VarSpec {
    location: CodeTableRef,
    t: Type
}

impl HasCodeRef for VarSpec {
    fn code_ref(&self) -> CodeTableRef {
        self.location
    }
}

impl VarSpec {
    pub fn new(location: CodeTableRef, t: Type) -> VarSpec {
        VarSpec {location, t}
    }
}


pub type VarEnv = HashMap<Name, VarSpec>;
pub type FunEnv = HashMap<Name, FunSpec>;
pub type TypedefEnv = HashMap<Name, TypeSpec>;


pub type ScopeName = Name;
pub type ScopePath = Vec<Name>;

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
