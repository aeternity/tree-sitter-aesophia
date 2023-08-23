use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Range;

pub type Name = String;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ann {}

#[derive(Clone, Debug)]
pub struct Node<T: Clone> {
    pub node: T,
    pub ann: Ann,
}

impl<T: Clone> Node<T> {
    pub fn map<T1: Clone, F>(self, f: F) -> Node<T1>
    where F: FnOnce(T) -> T1
    {
        Node {
            node: f(self.node.to_owned()),
            ann: self.ann
        }
    }

    pub fn boxed(self) -> NodeBox<T> {
        self.map(Box::new)
    }
}

pub type NodeOne<T> = Node<T>;
pub type NodeBox<T> = Node<Box<T>>;

pub type NodeOpt<T> = Option<Node<T>>;
pub type NodeOptBox<T> = Option<NodeBox<T>>;

#[derive(Clone, Debug)]
pub struct NodeMany<T: Clone> {
    pub nodes: Vec<Node<T>>,
    pub ann: Ann,
}

#[derive(Clone, Debug)]
pub struct NodeIndex<Idx, T: Clone>
where
    Idx: Hash,
{
    pub nodes: HashMap<Idx, Node<T>>,
    pub ann: Ann,
}


#[derive(Clone, Debug)]
pub struct Module {
    pub pragmas: NodeMany<Pragma>,
    pub includes: NodeMany<Include>,
    pub usings: NodeMany<Using>,
    pub decls: NodeMany<ScopeDecl>,
}

#[derive(Clone, Debug)]
pub enum UsingRange {
    Include(NodeMany<Name>),
    Exclude(NodeMany<Name>),
}

impl UsingRange {
    pub fn include_all(annot: Ann) -> UsingRange {
        UsingRange::Exclude(NodeMany {
            nodes: vec![],
            ann: annot,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Using {
    pub path: NodeMany<Name>,
    pub range: NodeOne<UsingRange>,
}

pub type Vsn = String;

#[derive(Clone, Debug)]
pub enum Pragma {
    CompilerVsn(Range<Vsn>),
}

#[derive(Clone, Debug)]
pub struct Include {
    pub path: String,
}

#[derive(Clone, Debug)]
pub enum ScopeDecl {
    Namespace{
        name: NodeOne<Name>,
        decls: NodeMany<ScopedDecl>
    },
    Contract {
        name: NodeOne<Name>,
        main: bool,
        implements: NodeMany<QName>,
        defs: NodeMany<ScopedDecl>,
        payable: bool,
    },
    ContractInterface{
        name: NodeOne<Name>,
        extends: NodeMany<QName>,
        payable: bool,
    },
}

#[derive(Clone, Debug)]
pub enum ScopedDecl {
    FunDef(FunDef),
    // TypeDef(TypeDef),
    // ScopeDecl(ScopeDecl),
}

#[derive(Clone, Debug)]
pub struct FunSig {
    pub name: NodeOne<Name>,
    pub signature: NodeOne<Type>,
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub public: bool,
    pub name: NodeOne<Name>,
    pub clauses: NodeMany<FunClause>,
    pub signature: NodeOpt<Type>,
}

#[derive(Clone, Debug)]
pub struct FunClause {
    pub args: NodeMany<Arg>,
    pub rettype: NodeOpt<Type>,
    pub body: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: NodeOne<Name>,
    pub typedecl: NodeOpt<Type>,
}

#[derive(Clone, Debug)]
pub enum TypeDef {
    Alias {
        name: NodeOne<Name>,
    },
    Record {
        name: NodeOne<Name>,
        fields: NodeMany<FieldDecl>,
    },
    Variant {
        name: NodeOne<Name>,
        constructors: NodeMany<Constructor>,
    },
}

#[derive(Clone, Debug)]
pub struct FieldDecl {
    pub name: NodeOne<Name>,
    pub typedecl: NodeOne<Type>,
}

#[derive(Clone, Debug)]
pub struct Constructor {
    pub name: NodeOne<Name>,
    pub args: NodeMany<Type>,
}


#[derive(Clone, Debug)]
pub enum Expr {
    Literal {
        val: NodeOne<Literal>,
    },
    Var {
        var: QName,
    },
    Lambda {
        args: NodeMany<Pattern>,
        body: NodeBox<Expr>,
    },
    Typed {
        expr: NodeBox<Expr>,
        t: NodeOne<Type>,
    },
    BinOp {
        op_l: NodeBox<Expr>,
        op_r: NodeBox<Expr>,
        op: BinOp,
    },
    UnOp {
        expr: NodeBox<Expr>,
        op: UnOp,
    },
    App {
        fun: NodeBox<Expr>,
        args: NodeMany<Expr>,
        named_args: NodeIndex<Name, Expr>,
    },
    Tuple {
        elems: NodeMany<Expr>,
    },
    List {
        elems: NodeMany<Expr>,
    },
    ListRange {
        start: NodeBox<Expr>,
        end: NodeBox<Expr>,
    },
    ListComp {
        yield_expr: NodeBox<Expr>,
        filters: NodeMany<ListCompFilter>,
    },
    Record {
        fields: NodeMany<RecordFieldAssign>
    },
    RecordUpdate {
        record: NodeBox<Expr>,
        updates: NodeMany<RecordFieldUpdate>,
    },
    Map {
        assigns: NodeMany<MapFieldAssign>
    },
    MapUpdate {
        map: NodeBox<Expr>,
        updates: NodeMany<MapFieldUpdate>
    },
    MapAccess {
        map: NodeBox<Expr>,
        key: NodeBox<Expr>,
        default: NodeOptBox<Expr>
    },
    Proj {
        expr: NodeBox<Expr>,
        field: NodeOne<Name>,
    },
    Switch {
        exprs: NodeMany<Expr>,
        cases: NodeMany<Case>,
    },
    If {
        conds: NodeMany<ExprCond>,
        neg: NodeBox<Expr>,
    },
    Block {
        stmts: NodeMany<Statement>,
        value: NodeBox<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct ExprCond {
    pub cond: NodeOne<Expr>,
    pub pos: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct RecordFieldUpdate {
    pub field: NodeOne<Name>,
    pub old_value: NodeOne<Name>,
    pub new_value: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct RecordFieldAssign {
    pub field: NodeOne<Name>,
    pub value: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct MapFieldUpdate {
    pub key: NodeBox<Expr>,
    pub old_value: NodeOpt<Name>,
    pub new_value: NodeBox<Expr>,
}

#[derive(Clone, Debug)]
pub struct MapFieldAssign {
    pub key: NodeBox<Expr>,
    pub value: NodeOpt<Name>,
    pub default: NodeBox<Expr>,
}

#[derive(Clone, Debug)]
pub enum ListCompFilter {
    Bind {
        pattern: NodeOne<Pattern>,
        expr: NodeOne<Expr>,
    },
    Let {
        pattern: NodeOne<Pattern>,
        expr: NodeOne<Expr>
    },
    If {
        cond: NodeOne<Expr>
    },
}

#[derive(Clone, Debug)]
pub struct Case {
    pattern: NodeOne<Pattern>,
    branch: NodeMany<CaseBranch>
}

#[derive(Clone, Debug)]
pub struct CaseBranch {
    pub guards: NodeMany<Expr>,
    pub body: NodeBox<Expr>,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Var {
        name: NodeOne<Name>,
    },
    Lit {
        value: NodeOne<Literal>,
    },
    List {
        elems: NodeMany<Pattern>,
    },
    Tuple {
        elems: NodeMany<Pattern>,
    },
    Record {
        fields: NodeMany<PatternRecordField>,
    },
    Op {
        op_l: NodeBox<Expr>,
        op_r: NodeBox<Expr>,
        op: NodeOne<BinOp>,
    },
    Let {
        name: NodeOne<Name>,
        pat: NodeBox<Pattern>,
    },
    Typed {
        pat: NodeBox<Pattern>,
        t: NodeOne<Type>,
    },
    App {
        con: NodeOne<Name>,
        args: NodeMany<Pattern>,
    },
}

#[derive(Clone, Debug)]
pub struct PatternRecordField {
    pub path: NodeMany<Name>,
    pub pattern: NodeOne<Pattern>,
}

// No nodes because it's an atom
#[derive(Clone, Debug)]
pub enum Literal {
    Int { val: i64 }, // TODO: big int!
    Bool { val: bool },
    Constructor { val: QName },
    Bytes { val: Vec<u8> },
    Address { val: Vec<u8> },
    EmptyMapOrRecord,
    String { val: String },
    Char { val: char },
    Wildcard,
    LambdaBinOp { val: BinOp },
    LambdaUnOp { val: UnOp },
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod, Pow,
    LT, LE, GT, GE, EQ, NE,
    And, Or,
    Cons, Concat,
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Neg, Not,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr {
        expr: NodeOne<Expr>,
    },
    Let {
        pattern: NodeOne<Pattern>,
        typedecl: NodeOpt<Type>,
        body: NodeOne<Expr>,
    },
    If {
        conds: NodeMany<ExprCond>,
        neg: NodeOpt<Expr>,
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Var {
        name: NodeOne<String>
    },
    PolyVar {
        name: NodeOne<String>
    },
    Fun {
        args: NodeMany<Type>,
        named_args: NodeIndex<Name, Type>,
        ret: NodeBox<Type>,
    },
    Tuple {
        elems: NodeMany<Type>
    },
    App {
        fun: NodeOne<String>, // no higher rank types
        args: NodeMany<Type>,
    },
}

#[derive(Clone, Debug)]
pub struct QName {
    pub name: NodeOne<Name>,
    pub path: NodeMany<Name>,
}
