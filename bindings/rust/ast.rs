use std::ops::Range;

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
pub type VecNode<T> = Vec<Node<T>>;

pub type NodeMany<T> = Node<Vec<Node<T>>>;


pub type Name = String;

#[derive(Clone, Debug)]
pub struct QName {
    pub path: NodeMany<Name>,
    pub name: NodeOne<Name>,
}


#[derive(Clone, Debug)]
pub struct Module {
    pub pragmas: VecNode<Pragma>,
    pub includes: VecNode<Include>,
    pub usings: VecNode<Using>,
    pub scopes: VecNode<ScopeDecl>,
}

#[derive(Clone, Debug)]
pub enum UsingSelect {
    Include(NodeMany<Name>),
    Exclude(NodeMany<Name>),
    Rename(Node<Name>),
    All
}

#[derive(Clone, Debug)]
pub struct Using {
    pub scope: NodeOne<QName>,
    pub select: NodeOne<UsingSelect>,
}

pub type SubVer = String;

#[derive(Clone, Debug)]
pub enum Pragma {
    CompilerVsn{
        op: NodeOne<BinOp>,
        vsn: NodeMany<SubVer>,
    },
}

#[derive(Clone, Debug)]
pub struct Include {
    pub path: String,
}

#[derive(Clone, Debug)]
pub enum ScopeDecl {
    Contract {
        name: NodeOne<Name>,
        main: bool,
        implements: VecNode<QName>,
        decls: VecNode<InContractDecl>,
        payable: bool,
    },
    ContractInterface{
        name: NodeOne<Name>,
        extends: VecNode<QName>,
        payable: bool,
        decls: VecNode<InInterfaceDecl>,
    },
    Namespace{
        name: NodeOne<Name>,
        decls: VecNode<InNamespaceDecl>,
    },
}

#[derive(Clone, Debug)]
pub enum InContractDecl {
    FunDef(FunDef),
    // TypeDef(TypeDef),
    // ScopeDecl(ScopeDecl),
}

#[derive(Clone, Debug)]
pub enum InInterfaceDecl {
    FunDecl(FunSig),
    // TypeDef(TypeDef),
    // ScopeDecl(ScopeDecl),
}

#[derive(Clone, Debug)]
pub enum InNamespaceDecl {
    FunDef(FunDef),
    // TypeDef(TypeDef),
    // ScopeDecl(ScopeDecl),
}

#[derive(Clone, Debug)]
pub struct FunSig {
    pub signature: NodeOne<Type>,
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub stateful: bool,
    pub payable: bool,
    pub public: bool,
    pub name: NodeOne<Name>,
    pub clauses: VecNode<FunClause>,
    pub signature: NodeOpt<Type>,
}

#[derive(Clone, Debug)]
pub struct FunClause {
    pub args: NodeMany<Pattern>,
    pub rettype: NodeOpt<Type>,
    pub body: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub enum TypeDef {
    Alias {
        name: NodeOne<Name>,
        params: NodeMany<Name>,
        def: NodeOne<Type>
    },
    Record {
        name: NodeOne<Name>,
        params: NodeMany<Name>,
        fields: NodeMany<FieldDecl>,
    },
    Variant {
        name: NodeOne<Name>,
        params: NodeMany<Name>,
        constructors: VecNode<Constructor>,
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
    pub params: NodeMany<Type>,
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
        op: NodeOne<BinOp>,
    },
    UnOp {
        op_l: NodeBox<Expr>,
        op: NodeOne<UnOp>,
    },
    App {
        fun: NodeBox<Expr>,
        args: NodeMany<ExprArg>,
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
        conds: VecNode<ExprCond>,
        neg: NodeBox<Expr>,
    },
    Block {
        stmts: VecNode<Statement>,
        value: NodeBox<Expr>,
    },
    Hole
}

#[derive(Clone, Debug)]
pub enum ExprArg {
    Arg{
        val: NodeOne<Expr>
    },
    NamedArg {
        name: Node<Name>,
        val: NodeOne<Expr>,
    }
}

#[derive(Clone, Debug)]
pub struct ExprCond {
    pub cond: NodeOne<Expr>,
    pub pos: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct RecordFieldUpdate {
    pub path: NodeMany<Name>,
    pub old_value: NodeOpt<Name>,
    pub new_value: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct RecordFieldAssign {
    pub field: NodeOne<Name>,
    pub value: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct MapFieldUpdate {
    pub key: NodeOne<Expr>,
    pub old_value: NodeOpt<Name>,
    pub new_value: NodeOne<Expr>,
}

#[derive(Clone, Debug)]
pub struct MapFieldAssign {
    pub key: NodeOne<Expr>,
    pub value: NodeOne<Expr>,
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
    pub pattern: NodeOne<Pattern>,
    pub branches: NodeMany<CaseBranch>
}

#[derive(Clone, Debug)]
pub struct CaseBranch {
    pub guards: NodeMany<Expr>,
    pub body: NodeOne<Expr>,
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
        op_l: NodeBox<Pattern>,
        op_r: NodeBox<Pattern>,
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
        fun: Node<Name>,
        args: NodeMany<Pattern>,
    },
    Wildcard
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
        named_args: NodeMany<TypeNamedArg>,
        ret: NodeBox<Type>,
    },
    Tuple {
        elems: NodeMany<Type>
    },
    App {
        fun: NodeBox<Type>,
        args: NodeMany<Type>,
    },
}

#[derive(Clone, Debug)]
pub struct TypeNamedArg {
    name: Node<Name>,
    typ: NodeBox<Type>,
}
