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

    pub fn rec(self) -> NodeRec<T> {
        self.map(Box::new)
    }
}

/// Single element node
pub type NodeOne<T> = Node<T>;

/// Single element node; break recursion
pub type NodeRec<T> = Node<Box<T>>;

/// Optional node
pub type NodeOpt<T> = Option<Node<T>>;

/// Optional node; break recursion
pub type NodeOptRec<T> = Option<NodeRec<T>>;

/// Collection of nodes. Use when the collection is not to be considered as an independent component
/// of the parent node. Examples:
///
/// - Field assignments in a record creation expression. Because assignments are in fact *the*
/// record creation, it makes no sense to wrap them in a separate node. There is no case where one
/// would address those fields without addressing the overall parent expression.
///
/// - Cases of a switch expression. Even though they do not define the entire parent expression,
/// they do not stand for an visible and considerable chunk of it. One could address all cases as a
/// collection of cases, but not as a unit container for them.
pub type Nodes<T> = Vec<Node<T>>;

/// Node with a collection of nodes. Use when the collection takes a place of an independent
/// subentity. Examples:
///
/// - Argument list of a function declaration. It is a clearly distinct section of the parent
/// expression, which is considered as a whole for example during type checking.
///
/// - Path in a qualified name. It describes the scope in which the name is referred, therefore it
/// should be considered a separate node.
pub type NodeMany<T> = Node<Nodes<T>>;


pub type Name = String;

#[derive(Clone, Debug)]
pub struct QName {
    pub path: NodeMany<Name>,
    pub name: NodeOne<Name>,
}


#[derive(Clone, Debug)]
pub struct Module {
    pub pragmas: Nodes<Pragma>,
    pub includes: Nodes<Include>,
    pub usings: Nodes<Using>,
    pub scopes: Nodes<ScopeDecl>,
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

pub type Version = Nodes<SubVer>;

#[derive(Clone, Debug)]
pub enum Pragma {
    CompilerVsn{
        op: NodeOne<BinOp>,
        vsn: Node<Version>,
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
        implements: Nodes<QName>,
        decls: Nodes<InContractDecl>,
        payable: bool,
    },
    ContractInterface{
        name: NodeOne<Name>,
        extends: Nodes<QName>,
        payable: bool,
        decls: Nodes<InInterfaceDecl>,
    },
    Namespace{
        name: NodeOne<Name>,
        decls: Nodes<InNamespaceDecl>,
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
    pub clauses: Nodes<FunClause>,
    pub signature: NodeOpt<Type>,
}

#[derive(Clone, Debug)]
pub struct FunClause {
    pub args: NodeMany<Pattern>,
    pub ret_type: NodeOpt<Type>,
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
        constructors: Nodes<Constructor>,
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
        body: NodeRec<Expr>,
    },
    Typed {
        expr: NodeRec<Expr>,
        t: NodeOne<Type>,
    },
    BinOp {
        op_l: NodeRec<Expr>,
        op_r: NodeRec<Expr>,
        op: NodeOne<BinOp>,
    },
    UnOp {
        op_l: NodeRec<Expr>,
        op: NodeOne<UnOp>,
    },
    App {
        fun: NodeRec<Expr>,
        args: NodeMany<ExprArg>,
    },
    Tuple {
        elems: Nodes<Expr>,
    },
    List {
        elems: Nodes<Expr>,
    },
    ListRange {
        start: NodeRec<Expr>,
        end: NodeRec<Expr>,
    },
    ListComp {
        yield_expr: NodeRec<Expr>,
        filters: Nodes<ListCompFilter>,
    },
    Record {
        fields: Nodes<RecordFieldAssign>
    },
    RecordUpdate {
        record: NodeRec<Expr>,
        updates: NodeMany<RecordFieldUpdate>,
    },
    Map {
        assigns: Nodes<MapFieldAssign>
    },
    MapUpdate {
        map: NodeRec<Expr>,
        updates: NodeMany<MapFieldUpdate>
    },
    MapAccess {
        map: NodeRec<Expr>,
        key: NodeRec<Expr>,
        default: NodeOptRec<Expr>
    },
    Proj {
        expr: NodeRec<Expr>,
        field: NodeOne<Name>,
    },
    Switch {
        exprs: NodeMany<Expr>,
        cases: Nodes<Case>,
    },
    If {
        conds: Nodes<ExprCond>,
        neg: NodeRec<Expr>,
    },
    Block {
        stmts: Nodes<Statement>,
        value: NodeRec<Expr>,
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
    pub branches: Nodes<CaseBranch>
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
        elems: Nodes<Pattern>,
    },
    Tuple {
        elems: Nodes<Pattern>,
    },
    Record {
        fields: Nodes<PatternRecordField>,
    },
    Op {
        op_l: NodeRec<Pattern>,
        op_r: NodeRec<Pattern>,
        op: NodeOne<BinOp>,
    },
    Let {
        name: NodeOne<Name>,
        pat: NodeRec<Pattern>,
    },
    Typed {
        pat: NodeRec<Pattern>,
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
        conds: Nodes<ExprCond>,
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
        named_args: Nodes<TypeNamedArg>,
        ret: NodeRec<Type>,
    },
    Tuple {
        elems: Nodes<Type>
    },
    App {
        fun: NodeRec<Type>,
        args: NodeMany<Type>,
    },
}

#[derive(Clone, Debug)]
pub struct TypeNamedArg {
    name: Node<Name>,
    typ: NodeRec<Type>,
}
