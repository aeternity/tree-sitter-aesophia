use std::fmt::{Display, self};

use crate::ast::ast::*;

const INDENT_SPACES: usize = 4;

impl<T: Clone + Display> Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for pragma in &self.pragmas {
            write!(f, "{pragma}")?;
        }
        for include in &self.includes {
            write!(f, "{include}")?;
        }
        for using in &self.usings {
            write!(f, "{using}")?;
        }
        for scope in &self.scopes {
            write!(f, "{scope}")?;
        }

        Ok(())
    }
}

impl Display for Using {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "using {}", self.scope.node)?;
        if let Some(alias) = &self.alias {
            write!(f, " as {}", alias)?;
        }
        let selection = match &self.select.node {
            UsingSelect::Include(items) => Some(("for", items)),
            UsingSelect::Exclude(items) => Some(("hiding", items)),
            UsingSelect::All => None,
        };
        if let Some((select_type, items)) = selection {
            write!(f, " {} [{}]", select_type, items.node.iter().map(|i| i.to_string()).collect::<Vec<String>>().join(", "))?;
        }
        writeln!(f)
    }
}

impl Display for Pragma {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pragma::CompilerVsn { op, vsn } =>
                writeln!(f, "@compiler {op} {}", vsn.node.iter().map(|n| n.to_string()).collect::<Vec<String>>().join(".")),
        }
    }
}

impl Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "include \"{}\"", self.path)
    }
}

impl Display for ScopeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.head.node {
            ScopeHead::Contract {main, implements: _, payable, .. } => {
                if payable {
                    write!(f, "payable ")?;
                }
                if main {
                    write!(f, "main ")?;
                }
                write!(f, "contract {} =", self.name)?;
            },
            ScopeHead::Interface { extends: _, payable: _, ..} => todo!(),
            ScopeHead::Namespace { } => todo!(),
        }

        for type_decl in &self.types {
            writeln!(f)?;
            writeln!(f, "{}{type_decl}", " ".repeat(INDENT_SPACES))?;
        }

        for fun_decl in &self.funs {
            writeln!(f)?;
            writeln!(f, "{}{fun_decl}", " ".repeat(INDENT_SPACES))?;
        }

        Ok(())
    }
}

impl Display for FunDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.stateful {
            write!(f, "stateful ")?;
        }
        if self.payable {
            write!(f, "payable ")?;
        }
        // TODO: public and private in namespaces must be handled differently
        if self.public {
            writeln!(f, "entrypoint")?;
        }
        else {
            writeln!(f, "function")?;
        }

        write!(f, "{}", " ".repeat(INDENT_SPACES * 2))?;

        write!(f, "{}", self.name)?;

        // TODO: Handle funs with many clauses
        write!(f, "(")?;
        for arg in &self.clauses[0].node.args.node {
            write!(f, "{arg}")?;
        }
        write!(f, ")")?;

        if let Some(type_node) = &self.clauses[0].node.ret_type {
            write!(f, " : {}", type_node.node)?;
        }

        writeln!(f, " =")?;

        write!(f, "{}", &self.clauses[0].node.body)?;

        Ok(())
    }
}

impl Display for FunDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.stateful {
            write!(f, "stateful ")?;
        }
        if self.payable {
            write!(f, "payable ")?;
        }

        writeln!(f, "entrypoint")?;

        write!(f, "{}", " ".repeat(INDENT_SPACES * 2))?;

        write!(f, "{}", self.name)?;

        write!(f, " : ")?;

        write!(f, "{}", self.signature)?;

        Ok(())
    }
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Alias { name, params, def } => {
                write!(f, "type {}", name.node)?;

                if !params.node.is_empty() {
                    write!(f, "({})",
                           params.node.iter()
                           .map(|p| p.to_string())
                           .collect::<Vec<String>>()
                           .join(", "))?;
                }

                write!(f, " = {}", def.node)
            },
            Self::Record { name, params, fields } => {
                todo!()
            }
            Self::Variant { name, params, constructors } => {
                todo!()
            }
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var { name } => write!(f, "{name}"),
            Type::PolyVar { name } => write!(f, "'{name}"),
            Type::Fun { args, ret } =>
                write!(f, "({}) => {ret}", args.node.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ")),
            Type::Tuple { elems  } =>
                write!(f, "({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" * ")),
            Type::App { fun, args } =>
                write!(f, "{fun}({})", args.node.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Var { name } => write!(f, "{name}"),
            Pattern::Lit { value: _ } => todo!(),
            Pattern::List { elems: _ } => todo!(),
            Pattern::Tuple { elems: _ } => todo!(),
            Pattern::Record { fields: _ } => todo!(),
            Pattern::Op { op_l: _, op_r: _, op: _ } => todo!(),
            Pattern::Let { name: _, pat: _ } => todo!(),
            Pattern::Typed { pat: _, t: _ } => todo!(),
            Pattern::App { fun: _, args: _ } => todo!(),
            Pattern::Wildcard => todo!(),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "mod",
            BinOp::Pow => "^",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::EQ => "==",
            BinOp::NE => "!=",
            BinOp::LT => "<",
            BinOp::GT => ">",
            BinOp::LE => "=<",
            BinOp::GE => ">=",
            BinOp::Cons => "::",
            BinOp::Concat => "++",
            BinOp::Pipe => "|>",
        };
        write!(f, "{str}")
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        };
        write!(f, "{str}")
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int { val } => write!(f, "{val}"),
            Literal::Bool { val } => write!(f, "{val}"),
            Literal::Constructor { val: _ } => todo!(),
            Literal::Bytes { val: _ } => todo!(),
            Literal::Address { val: _ } => todo!(),
            Literal::EmptyMapOrRecord => todo!(),
            Literal::String { val } => write!(f, "\"{val}\""),
            Literal::Char { val: _ } => todo!(),
            Literal::LambdaBinOp { val: _ } => todo!(),
            Literal::LambdaUnOp { val: _ } => todo!(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal { val } =>
                write!(f, "{val}"),
            Expr::Var { var: _ } => todo!(),
            Expr::Lambda { args: _, body: _ } => todo!(),
            Expr::Typed { expr: _, t: _ } => todo!(),
            Expr::BinOp { op_l, op_r, op } =>
                write!(f, "{} {} {}", op_l, op, op_r),
            Expr::UnOp { op, op_r } =>
                write!(f, "{}{}", op, op_r),
            Expr::App { fun: _, args: _ } => todo!(),
            Expr::Tuple { elems } =>
                write!(f, "({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
            Expr::List { elems } =>
                write!(f, "[{}]", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
            Expr::ListRange { start: _, end: _ } => todo!(),
            Expr::ListComp { yield_expr: _, filters: _ } => todo!(),
            Expr::Record { fields: _ } => todo!(),
            Expr::RecordUpdate { record: _, updates: _ } => todo!(),
            Expr::Map { assigns: _ } => todo!(),
            Expr::MapUpdate { map: _, updates: _ } => todo!(),
            Expr::MapAccess { map: _, key: _, default: _ } => todo!(),
            Expr::Proj { expr: _, field: _ } => todo!(),
            Expr::Switch { exprs: _, cases: _ } => todo!(),
            Expr::If { conds: _, neg: _ } => todo!(),
            Expr::Block { stmts: _, value } => {
                write!(f, "{}{value}", " ".repeat(INDENT_SPACES * 3))?;
                Ok(())
            }
            Expr::Hole => write!(f, "???"),
        }
    }
}
