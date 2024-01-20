use std::fmt::{Display, self};

use crate::ast::ast::*;

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
        //for scope in &self.scopes {
        //    write!(f, "{scope}")?;
        //}

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
