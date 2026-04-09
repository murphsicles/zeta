impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Int(value) => write!(f, "{}", value),
            Expr::Bool(value) => write!(f, "{}", value),
            Expr::Len(expr) => write!(f, "len({})", expr),
            Expr::Index(array, index) => write!(f, "{}[{}]", array, index),
            Expr::BinOp(left, op, right) => {
                let op_str = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                    BinOp::Mod => "%",
                };
                write!(f, "({} {} {})", left, op_str, right)
            }
            Expr::Apply(func, args) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}({})", func, args_str.join(", "))
            }
        }
    }
}