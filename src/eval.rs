use crate::syntax::ast::*;

type Env = im::HashMap<Name, Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f64),
    Closure(Closure),
    Tuple(Vec<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    env: Env,
    params: Vec<RcPattern>,
    body: Block,
}

impl Value {
    fn to_int(self) -> Result<i32, String> {
        match self {
            Value::Int(i) => Ok(i),
            _ => Err("not an int".into()),
        }
    }

    fn to_float(self) -> Result<f64, String> {
        match self {
            Value::Float(f) => Ok(f),
            _ => Err("not a float".into()),
        }
    }

    fn to_closure(self) -> Result<Closure, String> {
        match self {
            Value::Closure(c) => Ok(c),
            _ => Err("not a closure".into()),
        }
    }

    fn to_tuple(self, n: usize) -> Result<Vec<Value>, String> {
        match self {
            Value::Tuple(vs) => {
                if vs.len() == n {
                    Ok(vs)
                } else {
                    Err("wrong size tuple".into())
                }
            }
            _ => Err("not a tuple".into()),
        }
    }
}

fn do_bop(bop: &Bop, v1: Value, v2: Value) -> Result<Value, String> {
    match v1 {
        Value::Int(i) => Ok(Value::Int(match bop {
            Bop::Add => i + v2.to_int()?,
            Bop::Sub => i - v2.to_int()?,
            Bop::Mul => i * v2.to_int()?,
            Bop::Div => i / v2.to_int()?,
        })),
        Value::Float(f) => Ok(Value::Float(match bop {
            Bop::Add => f + v2.to_float()?,
            Bop::Sub => f - v2.to_float()?,
            Bop::Mul => f * v2.to_float()?,
            Bop::Div => f / v2.to_float()?,
        })),
        _ => Err("can't bop a closure".into()),
    }
}

impl RcExpr {
    pub fn eval(&self, env: &Env) -> Result<Value, String> {
        let val = match self.inner.as_ref() {
            Expr::Var(name) => match env.get(name) {
                None => return Err("no var".into()),
                Some(val) => val.clone(),
            },
            // Ann(RcExpr, RcType), // Annotated expressions
            Expr::Literal(Literal::Int(i)) => Value::Int(*i),
            Expr::Literal(Literal::Float(f)) => Value::Float(*f),
            Expr::Bop { bop, e1, e2 } => {
                let v1 = e1.eval(env)?;
                let v2 = e2.eval(env)?;
                do_bop(bop, v1, v2)?
            }
            // Block(Scope<Nest<(RcPattern, Embed<RcExpr>)>, RcExpr>),
            Expr::Lambda { params, body } => {
                let mut closed_env = Env::new();

                // build the closed environment from the free vars
                for var in self.free_vars() {
                    if let Some(val) = env.get(&var) {
                        closed_env.insert(var, val.clone());
                    } else {
                        return Err("Couldn't find a var".into());
                    }
                }

                Value::Closure(Closure {
                    params: params.iter().map(|p| p.clone()).collect(),
                    body: body.clone(),
                    env: closed_env,
                })
            }
            Expr::Call { func, args } => {
                let closure = func.eval(env)?.to_closure()?;
                let mut v_args = Vec::with_capacity(args.len());
                for a in args {
                    v_args.push(a.eval(env)?);
                }

                let mut closed_env = closure.env;
                for (p, v) in closure.params.iter().zip(v_args) {
                    p.bind(v, &mut closed_env)?;
                }

                closure.body.eval(&closed_env)?
            }
            Expr::Block(block) => block.eval(env)?,
            Expr::Tuple(exprs) => {
                let mut vals = Vec::with_capacity(exprs.len());
                for e in exprs {
                    vals.push(e.eval(env)?);
                }
                Value::Tuple(vals)
            }
        };

        Ok(val)
    }
}

impl Block {
    pub fn eval(&self, env: &Env) -> Result<Value, String> {
        let mut env = env.clone();
        for stmt in &self.stmts {
            stmt.eval(&mut env)?;
        }
        self.expr.eval(&env)
    }
}

impl Stmt {
    pub fn eval(&self, env: &mut Env) -> Result<(), String> {
        match self {
            Stmt::Let { pat, expr } => {
                let val = expr.eval(env)?;
                pat.bind(val, env)
            }
        }
    }
}

impl RcPattern {
    pub fn bind(&self, val: Value, env: &mut Env) -> Result<(), String> {
        match self.inner.as_ref() {
            Pattern::Wildcard => Ok(()),
            Pattern::Literal(Literal::Int(i)) => {
                let vi = val.to_int()?;
                if *i == vi {
                    Ok(())
                } else {
                    Err("not equal at binding site".into())
                }
            }
            Pattern::Literal(Literal::Float(f)) => {
                let vf = val.to_float()?;
                if *f == vf {
                    Ok(())
                } else {
                    Err("not equal at binding site".into())
                }
            }
            Pattern::Binder(name) => {
                env.insert(name.clone(), val);
                Ok(())
            }
            Pattern::Tuple(pats) => {
                let vals = val.to_tuple(pats.len())?;
                for (pat, val) in pats.iter().zip(vals) {
                    pat.bind(val, env)?;
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::{parse_expr, parse_pattern};

    fn parse_value(s: &str) -> Value {
        let expr = parse_expr(s);
        let env = Env::new();
        expr.eval(&env).unwrap()
    }

    fn mk_env(names_and_vals: &[(&str, &str)]) -> Env {
        names_and_vals
            .iter()
            .map(|&(n, v)| {
                let name: Name = n.into();
                let val = parse_value(v);
                (name, val)
            })
            .collect()
    }

    #[test]
    fn test_simple_eval() {
        let empty = Env::new();

        let e = parse_expr("2 + 5");
        assert_eq!(e.eval(&empty), Ok(Value::Int(7)));

        let e = parse_expr("(|x| {x * x})(3)");
        assert_eq!(e.eval(&empty), Ok(Value::Int(9)));

        let e = parse_expr("(1 + 1)");
        assert_eq!(e.eval(&empty), Ok(Value::Int(2)));

        let e = parse_expr("(1 + 1,)");
        assert_eq!(e.eval(&empty), Ok(Value::Tuple(vec![Value::Int(2)])));

        let e = parse_expr("(1 + 1, 1.0 + 1.0)");
        assert_eq!(
            e.eval(&empty),
            Ok(Value::Tuple(vec![Value::Int(2), Value::Float(2.0)]))
        );
    }

    #[test]
    fn test_pattern_bind() {
        fn bind(pat: RcPattern, val: Value) -> Result<Env, String> {
            let mut env = Env::new();
            pat.bind(val, &mut env)?;
            Ok(env)
        }

        let pat = parse_pattern("5");
        let val = Value::Int(5);
        assert_eq!(bind(pat.into(), val), Ok(Env::new()));

        let pat = parse_pattern("6");
        let val = Value::Int(5);
        assert!(bind(pat.into(), val).is_err());

        let pat = parse_pattern("(x, 2, y, _)");
        let val = parse_value("(1, 2, 3, 4)");
        assert_eq!(
            bind(pat.into(), val),
            Ok(mk_env(&[("x", "1"), ("y", "3"),]))
        );
    }

}
