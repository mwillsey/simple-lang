use crate::syntax::ast::*;

type Env = im::HashMap<Name, RcType>;

impl RcType {
    pub fn subtype(&self, other: &RcType) -> Result<(), String> {
        if self == other {
            Ok(())
        } else {
            Err("types not equal".into())
        }
    }
}

impl RcExpr {
    pub fn check_type(&self, env: &Env, other: &RcType) -> Result<(), String> {
        match self.inner.as_ref() {
            Expr::Lambda { params, body } => match other.inner.as_ref() {
                Type::Fn(param_types, ret_type) => {
                    let env = env.clone() + bind_multiple_types(params, &param_types)?;
                    body.check_type(&env, ret_type)
                }
                _ => Err("can't check lambda against non function".into()),
            },
            _ => self.infer_type(env)?.subtype(other),
        }
    }

    pub fn infer_type(&self, env: &Env) -> Result<RcType, String> {
        match self.inner.as_ref() {
            Expr::Var(name) => match env.get(name) {
                Some(t) => Ok(t.clone()),
                None => Err("can't find var".into()),
            },
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => Ok(Type::Int.into()),
                Literal::Float(_) => Ok(Type::Float.into()),
            },
            Expr::Bop { bop, e1, e2 } => {
                let _ = bop;
                let t1 = e1.infer_type(env)?;
                let t2 = e2.infer_type(env)?;
                match (t1.inner.as_ref(), t2.inner.as_ref()) {
                    (Type::Int, Type::Int) => Ok(Type::Int.into()),
                    (Type::Float, Type::Float) => Ok(Type::Float.into()),
                    _ => Err("binop error".into()),
                }
            }
            Expr::Block(block) => block.infer_type(env.clone()),
            Expr::Lambda { params, body } => {
                let mut param_types = Vec::with_capacity(params.len());
                for p in params {
                    match p.get_annotated_type() {
                        Some(t) => param_types.push(t),
                        None => return Err("need annotations on lambda".into()),
                    }
                }

                let env = env.clone() + bind_multiple_types(params, &param_types)?;
                let ret = body.infer_type(&env)?;
                Ok(Type::Fn(param_types, ret).into())
            }
            Expr::Call { func, args } => match func.infer_type(env)?.inner.as_ref() {
                Type::Fn(param_types, ret) => {
                    if param_types.len() != args.len() {
                        return Err("arg len mismatch".into());
                    }
                    for (pt, a) in param_types.iter().zip(args) {
                        a.check_type(env, pt)?
                    }
                    Ok(ret.clone())
                }
                _ => Err("calling a non function".into()),
            },
            Expr::Tuple(exprs) => {
                let mut types = Vec::with_capacity(exprs.len());
                for e in exprs {
                    types.push(e.infer_type(env)?);
                }
                Ok(Type::Tuple(types).into())
            }
            Expr::Match { .. } => unimplemented!(),
            Expr::Left { .. } => unimplemented!(),
            Expr::Right { .. } => unimplemented!(),
        }
    }
}

impl Block {
    pub fn infer_type(&self, mut env: Env) -> Result<RcType, String> {
        for stmt in &self.stmts {
            stmt.bind_type(&mut env)?;
        }
        self.expr.infer_type(&env)
    }
}

impl Stmt {
    pub fn bind_type(&self, env: &mut Env) -> Result<(), String> {
        match self {
            Stmt::Let { pat, expr } => {
                let typ = match pat.get_annotated_type() {
                    None => expr.infer_type(env)?,
                    Some(t) => {
                        // let was fully annotated, so we can check
                        // the expression instead of inferring it
                        expr.check_type(env, &t)?;
                        t
                    }
                };
                pat.bind_type(&typ).map(|en| env.extend(en))
            }
        }
    }
}

fn bind_multiple_types(patterns: &[RcPattern], types: &[RcType]) -> Result<Env, String> {
    if patterns.len() != types.len() {
        return Err("binding of unequal length".into());
    }

    let mut env = Env::new();

    for (p, t) in patterns.iter().zip(types) {
        for (name, typ) in p.bind_type(t)? {
            if let Some(_) = env.insert(name.clone(), typ) {
                return Err(format!("duplicate binding of {:?}", name));
            }
        }
    }

    Ok(env)
}

impl RcPattern {
    fn bind_type(&self, typ: &RcType) -> Result<Env, String> {
        match self.inner.as_ref() {
            Pattern::Wildcard => Ok(Env::new()),
            Pattern::Binder(name) => Ok(Env::unit(name.clone(), typ.clone())),
            Pattern::Literal(lit) => match (lit, typ.inner.as_ref()) {
                (Literal::Int(_), Type::Int) => Ok(Env::new()),
                (Literal::Float(_), Type::Float) => Ok(Env::new()),
                _ => Err("failed to bind to literal".into()),
            },
            Pattern::Annotated(pat, ann_type) => {
                ann_type.subtype(ann_type)?;
                pat.bind_type(ann_type)
            }
            Pattern::Tuple(pats) => match typ.inner.as_ref() {
                Type::Tuple(types) => bind_multiple_types(pats, &types),
                _ => Err("can't bind non-tuple to tuple".into()),
            },
        }
    }

    fn get_annotated_type(&self) -> Option<RcType> {
        match self.inner.as_ref() {
            Pattern::Wildcard => None,
            Pattern::Binder(_) => None,
            Pattern::Literal(lit) => match lit {
                Literal::Int(_) => Some(Type::Int.into()),
                Literal::Float(_) => Some(Type::Float.into()),
            },
            Pattern::Annotated(_pat, typ) => Some(typ.clone()),
            Pattern::Tuple(pats) => {
                let types: Option<Vec<RcType>> =
                    pats.iter().map(|p| p.get_annotated_type()).collect();
                types.map(|ts| Type::Tuple(ts).into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::{expr, pattern, typ};

    macro_rules! infer {
        ($e:expr) => {{
            let e = expr!($e);
            let env = Env::new();
            e.infer_type(&env)
        }};
    }

    #[test]
    fn test_infer_type() {
        assert_eq!(
            infer!({
                let x = 1;
                let square = |x: Int| x * x;
                square(x)
            }),
            Ok(typ!(Int))
        );

        assert_eq!(
            infer!(|x: Int| |y: Int| x * x),
            Ok(typ!(Fn (Int) -> Fn (Int) -> Int))
        );
    }

    #[test]
    fn test_infer_lambda() {
        // bare lambda should not be inferred
        assert!(infer!(|x| x + 1).is_err());

        // bound lambda without annotation should also not be inferred
        assert!(infer!({
            let inc = |x| x + 1;
            inc
        })
        .is_err());

        // bound lambda with annotation should be inferred
        assert_eq!(
            infer!({
                let inc: Fn(Int) -> Int = |x| x + 1;
                inc
            }),
            Ok(typ!(Fn(Int) -> Int))
        );

        // lambda at call site can also be inferred
        assert_eq!(
            infer!({
                let do_twice = |f: Fn(Int) -> Int, x: Int| f(f(x));
                do_twice(|x| x + 1, 1)
            }),
            Ok(typ!(Int))
        );
    }

    #[test]
    fn test_get_annotated_type() {
        let pat = pattern! { 1 };
        let typ = typ! { Int };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { x };
        assert_eq!(pat.get_annotated_type(), None);

        let pat = pattern! { ((x, y): (Int, Float)) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (x: Int, _: Float) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (3, y: Float) };
        let typ = typ! { (Int, Float) };
        assert_eq!(pat.get_annotated_type(), Some(typ));

        let pat = pattern! { (x: Int, y: Float, _) };
        assert_eq!(pat.get_annotated_type(), None);
    }
}
