use std::rc::Rc;

use im::HashSet as Set;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Fn { args: Vec<RcType>, ret: RcType },
}

pub type Name = Rc<str>;

/// Reference counted types
#[derive(Debug, Clone, PartialEq)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f64),
}

/// Patterns

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    // Ann(RcPattern, RcType),
    Literal(Literal),
    Binder(Name),
    // Record(Vec<(String, RcPattern)>),
    // Tag(String, RcPattern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RcPattern {
    pub inner: Rc<Pattern>,
}

impl From<Pattern> for RcPattern {
    fn from(src: Pattern) -> RcPattern {
        RcPattern {
            inner: Rc::new(src),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
}

/// Expressions

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Name),
    // Ann(RcExpr, RcType), // Annotated expressions
    Literal(Literal),
    Bop { bop: Bop, e1: RcExpr, e2: RcExpr },
    Block(Block),
    Lambda { params: Vec<RcPattern>, body: Block },
    Call { func: RcExpr, args: Vec<RcExpr> },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: RcExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { pat: RcPattern, expr: RcExpr },
}

/// Reference counted expressions
#[derive(Debug, Clone, PartialEq)]
pub struct RcExpr {
    pub inner: Rc<Expr>,
}

impl From<Expr> for RcExpr {
    fn from(src: Expr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl RcExpr {
    pub fn free_vars(&self) -> Set<Name> {
        self.free_vars_not_bound(&Set::new())
    }

    fn free_vars_not_bound(&self, bound: &Set<Name>) -> Set<Name> {
        match self.inner.as_ref() {
            Expr::Var(name) => {
                if !bound.contains(name) {
                    Set::unit(name.clone())
                } else {
                    Set::new()
                }
            }
            // Ann(RcExpr, RcType), // Annotated expressions
            Expr::Literal(_) => Set::new(),
            Expr::Bop { e1, e2, .. } => {
                e1.free_vars_not_bound(bound) + e2.free_vars_not_bound(bound)
            }
            Expr::Lambda { params, body } => {
                let mut bound = bound.clone();
                for pat in params {
                    pat.add_bound_vars(&mut bound)
                }
                body.free_vars_not_bound(&bound)
            }
            Expr::Call { func, args } => {
                let func_fv = func.free_vars_not_bound(bound);
                let args_fv = Set::unions(args.iter().map(|a| a.free_vars_not_bound(bound)));
                func_fv + args_fv
            }
            Expr::Block(block) => block.free_vars_not_bound(bound),
            Expr::Unit => Set::new(),
        }
    }
}

impl Block {
    fn free_vars_not_bound(&self, bound: &Set<Name>) -> Set<Name> {
        let mut bound = bound.clone();
        let stmts_free = Set::unions(self.stmts.iter().map(|s| s.free_vars_not_bound(&mut bound)));
        stmts_free + self.expr.free_vars_not_bound(&bound)
    }
}

impl Stmt {
    fn free_vars_not_bound(&self, bound: &mut Set<Name>) -> Set<Name> {
        match self {
            Stmt::Let { pat, expr } => {
                let free = expr.free_vars();
                pat.add_bound_vars(bound);
                free
            }
        }
    }
}

impl RcPattern {
    fn add_bound_vars(&self, bound: &mut Set<Name>) {
        match self.inner.as_ref() {
            Pattern::Wildcard => (),
            Pattern::Literal(_) => (),
            Pattern::Binder(name) => {
                bound.insert(name.clone());
            }
        }
    }
}

pub fn var(s: &'static str) -> RcExpr {
    Expr::Var(s.into()).into()
}

pub fn int(i: i32) -> RcExpr {
    Expr::Literal(Literal::Int(i)).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::grammar::parse_expr;

    fn set(strs: &[&str]) -> Set<Name> {
        strs.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn test_free_variables() {
        let e = parse_expr("|x| {x + x}");
        assert_eq!(e.free_vars(), set(&[]));

        let e = parse_expr("x + x");
        assert_eq!(e.free_vars(), set(&["x"]));

        let e = parse_expr("|x| {y + x}");
        assert_eq!(e.free_vars(), set(&["y"]));

        let e = parse_expr("|y| {|x| {x + y + z}}");
        assert_eq!(e.free_vars(), set(&["z"]));
    }

}
