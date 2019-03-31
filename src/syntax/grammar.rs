include!(concat!(env!("OUT_DIR"), "/syntax/grammar.rs"));

type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, &'a str>;

pub fn parse_expr(s: &str) -> Result<RcExpr, ParseError> {
    ExprParser::new().parse(s).into()
}

#[cfg(test)]
macro_rules! expr {
    ($e:expr) => {
        crate::syntax::grammar::parse_expr(&stringify!($e)).unwrap()
    };
}

pub fn parse_pattern(s: &str) -> Result<RcPattern, ParseError> {
    PatternParser::new().parse(s).into()
}

#[cfg(test)]
macro_rules! pattern {
    // this has to bind token tree (tt) instead of a pattern (pat)
    // because our pattern grammar is richer than rust's
    ($p:tt) => {
        crate::syntax::grammar::parse_pattern(&stringify!($p)).unwrap()
    };
}

pub fn parse_type(s: &str) -> Result<RcType, ParseError> {
    TypeParser::new().parse(s).into()
}

#[cfg(test)]
macro_rules! typ {
    ($t:ty) => {
        crate::syntax::grammar::parse_type(&stringify!($t)).unwrap()
    };
}

#[cfg(test)]
pub(crate) use {expr, pattern, typ};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple() {
        use crate::syntax::ast;
        use ast::Expr::*;

        fn binder(s: &str) -> RcPattern {
            ast::Pattern::Binder(s.into()).into()
        }

        assert_eq!(
            expr!(|x| |y| x * y),
            Lambda {
                params: vec![binder("x")],
                body: Lambda {
                    params: vec![binder("y")],
                    body: Bop {
                        bop: ast::Bop::Mul,
                        e1: var("x"),
                        e2: var("y"),
                    }
                    .into()
                }
                .into()
            }
            .into()
        );

        assert_eq!(
            expr!(1 + x()),
            Bop {
                bop: ast::Bop::Add,
                e1: int(1),
                e2: Call {
                    func: var("x"),
                    args: vec![],
                }
                .into(),
            }
            .into()
        );

        assert_eq!(
            expr!(1 * x()),
            Bop {
                bop: ast::Bop::Mul,
                e1: int(1),
                e2: Call {
                    func: var("x"),
                    args: vec![],
                }
                .into(),
            }
            .into()
        );

        assert_eq!(expr!((x)), var("x"));
        assert_eq!(expr!((x,)), Tuple(vec![var("x")]).into());
    }
}
