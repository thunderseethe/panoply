use chumsky::{Error, Parser};

// Folds the prefixes `lefts` into the base expression `right` using `f`.
fn prefold<L, R, I, C, F>(lefts: C, right: R, f: F) -> R
where
    I: DoubleEndedIterator<Item = L>,
    C: IntoIterator<Item = L, IntoIter = I>,
    F: Fn(L, R) -> R,
{
    lefts.into_iter().rfold(right, |r, l| f(l, r))
}

// Folds the postfixes `rights` into the base expression `left` using `f`.
fn postfold<L, R, C, F>(left: L, rights: C, f: F) -> L
where
    C: IntoIterator<Item = R>,
    F: Fn(L, R) -> L,
{
    rights.into_iter().fold(left, |l, r| f(l, r))
}

// Folds the op-expression pairs `oprights` into the base expression `left` using `f`.
fn inlfold<L, O, R, C, F>(left: L, oprights: C, f: F) -> L
where
    C: IntoIterator<Item = (O, R)>,
    F: Fn(L, O, R) -> L,
{
    oprights.into_iter().fold(left, |l, (o, r)| f(l, o, r))
}

// Folds the expression-op pairs `leftops` into the base expression `right` using `f`.
fn inrfold<L, O, R, I, C, F>(leftops: C, right: R, f: F) -> R
where
    I: DoubleEndedIterator<Item = (L, O)>,
    C: IntoIterator<Item = (L, O), IntoIter = I>,
    F: Fn(L, O, R) -> R,
{
    leftops.into_iter().rfold(right, |r, (l, o)| f(l, o, r))
}

// Returns a parser for the production `right ::= left right`, using `f` to fold each pair into a
// new expression.
pub(crate) fn prefix<I, L, R, E, PL, PR, F>(
    left: PL,
    right: PR,
    f: F,
) -> impl Clone + Parser<I, R, Error = E>
where
    I: Clone,
    E: Error<I>,
    PL: Clone + Parser<I, L, Error = E>,
    PR: Clone + Parser<I, R, Error = E>,
    F: Clone + Fn(L, R) -> R,
{
    left.repeated()
        .then(right)
        .map(move |(ls, r)| prefold(ls, r, f.clone()))
}

// Returns a parser for the production `left ::= left right`, using `f` to fold each pair into a
// new expression.
pub(crate) fn postfix<I, L, R, E, PL, PR, F>(
    left: PL,
    right: PR,
    f: F,
) -> impl Clone + Parser<I, L, Error = E>
where
    I: Clone,
    E: Error<I>,
    PL: Clone + Parser<I, L, Error = E>,
    PR: Clone + Parser<I, R, Error = E>,
    F: Clone + Fn(L, R) -> L,
{
    left.then(right.repeated())
        .map(move |(l, rs)| postfold(l, rs, f.clone()))
}

// Returns a parser for the production `left ::= left op right`, using `f` to fold each triple into
// a new expression.
pub(crate) fn infixl<I, L, O, R, E, PL, PO, PR, F>(
    left: PL,
    op: PO,
    right: PR,
    f: F,
) -> impl Clone + Parser<I, L, Error = E>
where
    I: Clone,
    E: Error<I>,
    PL: Clone + Parser<I, L, Error = E>,
    PO: Clone + Parser<I, O, Error = E>,
    PR: Clone + Parser<I, R, Error = E>,
    F: Clone + Fn(L, O, R) -> L,
{
    left.then(op.then(right).repeated())
        .map(move |(l, ors)| inlfold(l, ors, f.clone()))
}

// Returns a parser for the production `right ::= left op right`, using `f` to fold each triple into
// a new expression.
pub(crate) fn infixr<I, L, O, R, E, PL, PO, PR, F>(
    left: PL,
    op: PO,
    right: PR,
    f: F,
) -> impl Clone + Parser<I, R, Error = E>
where
    I: Clone,
    E: Error<I>,
    PL: Clone + Parser<I, L, Error = E>,
    PO: Clone + Parser<I, O, Error = E>,
    PR: Clone + Parser<I, R, Error = E>,
    F: Clone + Fn(L, O, R) -> R,
{
    left.then(op)
        .repeated()
        .then(right)
        .map(move |(los, r)| inrfold(los, r, f.clone()))
}

// Returns a parser for the production `term ::= term op term`, using `f` to fold each triple into a
// new expression left-associatively.
#[allow(dead_code)]
pub(crate) fn infixl1<I, T, O, E, PT, PO, F>(
    term: PT,
    op: PO,
    f: F,
) -> impl Clone + Parser<I, T, Error = E>
where
    I: Clone,
    E: Error<I>,
    PT: Clone + Parser<I, T, Error = E>,
    PO: Clone + Parser<I, O, Error = E>,
    F: Clone + Fn(T, O, T) -> T,
{
    infixl(term.clone(), op, term, f)
}

// Returns a parser for the production `term ::= term op term`, using `f` to fold each triple into a
// new expression right-associatively.
#[allow(dead_code)]
pub(crate) fn infixr1<I, T, O, E, PT, PO, F>(
    term: PT,
    op: PO,
    f: F,
) -> impl Clone + Parser<I, T, Error = E>
where
    I: Clone,
    E: Error<I>,
    PT: Clone + Parser<I, T, Error = E>,
    PO: Clone + Parser<I, O, Error = E>,
    F: Clone + Fn(T, O, T) -> T,
{
    infixr(term.clone(), op, term, f)
}

#[cfg(test)]
mod tests {
    use chumsky::{
        prelude::{end, just, Simple},
        Parser,
    };

    use super::{infixl1, infixr1, postfix, prefix};

    fn node2(left: impl AsRef<str>, right: impl AsRef<str>) -> String {
        format!("({} {})", left.as_ref(), right.as_ref())
    }

    fn node3(left: impl AsRef<str>, middle: impl AsRef<str>, right: impl AsRef<str>) -> String {
        format!("({} {} {})", left.as_ref(), middle.as_ref(), right.as_ref())
    }

    fn parser() -> impl Parser<char, String, Error = Simple<char>> {
        let a = just("a".to_owned());
        let b = just("b".to_owned());
        let l = just("l".to_owned());
        let r = just("r".to_owned());
        let y = just("y".to_owned());
        let z = just("z".to_owned());
        let atom = just("_".to_owned());

        let affix = prefix(b, postfix(atom, y, node2), node2);
        let infix_r = infixr1(affix, r, node3);
        let infix_l = infixl1(infix_r, l, node3);
        postfix(prefix(a, infix_l, node2), z, node2).then_ignore(end())
    }

    #[test]
    fn test_affix_precedence() {
        let tree = parser().parse("abb_yzz");
        assert_matches!(tree, Ok(..));
        assert_eq!(tree.unwrap(), "(((a (b (b (_ y)))) z) z)".to_owned());
    }

    #[test]
    fn test_infix_associativity() {
        let ltree = parser().parse("_l_l_l_");
        assert_matches!(ltree, Ok(..));
        assert_eq!(ltree.unwrap(), "(((_ l _) l _) l _)".to_owned());

        let rtree = parser().parse("_r_r_r_");
        assert_matches!(rtree, Ok(..));
        assert_eq!(rtree.unwrap(), "(_ r (_ r (_ r _)))".to_owned());
    }

    #[test]
    fn test_infix_precedence() {
        let tree = parser().parse("_l_r_l_r_");
        assert_matches!(tree, Ok(..));
        assert_eq!(tree.unwrap(), "((_ l (_ r _)) l (_ r _))".to_owned());
    }

    #[test]
    fn test_affix_infix_precedence() {
        let tree = parser().parse("a_l_yrb_z");
        assert_matches!(tree, Ok(..));
        assert_eq!(tree.unwrap(), "((a (_ l ((_ y) r (b _)))) z)".to_owned());
    }
}
