pub mod ast;
pub mod cst;
pub mod span;

/// Assert that each $id is an ident with text equal to it's variable name.
#[macro_export]
macro_rules! assert_ident_text_matches_name {
    ($db: ident, $id:ident) => { assert_ident_text_matches_name!($db, [$id]) };
    ($db: ident, [$($id:ident),*]) => {{
        $(
            assert_eq!($id.text(&$db), stringify!($id));
        )*
    }}
}
