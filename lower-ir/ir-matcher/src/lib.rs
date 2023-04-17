extern crate proc_macro;

use im::HashSet;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{parse::Parse, punctuated::Punctuated, *};

#[derive(Default)]
struct Uniq {
    supply: usize,
}
impl Uniq {
    fn next(&mut self) -> usize {
        let next = self.supply;
        self.supply += 1;
        next
    }

    fn ident(&mut self, tag: &str) -> Ident {
        Ident::new(&format!("{}{}", tag, self.next()), Span::call_site())
    }
}

struct Bindings {
    args: Punctuated<Ident, Token![,]>,
    _comma: Token![,],
    matcher: Box<IrMatch>,
}

impl Parse for Bindings {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        Ok(Bindings {
            args: content.call(Punctuated::parse_separated_nonempty)?,
            _comma: input.parse()?,
            matcher: input.parse()?,
        })
    }
}

struct App {
    head: Box<IrMatch>,
    _comma: Token![,],
    spine: Punctuated<Box<IrMatch>, Token![,]>,
}

impl Parse for App {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        Ok(App {
            head: content.parse()?,
            _comma: content.parse()?,
            spine: content.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}

enum IrMatch {
    Int(Pat),
    Var(Ident),
    Abs(Bindings),
    TyAbs(Bindings),
    App(App),
    TyApp(Box<IrMatch>, Pat),
    Case(Box<IrMatch>, Pat),
    FieldProj(Pat, Box<IrMatch>),
    Tag(Pat, Box<IrMatch>),
    Struct(Pat),
}

impl Parse for IrMatch {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let id = input.parse::<Ident>()?;
        let content;
        parenthesized!(content in input);
        Ok(match id.to_string().as_ref() {
            "TyAbs" => IrMatch::TyAbs(content.parse()?),
            "Abs" => IrMatch::Abs(content.parse()?),
            "Int" => IrMatch::Int(content.parse()?),
            "Var" => IrMatch::Var(content.parse()?),
            "Struct" => IrMatch::Struct(content.parse()?),
            "App" => IrMatch::App(content.parse()?),
            "TyApp" => {
                let forall = content.parse()?;
                content.parse::<Token![,]>()?;
                let ty = content.parse()?;
                IrMatch::TyApp(forall, ty)
            }
            "Case" => {
                let discr = content.parse()?;
                content.parse::<Token![,]>()?;
                let cases = content.parse()?;
                IrMatch::Case(discr, cases)
            }
            "FieldProj" => {
                let index = content.parse()?;
                content.parse::<Token![,]>()?;
                let term = content.parse()?;
                IrMatch::FieldProj(index, term)
            }
            "Tag" => {
                let tag = content.parse()?;
                content.parse::<Token![,]>()?;
                let term = content.parse()?;
                IrMatch::Tag(tag, term)
            }
            _ => return Err(input.error("Expected an IrKind variant")),
        })
    }
}

impl IrMatch {
    fn into_match(
        self,
        id: proc_macro2::TokenStream,
        match_res: proc_macro2::TokenStream,
        uniq: &mut Uniq,
        bound: HashSet<Ident>,
    ) -> proc_macro2::TokenStream {
        match self {
            IrMatch::Int(pat) => quote! {
                match #id {
                    IrKind::Int(#pat) => #match_res
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Int(#pat)), e),
                }
            },
            IrMatch::Var(var) => {
                let (pat, match_res) = if bound.contains(&var) {
                    let vee = uniq.ident("var");
                    let match_res = quote!({ assert_eq!(#var, #vee); #match_res });
                    (vee, match_res)
                } else {
                    (var, match_res)
                };
                quote!( match #id {
                    IrKind::Var(#pat) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Var(#pat)), e),
                })
            }
            IrMatch::Abs(bindings) => {
                let tmp = uniq.ident("body");
                let mut bound = bound;
                bound.extend(bindings.args.iter().cloned());
                let mut args = bindings.args.into_iter();
                let arg = args.next().unwrap();
                let (tmp, match_res) = args
                    .rfold((tmp.clone(), bindings.matcher.into_match(quote!(#tmp.kind()), match_res, uniq, bound)),
                    |(tmp, arm), arg| {
                        let id = uniq.ident("body");
                        (id.clone(), quote!(match #id.kind() {
                            IrKind::Abs(#arg, #tmp) => #arm
                            ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Abs(#arg, #tmp)), e),
                        }))
                    });
                quote! {
                    match #id {
                        Abs(#arg, #tmp) => #match_res
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Abs(#arg, #tmp)), e),
                    }
                }
            }
            IrMatch::App(app) => {
                let head = app.head;
                let mut spine = app.spine.into_iter();
                let arg = spine.next().unwrap();

                let level = uniq.next();
                let hd_id = Ident::new(&format!("head{level}"), Span::call_site());
                let arg_id = Ident::new(&format!("arg{level}"), Span::call_site());
                let arg_match =
                    arg.into_match(quote!(#arg_id.kind()), match_res, uniq, bound.clone());
                let hd_match =
                    head.into_match(quote!(#hd_id.kind()), arg_match, uniq, bound.clone());
                let (hd, arg, match_res) = spine.fold((hd_id, arg_id, hd_match), |(hd, arg, match_), next_arg| {
                    let level = uniq.next();
                    let hd_id = Ident::new(&format!("head{level}"), Span::call_site());
                    let arg_id = Ident::new(&format!("arg{level}"), Span::call_site());
                    let next_match = next_arg.into_match(quote!(#arg_id.kind()), match_, uniq, bound.clone());
                    (hd_id.clone(), arg_id, quote!(match #hd_id.kind() {
                        IrKind::App(#hd, #arg) => #next_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(head, arg)), e),
                    }))
                });
                quote!(match #id {
                    IrKind::App(#hd, #arg) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(head, arg)), e),
                })
            }
            IrMatch::TyAbs(bindings) => {
                let tmp = uniq.ident("body");
                let body_match =
                    bindings
                        .matcher
                        .into_match(quote!(#tmp.kind()), match_res, uniq, bound);

                let mut args = bindings.args.into_iter();
                let tv = args.next();
                let (tmp, match_) =
                    args.rfold((tmp, body_match), |(tmp, matcher), tv| {
                            let id = uniq.ident("body");
                            (
                                id.clone(),
                                quote! {
                                    match #id.kind() {
                                        IrKind::TyAbs(#tv, #tmp) => #matcher,
                                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(TyAbs(#tv, #tmp)), e),
                                    }
                                },
                            )
                        });

                quote! {
                    match #id {
                        IrKind::TyAbs(#tv, #tmp) => #match_,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(TyAbs(#tv, #tmp)), e),
                    }
                }
            }
            IrMatch::TyApp(forall, ty) => {
                let forall_tmp = uniq.ident("forall");
                let forall_match =
                    forall.into_match(quote!(#forall_tmp.kind()), match_res, uniq, bound);
                quote! {
                    match #id {
                        IrKind::TyApp(#forall_tmp, #ty) => #forall_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(TyApp(#forall_tmp, #ty)), e),
                    }

                }
            }
            IrMatch::Struct(pat) => quote! {
                match #id {
                    IrKind::Struct(#pat) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Struct(elems)), e),
                }
            },
            IrMatch::FieldProj(indx, ir) => {
                let ir_tmp = uniq.ident("ir");
                let ir_match = ir.into_match(quote!(#ir_tmp.kind()), match_res, uniq, bound);
                quote! {
                    match #id {
                        IrKind::FieldProj(#indx, #ir_tmp) => #ir_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(FieldProj(#indx, #ir_tmp)), e),
                    }
                }
            }
            IrMatch::Tag(tag, ir) => {
                let ir_tmp = uniq.ident("ir");
                let ir_match = ir.into_match(quote!(#ir_tmp.kind()), match_res, uniq, bound);
                quote! {
                    match #id {
                        IrKind::Tag(#tag, #ir_tmp) => #ir_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Tag(#tag, #ir_tmp)), e),
                    }
                }
            }
            IrMatch::Case(discr, cases) => {
                let discr_tmp = uniq.ident("discr");
                let discr_match =
                    discr.into_match(quote!(#discr_tmp.kind()), match_res, uniq, bound);
                quote! {
                    match #id {
                        IrKind::Case(#discr_tmp, #cases) => #discr_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Case(#discr_tmp, #cases)), e),
                    }
                }
            }
        }
    }
}

struct IrMatchExpr {
    actual: Ident,
    _comma: Token![,],
    pattern: IrMatch,
    body: Option<Box<Expr>>,
}
impl Parse for IrMatchExpr {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        Ok(IrMatchExpr {
            actual: input.parse()?,
            _comma: input.parse()?,
            pattern: input.parse()?,
            body: input
                .peek(Token![=>])
                .then(|| {
                    input.parse::<Token![=>]>()?;
                    input.parse()
                })
                .transpose()?,
        })
    }
}

#[proc_macro]
pub fn ir_matcher(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ir_match = parse_macro_input!(input as IrMatchExpr);
    let id = ir_match.actual;
    let mut uniq = Uniq::default();
    let bound = HashSet::default();
    let t = ir_match.pattern.into_match(
        quote!(&#id.kind),
        ir_match.body.to_token_stream(),
        &mut uniq,
        bound,
    );
    //quote!(compile_error!(stringify!(#t))).into()
    t.into()
}
