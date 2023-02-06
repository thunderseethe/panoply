extern crate proc_macro;

use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::*;

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
    Var(Pat),
    Abs(Bindings),
    TyAbs(Bindings),
    App(App),
    TyApp(Box<IrMatch>, Pat),
    Case(Box<IrMatch>, Pat),
    FieldProj(Pat, Box<IrMatch>),
    Tag(Pat, Box<IrMatch>),
    Struct(Punctuated<IrMatch, Token![,]>),
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
            "Struct" => IrMatch::Struct(
                content
                    .call(Punctuated::parse_separated_nonempty)
                    .unwrap_or(Punctuated::new()),
            ),
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
    fn as_match(
        self,
        id: proc_macro2::TokenStream,
        match_res: proc_macro2::TokenStream,
        uniq: &mut Uniq,
    ) -> proc_macro2::TokenStream {
        match self {
            IrMatch::Int(pat) => quote! {
                match #id {
                    IrKind::Int(#pat) => #match_res
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Int(#pat)), e),
                }
            },
            IrMatch::Var(pat) => quote! {
                match #id {
                    IrKind::Var(#pat) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Var(#pat)), e),
                }
            },
            IrMatch::Abs(bindings) => {
                let tmp = Ident::new("body", Span::call_site());
                let mut args = bindings.args.into_iter();
                let arg = args.next().unwrap();
                let (tmp, match_res) = args
                    .into_iter()
                    .rfold((tmp.clone(), bindings.matcher.as_match(quote!(&#tmp.ptr.kind), match_res, uniq)),
                    |(tmp, arm), arg| {
                        let id = Ident::new("body", Span::call_site());
                        (id.clone(), quote!(match &#id.ptr.kind {
                            IrKind::Abs(#arg, #tmp) => #arm
                            ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Abs(#arg, #tmp)), e),
                        }))
                    });
                let t = quote! {
                    match #id {
                        Abs(#arg, #tmp) => #match_res
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Abs(#arg, #tmp)), e),
                    }
                };

                //quote!(compile_error!(stringify!(#t)))
                t
            }
            IrMatch::App(app) => {
                /*
                 * App(App(App(x, y), z), w)
                 * match ... {
                 *   App(head1, arg0) => match arg0 {
                 *     w => match head0 {
                 *       App(head1, arg1) => match arg1 {
                 *          z => match head1 {
                 *              App(head2, arg2) => match arg2 {
                 *                  y => match head2 {
                 *                      x => match_res
                 *                  }
                 *              }
                 *          }
                 *       }
                 *     }
                 *   }
                 * }
                 */

                let head = app.head;
                let mut spine = app.spine.into_iter();
                let arg = spine.next().unwrap();

                let level = uniq.next();
                let hd_id = Ident::new(&format!("head{level}"), Span::call_site());
                let arg_id = Ident::new(&format!("arg{level}"), Span::call_site());
                let arg_match = arg.as_match(quote!(&#arg_id.ptr.kind), match_res, uniq);
                let hd_match = head.as_match(quote!(&#hd_id.ptr.kind), arg_match, uniq);
                let (hd, arg, match_res) = spine.into_iter().fold((hd_id, arg_id, hd_match), |(hd, arg, match_), next_arg| {
                    let level = uniq.next();
                    let hd_id = Ident::new(&format!("head{level}"), Span::call_site());
                    let arg_id = Ident::new(&format!("arg{level}"), Span::call_site());
                    let next_match = next_arg.as_match(quote!(&#arg_id.ptr.kind), match_, uniq);
                    (hd_id.clone(), arg_id, quote!(match &#hd_id.ptr.kind {
                        IrKind::App(#hd, #arg) => #next_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(head, arg)), e),
                    }))
                });
                quote!(match #id {
                    IrKind::App(#hd, #arg) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(head, arg)), e),
                })

                /*let hd = Ident::new(&format!("head{}", uniq), Span::call_site());
                let len = app.spine.len();
                let mut spine = app.spine.into_iter();
                let arg = spine.next().unwrap();
                let (hd_id, app, uniq) = spine.fold(
                    (hd.clone(), app.head.as_match(quote!(&#hd.ptr.kind), match_res, uniq + len), uniq + 1),
                    |(hd, app, i), arg| {
                        let head_id = Ident::new(&format!("head{}", i), Span::call_site());
                        let arg_id = Ident::new(&format!("arg{}", i), Span::call_site());
                        let arg_match = arg.as_match(quote!(&#arg_id.ptr.kind), app, uniq + len);
                        ( head_id.clone()
                        , quote!(match &#head_id.ptr.kind {
                            IrKind::App(#hd, #arg_id) => #arg_match,
                            ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(#hd, #arg_id)), e),
                        })
                        , i + 1
                        )
                    });
                let arg_id = Ident::new(&format!("arg{}", uniq), Span::call_site());
                let match_ = arg.as_match(quote!(&#arg_id.ptr.kind), app, uniq + 1);
                quote! {
                    match #id {
                        IrKind::App(#hd_id, #arg_id) => #match_,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(App(#hd, #arg_id)), e),
                    }

                }*/
            }
            IrMatch::TyAbs(bindings) => {
                let tmp = Ident::new("body", Span::call_site());
                let body_match = bindings
                    .matcher
                    .as_match(quote!(&#tmp.ptr.kind), match_res, uniq);

                let mut args = bindings.args.into_iter();
                let tv = args.next();
                let (tmp, match_) =
                    args.into_iter()
                        .rfold((tmp, body_match), |(tmp, matcher), tv| {
                            let id = Ident::new("body", Span::call_site());
                            (
                                id.clone(),
                                quote! {
                                    match &#id.ptr.kind {
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
                let forall_tmp = Ident::new("forall", Span::call_site());
                let forall_match = forall.as_match(quote!(&#forall_tmp.ptr.kind), match_res, uniq);
                quote! {
                    match #id {
                        IrKind::TyApp(#forall_tmp, #ty) => #forall_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(TyApp(#forall_tmp, #ty)), e),
                    }

                }
            }
            IrMatch::Struct(_) => quote! {
                match #id {
                    IrKind::Struct(elems) => #match_res,
                    ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Struct(elems)), e),
                }
            },
            IrMatch::FieldProj(indx, ir) => {
                let ir_tmp = Ident::new("ir", Span::call_site());
                let ir_match = ir.as_match(quote!(&#ir_tmp.ptr.kind), match_res, uniq);
                quote! {
                    match #id {
                        IrKind::FieldProj(#indx, #ir_tmp) => #ir_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(FieldProj(#indx, #ir_tmp)), e),
                    }
                }
            }
            IrMatch::Tag(tag, ir) => {
                let ir_tmp = Ident::new("ir", Span::call_site());
                let ir_match = ir.as_match(quote!(&#ir_tmp.ptr.kind), match_res, uniq);
                quote! {
                    match #id {
                        IrKind::Tag(#tag, #ir_tmp) => #ir_match,
                        ref e => panic!("assertion failed: expected `{}` but found `{:?}`", stringify!(Tag(#tag, #ir_tmp)), e),
                    }
                }
            }
            IrMatch::Case(discr, cases) => {
                let discr_tmp = Ident::new("discr", Span::call_site());
                let discr_match = discr.as_match(quote!(&#discr_tmp.ptr.kind), match_res, uniq);
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
    _fat_arrow: Token![=>],
    body: Box<Expr>,
}
impl Parse for IrMatchExpr {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        Ok(IrMatchExpr {
            actual: input.parse()?,
            _comma: input.parse()?,
            pattern: input.parse()?,
            _fat_arrow: input.parse()?,
            body: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn ir_matcher(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ir_match = parse_macro_input!(input as IrMatchExpr);
    let id = ir_match.actual;
    let mut uniq = Uniq::default();
    ir_match
        .pattern
        .as_match(quote!(#id.kind), ir_match.body.to_token_stream(), &mut uniq)
        .into()
}
