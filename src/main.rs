#![allow(
    unused_imports,
    unused_variables,
    unreachable_code,
    non_snake_case,
    dead_code
)]

use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::bytes::complete::take_while;
use nom::bytes::complete::{take, take_till, take_until};
use nom::character::complete::char;
use nom::character::{is_alphabetic, is_alphanumeric};
use nom::combinator::{map, opt, recognize, value};
use nom::error::ErrorKind;
use nom::number::complete::double;
use nom::sequence::tuple;
use nom::{Err, IResult, Parser};

mod json;
mod math;

fn main() {
    println!("empty");
}

// //3+2*5
// //Function(Add,[Number(3), Function(Multiply,[Number(2), Number(5)])])

// // enum Component {
// //     Number(f64), //todo: handle pi as special case
// //     Variable(String),
// //     Group {
// //         symbol: GroupSymbol,
// //         components: Vec<Component>,
// //     },
// //     Unary {
// //         operator: Operator,
// //         component: Box<Component>,
// //     },
// //     Binary {
// //         operator: Operator,
// //         left: Box<Component>,
// //         right: Box<Component>,
// //     },
// // }
// // enum Operator {
// //     Add,
// //     Multiply,
// //     Sin,
// //     //...
// // }

// // #[derive(Debug, Clone, PartialEq)]
// // enum Atom {
// //     Number(f64), //todo: handle pi as special case
// //     Variable(String),
// // }
// // #[derive(Debug, Clone, PartialEq)]
// // enum Block {
// //     Paren(Expr),
// //     Bracket(Expr),
// //     Curly(Expr),
// //     Pipe(Expr),
// // }

// // #[derive(Debug, Clone, PartialEq, Eq)]
// // enum Action {
// //     Unknown,
// //     Add,
// //     Multiply,
// // }
// // #[derive(Debug, Clone, PartialEq)]
// // enum Expr {
// //     Unary(Box<Expr>, Action),
// //     Binary(Box<Expr>, Box<Expr>, Action),
// //     Ternary,
// //     Block(Box<Block>),
// //     Expr(Box<Expr>),
// //     Atom(Atom),
// // }

// // fn parse_block(input: &str, open: char, close: char) -> IResult<&str, &str, ()> {
// //     let tu = tuple((
// //         value("", char(open)),
// //         alt((
// //             move |i| parse_block(i, open, close),
// //             take_till(|c| c == close),
// //         )),
// //     ));
// //     let p = recognize(tu).parse(input);
// //     p
// // }

// // fn parse_subscript(input: &str) -> IResult<&str, &str, ()> {
// //     recognize(tuple((
// //         is_a("_"),
// //         take_while(|chr| is_alphanumeric(chr as u8)),
// //     )))
// //     .parse(input)
// // }
// // fn parse_variable(input: &str) -> IResult<&str, &str, ()> {
// //     recognize(tuple((
// //         alt(
// //             //Start
// //             (
// //                 recognize(tuple((
// //                     is_a("_"),
// //                     take_while(|chr| is_alphanumeric(chr as u8)),
// //                 ))),
// //                 recognize(tuple((
// //                     take_while(|chr| is_alphabetic(chr as u8)),
// //                     opt(take_while(|chr| is_alphanumeric(chr as u8))),
// //                 ))),
// //             ),
// //         ),
// //         opt(parse_subscript),
// //     )))
// //     .parse(input)
// // }

// // fn parse_number(input: &str) -> IResult<&str, f64, ()> {
// //     double(input)
// // }
// // fn parse_add_symbol(input: &str) -> IResult<&str, Action, ()> {
// //     value(Action::Add, char('+')).parse(input)
// // }
// // fn parse_expr(input: &str) -> IResult<&str, Expr, ()> {
// //     let (tail, left) = parse_number(input).unwrap();
// //     let (tail, action) = parse_add_symbol(tail).unwrap();
// //     let (tail, right) = parse_number(tail).unwrap();
// //     Ok((
// //         tail,
// //         Expr::Binary(
// //             Box::new(Expr::Atom(Atom::Number(left))),
// //             Box::new(Expr::Atom(Atom::Number(right))),
// //             action,
// //         ),
// //     ))
// // }

// // #[test]
// // fn test_block() {
// //     assert_eq!(parse_block("(frog+2)", '(', ')'), Ok(("", "frog+2")));
// // }

// // #[test]
// // fn test_double() {
// //     assert_eq!(parse_number("1.2"), Ok(("", 1.2)));
// // }
// // #[test]
// // fn test_sci_number() {
// //     assert_eq!(parse_number("-2e-04"), Ok(("", -2e-04)));
// // }
// // #[test]
// // fn test_add_symbol() {
// //     assert_eq!(parse_add_symbol("+"), Ok(("", Action::Add)));
// // }
// // #[test]
// // fn test_variable() {
// //     assert_eq!(parse_variable("_9foo3_9a"), Ok(("", "_9foo3_9a")));
// // }
// // #[test]
// // fn test_expr_simple() {
// //     let expr = Expr::Binary(
// //         Box::new(Expr::Atom(Atom::Number(3f64))),
// //         Box::new(Expr::Atom(Atom::Number(2f64))),
// //         Action::Add,
// //     );
// //     assert_eq!(parse_expr("3+2"), Ok(("", expr)));
// // }

// // #[test]
// // fn test_expr_recursive_order_of_operations1() {
// //     let innerExpr = Expr::Binary(
// //         Box::new(Expr::Atom(Atom::Number(2f64))),
// //         Box::new(Expr::Atom(Atom::Number(5f64))),
// //         Action::Multiply,
// //     );
// //     let outerExpr = Expr::Binary(
// //         Box::new(Expr::Atom(Atom::Number(3f64))),
// //         Box::new(innerExpr),
// //         Action::Add,
// //     );
// //     assert_eq!(parse_expr("3+2*5"), Ok(("", outerExpr)));
// // }
// // #[test]
// // fn test_expr_recursive_order_of_operations2() {
// //     unimplemented!();
// //     let innerExpr = Expr::Binary(
// //         Box::new(Expr::Atom(Atom::Number(2f64))),
// //         Box::new(Expr::Atom(Atom::Number(5f64))),
// //         Action::Multiply,
// //     );
// //     let outerExpr = Expr::Binary(
// //         Box::new(Expr::Atom(Atom::Number(3f64))),
// //         Box::new(innerExpr),
// //         Action::Add,
// //     );
// //     assert_eq!(parse_expr("(3+2)*5"), Ok(("", outerExpr)));
// // }

// // #[macro_use]
// // extern crate uom;
// // use uom::si::force::pound_force;
// // use uom::si::length::inch;
// // use uom::si::pressure::psi;

// // // fn main() {
// // //     let length = uom::si::f32::Length::new::<inch>(5.0);
// // //     let width = uom::si::f32::Length::new::<inch>(5.0);
// // //     let area = length * width;
// // //     let force = uom::si::f32::Force::new::<pound_force>(1000.0);
// // //     let stress = force / area;
// // //     println!("{:?}", stress.into_format_args(psi,  uom::fmt::DisplayStyle::Abbreviation));
// // // }

// fn main() {
//     use nom::bytes::complete::tag;
//     use nom::sequence::pair;

//     let mut parser = pair(tag("A"), tag("Z"));

//     assert_eq!(parser("AbcZ"), Ok(("", ("A", "Z"))));
//     assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
// }
