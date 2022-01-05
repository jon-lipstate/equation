use std::{borrow::Cow, cmp::Ordering, collections::HashMap, num::ParseFloatError};

use nom::{
    branch::alt,
    bytes::complete::{is_a, take_till, take_while, take_while1},
    character::{
        complete::{char, multispace0},
        is_alphabetic, is_alphanumeric,
    },
    combinator::{map, opt, recognize, value},
    error::{ErrorKind, FromExternalError, ParseError},
    multi,
    number::complete::double,
    sequence::{preceded, tuple},
    IResult, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate, //-1*n
    Exponent,
    Sin,
}
impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.priority().partial_cmp(&other.priority())
    }
}
impl Action {
    fn priority(&self) -> usize {
        //P E MD AS
        match &self {
            Action::Add => 1,
            Action::Subtract => 1,
            Action::Multiply => 2,
            Action::Divide => 2,
            Action::Exponent => 3,
            _ => 5, //All other actions *should* be Parens (sin, negate etc)
        }
    }
    fn to_str(&self) -> &str {
        match *self {
            Action::Add => "+",
            Action::Subtract => "-",
            Action::Multiply => "*",
            Action::Divide => "/",
            // Action::Sin => "sin",
            // Action::Cos => "cos",
            // Action::Tan => "tan",
            // Action::Abs => "abs",
            _ => "ERROR",
        }
    }
    fn try_parse(s: &str) -> Option<Action> {
        match s {
            "+" => Some(Action::Add),
            "-" => Some(Action::Subtract),
            "*" => Some(Action::Multiply),
            "/" => Some(Action::Divide),
            // "sin" => Some(Action::Sin),
            // "cos" => Some(Action::Cos),
            // "tan" => Some(Action::Tan),
            // "abs" => Some(Action::Abs),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ExprValue<'i> {
    Number(f64),
    Ident(&'i str),
    Action(Action),
    Group(Vec<ExprValue<'i>>),
    Unary((Box<ExprValue<'i>>, Action)), //use this or group?
    Binary((Box<ExprValue<'i>>, Box<ExprValue<'i>>, Action)),
}

//Number
fn lex_number<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, f64, E> {
    let mut number = preceded(multispace0, double);

    match number.parse(input) {
        Ok((tail, val)) => Ok((tail, val)),
        Err(e) => Err(e),
    }
}
fn lex_pi<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, f64, E> {
    value(std::f64::consts::PI, preceded(multispace0, is_a("π"))).parse(input)
}

//Ident
fn lex_subscript<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(tuple((
        is_a("_"),
        take_while(|chr| is_alphanumeric(chr as u8)),
    )))
    .parse(input)
}
fn lex_variable<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    preceded(
        multispace0,
        recognize(tuple((
            alt(
                //Start
                (
                    recognize(tuple((
                        is_a("_"),
                        take_while1(|chr| is_alphanumeric(chr as u8)),
                    ))),
                    recognize(tuple((
                        take_while1(|chr| is_alphabetic(chr as u8)),
                        opt(take_while1(|chr| is_alphanumeric(chr as u8))),
                    ))),
                ),
            ),
            opt(lex_subscript),
        ))),
    )
    .parse(input)
}
//Action
fn lex_action<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Action, E> {
    preceded(
        multispace0,
        alt((
            value(Action::Add, char('+')),
            value(Action::Subtract, char('-')),
            value(Action::Multiply, char('*')),
            value(Action::Divide, char('/')),
            value(Action::Sin, is_a("sin")),
            // value(Action::Abs, is_a("abs")),
        )),
    )
    .parse(input)
}
//Group
fn lex_group<'i, T, E: ParseError<&'i str>, C>(
    open: char,
    close: char,
    mut subparser: impl Parser<&'i str, T, E>,
    empty_collection: impl Fn() -> C,
    collection_fold: impl Fn(C, T) -> C,
) -> impl Parser<&'i str, C, E> {
    let mut parse_open = preceded(multispace0, char(open));
    let mut parse_close = preceded(multispace0, char(close));

    move |input: &'i str| {
        let (mut input, _) = parse_open(input)?;

        let mut collection = empty_collection();

        match parse_close.parse(input) {
            Ok((tail, _)) => return Ok((tail, collection)),
            Err(nom::Err::Error(_)) => {}
            Err(err) => return Err(err),
        };
        loop {
            let (tail, item) = subparser.parse(input)?;

            collection = collection_fold(collection, item);

            input = tail;

            let err1 = match parse_close.parse(input) {
                Ok((tail, _)) => return Ok((tail, collection)),
                Err(nom::Err::Error(err)) => err,
                Err(err) => return Err(err),
            };
        } // end loop
    } // end lambda
}
fn lex_parens<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, Vec<ExprValue<'i>>, E> {
    let group = lex_group('(', ')', lex_value, Vec::new, |mut col, val| {
        col.push(val);
        col
    })
    .parse(input);
    // return (group, GroupSymbol::Paren);
    group
}

//ExprValue
fn lex_value<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, ExprValue<'i>, E> {
    alt((
        map(lex_action, ExprValue::Action),
        map(lex_number, ExprValue::Number),
        map(lex_pi, ExprValue::Number),
        map(lex_variable, ExprValue::Ident),
        map(lex_parens, ExprValue::Group),
        //parse ExprValues to produce these:
        // map(parse_unary, ExprValue::Unary),
        // map(parse_binary, ExprValue::Binary),
    ))
    .parse(input)
}
///TODO: Transform to IResult
fn lex_values<'i>(input: &'i str) -> Vec<ExprValue<'i>> {
    let mut v = Vec::new();
    let mut input = input;
    loop {
        let parsed = lex_value::<'i, ()>.parse(input);
        if parsed.is_ok() {
            let (tail, item) = parsed.unwrap();
            v.push(item);
            input = tail;
        } else {
            break;
        }
    } // end loop

    v
}

// pub trait Parser<I, O, E> {
//     /// A parser takes in input type, and returns a `Result` containing
//     /// either the remaining input and the output value, or an error
//     fn parse(&mut self, input: I) -> IResult<I, O, E>;

fn variant_eq<T>(a: &T, b: &T) -> bool {
    //https://stackoverflow.com/questions/32554285/compare-enums-only-by-variant-not-value
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn action_any<'i>(input: &'i [ExprValue<'i>]) -> IResult<&'i [ExprValue<'i>], ExprValue<'i>, ()> {
    move |i: &'i [ExprValue<'i>]| match (i).iter_elements().next().map(|t| {
        let b = variant_eq(t, &ExprValue::Action(Action::Add));
        (&t, b)
    }) {
        Some((&t, b)) => Ok((i.slice(1..), t)),
        _ => Err(nom::Err::Error(())),
    }
}
// fn action_specific<'i>(
//     input: &'i [ExprValue<'i>],
//     action: Action,
// ) -> IResult<&'i [ExprValue<'i>], ExprValue<'i>, ()> {
//     move |i: &'i [ExprValue<'i>]| match (i).iter_elements().next().map(|t| {
//         let b = t == &ExprValue::Action(action);
//         (&t, b)
//     }) {
//         Some((&t, b)) => Ok((i.slice(1..), t)),
//         _ => Err(nom::Err::Error(())),
//     }
// }

fn insert_negate<'i>(
    input: &'i [ExprValue<'i>],
) -> IResult<&'i [ExprValue<'i>], ExprValue<'i>, ()> {
    // let sub = |t: &ExprValue| return t == &ExprValue::Action(Action::Subtract);
    // let double_action = |a: &ExprValue, s: &ExprValue| return variant_eq(a, s);
    let mut is_action = |e: &ExprValue| return variant_eq(e, &ExprValue::Action(Action::Add));

    // preceded(, second)
    unimplemented!()
}

fn modify_tokens<'i>(mut tokens: Vec<ExprValue<'i>>) -> Vec<ExprValue<'i>> {
    let tslice = &tokens[..];
    // let res = insert_negate.parse(tslice);
    let res = insert_negate.parse(tslice);
    tokens
}

#[test]
fn test_value() {
    let v = r##"3+2*5(f)"##;
    let (tail, expr) = lex_value::<()>.parse(v).unwrap();
    println!("tail: {:?}", tail);
    println!("expr: {:?}", expr);
}
#[test]
fn test_values() {
    let v = r##"3+2*5(f)"##;
    let expr = lex_values(v);
    println!("expr: {:?}", expr);
    assert_eq!(expr.len(), 6);
}
