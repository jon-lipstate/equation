use std::{borrow::Cow, collections::HashMap, num::ParseFloatError};

use nom::{
    branch::alt,
    bytes::complete::{is_a, take_till, take_while},
    character::{
        complete::{char, multispace0},
        is_alphabetic, is_alphanumeric,
    },
    combinator::{map, opt, recognize, value},
    error::{ErrorKind, FromExternalError, ParseError},
    number::complete::double,
    sequence::tuple,
    IResult, Parser,
};

#[derive(Debug, Clone)]
enum UnaryAction {
    Negate,
    Sin,
}
#[derive(Debug, Clone)]
enum Action {
    Add,
    Subtract,
    Multiply,
    Divide,
    Sin,
}
impl Action {
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
// #[derive(Debug, Clone)]
// enum GroupSymbol {
//     Paren,
//     Curly,
//     Bracket,
//     Pipe,
// }

// enum atomic {
//     num, ident, action, group
// }

#[derive(Debug, Clone)]
enum ExprValue<'i> {
    Number(f64),
    Ident(&'i str),
    Action(Action),
    Group(Vec<ExprValue<'i>>),
    Unary((Box<ExprValue<'i>>, Action)), //use this or group?
    Binary((Box<ExprValue<'i>>, Box<ExprValue<'i>>, Action)),
}
//Number
fn parse_number<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, f64, E> {
    match double.parse(input) {
        Ok((tail, val)) => Ok((tail, val)),
        Err(e) => Err(e), //Err(nom::Err::Error(e)),
    }
}
fn parse_pi<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, f64, E> {
    unimplemented!();
    // value(std::f64::consts::PI, char('π'))
}

//Ident
fn parse_subscript<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(tuple((
        is_a("_"),
        take_while(|chr| is_alphanumeric(chr as u8)),
    )))
    .parse(input)
}
fn parse_variable<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(tuple((
        alt(
            //Start
            (
                recognize(tuple((
                    is_a("_"),
                    take_while(|chr| is_alphanumeric(chr as u8)),
                ))),
                recognize(tuple((
                    take_while(|chr| is_alphabetic(chr as u8)),
                    opt(take_while(|chr| is_alphanumeric(chr as u8))),
                ))),
            ),
        ),
        opt(parse_subscript),
    )))
    .parse(input)
}
//Action
fn parse_action<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Action, E> {
    alt((
        value(Action::Add, char('+')),
        value(Action::Subtract, char('-')),
        value(Action::Multiply, char('*')),
        value(Action::Divide, char('/')),
        value(Action::Sin, is_a("sin")),
        // value(Action::Abs, is_a("abs")),
    ))
    .parse(input)
}
//Group
fn parse_group<'i, T, E: ParseError<&'i str>, C>(
    open: char,
    close: char,
    mut subparser: impl Parser<&'i str, T, E>,
    empty_collection: impl Fn() -> C,
    collection_fold: impl Fn(C, T) -> C,
) -> impl Parser<&'i str, C, E> {
    let mut parse_open = tuple((char(open), multispace0));
    let mut parse_close = tuple((char(close), multispace0));
    println!("parse_group: fix comma seperator");
    let mut parse_comma = tuple((char(','), multispace0));

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
            match parse_comma.parse(input) {
                Ok((tail, _)) => input = tail,
                Err(nom::Err::Error(err2)) => return Err(nom::Err::Error(err1.or(err2))),
                Err(err) => return Err(err),
            }
        } // end loop
    } // end lambda
}
fn parse_parens<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, Vec<ExprValue<'i>>, E> {
    let group = parse_group('(', ')', parse_value, Vec::new, |mut col, val| {
        col.push(val);
        col
    })
    .parse(input);
    // return (group, GroupSymbol::Paren);
    group
}
//Unary
fn parse_unary<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, (Cow<'i, ExprValue<'i>>, Action), E> {
    unimplemented!()
}
fn parse_binary<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, (Cow<'i, ExprValue<'i>>, Cow<'i, ExprValue<'i>>, Action), E> {
    unimplemented!()
}

//ExprValue
fn parse_value<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, ExprValue<'i>, E> {
    // unimplemented!()
    // let pn: Result<(&str, f64), nom::Err<E>> = parse_number.parse(input);

    alt((
        map(parse_number, ExprValue::Number),
        map(parse_pi, ExprValue::Number),
        map(parse_action, ExprValue::Action),
        map(parse_variable, ExprValue::Ident),
        // map(parse_parens, ExprValue::Group),
        // map(parse_unary, ExprValue::Unary),
        // map(parse_binary, ExprValue::Binary),
    ))
    .parse(input)
}
fn parse_expression<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, ExprValue<'i>, E> {
    unimplemented!()
}
//todo: order of operations, resort after parsed??
//3+2*5
//#+#*#
//(3+2)*5

#[test]
fn test_value() {
    let v = "sin";
    let (tail, expr) = parse_value::<()>.parse(v).unwrap();
    println!("tail: {:?}", tail);
    println!("expr: {:?}", expr);
}
