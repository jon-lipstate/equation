use nom::character::complete::char;
use nom::combinator::value;
use nom::number::complete::double;
use nom::{Err, IResult, Parser};

#[derive(Debug, Clone, PartialEq)]
enum Atom<'i> {
    Number(f64),
    Pi,
    Ident(&'i str),
    Group(Group<'i>),
}
#[derive(Debug, Clone, PartialEq)]
enum Group<'i> {
    Paren(Expr<'i>),
    Bracket(Expr<'i>),
    Curly(Expr<'i>),
    Pipe(Expr<'i>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    Unknown,
    Add,
}
#[derive(Debug, Clone, PartialEq)]
enum Expr<'i> {
    Unary(Box<Atom<'i>>, Action),
    Binary(Box<Atom<'i>>, Box<Atom<'i>>, Action),
    Ternary,
}

fn parse_number(input: &str) -> IResult<&str, f64, ()> {
    double(input)
}
fn parse_add_symbol(input: &str) -> IResult<&str, Action, ()> {
    let mut char_parse = char('+');
    value(Action::Add, char_parse).parse(input)
}
fn parse_expr(input: &str) -> IResult<&str, Expr, ()> {
    let (tail, left) = parse_number(input).unwrap();
    let (tail, action) = parse_add_symbol(tail).unwrap();
    let (tail, right) = parse_number(tail).unwrap();
    Ok((
        tail,
        Expr::Binary(
            Box::new(Atom::Number(left)),
            Box::new(Atom::Number(right)),
            action,
        ),
    ))
}

#[test]
fn test_double() {
    assert_eq!(parse_number("1"), Ok(("", 1.0)));
}
#[test]
fn test_add_symbol() {
    assert_eq!(parse_add_symbol("+"), Ok(("", Action::Add)));
}
#[test]
fn test_expr() {
    assert_eq!(
        parse_expr("3+2"),
        Ok((
            "",
            Expr::Binary(
                Box::new(Atom::Number(3f64)),
                Box::new(Atom::Number(2f64)),
                Action::Add
            )
        ))
    );
}

fn main() {
    println!("Hello, world!");
}
