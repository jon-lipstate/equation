use nom::character::complete::char;
use nom::combinator::value;
use nom::number::complete::double;
use nom::{Err, IResult, Parser};

//3+2*5
//Function(Add,[Number(3), Function(Multiply,[Number(2), Number(5)])])

enum Component {
    Number(f64), //todo: handle pi as special case
    Variable(String),
    Group{symbol: GroupSymbol, components: Vec<Component>},
    Unary{operator: Operator, component: Box<Component>},
    Binary{operator: Operator, left: Box<Component>, right: Box<Component>},
}
enum Operator {
    Add,
    Multiply,
    Sin,
    //...
}
enum GroupSymbol {
    Paren,
    Curly,
    Bracket,
    Pipe
}


#[derive(Debug, Clone, PartialEq)]
enum Atom {
    Number(f64), //todo: handle pi as special case
    Variable(String)
}
#[derive(Debug, Clone, PartialEq)]
enum Block {
    Paren(Expr),
    Bracket(Expr),
    Curly(Expr),
    Pipe(Expr),
}



#[derive(Debug, Clone, PartialEq, Eq)]
enum Action {
    Unknown,
    Add,
    Multiply
}
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Unary(Box<Expr>, Action),
    Binary(Box<Expr>, Box<Expr>, Action),
    Ternary,
    Block(Box<Block>),
    Expr(Box<Expr>),
    Atom(Atom)
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
            Box::new(Expr::Atom(Atom::Number(left))),
            Box::new(Expr::Atom(Atom::Number(right))),
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
fn test_expr_simple() {
    let expr = Expr::Binary(
        Box::new(Expr::Atom(Atom::Number(3f64))),
        Box::new(Expr::Atom(Atom::Number(2f64))),
        Action::Add
    );
    assert_eq!(
        parse_expr("3+2"),
        Ok((
            "",
            expr
        ))
    );
}
#[test]
fn test_expr_recursive_order_of_operations1() {
    let innerExpr = Expr::Binary(
        Box::new(Expr::Atom(Atom::Number(2f64))),
        Box::new(Expr::Atom(Atom::Number(5f64))),
        Action::Multiply
    );
    let outerExpr = Expr::Binary(
        Box::new(Expr::Atom(Atom::Number(3f64))),
        Box::new(innerExpr),
        Action::Add
    );
    assert_eq!(
        parse_expr("3+2*5"),
        Ok((
            "",
            outerExpr
        ))
    );
}
#[test]
fn test_expr_recursive_order_of_operations2() {
    unimplemented!();
    let innerExpr = Expr::Binary(
        Box::new(Expr::Atom(Atom::Number(2f64))),
        Box::new(Expr::Atom(Atom::Number(5f64))),
        Action::Multiply
    );
    let outerExpr = Expr::Binary(
        Box::new(Expr::Atom(Atom::Number(3f64))),
        Box::new(innerExpr),
        Action::Add
    );
    assert_eq!(
        parse_expr("(3+2)*5"),
        Ok((
            "",
            outerExpr
        ))
    );
}


#[macro_use]
extern crate uom;
use uom::si::length::{inch};
use uom::si::force::{pound_force};
use uom::si::pressure::psi;

fn main() {
    let length = uom::si::f32::Length::new::<inch>(5.0);
    let width = uom::si::f32::Length::new::<inch>(5.0);
    let area = length * width;
    let force = uom::si::f32::Force::new::<pound_force>(1000.0);
    let stress = force / area;
    println!("{:?}", stress.into_format_args(psi,  uom::fmt::DisplayStyle::Abbreviation));
}
