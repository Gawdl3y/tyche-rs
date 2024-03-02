use chumsky::Parser;

use crate::{dice::Dice, expr::Term, parse::expr};

#[test]
fn basic_negation() {
	let expected = Term::Neg(Box::new(Term::Num(42)));
	let ast = expr().parse("-42").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_addition() {
	let expected = Term::Add(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = expr().parse("42 + 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_subtraction() {
	let expected = Term::Sub(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = expr().parse("42 - 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_multiplication() {
	let expected = Term::Mul(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = expr().parse("42 * 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_down() {
	let expected = Term::DivDown(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let ast = expr().parse("50 / 11").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_up() {
	let expected = Term::DivUp(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let ast = expr().parse("50 \\ 11").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn complex_math() {
	let expected = Term::Sub(
		Box::new(Term::Mul(
			Box::new(Term::Neg(Box::new(Term::Num(5)))),
			Box::new(Term::Add(Box::new(Term::Num(3)), Box::new(Term::Num(1)))),
		)),
		Box::new(Term::DivDown(
			Box::new(Term::Neg(Box::new(Term::Num(4)))),
			Box::new(Term::Num(2)),
		)),
	);
	let ast = expr().parse("-5 * (3 + 1) - -4 / 2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_dice_math() {
	let dice = Dice::new(4, 6);
	let expected = Term::Add(Box::new(Term::Dice(dice)), Box::new(Term::Num(8)));
	let ast = expr().parse("4d6 + 8").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn invalid_token() {
	let bigboi = i32::MAX as i64 + 1;
	let term = format!("{}", bigboi);
	let ast = expr().parse(&term);
	assert!(ast.has_errors());
}
