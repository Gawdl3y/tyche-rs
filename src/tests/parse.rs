use chumsky::Parser;

use std::num::NonZeroU8;

use crate::{
	dice::{Condition, Dice},
	parse::{dice as dice_parser, term as term_parser},
	term::Term,
};

#[test]
fn basic_negation() {
	let expected = Term::Neg(Box::new(Term::Num(42)));
	let ast = term_parser().parse("-42").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_addition() {
	let expected = Term::Add(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = term_parser().parse("42 + 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_subtraction() {
	let expected = Term::Sub(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = term_parser().parse("42 - 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_multiplication() {
	let expected = Term::Mul(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let ast = term_parser().parse("42 * 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_down() {
	let expected = Term::DivDown(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let ast = term_parser().parse("50 / 11").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_up() {
	let expected = Term::DivUp(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let ast = term_parser().parse("50 \\ 11").unwrap();
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
	let ast = term_parser().parse("-5 * (3 + 1) - -4 / 2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_dice_math() {
	let dice = Dice::new(4, 6);
	let expected = Term::Add(Box::new(Term::Dice(dice)), Box::new(Term::Num(8)));
	let ast = term_parser().parse("4d6 + 8").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(6).unwrap())
		.explode(None, true)
		.build();
	let ast = dice_parser().parse("4d6x").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode_once() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(6).unwrap())
		.explode(None, false)
		.build();
	let ast = dice_parser().parse("4d6xo").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode_condition() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(6).unwrap())
		.explode(Some(Condition::Gte(NonZeroU8::new(5).unwrap())), true)
		.build();
	let ast = dice_parser().parse("4d6x>=5").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_high() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(20).unwrap())
		.keep_high(NonZeroU8::new(1).unwrap())
		.build();
	let ast = dice_parser().parse("4d20kh").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_high_2() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(20).unwrap())
		.keep_high(NonZeroU8::new(2).unwrap())
		.build();
	let ast = dice_parser().parse("4d20kh2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_low() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(20).unwrap())
		.keep_low(NonZeroU8::new(1).unwrap())
		.build();
	let ast = dice_parser().parse("4d20kl").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_low_2() {
	let expected = Dice::builder()
		.count(4)
		.sides(NonZeroU8::new(20).unwrap())
		.keep_low(NonZeroU8::new(2).unwrap())
		.build();
	let ast = dice_parser().parse("4d20kl2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn invalid_token() {
	let bigboi = i32::MAX as i64 + 1;
	let term = format!("{}", bigboi);
	let ast = term_parser().parse(&term);
	assert!(ast.has_errors());
}
