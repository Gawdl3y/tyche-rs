use chumsky::Parser;

use crate::{
	dice::{modifier::Condition, Dice},
	expr::Expr,
	parse::{dice as dice_parser, expr as expr_parser},
};

#[test]
fn basic_negation() {
	let expected = Expr::Neg(Box::new(Expr::Num(42)));
	let ast = expr_parser().parse("-42").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_addition() {
	let expected = Expr::Add(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let ast = expr_parser().parse("42 + 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_subtraction() {
	let expected = Expr::Sub(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let ast = expr_parser().parse("42 - 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_multiplication() {
	let expected = Expr::Mul(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let ast = expr_parser().parse("42 * 69").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_down() {
	let expected = Expr::DivDown(Box::new(Expr::Num(50)), Box::new(Expr::Num(11)));
	let ast = expr_parser().parse("50 / 11").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_division_rounded_up() {
	let expected = Expr::DivUp(Box::new(Expr::Num(50)), Box::new(Expr::Num(11)));
	let ast = expr_parser().parse("50 \\ 11").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn complex_math() {
	let expected = Expr::Sub(
		Box::new(Expr::Mul(
			Box::new(Expr::Neg(Box::new(Expr::Num(5)))),
			Box::new(Expr::Add(Box::new(Expr::Num(3)), Box::new(Expr::Num(1)))),
		)),
		Box::new(Expr::DivDown(
			Box::new(Expr::Neg(Box::new(Expr::Num(4)))),
			Box::new(Expr::Num(2)),
		)),
	);
	let ast = expr_parser().parse("-5 * (3 + 1) - -4 / 2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn basic_dice_math() {
	let dice = Dice::new(4, 6);
	let expected = Expr::Add(Box::new(Expr::Dice(dice)), Box::new(Expr::Num(8)));
	let ast = expr_parser().parse("4d6 + 8").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode() {
	let expected = Dice::builder().count(4).sides(6).explode(None, true).build();
	let ast = dice_parser().parse("4d6x").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode_once() {
	let expected = Dice::builder().count(4).sides(6).explode(None, false).build();
	let ast = dice_parser().parse("4d6xo").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_explode_condition() {
	let expected = Dice::builder()
		.count(4)
		.sides(6)
		.explode(Some(Condition::Gte(5)), true)
		.build();
	let ast = dice_parser().parse("4d6x>=5").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_high() {
	let expected = Dice::builder().count(4).sides(20).keep_high(1).build();
	let ast = dice_parser().parse("4d20kh").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_high_2() {
	let expected = Dice::builder().count(4).sides(20).keep_high(2).build();
	let ast = dice_parser().parse("4d20kh2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_low() {
	let expected = Dice::builder().count(4).sides(20).keep_low(1).build();
	let ast = dice_parser().parse("4d20kl").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn dice_keep_low_2() {
	let expected = Dice::builder().count(4).sides(20).keep_low(2).build();
	let ast = dice_parser().parse("4d20kl2").unwrap();
	assert_eq!(ast, expected);
}

#[test]
fn invalid_token() {
	let bigboi = i32::MAX as i64 + 1;
	let expr = bigboi.to_string();
	let ast = expr_parser().parse(&expr);
	assert!(ast.has_errors());
}
