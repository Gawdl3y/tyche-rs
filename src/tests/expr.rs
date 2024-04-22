use crate::{
	dice::{roller::FastRand as FastRandRoller, Dice},
	expr::{CalcError, Evaled, Expr},
};

#[test]
fn basic_negation() {
	let expr = Expr::Neg(Box::new(Expr::Num(42)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), -42);
}

#[test]
fn basic_addition() {
	let expr = Expr::Add(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), 111);
}

#[test]
fn basic_subtraction() {
	let expr = Expr::Sub(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), -27);
}

#[test]
fn basic_multiplication() {
	let expr = Expr::Mul(Box::new(Expr::Num(42)), Box::new(Expr::Num(69)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), 2898);
}

#[test]
fn basic_division_rounded_down() {
	let expr = Expr::DivDown(Box::new(Expr::Num(50)), Box::new(Expr::Num(11)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), 4);
}

#[test]
fn basic_division_rounded_up() {
	let expr = Expr::DivUp(Box::new(Expr::Num(50)), Box::new(Expr::Num(11)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), 5);
}

#[test]
fn complex_math() {
	let expr = Expr::Sub(
		Box::new(Expr::Mul(
			Box::new(Expr::Neg(Box::new(Expr::Num(5)))),
			Box::new(Expr::Add(Box::new(Expr::Num(3)), Box::new(Expr::Num(1)))),
		)),
		Box::new(Expr::DivDown(
			Box::new(Expr::Neg(Box::new(Expr::Num(4)))),
			Box::new(Expr::Num(2)),
		)),
	);
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert_eq!(result.unwrap(), -18);
}

#[test]
fn basic_dice_math() {
	let dice = Dice::new(4, 6);
	let expr = Expr::Add(Box::new(Expr::Dice(dice)), Box::new(Expr::Num(8)));
	let evaled = expr.eval(&mut FastRandRoller::default()).unwrap();

	let dice_total = match evaled {
		Evaled::Add(ref boxed, _) => match boxed.as_ref() {
			Evaled::Dice(evaled_dice) => evaled_dice.total().unwrap() as i32,
			_ => panic!(),
		},
		_ => panic!(),
	};

	let total = evaled.calc().unwrap();
	assert_eq!(total, dice_total + 8);
}

#[test]
fn calc_overflow() {
	let expr = Expr::Add(Box::new(Expr::Num(i32::MAX)), Box::new(Expr::Num(1)));
	let result = expr.eval(&mut FastRandRoller::default()).unwrap().calc();
	assert!(result.is_err());
	assert!(matches!(result.unwrap_err(), CalcError::Overflow(..)));
}
