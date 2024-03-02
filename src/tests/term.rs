use crate::{
	dice::Dice,
	term::{Error, EvaledTerm, Term},
};

#[test]
fn basic_negation() {
	let term = Term::Neg(Box::new(Term::Num(42)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), -42);
}

#[test]
fn basic_addition() {
	let term = Term::Add(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), 111);
}

#[test]
fn basic_subtraction() {
	let term = Term::Sub(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), -27);
}

#[test]
fn basic_multiplication() {
	let term = Term::Mul(Box::new(Term::Num(42)), Box::new(Term::Num(69)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), 2898);
}

#[test]
fn basic_division_rounded_down() {
	let term = Term::DivDown(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), 4);
}

#[test]
fn basic_division_rounded_up() {
	let term = Term::DivUp(Box::new(Term::Num(50)), Box::new(Term::Num(11)));
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), 5);
}

#[test]
fn complex_math() {
	let term = Term::Sub(
		Box::new(Term::Mul(
			Box::new(Term::Neg(Box::new(Term::Num(5)))),
			Box::new(Term::Add(Box::new(Term::Num(3)), Box::new(Term::Num(1)))),
		)),
		Box::new(Term::DivDown(
			Box::new(Term::Neg(Box::new(Term::Num(4)))),
			Box::new(Term::Num(2)),
		)),
	);
	let result = term.eval().unwrap().calc();
	assert_eq!(result.unwrap(), -18);
}

#[test]
fn basic_dice_math() {
	let dice = Dice::new(4, 6);
	let term = Term::Add(Box::new(Term::Dice(dice)), Box::new(Term::Num(8)));
	let evaled = term.eval().unwrap();

	let dice_total = match evaled {
		EvaledTerm::Add(ref boxed, _) => match boxed.as_ref() {
			EvaledTerm::Dice(evaled_dice) => evaled_dice.total().unwrap() as i32,
			_ => panic!(),
		},
		_ => panic!(),
	};

	let total = evaled.calc().unwrap();
	assert_eq!(total, dice_total + 8);
}

#[test]
fn calc_overflow() {
	let term = Term::Add(Box::new(Term::Num(i32::MAX)), Box::new(Term::Num(1)));
	let result = term.eval().unwrap().calc();
	assert!(result.is_err());
	assert!(matches!(result.unwrap_err(), Error::Overflow));
}
