#![feature(test)]

extern crate test;

use test::Bencher;

use dicey::{
	dice::{Dice, DieRoll, Rolled},
	term::Describe,
};

#[bench]
fn roll_4d8(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	b.iter(|| dice.roll().unwrap());
}

#[bench]
fn roll_8d6x(b: &mut Bencher) {
	let dice = Dice::new_exploding(8, 6);
	b.iter(|| dice.roll().unwrap());
}

#[bench]
fn roll_100d20(b: &mut Bencher) {
	let dice = Dice::new(100, 20);
	b.iter(|| dice.roll().unwrap())
}

#[bench]
fn roll_and_total_4d8(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	b.iter(|| dice.roll().unwrap().total().unwrap());
}

#[bench]
fn roll_and_total_8d6x(b: &mut Bencher) {
	let dice = Dice::new_exploding(8, 6);
	b.iter(|| dice.roll().unwrap().total().unwrap());
}

#[bench]
fn roll_and_total_100d20(b: &mut Bencher) {
	let dice = Dice::new(100, 20);
	b.iter(|| dice.roll().unwrap().total().unwrap());
}

#[bench]
fn explain_4d8_result(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	let roll = Rolled {
		rolls: vec![DieRoll::new(6), DieRoll::new(6), DieRoll::new(6), DieRoll::new(6)],
		dice: &dice,
	};
	b.iter(|| roll.describe(None))
}

#[bench]
fn explain_8d6x_result(b: &mut Bencher) {
	let dice = Dice::new_exploding(8, 6);
	let modifier = dice.modifiers.first();
	let roll = Rolled {
		rolls: vec![
			DieRoll::new(6),
			DieRoll::new(2),
			DieRoll::new(4),
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(6),
			DieRoll::new(1),
			DieRoll::new(2),
			DieRoll {
				added_by: modifier,
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: modifier,
				..DieRoll::new(6)
			},
			DieRoll {
				added_by: modifier,
				..DieRoll::new(3)
			},
		],
		dice: &dice,
	};
	b.iter(|| roll.describe(None))
}
