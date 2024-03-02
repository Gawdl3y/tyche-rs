#![feature(test)]

extern crate test;

use test::Bencher;

use dicey::{
	dice::{Dice, Roll},
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
	let roll = Roll {
		rolls: vec![6, 8, 3, 4],
		explosions: None,
		dice: &dice,
	};
	b.iter(|| roll.describe(None))
}

#[bench]
fn explain_8d6x_result(b: &mut Bencher) {
	let dice = Dice::new_exploding(8, 6);
	let roll = Roll {
		rolls: vec![6, 2, 4, 4, 5, 6, 1, 2],
		explosions: Some(vec![4, 6, 3]),
		dice: &dice,
	};
	b.iter(|| roll.describe(None))
}
