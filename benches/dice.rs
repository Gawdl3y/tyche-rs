#![feature(test)]

extern crate test;

use std::borrow::Cow;

use test::Bencher;

use dicey::{
	dice::{roller::FastRand, Dice, DieRoll, Rolled},
	expr::Describe,
};

#[bench]
fn roll_4d8(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap());
}

#[bench]
fn roll_8d6x(b: &mut Bencher) {
	let dice = Dice::builder().count(8).sides(6).explode(None, true).build();
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap());
}

#[bench]
fn roll_100d20(b: &mut Bencher) {
	let dice = Dice::new(100, 20);
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap())
}

#[bench]
fn roll_absurd(b: &mut Bencher) {
	let dice: Dice = "100d42min3max40rr<6x>37kh20kl10xo>29".parse().unwrap();
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap())
}

#[bench]
fn roll_and_total_4d8(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap().total().unwrap());
}

#[bench]
fn roll_and_total_8d6x(b: &mut Bencher) {
	let dice = Dice::builder().count(8).sides(6).explode(None, true).build();
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap().total().unwrap());
}

#[bench]
fn roll_and_total_100d20(b: &mut Bencher) {
	let dice = Dice::new(100, 20);
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap().total().unwrap());
}

#[bench]
fn roll_and_total_absurd(b: &mut Bencher) {
	let dice: Dice = "100d42min3max40rr<6x>37kh20kl10xo>29".parse().unwrap();
	let mut rng = FastRand::default();
	b.iter(|| dice.roll(&mut rng, true).unwrap().total().unwrap())
}

#[bench]
fn explain_4d8_result(b: &mut Bencher) {
	let dice = Dice::new(4, 8);
	let roll = Rolled {
		rolls: vec![DieRoll::new(6), DieRoll::new(6), DieRoll::new(6), DieRoll::new(6)],
		dice: Cow::Owned(dice),
	};
	b.iter(|| roll.describe(None))
}

#[bench]
fn explain_8d6x_result(b: &mut Bencher) {
	let dice = Dice::builder().count(8).sides(6).explode(None, true).build();
	let modifier = Some(dice.modifiers[0]);
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
			{
				let mut roll = DieRoll::new(4);
				roll.added_by = modifier;
				roll
			},
			{
				let mut roll = DieRoll::new(6);
				roll.added_by = modifier;
				roll
			},
			{
				let mut roll = DieRoll::new(3);
				roll.added_by = modifier;
				roll
			},
		],
		dice: Cow::Owned(dice),
	};
	b.iter(|| roll.describe(None))
}
