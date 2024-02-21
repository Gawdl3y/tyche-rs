use std::num::NonZeroU8;

use crate::dice::{Dice, Roll};

#[test]
fn single_d20() {
	let dice = construct_plain(1, 20);
	let roll = rolls_successfully_and_in_range(&dice);
	assert!(roll.explosions.is_none());
	assert_eq!(roll.dice, &dice);
}

#[test]
fn double_d8() {
	let dice = construct_plain(2, 8);
	let roll = rolls_successfully_and_in_range(&dice);
	assert!(roll.explosions.is_none());
	assert_eq!(roll.dice, &dice);
}

#[test]
fn hundred_d42s() {
	let dice = construct_plain(100, 42);
	let roll = rolls_successfully_and_in_range(&dice);
	assert!(roll.explosions.is_none());
	assert_eq!(roll.dice, &dice);
}

#[test]
fn max_dice() {
	let dice = construct_plain(u8::MAX, u8::MAX);
	let roll = rolls_successfully_and_in_range(&dice);
	assert!(roll.explosions.is_none());
	assert_eq!(roll.dice, &dice);
}

#[test]
fn exploding_max_d4s() {
	let dice = construct_exploding(u8::MAX, 4);
	let roll = rolls_successfully_and_in_range(&dice);
	assert!(roll.explosions.is_some());

	let explosions = roll.explosions.unwrap();
	assert!(!explosions.is_empty());
	assert!(explosions.len() < u8::MAX as usize);
	rolls_in_range(&explosions, 4);
}

#[test]
fn all_dice_sides_occur() {
	let dice = Dice::new(u8::MAX, 20);
	let mut rolls = Vec::new();

	for _ in 1..=1000 {
		let roll = dice.roll();
		assert!(roll.is_ok());
		rolls.append(&mut roll.unwrap().rolls);
	}

	rolls_in_range(&rolls, 20);

	for side in 1..20 {
		assert!(rolls.iter().filter(|r| **r == side).count() > 0);
	}
}

fn construct_plain(count: u8, sides: u8) -> Dice {
	let dice = Dice::new(count, sides);
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, NonZeroU8::new(sides).unwrap());
	assert!(!dice.explode);

	dice
}

fn construct_exploding(count: u8, sides: u8) -> Dice {
	let dice = Dice::new_exploding(count, sides);
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, NonZeroU8::new(sides).unwrap());
	assert!(dice.explode);

	dice
}

fn rolls_successfully_and_in_range<'a>(dice: &'a Dice) -> Roll<'a> {
	let result = dice.roll();
	assert!(result.is_ok());

	let roll = result.unwrap();
	assert_eq!(roll.rolls.len(), roll.dice.count as usize);
	rolls_in_range(&roll.rolls, roll.dice.sides.into());

	roll
}

fn rolls_in_range(rolls: &[u8], sides: u8) {
	assert!(!rolls.iter().any(|r| *r < 1 || *r > sides));
}
