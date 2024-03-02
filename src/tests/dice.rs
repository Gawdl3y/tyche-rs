use std::num::NonZeroU8;

use crate::dice::{Dice, DieRoll, Modifier, Rolls};

#[test]
fn single_d20() {
	let dice = construct_plain(1, 20);
	let rolls = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolls.rolls.len(), 1);
	assert_eq!(rolls.dice, &dice);
}

#[test]
fn double_d8() {
	let dice = construct_plain(2, 8);
	let rolls = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolls.rolls.len(), 2);
	assert_eq!(rolls.dice, &dice);
}

#[test]
fn hundred_d42s() {
	let dice = construct_plain(100, 42);
	let rolls = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolls.rolls.len(), 100);
	assert_eq!(rolls.dice, &dice);
}

#[test]
fn max_dice() {
	let dice = construct_plain(u8::MAX, u8::MAX);
	let rolls = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolls.rolls.len(), u8::MAX as usize);
	assert_eq!(rolls.dice, &dice);
}

#[test]
fn exploding_max_d4s() {
	let dice = construct_exploding(u8::MAX, 4);
	let rolls = rolls_successfully_and_in_range(&dice);
	assert!(rolls.rolls.len() > u8::MAX as usize);

	let explosions = rolls
		.rolls
		.into_iter()
		.filter(|roll| !roll.is_original())
		.collect::<Vec<_>>();
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
		assert!(rolls.iter().filter(|roll| roll.val.get() == side).count() > 0);
	}
}

#[test]
fn dice_equality() {
	let da = Dice::new(4, 8);
	let db = Dice::new(4, 8);
	assert_eq!(da, db);
}

#[test]
fn dice_inequality() {
	let da = Dice::new(4, 8);
	let db = Dice::new(4, 20);
	assert_ne!(da, db);

	let da = Dice::new(4, 8);
	let db = Dice::new(2, 8);
	assert_ne!(da, db);

	let da = Dice::new(4, 8);
	let db = Dice::new_exploding(4, 8);
	assert_ne!(da, db);
}

#[test]
fn roll_equality() {
	let da = Dice::new_exploding(4, 8);
	let db = Dice::new_exploding(4, 8);
	let ra = Rolls {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(7)
			},
		],
		dice: &da,
	};
	let rb = Rolls {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: db.modifiers.first(),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: db.modifiers.first(),
				..DieRoll::new(7)
			},
		],
		dice: &db,
	};
	assert_eq!(ra, rb);
}

#[test]
fn roll_inequality() {
	let da = Dice::new_exploding(4, 8);
	let db = Dice::new_exploding(4, 8);
	let ra = Rolls {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(7)
			},
		],
		dice: &da,
	};
	let rb = Rolls {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(6),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: db.modifiers.first(),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: db.modifiers.first(),
				..DieRoll::new(7)
			},
		],
		dice: &db,
	};
	assert_ne!(ra, rb);

	let da = Dice::new_exploding(4, 8);
	let db = Dice::new(4, 8);
	let ra = Rolls {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: da.modifiers.first(),
				..DieRoll::new(7)
			},
		],
		dice: &da,
	};
	let rb = Rolls {
		rolls: vec![DieRoll::new(4), DieRoll::new(5), DieRoll::new(8), DieRoll::new(8)],
		dice: &db,
	};
	assert_ne!(ra, rb);
}

fn construct_plain(count: u8, sides: u8) -> Dice {
	let dice = Dice::new(count, sides);
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, NonZeroU8::new(sides).unwrap());
	assert_eq!(dice.modifiers.len(), 0);
	dice
}

fn construct_exploding(count: u8, sides: u8) -> Dice {
	let dice = Dice::new_exploding(count, sides);
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, NonZeroU8::new(sides).unwrap());
	assert_eq!(dice.modifiers.len(), 1);
	assert!(matches!(dice.modifiers.first().unwrap(), Modifier::Explode(..)));
	dice
}

fn rolls_successfully_and_in_range<'a>(dice: &'a Dice) -> Rolls<'a> {
	let result = dice.roll();
	assert!(result.is_ok());

	let rolls = result.unwrap();
	rolls_in_range(&rolls.rolls, rolls.dice.sides.into());

	rolls
}

fn rolls_in_range(rolls: &[DieRoll], sides: u8) {
	assert!(!rolls.iter().any(|roll| roll.val.get() < 1 || roll.val.get() > sides));
}
