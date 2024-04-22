use std::borrow::Cow;

use crate::dice::{
	roller::{FastRand, Roller},
	Dice, DieRoll, Modifier, Rolled,
};

#[test]
fn single_d20() {
	let dice = construct_plain(1, 20);
	let rolled = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolled.rolls.len(), 1);
	assert_eq!(*rolled.dice, dice);
}

#[test]
fn double_d8() {
	let dice = construct_plain(2, 8);
	let rolled = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolled.rolls.len(), 2);
	assert_eq!(*rolled.dice, dice);
}

#[test]
fn hundred_d42s() {
	let dice = construct_plain(100, 42);
	let rolled = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolled.rolls.len(), 100);
	assert_eq!(*rolled.dice, dice);
}

#[test]
fn max_dice() {
	let dice = construct_plain(u8::MAX, u8::MAX);
	let rolled = rolls_successfully_and_in_range(&dice);
	assert_eq!(rolled.rolls.len(), u8::MAX as usize);
	assert_eq!(*rolled.dice, dice);
}

#[test]
fn exploding_max_d4s() {
	let dice = construct_exploding(u8::MAX, 4);
	let rolled = rolls_successfully_and_in_range(&dice);
	assert!(rolled.rolls.len() > u8::MAX as usize);

	let explosions = rolled
		.rolls
		.into_iter()
		.filter(DieRoll::is_additional)
		.collect::<Vec<_>>();
	assert!(!explosions.is_empty());
	assert!(explosions.len() < u8::MAX as usize);
	rolls_in_range(&explosions, 4);
}

#[test]
fn all_dice_sides_occur() {
	let dice = Dice::new(u8::MAX, 20);
	let mut rng = FastRand::default();
	let mut rolls = Vec::new();

	for _ in 1..=1000 {
		let rolled = rng.roll(&dice, true);
		assert!(rolled.is_ok());
		rolls.append(&mut rolled.unwrap().rolls);
	}

	rolls_in_range(&rolls, 20);

	for side in 1..=20 {
		assert!(rolls.iter().filter(|roll| roll.val == side).count() > 0);
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
	let db = Dice::builder().count(4).sides(8).explode(None, true).build();
	assert_ne!(da, db);
}

#[test]
fn roll_equality() {
	let da = Dice::builder().count(4).sides(8).explode(None, true).build();
	let db = Dice::builder().count(4).sides(8).explode(None, true).build();
	let ra = Rolled {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(7)
			},
		],
		dice: Cow::Owned(da),
	};
	let rb = Rolled {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: Some(db.modifiers[0]),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: Some(db.modifiers[0]),
				..DieRoll::new(7)
			},
		],
		dice: Cow::Owned(db),
	};
	assert_eq!(ra, rb);
}

#[test]
fn roll_inequality() {
	let da = Dice::builder().count(4).sides(8).explode(None, true).build();
	let db = Dice::builder().count(4).sides(8).explode(None, true).build();
	let ra = Rolled {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(7)
			},
		],
		dice: Cow::Owned(da),
	};
	let rb = Rolled {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(6),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: Some(db.modifiers[0]),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: Some(db.modifiers[0]),
				..DieRoll::new(7)
			},
		],
		dice: Cow::Owned(db),
	};
	assert_ne!(ra, rb);

	let da = Dice::builder().count(4).sides(8).explode(None, true).build();
	let db = Dice::new(4, 8);
	let ra = Rolled {
		rolls: vec![
			DieRoll::new(4),
			DieRoll::new(5),
			DieRoll::new(8),
			DieRoll::new(8),
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(4)
			},
			DieRoll {
				added_by: Some(da.modifiers[0]),
				..DieRoll::new(7)
			},
		],
		dice: Cow::Owned(da),
	};
	let rb = Rolled {
		rolls: vec![DieRoll::new(4), DieRoll::new(5), DieRoll::new(8), DieRoll::new(8)],
		dice: Cow::Owned(db),
	};
	assert_ne!(ra, rb);
}

fn construct_plain(count: u8, sides: u8) -> Dice {
	let dice = Dice::new(count, sides);
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, sides);
	assert_eq!(dice.modifiers.len(), 0);
	dice
}

fn construct_exploding(count: u8, sides: u8) -> Dice {
	let dice = Dice::builder().count(count).sides(sides).explode(None, true).build();
	assert_eq!(dice.count, count);
	assert_eq!(dice.sides, sides);
	assert_eq!(dice.modifiers.len(), 1);
	assert!(matches!(dice.modifiers[0], Modifier::Explode { .. }));
	dice
}

fn rolls_successfully_and_in_range(dice: &Dice) -> Rolled {
	let result = FastRand::default().roll(dice, true);
	assert!(result.is_ok());

	let rolled = result.unwrap();
	rolls_in_range(&rolled.rolls, rolled.dice.sides);

	rolled
}

fn rolls_in_range(rolls: &[DieRoll], sides: u8) {
	assert!(!rolls.iter().any(|roll| roll.val < 1 || roll.val > sides));
}
