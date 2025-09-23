//! Dice modifiers and their related types.

use alloc::{
	borrow::ToOwned,
	format,
	string::{String, ToString},
	vec::Vec,
};
use core::fmt;

use super::{roller::Roller, Error, Rolled};

/// Routines that can be applied to [`Dice`](super::Dice) to automatically manipulate resulting [`Rolled`] dice sets
/// from them as part of their rolling process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Modifier {
	/// Rerolls (drops original and adds a newly-rolled die) dice that meet a condition.
	///
	/// # Examples
	///
	/// ## Reroll recursively (`rr`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6rr1 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).reroll(Condition::Eq(1), true).build();
	/// let premade_rolls = [3, 6, 1, 2, 1, 4];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// // Only the first four predetermined values will be used for the dice rolls since there are only four dice to roll.
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply an rr1 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let rr1_mod = Modifier::Reroll {
	/// 	cond: Condition::Eq(1),
	/// 	recurse: true,
	/// };
	/// rr1_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop the 1 roll, roll a new 1 from the predetermined RNG, drop that too,
	/// // then roll a new 4 from the RNG.
	/// // Final expected rolled dice set, after rr1 modifier: 4d6rr1[3, 6, 1 (d), 2, 1 (d), 4]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[2].drop(rr1_mod);
	/// expected.rolls[4].add(rr1_mod);
	/// expected.rolls[4].drop(rr1_mod);
	/// expected.rolls[5].add(rr1_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	///
	/// ## Reroll once (`r`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6r1 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).reroll(Condition::Eq(1), false).build();
	/// let premade_rolls = [3, 6, 1, 2, 1];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// // Only the first four predetermined values will be used for the dice rolls since there are only four dice to roll.
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply an r1 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let r1_mod = Modifier::Reroll {
	/// 	cond: Condition::Eq(1),
	/// 	recurse: false,
	/// };
	/// r1_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop the 1 roll and roll a new 1 from the predetermined RNG.
	/// // Final expected rolled dice set, after r1 modifier: 4d6r1[3, 6, 1 (d), 2, 1]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[2].drop(r1_mod);
	/// expected.rolls[4].add(r1_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	Reroll {
		/// Condition that rolls must pass in order to be rerolled
		cond: Condition,

		/// Whether the reroll should be done repeatedly until the rerolled die no longer meets the condition
		recurse: bool,
	},

	/// Explodes (keeps original and adds an additional newly-rolled die) dice that meet a condition.
	///
	/// # Examples
	///
	/// ## Explode recursively (`x`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6x dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).explode(None, true).build();
	/// let premade_rolls = [3, 6, 1, 2, 6, 4];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// // Only the first four predetermined values will be used for the dice rolls since there are only four dice to roll.
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply an x modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let x_mod = Modifier::Explode {
	/// 	cond: None,
	/// 	recurse: true,
	/// };
	/// x_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will see that a 6 (the max die value) was rolled, roll a new additional 6, see
	/// // that is also the max die value, then roll a new 4 as well.
	/// // Final expected rolled dice set, after x modifier: 4d6x[3, 6, 1, 2, 6, 4]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[4].add(x_mod);
	/// expected.rolls[5].add(x_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	///
	/// ## Explode once (`xo`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6xo dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).explode(None, false).build();
	/// let premade_rolls = [3, 6, 1, 2, 6];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// // Only the first four predetermined values will be used for the dice rolls since there are only four dice to roll.
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply an xo modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let xo_mod = Modifier::Explode {
	/// 	cond: None,
	/// 	recurse: false,
	/// };
	/// xo_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will see that a 6 (the max die value) was rolled and roll a new additional 6.
	/// // Final expected rolled dice set, after xo modifier: 4d6xo[3, 6, 1, 2, 6]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[4].add(xo_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	Explode {
		/// Condition that rolls must pass in order to explode.
		/// If `None`, the roll values must be equal to the number of sides of the dice being rolled.
		cond: Option<Condition>,

		/// Whether the explosion should be done repeatedly for any additional rolls that also meet the condition
		recurse: bool,
	},

	/// Keeps only the highest x dice, dropping the rest.
	///
	/// # Examples
	///
	/// ## Keep highest die (`kh`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6kh dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_high(1).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a kh modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let kh_mod = Modifier::KeepHigh(1);
	/// kh_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop all rolls except the highest one, so 3, 1, and 2 will be dropped.
	/// // Final expected rolled dice set, after kh modifier: 4d6kh[3 (d), 6, 1 (d), 2 (d)]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[0].drop(kh_mod);
	/// expected.rolls[2].drop(kh_mod);
	/// expected.rolls[3].drop(kh_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	///
	/// ## Keep highest 2 dice (`kh2`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6kh2 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_high(2).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a kh2 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let kh2_mod = Modifier::KeepHigh(2);
	/// kh2_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop all rolls except the two highest, so 1 and 2 will be dropped.
	/// // Final expected rolled dice set, after kh2 modifier: 4d6kh2[3, 6, 1 (d), 2 (d)]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[2].drop(kh2_mod);
	/// expected.rolls[3].drop(kh2_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	KeepHigh(u8),

	/// Keeps only the lowest x dice, dropping the rest.
	///
	/// # Examples
	///
	/// ## Keep lowest die (`kl`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6kl dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_low(1).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a kl modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let kl_mod = Modifier::KeepLow(1);
	/// kl_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop all rolls except the lowest one, so 3, 6, and 2 will be dropped.
	/// // Final expected rolled dice set, after kl modifier: 4d6kl[3 (d), 6 (d), 1, 2 (d)]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[0].drop(kl_mod);
	/// expected.rolls[1].drop(kl_mod);
	/// expected.rolls[3].drop(kl_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	///
	/// ## Keep lowest 2 dice (`kl2`)
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6kl2 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_low(2).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a kl2 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let kl2_mod = Modifier::KeepLow(2);
	/// kl2_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will drop all rolls except the two lowest, so 3 and 6 will be dropped.
	/// // Final expected rolled dice set, after kl2 modifier: 4d6kl2[3 (d), 6 (d), 1, 2]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[0].drop(kl2_mod);
	/// expected.rolls[1].drop(kl2_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	KeepLow(u8),

	/// Replaces values of rolls lower than a minimum with the minimum.
	///
	/// # Examples
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6min3 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).min(3).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a min3 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let min3_mod = Modifier::Min(3);
	/// min3_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will replace the values of all rolls less than 3 with 3, so 1 and 2 will
	/// // both become 3.
	/// // Final expected rolled dice set, after min3 modifier: 4d6min3[3, 6, 3 (m), 3 (m)]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[2].change(min3_mod, 3);
	/// expected.rolls[3].change(min3_mod, 3);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	Min(u8),

	/// Replaces values of rolls higher than a maximum with the maximum.
	///
	/// # Examples
	/// ```
	/// use tyche::dice::{modifier::{Condition, Modifier}, roller::{Iter as IterRoller, Roller}, Dice, Rolled};
	///
	/// // Build the 4d6max3 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).max(3).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = IterRoller::new(premade_rolls);
	///
	/// // Roll the dice, but don't have it automatically apply its modifiers (passing `false` as the second `roll()` param).
	/// let mut rolled = rng.roll(&dice, false)?;
	///
	/// // Explicitly create and apply a max3 modifier. This is usually not necessary since the dice has its own instance of
	/// // it from the builder and will automatically apply it when passing `true` to `roll()`, but we do it this way here
	/// // for demonstration purposes.
	/// let max3_mod = Modifier::Max(3);
	/// max3_mod.apply(&mut rolled, &mut rng)?;
	///
	/// // Upon being applied, the modifier will replace the values of all rolls greater than 3 with 3, so 6 will become 3.
	/// // Final expected rolled dice set, after max3 modifier: 4d6max3[3, 3 (m), 1, 2]
	/// let mut expected = Rolled::from_dice_and_rolls(&dice, premade_rolls);
	/// expected.rolls[1].change(max3_mod, 3);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), tyche::dice::Error>(())
	/// ```
	Max(u8),
	//
	// /// Count the number of dice that meet or don't meet (second parameter `true` for meets, `false` for does not meet)
	// /// the given condition.
	// CountCond(Condition, bool),

	// /// Count the number of dice that are even (`true`) or odd (`false`)
	// CountParity(bool),

	// /// Subtract the number of dice that meet the given condition
	// SubCond(Condition),

	// /// Subtract the values of dice that meet the given condition
	// SubCondVal(Condition),

	// /// Subtract a value from the total
	// Margin(u8),
}

impl Modifier {
	/// Applies the modifier to a set of rolls, using a given roller if additional die rolls are needed.
	///
	/// # Errors
	/// If applying the modifier would result in infinite additional die rolls, an error variant is returned.
	pub fn apply(self, rolled: &mut Rolled, rng: &mut impl Roller) -> Result<(), Error> {
		match self {
			Self::Reroll { cond, recurse } => self.apply_reroll(rolled, rng, cond, recurse)?,
			Self::Explode { cond, recurse } => self.apply_explode(rolled, rng, cond, recurse)?,
			Self::KeepHigh(count) => self.apply_keep_high(rolled, count),
			Self::KeepLow(count) => self.apply_keep_low(rolled, count),
			Self::Min(min) => self.apply_min(rolled, min),
			Self::Max(max) => self.apply_max(rolled, max),
		}

		Ok(())
	}

	/// Applies the [`Self::Reroll`] variant to a set of rolled dice.
	fn apply_reroll(
		self,
		rolled: &mut Rolled,
		rng: &mut impl Roller,
		cond: Condition,
		recurse: bool,
	) -> Result<(), Error> {
		// Prevent recursively rerolling dice that would result in infinite rerolls
		if recurse {
			match cond {
				Condition::Eq(other) if other == 1 && rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Condition::Gt(0) | Condition::Gte(..=1) => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Condition::Lt(other) if other > rolled.dice.sides => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Condition::Lte(other) if other >= rolled.dice.sides => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				_ => {}
			}
		}

		loop {
			// Determine which rolls qualify for reroll
			let mut to_reroll = rolled
				.rolls
				.iter_mut()
				.filter(|roll| roll.is_kept())
				.filter(|roll| cond.check(roll.val))
				.collect::<Vec<_>>();

			if to_reroll.is_empty() {
				break;
			}

			// Roll additional dice and drop the originals
			let mut rerolls = Vec::with_capacity(to_reroll.len());
			for roll in &mut to_reroll {
				let mut reroll = rng.roll_die(rolled.dice.sides);
				reroll.add(self);
				rerolls.push(reroll);
				roll.drop(self);
			}

			// Add the rerolls to the rolls
			rolled.rolls.append(&mut rerolls);

			if !recurse {
				break;
			}
		}

		Ok(())
	}

	/// Applies the [`Self::Explode`] variant to a set of rolled dice.
	fn apply_explode(
		self,
		rolled: &mut Rolled,
		rng: &mut impl Roller,
		cond: Option<Condition>,
		recurse: bool,
	) -> Result<(), Error> {
		// Prevent recursively exploding dice that would result in infinite explosions
		if recurse {
			match cond {
				Some(Condition::Eq(other)) if other == 1 && rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Some(Condition::Gt(0) | Condition::Gte(..=1)) => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Some(Condition::Lt(other)) if other > rolled.dice.sides => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				Some(Condition::Lte(other)) if other >= rolled.dice.sides => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				None if rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls((*rolled.dice).clone()));
				}
				_ => {}
			}
		}

		// Determine how many initial rolls qualify for explosion
		let mut to_explode = rolled
			.rolls
			.iter()
			.filter(|roll| roll.is_kept())
			.filter(|roll| match cond {
				Some(cond) => cond.check(roll.val),
				None => roll.val == rolled.dice.sides,
			})
			.count();

		loop {
			// Roll additional dice
			let mut explosions = Vec::with_capacity(to_explode);
			for _ in 0..to_explode {
				let mut roll = rng.roll_die(rolled.dice.sides);
				roll.add(self);
				explosions.push(roll);
			}

			// Determine how many additional rolls qualify for explosion
			to_explode = if recurse {
				explosions
					.iter()
					.filter(|roll| match cond {
						Some(cond) => cond.check(roll.val),
						None => roll.val == rolled.dice.sides,
					})
					.count()
			} else {
				0
			};

			// Add the explosions to the rolls and finish if there are no further rolls to explode
			rolled.rolls.append(&mut explosions);
			if to_explode == 0 {
				break;
			}
		}

		Ok(())
	}

	/// Applies the [`Self::KeepHigh`] variant to a set of rolled dice.
	fn apply_keep_high(self, rolled: &mut Rolled, count: u8) {
		let mut refs = rolled
			.rolls
			.iter_mut()
			.filter(|roll| roll.is_kept())
			.collect::<Vec<_>>();
		refs.sort();
		refs.reverse();
		refs.iter_mut().skip(count as usize).for_each(|roll| roll.drop(self));
	}

	/// Applies the [`Self::KeepLow`] variant to a set of rolled dice.
	fn apply_keep_low(self, rolled: &mut Rolled, count: u8) {
		let mut refs = rolled
			.rolls
			.iter_mut()
			.filter(|roll| roll.is_kept())
			.collect::<Vec<_>>();
		refs.sort();
		refs.iter_mut().skip(count as usize).for_each(|roll| roll.drop(self));
	}

	/// Applies the [`Self::Min`] variant to a set of rolled dice.
	fn apply_min(self, rolled: &mut Rolled, min: u8) {
		rolled
			.rolls
			.iter_mut()
			.filter(|roll| roll.is_kept() && roll.val < min)
			.for_each(|roll| roll.change(self, min));
	}

	/// Applies the [`Self::Max`] variant to a set of rolled dice.
	fn apply_max(self, rolled: &mut Rolled, max: u8) {
		rolled
			.rolls
			.iter_mut()
			.filter(|roll| roll.is_kept() && roll.val > max)
			.for_each(|roll| roll.change(self, max));
	}
}

impl fmt::Display for Modifier {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}",
			match self {
				Self::Reroll { recurse, .. } => format!("r{}", recurse.then_some("r").unwrap_or("")),
				Self::Explode { recurse, .. } => format!("x{}", recurse.then_some("").unwrap_or("o")),
				Self::KeepHigh(count) => format!("kh{}", if *count > 1 { count.to_string() } else { String::new() }),
				Self::KeepLow(count) => format!("kl{}", if *count > 1 { count.to_string() } else { String::new() }),
				Self::Min(min) => format!("min{min}"),
				Self::Max(max) => format!("max{max}"),
			},
			match self {
				Self::Reroll { cond, .. } | Self::Explode { cond: Some(cond), .. } => cond.to_string(),
				Self::Explode { cond: None, .. }
				| Self::KeepHigh(..)
				| Self::KeepLow(..)
				| Self::Max(..)
				| Self::Min(..) => String::new(),
			}
		)
	}
}

/// Test that die values can be checked against
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[expect(clippy::exhaustive_enums, reason = "Unlikely to change, no logical fallback")]
pub enum Condition {
	/// Checks whether values are equal to its own value. Symbol: `=`
	Eq(u8),

	/// Checks whether values are greater than its own value. Symbol: `>`
	Gt(u8),

	/// Checks whether values are greater than or equal to its own value. Symbol: `>=`
	Gte(u8),

	/// Checks whether values are less than its own value. Symbol: `<`
	Lt(u8),

	/// Checks whether values are less than or equal to its own value. Symbol: `<=`
	Lte(u8),
}

impl Condition {
	/// Creates a condition from its corresponding symbol and a given value.
	///
	/// # Errors
	/// If the symbol doesn't match to a known condition variant, an error variant will be returned.
	pub fn from_symbol_and_val(symbol: &str, val: u8) -> Result<Self, Error> {
		Ok(match symbol {
			"=" => Self::Eq(val),
			">" => Self::Gt(val),
			">=" => Self::Gte(val),
			"<" => Self::Lt(val),
			"<=" => Self::Lte(val),
			_ => return Err(Error::UnknownCondition(symbol.to_owned())),
		})
	}

	/// Checks a value against the condition.
	#[must_use]
	pub const fn check(&self, val: u8) -> bool {
		match self {
			Self::Eq(expected) => val == *expected,
			Self::Gt(expected) => val > *expected,
			Self::Gte(expected) => val >= *expected,
			Self::Lt(expected) => val < *expected,
			Self::Lte(expected) => val <= *expected,
		}
	}

	/// Gets the symbol that represents the condition.
	#[must_use]
	pub const fn symbol(&self) -> &'static str {
		match self {
			Self::Eq(..) => "=",
			Self::Gt(..) => ">",
			Self::Gte(..) => ">=",
			Self::Lt(..) => "<",
			Self::Lte(..) => "<=",
		}
	}
}

impl fmt::Display for Condition {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}",
			self.symbol(),
			match self {
				Self::Eq(expected)
				| Self::Gt(expected)
				| Self::Gte(expected)
				| Self::Lt(expected)
				| Self::Lte(expected) => expected,
			}
		)
	}
}
