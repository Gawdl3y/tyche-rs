//! All functionality for directly creating dice, rolling them, and working with their resulting rolls.
//!
//! This is the home of the dice "primitives". For using as part of a larger expression, see [`Expr::dice`].
//!
//! [`Expr::dice`]: crate::expr::Expr::Dice

pub mod roller;

use std::{borrow::Cow, cmp, fmt};

use crate::expr::Describe;

use self::roller::Roller;

/// A set of one or more rollable dice with a specific number of sides, along with a collection of modifiers to apply to
/// any resulting rolls from them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::exhaustive_structs)]
pub struct Dice {
	/// Number of dice to roll
	pub count: u8,

	/// Number of sides for each die
	pub sides: u8,

	/// Modifiers to automatically apply to rolls from this set of dice
	pub modifiers: Vec<Modifier>,
}

impl Dice {
	/// Creates a new set of dice matching this one but without any modifiers.
	#[must_use]
	#[inline]
	pub const fn plain(&self) -> Self {
		Self::new(self.count, self.sides)
	}

	/// Creates a new set of dice with a given count and number of sides.
	#[must_use]
	pub const fn new(count: u8, sides: u8) -> Self {
		Self {
			count,
			sides,
			modifiers: Vec::new(),
		}
	}

	/// Creates a new dice builder.
	#[must_use]
	#[inline]
	pub fn builder() -> Builder {
		Builder::default()
	}
}

impl Default for Dice {
	/// Creates the default dice (1d20).
	#[inline]
	fn default() -> Self {
		Self::new(1, 20)
	}
}

impl fmt::Display for Dice {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}d{}{}",
			self.count,
			self.sides,
			self.modifiers.iter().map(ToString::to_string).collect::<String>()
		)
	}
}

/// Routines that can be applied to [`Dice`] to automatically manipulate resulting [`Rolled`] dice sets from them
/// as part of their rolling process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Modifier {
	/// Rerolls (drops original and adds a newly-rolled die) dice that meet a condition.
	///
	/// # Examples
	///
	/// ## Reroll recursively (`rr`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6rr1 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).reroll(Condition::Eq(1), true).build();
	/// let premade_rolls = [3, 6, 1, 2, 1, 4];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[2].drop(rr1_mod);
	/// expected.rolls[4].add(rr1_mod);
	/// expected.rolls[4].drop(rr1_mod);
	/// expected.rolls[5].add(rr1_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Reroll once (`r`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6r1 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).reroll(Condition::Eq(1), false).build();
	/// let premade_rolls = [3, 6, 1, 2, 1];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[2].drop(r1_mod);
	/// expected.rolls[4].add(r1_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
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
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6x dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).explode(None, true).build();
	/// let premade_rolls = [3, 6, 1, 2, 6, 4];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[4].add(x_mod);
	/// expected.rolls[5].add(x_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Explode once (`xo`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6xo dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).explode(None, false).build();
	/// let premade_rolls = [3, 6, 1, 2, 6];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[4].add(xo_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
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
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6kh dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_high(1).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[0].drop(kh_mod);
	/// expected.rolls[2].drop(kh_mod);
	/// expected.rolls[3].drop(kh_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Keep highest 2 dice (`kh2`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6kh2 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_high(2).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[2].drop(kh2_mod);
	/// expected.rolls[3].drop(kh2_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	KeepHigh(u8),

	/// Keeps only the lowest x dice, dropping the rest.
	///
	/// # Examples
	///
	/// ## Keep lowest die (`kl`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6kl dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_low(1).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[0].drop(kl_mod);
	/// expected.rolls[1].drop(kl_mod);
	/// expected.rolls[3].drop(kl_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Keep lowest 2 dice (`kl2`)
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6kl2 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).keep_low(2).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[0].drop(kl2_mod);
	/// expected.rolls[1].drop(kl2_mod);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	KeepLow(u8),

	/// Replaces values of rolls lower than a minimum with the minimum.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6min3 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).min(3).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[2].change(min3_mod, 3);
	/// expected.rolls[3].change(min3_mod, 3);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	Min(u8),

	/// Replaces values of rolls higher than a maximum with the maximum.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{roller::{Iter, Roller}, Condition, Dice, Modifier, Rolled};
	///
	/// // Build the 4d6max3 dice set and create a roller that has predetermined values for the dice rolls
	/// let dice = Dice::builder().count(4).sides(6).max(3).build();
	/// let premade_rolls = [3, 6, 1, 2];
	/// let mut rng = Iter::new(premade_rolls);
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
	/// let mut expected = Rolled::from_dice_and_rolls(dice.clone(), premade_rolls);
	/// expected.rolls[1].change(max3_mod, 3);
	/// assert_eq!(rolled, expected);
	/// # Ok::<(), dicey::dice::Error>(())
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
		};

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

			// Determine how many additional rolls qualify for explosion, then add the explosions to the rolls
			to_explode = recurse
				.then(|| {
					explosions
						.iter()
						.filter(|roll| match cond {
							Some(cond) => cond.check(roll.val),
							None => roll.val == rolled.dice.sides,
						})
						.count()
				})
				.unwrap_or(0);
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
#[allow(clippy::exhaustive_enums)]
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

/// Single die produced from rolling [`Dice`] and optionally applying [`Modifier`]s
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct DieRoll {
	/// Value that was rolled
	pub val: u8,

	/// Modifier that caused the addition of this die, if any
	pub added_by: Option<Modifier>,

	/// Modifier that caused the drop of this die, if any
	pub dropped_by: Option<Modifier>,

	/// Modifications that were made to the value of the roll
	pub changes: Vec<ValChange>,
}

impl DieRoll {
	/// Marks this die roll as added by a given modifier, setting [`Self::added_by`].
	///
	/// # Panics
	/// Panics if `Self::added_by` is already [`Some`].
	pub fn add(&mut self, from: Modifier) {
		assert!(
			self.added_by.is_none(),
			"marking a die as added that has already been marked as added by another modifier"
		);
		self.added_by = Some(from);
	}

	/// Marks this die roll as dropped by a given modifier, setting [`Self::dropped_by`].
	///
	/// # Panics
	/// Panics if `Self::dropped_by` is already [`Some`].
	pub fn drop(&mut self, from: Modifier) {
		assert!(
			self.dropped_by.is_none(),
			"marking a die as dropped that has already been marked as dropped by another modifier"
		);
		self.dropped_by = Some(from);
	}

	/// Replaces the die roll's value and logs the change made.
	pub fn change(&mut self, from: Modifier, new_val: u8) {
		self.changes.push(ValChange {
			before: self.val,
			after: new_val,
			cause: from,
		});
		self.val = new_val;
	}

	/// Indicates whether this die roll was part of the original set (not added by a modifier).
	#[must_use]
	#[inline]
	pub const fn is_original(&self) -> bool {
		self.added_by.is_none()
	}

	/// Indicates whether this die roll was added as the result of a modifier being applied.
	/// This is the direct inverse of [`DieRoll::is_original()`].
	#[must_use]
	#[inline]
	pub const fn is_additional(&self) -> bool {
		self.added_by.is_some()
	}

	/// Indicates whether this die roll has been dropped by a modifier.
	#[must_use]
	#[inline]
	pub const fn is_dropped(&self) -> bool {
		self.dropped_by.is_some()
	}

	/// Indicates whether this die roll is being kept (has *not* been dropped by a modifier).
	/// This is the direct inverse of [`DieRoll::is_dropped()`].
	#[must_use]
	#[inline]
	pub const fn is_kept(&self) -> bool {
		self.dropped_by.is_none()
	}

	/// Indicates whether this die roll's value has been directly changed by a modifier.
	#[must_use]
	#[inline]
	pub fn is_changed(&self) -> bool {
		!self.changes.is_empty()
	}

	/// Creates a new die roll with the given value.
	#[must_use]
	pub const fn new(val: u8) -> Self {
		Self {
			val,
			added_by: None,
			dropped_by: None,
			changes: Vec::new(),
		}
	}
}

impl PartialOrd for DieRoll {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for DieRoll {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.val.cmp(&other.val)
	}
}

impl fmt::Display for DieRoll {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The format of a die roll is simply the plain numeric value of the roll.
	/// If the roll was dropped, it is appended with ` (d)`.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::DieRoll;
	///
	/// let roll = DieRoll::new(4);
	/// assert_eq!(roll.to_string(), "4");
	/// ```
	///
	/// ```
	/// use dicey::dice::{DieRoll, Modifier};
	///
	/// let mut roll = DieRoll::new(16);
	/// let kh_mod = Modifier::KeepHigh(1);
	/// roll.drop(kh_mod);
	/// assert_eq!(roll.to_string(), "16 (d)");
	/// ```
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}{}",
			self.val,
			if self.is_changed() { " (m)" } else { "" },
			if self.is_dropped() { " (d)" } else { "" }
		)
	}
}

/// Details about a modification made to a [`DieRoll`] as a result of a [`Modifier`] being applied to it
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::exhaustive_structs)]
pub struct ValChange {
	/// Roll value before the change was made
	pub before: u8,

	/// Roll value after the change was made
	pub after: u8,

	/// Modifier that caused the change
	pub cause: Modifier,
}

/// Representation of the result from rolling [`Dice`]
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::exhaustive_structs)]
pub struct Rolled<'a> {
	/// Each individual die roll that was made
	pub rolls: Vec<DieRoll>,

	/// Dice that were rolled to produce this
	pub dice: Cow<'a, Dice>,
}

impl Rolled<'_> {
	/// Calculates the total of all roll values.
	///
	/// # Errors
	/// If there is an integer overflow while summing the die rolls, an error variant is returned.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{Dice, roller::{FastRand, Roller}};
	///
	/// let dice = Dice::new(4, 8);
	/// let rolled = FastRand::default().roll(&dice, true)?;
	/// let total = rolled.total()?;
	/// assert_eq!(total, rolled.rolls.iter().map(|roll| roll.val as u16).sum());
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	pub fn total(&self) -> Result<u16, Error> {
		let mut sum: u16 = 0;

		// Sum all rolls that haven't been dropped
		for r in self.rolls.iter().filter(|roll| roll.is_kept()) {
			sum = sum
				.checked_add(u16::from(r.val))
				.ok_or_else(|| Error::Overflow(self.clone().into_owned()))?;
		}

		Ok(sum)
	}

	/// Moves all of self's owned data into a new instance and clones any unowned data in order to create a `'static`
	/// instance of self.
	#[must_use]
	pub fn into_owned(self) -> Rolled<'static> {
		Rolled {
			rolls: self.rolls,
			dice: Cow::Owned(self.dice.into_owned()),
		}
	}

	/// Creates a new rolled set of dice from a given set of dice and an iterator of values.
	#[must_use]
	pub fn from_dice_and_rolls(dice: Dice, rolls: impl IntoIterator<Item = u8>) -> Self {
		Rolled {
			rolls: rolls.into_iter().map(DieRoll::new).collect(),
			dice: Cow::Owned(dice),
		}
	}
}

impl Describe for Rolled<'_> {
	/// Builds a string of the dice the roll is from and a list of all of the individual rolled dice
	/// (see [`DieRoll::fmt()`]).
	///
	/// If `list_limit` is specified and there are more rolls than it, the list of rolled dice will be truncated and
	/// appended with "X more..." (where X is the remaining roll count past the max).
	///
	/// # Examples
	/// ```
	/// use std::borrow::Cow;
	/// use dicey::{dice::{Dice, DieRoll, Rolled}, expr::Describe};
	///
	/// let dice = Dice::builder().count(4).sides(6).keep_high(2).build();
	/// let kh_mod = dice.modifiers[0];
	/// let rolled = Rolled {
	/// 	rolls: vec![
	/// 		DieRoll::new(6),
	/// 		{
	/// 			let mut roll = DieRoll::new(2);
	/// 			roll.drop(kh_mod);
	/// 			roll
	/// 		},
	/// 		DieRoll::new(5),
	/// 		{
	/// 			let mut roll = DieRoll::new(3);
	/// 			roll.drop(kh_mod);
	/// 			roll
	/// 		},
	/// 	],
	/// 	dice: Cow::Borrowed(&dice),
	/// };
	///
	/// assert_eq!(rolled.describe(None), "4d6kh2[6, 2 (d), 5, 3 (d)]");
	/// assert_eq!(rolled.describe(Some(2)), "4d6kh2[6, 2 (d), 2 more...]");
	/// ```
	///
	/// [`DieRoll::fmt()`]: ./struct.DieRoll.html#method.fmt
	fn describe(&self, list_limit: Option<usize>) -> String {
		let list_limit = list_limit.unwrap_or(usize::MAX);
		let total_rolls = self.rolls.len();
		let truncated_rolls = total_rolls.saturating_sub(list_limit);

		format!(
			"{}[{}{}]",
			self.dice,
			self.rolls
				.iter()
				.take(list_limit)
				.map(ToString::to_string)
				.collect::<Vec<_>>()
				.join(", "),
			if truncated_rolls > 0 {
				format!(", {truncated_rolls} more...")
			} else {
				String::new()
			}
		)
	}
}

impl fmt::Display for Rolled<'_> {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output is equivalent to calling [`Self::describe(None)`].
	///
	/// [`Self::describe(None)`]: Self::describe()
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// An error resulting from a dice operation
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
	/// There was an integer overflow when performing mathematical operations on roll values.
	/// This normally should not ever happen given the types used for die counts, sides, and totals.
	#[error("integer overflow")]
	Overflow(Rolled<'static>),

	/// Rolling the dice specified would result in infinite rolls.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{Dice, Error, roller::{FastRand, Roller}};
	///
	/// let dice = Dice::builder().count(4).sides(1).explode(None, true).build();
	/// assert!(matches!(FastRand::default().roll(&dice, true), Err(Error::InfiniteRolls(..))));
	/// ```
	#[error("{0} would result in infinite rolls")]
	InfiniteRolls(Dice),

	/// The provided symbol doesn't match to a known condition.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{Condition, Error};
	///
	/// let cond = Condition::from_symbol_and_val("!", 4);
	/// assert!(matches!(cond, Err(Error::UnknownCondition(..))));
	/// ```
	#[error("unknown condition symbol: {0}")]
	UnknownCondition(String),
}

/// Builds [`Dice`] with a fluent interface.
///
/// # Examples
///
/// ## Basic dice
/// ```
/// use dicey::dice::Dice;
///
/// let dice = Dice::builder().count(2).sides(6).build();
/// assert_eq!(dice, Dice::new(2, 6));
/// ```
///
/// ## Single modifier
/// ```
/// use dicey::dice::{Dice, Modifier};
///
/// let dice = Dice::builder().count(6).sides(8).explode(None, true).build();
/// assert_eq!(
/// 	dice,
/// 	Dice {
/// 		count: 6,
/// 		sides: 8,
/// 		modifiers: vec![Modifier::Explode {
/// 			cond: None,
/// 			recurse: true,
/// 		}],
/// 	},
/// );
/// ```
///
/// ## Multiple modifiers
/// ```
/// use dicey::dice::{Condition, Dice, Modifier};
///
/// let dice = Dice::builder()
/// 	.count(6)
/// 	.sides(8)
/// 	.reroll(Condition::Eq(1), false)
/// 	.keep_high(4)
/// 	.build();
/// assert_eq!(
/// 	dice,
/// 	Dice {
/// 		count: 6,
/// 		sides: 8,
/// 		modifiers: vec![
/// 			Modifier::Reroll {
/// 				cond: Condition::Eq(1),
/// 				recurse: false
/// 			},
/// 			Modifier::KeepHigh(4),
/// 		],
/// 	},
/// );
/// ```
#[derive(Debug, Clone, Default)]
pub struct Builder(Dice);

impl Builder {
	/// Sets the number of dice to roll.
	#[must_use]
	pub const fn count(mut self, count: u8) -> Self {
		self.0.count = count;
		self
	}

	/// Sets the number of sides per die.
	#[must_use]
	pub const fn sides(mut self, sides: u8) -> Self {
		self.0.sides = sides;
		self
	}

	/// Adds a reroll modifier to the dice.
	#[must_use]
	pub fn reroll(mut self, cond: Condition, recurse: bool) -> Self {
		self.0.modifiers.push(Modifier::Reroll { cond, recurse });
		self
	}

	/// Adds an exploding modifier to the dice.
	#[must_use]
	pub fn explode(mut self, cond: Option<Condition>, recurse: bool) -> Self {
		self.0.modifiers.push(Modifier::Explode { cond, recurse });
		self
	}

	/// Adds a keep highest modifier to the dice.
	#[must_use]
	pub fn keep_high(mut self, count: u8) -> Self {
		self.0.modifiers.push(Modifier::KeepHigh(count));
		self
	}

	/// Adds a keep lowest modifier to the dice.
	#[must_use]
	pub fn keep_low(mut self, count: u8) -> Self {
		self.0.modifiers.push(Modifier::KeepLow(count));
		self
	}

	/// Adds a minimum modifier to the dice.
	#[must_use]
	pub fn min(mut self, min: u8) -> Self {
		self.0.modifiers.push(Modifier::Min(min));
		self
	}

	/// Adds a maximum modifier to the dice.
	#[must_use]
	pub fn max(mut self, max: u8) -> Self {
		self.0.modifiers.push(Modifier::Max(max));
		self
	}

	/// Finalizes the dice.
	#[must_use]
	pub fn build(self) -> Dice {
		self.0
	}
}
