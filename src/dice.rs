//! All functionality for directly creating dice, rolling them, and working with their resulting rolls.
//!
//! This is the home of the dice "primitives". For using as part of a larger expression, see [`Expr::dice`].
//!
//! [`Expr::dice`]: crate::expr::Expr::Dice

use std::{cmp, fmt};

use fastrand::Rng;

use crate::expr::Describe;

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
	/// Rolls the dice and applies all of its modifiers to the rolls using the default Rng.
	///
	/// # Errors
	/// If any errors are encountered while applying the dice's modifiers, an error variant is returned.
	#[inline]
	pub fn roll(&self) -> Result<Rolled, Error> {
		self.roll_using_rng(&mut Rng::new())
	}

	/// Rolls the dice and applies all of its modifiers to the rolls using the given Rng.
	///
	/// # Errors
	/// If any errors are encountered while applying the dice's modifiers, an error variant is returned.
	pub fn roll_using_rng(&self, rng: &mut Rng) -> Result<Rolled, Error> {
		// Roll the dice!
		let mut rolls = Vec::with_capacity(self.count as usize);
		for _ in 0..self.count {
			rolls.push(self.roll_single_using_rng(rng));
		}

		// Apply all modifiers
		let mut rolls = Rolled { rolls, dice: self };
		for modifier in &self.modifiers {
			modifier.apply_using_rng(&mut rolls, rng)?;
		}

		Ok(rolls)
	}

	/// Rolls a single die (with the same number of sides as the dice in this set)
	/// with no modifiers using the default Rng.
	#[must_use]
	#[inline]
	pub fn roll_single(&self) -> DieRoll {
		DieRoll::new_rand(self.sides)
	}

	/// Rolls a single die (with the same number of sides as the dice in this set)
	/// with no modifiers using the given Rng.
	#[must_use]
	#[inline]
	pub fn roll_single_using_rng(&self, rng: &mut Rng) -> DieRoll {
		DieRoll::new_rand_using_rng(self.sides, rng)
	}

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Modifier {
	/// Rerolls (drops original and adds a newly-rolled die) dice that meet a condition.
	///
	/// # Examples
	///
	/// ## Reroll recursively (`rr`)
	/// ```
	/// use dicey::dice::{Condition, Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6rr
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.reroll(Condition::Eq(1), true)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6rr[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::Reroll` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let rr_mod = &dice.modifiers[0];
	/// rr_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6rr[3, 6, 1, 2, x...]
	/// assert!(rolled.rolls.len() > 4);
	/// assert_eq!(rolled.rolls[2].dropped_by, Some(rr_mod));
	/// rolled.rolls.iter().skip(4).for_each(|roll| assert_eq!(roll.added_by, Some(rr_mod)));
	/// rolled
	/// 	.rolls
	/// 	.iter()
	/// 	.skip(4)
	/// 	.take(rolled.rolls.len() - 5)
	/// 	.for_each(|roll| assert_eq!(roll.dropped_by, Some(rr_mod)));
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Reroll once (`r`)
	/// ```
	/// use dicey::dice::{Condition, Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6r
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.reroll(Condition::Eq(1), false)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6r[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::Reroll` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let r_mod = &dice.modifiers[0];
	/// r_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6r[3, 6, 1, 2, x]
	/// assert_eq!(rolled.rolls.len(), 5);
	/// assert_eq!(rolled.rolls[2].dropped_by, Some(r_mod));
	/// assert_eq!(rolled.rolls[4].added_by, Some(r_mod));
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
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6x
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.explode(None, true)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6x[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::Explode` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let x_mod = &dice.modifiers[0];
	/// x_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6xo[3, 6, 1, 2, x...]
	/// assert!(rolled.rolls.len() > 4);
	/// rolled.rolls.iter().skip(4).for_each(|roll| assert_eq!(roll.added_by, Some(x_mod)));
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Explode once (`xo`)
	/// ```
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6xo
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.explode(None, false)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6xo[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::Explode` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let xo_mod = &dice.modifiers[0];
	/// xo_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6xo[3, 6, 1, 2, x]
	/// assert_eq!(rolled.rolls.len(), 5);
	/// assert_eq!(rolled.rolls[4].added_by, Some(xo_mod));
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
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6kh
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.keep_high(1)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6kh[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::KeepHigh(1)` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let kh_mod = &dice.modifiers[0];
	/// kh_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6kh[3 (d), 6, 1 (d), 2 (d)]
	/// assert_eq!(
	/// 	rolled,
	/// 	Rolled {
	/// 		rolls: vec![
	/// 			{
	/// 				let mut roll = DieRoll::new(3);
	/// 				roll.drop(kh_mod);
	/// 				roll
	/// 			},
	/// 			DieRoll::new(6),
	/// 			{
	/// 				let mut roll = DieRoll::new(1);
	/// 				roll.drop(kh_mod);
	/// 				roll
	/// 			},
	/// 			{
	/// 				let mut roll = DieRoll::new(2);
	/// 				roll.drop(kh_mod);
	/// 				roll
	/// 			},
	/// 		],
	/// 		dice: &dice,
	/// 	}
	/// );
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Keep highest 2 dice (`kh2`)
	/// ```
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6kh2
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.keep_high(2)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6kh2[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::KeepHigh(2)` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let kh2_mod = &dice.modifiers[0];
	/// kh2_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6kh2[3, 6, 1 (d), 2 (d)]
	/// assert_eq!(
	/// 	rolled,
	/// 	Rolled {
	/// 		rolls: vec![
	/// 			DieRoll::new(3),
	/// 			DieRoll::new(6),
	/// 			{
	/// 				let mut roll = DieRoll::new(1);
	/// 				roll.drop(kh2_mod);
	/// 				roll
	/// 			},
	/// 			{
	/// 				let mut roll = DieRoll::new(2);
	/// 				roll.drop(kh2_mod);
	/// 				roll
	/// 			},
	/// 		],
	/// 		dice: &dice,
	/// 	}
	/// );
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	KeepHigh(u8),

	/// Keeps only the lowest x dice, dropping the rest.
	///
	/// # Examples
	///
	/// ## Keep lowest die (`kl`)
	/// ```
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6kl
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.keep_low(1)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6kl[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::KeepLow(1)` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let kl_mod = &dice.modifiers[0];
	/// kl_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6kl[3 (d), 6 (d), 1, 2 (d)]
	/// assert_eq!(
	/// 	rolled,
	/// 	Rolled {
	/// 		rolls: vec![
	/// 			{
	/// 				let mut roll = DieRoll::new(3);
	/// 				roll.drop(kl_mod);
	/// 				roll
	/// 			},
	/// 			{
	/// 				let mut roll = DieRoll::new(6);
	/// 				roll.drop(kl_mod);
	/// 				roll
	/// 			},
	/// 			DieRoll::new(1),
	/// 			{
	/// 				let mut roll = DieRoll::new(2);
	/// 				roll.drop(kl_mod);
	/// 				roll
	/// 			},
	/// 		],
	/// 		dice: &dice,
	/// 	}
	/// );
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	///
	/// ## Keep lowest 2 dice (`kl2`)
	/// ```
	/// use dicey::dice::{Dice, DieRoll, Rolled};
	///
	/// // Dice set: 4d6kl2
	/// let dice = Dice::builder()
	/// 	.count(4)
	/// 	.sides(6)
	/// 	.keep_low(2)
	/// 	.build();
	/// // Rolled dice set (before applying modifier): 4d6kl2[3, 6, 1, 2]
	/// let mut rolled = Rolled {
	/// 	rolls: vec![DieRoll::new(3), DieRoll::new(6), DieRoll::new(1), DieRoll::new(2)],
	/// 	dice: &dice,
	/// };
	///
	/// // Could also use a `Modifier::KeepLow(2)` directly rather than getting the modifier from the dice,
	/// // but that wouldn't maintain the reference to the dice's modifier (like how [`Dice::roll()`] does)
	/// let kl2_mod = &dice.modifiers[0];
	/// kl2_mod.apply(&mut rolled)?;
	///
	/// // Final rolled dice set: 4d6kl2[3 (d), 6 (d), 1, 2]
	/// assert_eq!(
	/// 	rolled,
	/// 	Rolled {
	/// 		rolls: vec![
	/// 			{
	/// 				let mut roll = DieRoll::new(3);
	/// 				roll.drop(kl2_mod);
	/// 				roll
	/// 			},
	/// 			{
	/// 				let mut roll = DieRoll::new(6);
	/// 				roll.drop(kl2_mod);
	/// 				roll
	/// 			},
	/// 			DieRoll::new(1),
	/// 			DieRoll::new(2),
	/// 		],
	/// 		dice: &dice,
	/// 	}
	/// );
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	KeepLow(u8),
	//
	// /// Replace all dice lower than a given minimum value with the minimum
	// Min(u8),

	// /// Replace all dice higher than a given maximum value with the maximum
	// Max(u8),

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
	/// Applies the modifier to a set of rolls using the default Rng where needed.
	///
	/// # Errors
	/// If applying the modifier would result in infinite additional die rolls, an error variant is returned.
	#[inline]
	pub fn apply<'r, 'm: 'r>(&'m self, rolls: &mut Rolled<'r>) -> Result<(), Error> {
		self.apply_using_rng(rolls, &mut Rng::new())
	}

	/// Applies the modifier to a set of rolls using a given Rng where needed.
	///
	/// # Errors
	/// If applying the modifier would result in infinite additional die rolls, an error variant is returned.
	pub fn apply_using_rng<'r, 'm: 'r>(&'m self, rolled: &mut Rolled<'r>, rng: &mut Rng) -> Result<(), Error> {
		match self {
			Self::Reroll { cond, recurse } => self.apply_reroll(rolled, rng, cond, *recurse)?,
			Self::Explode { cond, recurse } => self.apply_explode(rolled, rng, cond, *recurse)?,
			Self::KeepHigh(count) => self.apply_keep_high(rolled, *count),
			Self::KeepLow(count) => self.apply_keep_low(rolled, *count),
		};

		Ok(())
	}

	/// Applies the [`Self::Reroll`] variant to a set of rolled dice.
	fn apply_reroll<'r, 'm: 'r>(
		&'m self,
		rolled: &mut Rolled<'r>,
		rng: &mut Rng,
		cond: &Condition,
		recurse: bool,
	) -> Result<(), Error> {
		// Prevent recursively rerolling dice that would result in infinite rerolls
		if recurse {
			match cond {
				Condition::Eq(other) if *other == 1 && rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Condition::Gt(other) if *other == 0 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Condition::Gte(other) if *other <= 1 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Condition::Lt(other) if *other > rolled.dice.sides => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Condition::Lte(other) if *other >= rolled.dice.sides => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				_ => {}
			}
		}

		loop {
			// Determine which rolls qualify for reroll
			let mut to_reroll = rolled
				.rolls
				.iter_mut()
				.filter(|roll| !roll.is_dropped())
				.filter(|roll| cond.check(roll.val))
				.collect::<Vec<_>>();

			if to_reroll.is_empty() {
				break;
			}

			// Roll additional dice and drop the originals
			let mut rerolls = Vec::with_capacity(to_reroll.len());
			for roll in &mut to_reroll {
				let mut reroll = rolled.dice.roll_single_using_rng(rng);
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
	fn apply_explode<'r, 'm: 'r>(
		&'m self,
		rolled: &mut Rolled<'r>,
		rng: &mut Rng,
		cond: &Option<Condition>,
		recurse: bool,
	) -> Result<(), Error> {
		// Prevent recursively exploding dice that would result in infinite explosions
		if recurse {
			match cond {
				Some(Condition::Eq(other)) if *other == 1 && rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Some(Condition::Gt(other)) if *other == 0 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Some(Condition::Gte(other)) if *other <= 1 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Some(Condition::Lt(other)) if *other > rolled.dice.sides => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				Some(Condition::Lte(other)) if *other >= rolled.dice.sides => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				None if rolled.dice.sides == 1 => {
					return Err(Error::InfiniteRolls(rolled.dice.clone()));
				}
				_ => {}
			}
		}

		// Determine how many initial rolls qualify for explosion
		let mut to_explode = rolled
			.rolls
			.iter()
			.filter(|roll| !roll.is_dropped())
			.filter(|roll| match cond {
				Some(cond) => cond.check(roll.val),
				None => roll.val == rolled.dice.sides,
			})
			.count();

		loop {
			// Roll additional dice
			let mut explosions = Vec::with_capacity(to_explode);
			for _ in 0..to_explode {
				let mut roll = rolled.dice.roll_single_using_rng(rng);
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
	fn apply_keep_high<'r, 'm: 'r>(&'m self, rolled: &mut Rolled<'r>, count: u8) {
		let mut refs = rolled
			.rolls
			.iter_mut()
			.filter(|roll| !roll.is_dropped())
			.collect::<Vec<_>>();
		refs.sort();
		refs.reverse();
		refs.iter_mut().skip(count as usize).for_each(|roll| roll.drop(self));
	}

	/// Applies the [`Self::KeepLow`] variant to a set of rolled dice.
	fn apply_keep_low<'r, 'm: 'r>(&'m self, rolled: &mut Rolled<'r>, count: u8) {
		let mut refs = rolled
			.rolls
			.iter_mut()
			.filter(|roll| !roll.is_dropped())
			.collect::<Vec<_>>();
		refs.sort();
		refs.iter_mut().skip(count as usize).for_each(|roll| roll.drop(self));
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
			},
			match self {
				Self::Reroll { cond, .. } | Self::Explode { cond: Some(cond), .. } => cond.to_string(),
				Self::Explode { cond: None, .. } | Self::KeepHigh(..) | Self::KeepLow(..) => String::new(),
			}
		)
	}
}

/// Test that die values can be checked against
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct DieRoll<'m> {
	/// Value that was rolled
	pub val: u8,

	/// Modifier that caused the addition of this die, if any
	pub added_by: Option<&'m Modifier>,

	/// Modifier that caused the drop of this die, if any
	pub dropped_by: Option<&'m Modifier>,
}

impl<'m> DieRoll<'m> {
	/// Marks this die roll as added by a given modifier, setting [`Self::added_by`].
	///
	/// # Panics
	/// Panics if `Self::added_by` is already [`Some`].
	pub fn add(&mut self, from: &'m Modifier) {
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
	pub fn drop(&mut self, from: &'m Modifier) {
		assert!(
			self.dropped_by.is_none(),
			"marking a die as dropped that has already been marked as dropped by another modifier"
		);
		self.dropped_by = Some(from);
	}

	/// Indicates whether this die roll was part of the original set (not added by a modifier).
	#[must_use]
	#[inline]
	pub const fn is_original(&self) -> bool {
		self.added_by.is_none()
	}

	/// Indicates whether this die roll was been dropped by a modifier.
	#[must_use]
	#[inline]
	pub const fn is_dropped(&self) -> bool {
		self.dropped_by.is_some()
	}

	/// Creates a new die roll with the given value.
	#[must_use]
	pub const fn new(val: u8) -> Self {
		Self {
			val,
			added_by: None,
			dropped_by: None,
		}
	}

	/// Creates a new die roll with a random value using the default Rng.
	#[must_use]
	pub fn new_rand(max: u8) -> Self {
		let mut rng = Rng::new();
		Self::new_rand_using_rng(rng.u8(1..=max), &mut rng)
	}

	/// Creates a new die roll with a random value using the given Rng.
	#[must_use]
	pub fn new_rand_using_rng(max: u8, rng: &mut Rng) -> Self {
		Self::new(if max > 0 { rng.u8(1..=max) } else { 0 })
	}
}

impl PartialOrd for DieRoll<'_> {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for DieRoll<'_> {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.val.cmp(&other.val)
	}
}

impl fmt::Display for DieRoll<'_> {
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
	/// roll.drop(&kh_mod);
	/// assert_eq!(roll.to_string(), "16 (d)");
	/// ```
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.val, if self.is_dropped() { " (d)" } else { "" })
	}
}

/// Representation of the result from rolling [`Dice`]
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::exhaustive_structs)]
pub struct Rolled<'a> {
	/// Each individual die roll that was made
	pub rolls: Vec<DieRoll<'a>>,

	/// Dice that were rolled to produce this
	pub dice: &'a Dice,
}

impl Rolled<'_> {
	/// Calculates the total of all roll values.
	///
	/// # Errors
	/// If there is an integer overflow while summing the die rolls, an error variant is returned.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::Dice;
	///
	/// let dice = Dice::new(4, 8);
	/// let rolled = dice.roll()?;
	/// let total = rolled.total()?;
	/// assert_eq!(total, rolled.rolls.iter().map(|roll| roll.val as u16).sum());
	/// # Ok::<(), dicey::dice::Error>(())
	/// ```
	pub fn total(&self) -> Result<u16, Error> {
		let mut sum: u16 = 0;

		// Sum all rolls that haven't been dropped
		for r in self.rolls.iter().filter(|roll| !roll.is_dropped()) {
			sum = sum.checked_add(u16::from(r.val)).ok_or(Error::Overflow)?;
		}

		Ok(sum)
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
	/// use dicey::{dice::{Dice, DieRoll, Rolled}, expr::Describe};
	///
	/// let dice = Dice::builder().count(4).sides(6).keep_high(2).build();
	/// let kh_mod = &dice.modifiers[0];
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
	/// 	dice: &dice,
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
	Overflow,

	/// Rolling the dice specified would result in infinite rolls.
	///
	/// # Examples
	/// ```
	/// use dicey::dice::{Dice, Error};
	///
	/// let dice = Dice::builder().count(4).sides(1).explode(None, true).build();
	/// assert!(matches!(dice.roll(), Err(Error::InfiniteRolls(..))));
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

	/// Finalizes the dice.
	#[must_use]
	pub fn build(self) -> Dice {
		self.0
	}
}
