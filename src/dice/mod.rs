//! All functionality for directly creating dice, rolling them, and working with their resulting rolls.
//!
//! This is the home of the dice "primitives". For using as part of a larger expression, see [`Expr::dice`].
//!
//! [`Expr::dice`]: crate::expr::Expr::Dice

pub mod modifier;
pub mod roller;

use alloc::{
	borrow::Cow,
	format,
	string::{String, ToString},
	vec::Vec,
};
use core::{cmp, fmt};

use self::modifier::Condition;
pub use self::{modifier::Modifier, roller::Roller};
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
	/// use tyche::dice::DieRoll;
	///
	/// let roll = DieRoll::new(4);
	/// assert_eq!(roll.to_string(), "4");
	/// ```
	///
	/// ```
	/// use tyche::dice::{DieRoll, Modifier};
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
	/// use tyche::dice::{roller::{FastRand as FastRandRoller, Roller}, Dice};
	///
	/// let dice = Dice::new(4, 8);
	/// let rolled = FastRandRoller::default().roll(&dice, true)?;
	/// let total = rolled.total()?;
	/// assert_eq!(total, rolled.rolls.iter().map(|roll| roll.val as u16).sum());
	/// # Ok::<(), tyche::dice::Error>(())
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
	pub fn from_dice_and_rolls(dice: &Dice, rolls: impl IntoIterator<Item = u8>) -> Rolled {
		Rolled {
			rolls: rolls.into_iter().map(DieRoll::new).collect(),
			dice: Cow::Borrowed(dice),
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
	/// use tyche::{dice::{Dice, DieRoll, Rolled}, expr::Describe};
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
	/// use tyche::dice::{roller::{FastRand as FastRandRoller, Roller}, Dice, Error};
	///
	/// let dice = Dice::builder().count(4).sides(1).explode(None, true).build();
	/// assert!(matches!(FastRandRoller::default().roll(&dice, true), Err(Error::InfiniteRolls(..))));
	/// ```
	#[error("{0} would result in infinite rolls")]
	InfiniteRolls(Dice),

	/// The provided symbol doesn't match to a known condition.
	///
	/// # Examples
	/// ```
	/// use tyche::dice::{modifier::Condition, Error};
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
/// use tyche::Dice;
///
/// let dice = Dice::builder().count(2).sides(6).build();
/// assert_eq!(dice, Dice::new(2, 6));
/// ```
///
/// ## Single modifier
/// ```
/// use tyche::dice::{Dice, Modifier};
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
/// use tyche::dice::{modifier::{Condition, Modifier}, Dice};
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
