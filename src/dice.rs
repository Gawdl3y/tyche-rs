use std::{cmp, fmt, num::NonZeroU8};

use fastrand::Rng;

use crate::term::Describe;

/// Representation of a set of dice that can be rolled
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dice {
	/// Number of dice to roll
	pub count: u8,

	/// Number of sides for each die
	pub sides: NonZeroU8,

	/// Modifiers to apply to rolls from this set of dice
	pub modifiers: Vec<Modifier>,
}

impl Dice {
	/// Rolls the dice using the default Rng
	pub fn roll(&self) -> Result<Rolled, Error> {
		self.roll_with_rng(&mut Rng::new())
	}

	/// Rolls the dice with the given Rng
	pub fn roll_with_rng(&self, rng: &mut Rng) -> Result<Rolled, Error> {
		// Roll the dice!
		let mut rolls = Vec::with_capacity(self.count as usize);
		for _ in 0..self.count {
			rolls.push(self.roll_single_with_rng(rng));
		}

		// Apply all modifiers
		let mut rolls = Rolled { rolls, dice: self };
		for modifier in self.modifiers.iter() {
			modifier.apply_with_rng(&mut rolls, rng)?;
		}

		Ok(rolls)
	}

	/// Rolls a single die of this type with no modifiers using the default Rng
	pub fn roll_single(&self) -> DieRoll {
		DieRoll::new_rand(self.sides.get())
	}

	/// Rolls a single die of this type with no modifiers using the given Rng
	pub fn roll_single_with_rng(&self, rng: &mut Rng) -> DieRoll {
		DieRoll::new_rand_with_rng(self.sides.get(), rng)
	}

	/// Creates a new set of dice with a given count and number of sides. Panics if the number of sides given is 0.
	pub fn new(count: u8, sides: u8) -> Self {
		Self {
			count,
			sides: NonZeroU8::new(sides).expect("dice sides must be nonzero"),
			modifiers: Vec::new(),
		}
	}

	/// Creates a new dice builder
	pub fn builder() -> Builder {
		Builder::default()
	}
}

impl Default for Dice {
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
			self.modifiers
				.iter()
				.map(|m| m.to_string())
				.collect::<Vec<_>>()
				.join("")
		)
	}
}

/// A modifier that can be applied to a set of [Dice] to manipulate resulting [Rolls] from them
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier {
	/// Exploding dice - automatically rolls additional dice for any that meet a given condition
	/// (default being equal to the number of sides for the dice).
	/// If the second parameter is `true`, this is done recursively for any additional rolls that also meet the condition.
	Explode(Option<Condition>, bool),

	/// Keep the highest x dice, dropping the rest
	KeepHigh(NonZeroU8),

	/// Keep the lowest x dice, dropping the rest
	KeepLow(NonZeroU8),
}

impl Modifier {
	/// Applies the modifier to a set of rolls using the default Rng where needed
	pub fn apply<'rolled, 'modifier: 'rolled>(&'modifier self, rolls: &mut Rolled<'rolled>) -> Result<(), Error> {
		self.apply_with_rng(rolls, &mut Rng::new())
	}

	/// Applies the modifier to a set of rolls using a given Rng where needed
	pub fn apply_with_rng<'rolled, 'modifier: 'rolled>(
		&'modifier self,
		rolled: &mut Rolled<'rolled>,
		rng: &mut Rng,
	) -> Result<(), Error> {
		match self {
			Self::Explode(cond, recurse) => {
				// Don't allow recursively exploding dice with 1 side since that would result in infinite explosions
				if *recurse && rolled.dice.sides.get() == 1 {
					return Err(Error::InfiniteExplosion(rolled.dice.clone()));
				}

				// Determine how many initial rolls qualify for explosion
				let mut to_explode = rolled
					.rolls
					.iter()
					.filter(|r| !r.is_dropped())
					.filter(|r| match cond {
						Some(cond) => cond.check(r.val),
						None => r.val == rolled.dice.sides,
					})
					.count();

				loop {
					// Roll additional dice
					let mut explosions = Vec::with_capacity(to_explode);
					for _ in 0..to_explode {
						let mut roll = rolled.dice.roll_single_with_rng(rng);
						roll.added_by = Some(self);
						explosions.push(roll);
					}

					// Determine how many additional rolls qualify for explosion, then add the explosions to the rolls
					to_explode = recurse
						.then(|| {
							explosions
								.iter()
								.filter(|r| match cond {
									Some(cond) => cond.check(r.val),
									None => r.val == rolled.dice.sides,
								})
								.count()
						})
						.unwrap_or(0);
					rolled.rolls.append(&mut explosions);

					if to_explode == 0 {
						break;
					}
				}
			}

			Self::KeepHigh(count) => {
				let mut refs = rolled.rolls.iter_mut().filter(|r| !r.is_dropped()).collect::<Vec<_>>();
				refs.sort();
				refs.reverse();
				refs.iter_mut()
					.skip(count.get() as usize)
					.for_each(|roll| roll.dropped_by = Some(self));
			}

			Self::KeepLow(count) => {
				let mut refs = rolled.rolls.iter_mut().filter(|r| !r.is_dropped()).collect::<Vec<_>>();
				refs.sort();
				refs.iter_mut()
					.skip(count.get() as usize)
					.for_each(|roll| roll.dropped_by = Some(self));
			}
		};

		Ok(())
	}
}

impl fmt::Display for Modifier {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}",
			match self {
				Self::Explode(_, recurse) => format!("x{}", recurse.then_some("").unwrap_or("o")),
				Self::KeepHigh(count) => format!(
					"kh{}",
					if count.get() > 1 {
						count.to_string()
					} else {
						"".to_owned()
					}
				),
				Self::KeepLow(count) => format!(
					"kl{}",
					if count.get() > 1 {
						count.to_string()
					} else {
						"".to_owned()
					}
				),
			},
			match self {
				Self::Explode(Some(cond), _) => cond.to_string(),
				Self::Explode(None, _) | Self::KeepHigh(..) | Self::KeepLow(..) => "".to_owned(),
			}
		)
	}
}

/// Conditions that die values can be tested against
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
	Eq(NonZeroU8),
	Gt(NonZeroU8),
	Gte(NonZeroU8),
	Lt(NonZeroU8),
	Lte(NonZeroU8),
}

impl Condition {
	/// Creates a Condition from its corresponding symbol and a value
	pub fn from_symbol_and_val(symbol: &str, val: NonZeroU8) -> Result<Self, Error> {
		Ok(match symbol {
			"=" => Self::Eq(val),
			">" => Self::Gt(val),
			">=" => Self::Gte(val),
			"<" => Self::Lt(val),
			"<=" => Self::Lte(val),
			_ => return Err(Error::UnknownCondition(symbol.to_owned())),
		})
	}

	/// Checks a value against the condition
	pub fn check(&self, val: NonZeroU8) -> bool {
		match self {
			Self::Eq(expected) => val == *expected,
			Self::Gt(expected) => val > *expected,
			Self::Gte(expected) => val >= *expected,
			Self::Lt(expected) => val < *expected,
			Self::Lte(expected) => val <= *expected,
		}
	}

	/// Gets the symbol that represents this condition
	pub fn symbol(&self) -> &'static str {
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

/// Represents a single rolled die
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DieRoll<'a> {
	/// Value that was rolled
	pub val: NonZeroU8,

	/// Modifier that caused the addition of this die
	pub added_by: Option<&'a Modifier>,

	/// Modifier that caused the drop of this die
	pub dropped_by: Option<&'a Modifier>,
}

impl DieRoll<'_> {
	/// Checks whether this die roll was part of the original set, not from a modifier
	pub fn is_original(&self) -> bool {
		self.added_by.is_none()
	}

	/// Checks whether this die roll has been dropped by a modifier
	pub fn is_dropped(&self) -> bool {
		self.dropped_by.is_some()
	}

	/// Creates a new DieRoll with the given value
	pub fn new(val: u8) -> Self {
		Self {
			val: NonZeroU8::new(val).expect("roll val must be nonzero"),
			added_by: None,
			dropped_by: None,
		}
	}

	/// Creates a new DieRoll with a random value using the default Rng
	pub fn new_rand(max: u8) -> Self {
		let mut rng = Rng::new();
		Self::new_rand_with_rng(rng.u8(1..=max), &mut rng)
	}

	/// Creates a new DieRoll with a random value using the given Rng
	pub fn new_rand_with_rng(max: u8, rng: &mut Rng) -> Self {
		Self::new(rng.u8(1..=max))
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
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", self.val, if self.is_dropped() { " (d)" } else { "" })
	}
}

/// Representation of the result from rolling a single set of [Dice]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rolled<'a> {
	/// Each individual die roll
	pub rolls: Vec<DieRoll<'a>>,

	/// Dice that were rolled to obtain this
	pub dice: &'a Dice,
}

impl Rolled<'_> {
	/// Sums all rolls and explosions
	pub fn total(&self) -> Result<u16, Error> {
		let mut sum: u16 = 0;

		// Sum all rolls that haven't been dropped
		for r in self.rolls.iter().filter(|r| !r.is_dropped()) {
			sum = sum.checked_add(r.val.get().into()).ok_or(Error::Overflow)?;
		}

		Ok(sum)
	}
}

impl Describe for Rolled<'_> {
	/// Builds a string of the dice expression the roll is from and all of the individual rolled dice.
	/// If max_rolls is specified and there are more rolls than it, the output will be truncated and appended with
	/// "X more..." (where X is the remaining roll count past the max).
	/// Example output: `3d6[6, 2, 5]`
	fn describe(&self, max_rolls: Option<usize>) -> String {
		let max_rolls = max_rolls.unwrap_or(usize::MAX);
		let total_rolls = self.rolls.len();
		let truncated_rolls = total_rolls.saturating_sub(max_rolls);

		format!(
			"{}[{}{}]",
			self.dice,
			self.rolls
				.iter()
				.take(max_rolls)
				.map(|r| r.to_string())
				.collect::<Vec<String>>()
				.join(", "),
			if truncated_rolls > 0 {
				format!(", {} more...", truncated_rolls)
			} else {
				"".to_owned()
			}
		)
	}
}

impl fmt::Display for Rolled<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// An error resulting from a dice operation
#[derive(thiserror::Error, Debug)]
pub enum Error {
	#[error("integer overflow")]
	Overflow,

	#[error("{0} would result in infinite explosions")]
	InfiniteExplosion(Dice),

	#[error("unknown condition symbol: {0}")]
	UnknownCondition(String),
}

/// Builds [Dice] with a fluent interface
#[derive(Debug, Clone, Default)]
pub struct Builder(Dice);

impl Builder {
	/// Sets the number of dice to roll
	pub fn count(mut self, count: u8) -> Self {
		self.0.count = count;
		self
	}

	/// Sets the number of sides per die
	pub fn sides(mut self, sides: NonZeroU8) -> Self {
		self.0.sides = sides;
		self
	}

	/// Adds the exploding modifier to the dice
	pub fn explode(mut self, cond: Option<Condition>, recurse: bool) -> Self {
		self.0.modifiers.push(Modifier::Explode(cond, recurse));
		self
	}

	/// Adds the keep highest modifier to the dice
	pub fn keep_high(mut self, count: NonZeroU8) -> Self {
		self.0.modifiers.push(Modifier::KeepHigh(count));
		self
	}

	/// Adds the keep lowest modifier to the dice
	pub fn keep_low(mut self, count: NonZeroU8) -> Self {
		self.0.modifiers.push(Modifier::KeepLow(count));
		self
	}

	/// Finalizes the dice
	pub fn build(self) -> Dice {
		self.0
	}
}
