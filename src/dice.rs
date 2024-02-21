use std::{fmt, num::NonZeroU8};

use fastrand::Rng;

use crate::expr::Describe;

/// Representation of a set of dice that can be rolled
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dice {
	/// Number of dice to roll
	pub count: u8,

	/// Number of sides for each die
	pub sides: NonZeroU8,

	/// Whether the dice should explode (if a die rolls its max value, roll another die in addition to it, recursively)
	pub explode: bool,
}

impl Dice {
	/// Rolls the dice using random results and returns data about the rolls
	pub fn roll(&self) -> Result<Roll, Error> {
		let mut rng = Rng::new();

		// Roll the dice!
		let mut rolls = vec![0; self.count as usize];
		for r in rolls.iter_mut() {
			*r = self.rand(&mut rng);
		}

		// Explode the dice if necessary
		let explosions = if self.explode {
			if self.sides.get() == 1 {
				return Err(Error::InfiniteExplosion(self.clone()));
			}
			self.explode(&rolls, &mut rng).or_else(|| Some(Vec::new()))
		} else {
			None
		};

		Ok(Roll {
			rolls,
			explosions,
			dice: self,
		})
	}

	/// Recursively rolls additional dice for any rolls that are the max possible value of the die
	fn explode(&self, rolls: &[u8], rng: &mut Rng) -> Option<Vec<u8>> {
		// Determine how many rolls qualify for explosion
		let to_explode = rolls.iter().filter(|x| **x == self.sides.into()).count();
		if to_explode == 0 {
			return None;
		}

		// Roll additional dice
		let mut explosions = Vec::with_capacity(to_explode);
		for _ in 0..to_explode {
			explosions.push(self.rand(rng));
		}

		// Recurse, rolling further additional dice if necessary
		let further_explosions = self.explode(&explosions, rng);
		if let Some(mut further_explosions) = further_explosions {
			explosions.append(&mut further_explosions);
		}

		Some(explosions)
	}

	/// Generates a random number between 1 and [Self::sides] (inclusive) using a given generator
	fn rand(&self, rng: &mut Rng) -> u8 {
		rng.u8(1..=self.sides.get())
	}

	/// Creates a new set of dice with a given count and number of sides. Panics if the number of sides given is 0.
	pub fn new(count: u8, sides: u8) -> Self {
		Self {
			count,
			sides: NonZeroU8::new(sides).expect("dice sides must be nonzero"),
			explode: false,
		}
	}

	/// Creates a new set of exploding dice with a given count and number of sides
	pub fn new_exploding(count: u8, sides: u8) -> Self {
		Self {
			explode: true,
			..Self::new(count, sides)
		}
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
			if self.explode { "x" } else { "" }
		)
	}
}

/// Representation of the result from rolling a single set of [Dice]
#[derive(Debug, Clone)]
pub struct Roll<'a> {
	/// Each individual die result rolled
	pub rolls: Vec<u8>,

	/// Additional rolls made for any explosions that occurred
	pub explosions: Option<Vec<u8>>,

	/// Dice that were rolled to obtain this
	pub dice: &'a Dice,
}

impl Roll<'_> {
	/// Checks whether the dice were exploding ([Self::explosions] is [Some])
	pub fn is_exploding(&self) -> bool {
		self.explosions.is_some()
	}

	/// Sums all rolls and explosions
	pub fn total(&self) -> Result<u16, Error> {
		let mut sum: u16 = 0;

		// Sum the rolls
		for &r in self.rolls.iter() {
			sum = sum.checked_add(r.into()).ok_or(Error::Overflow)?;
		}

		// Sum the explosions
		if let Some(explosions) = &self.explosions {
			for &x in explosions.iter() {
				sum = sum.checked_add(x.into()).ok_or(Error::Overflow)?;
			}
		}

		Ok(sum)
	}
}

impl Describe for Roll<'_> {
	/// Builds a string of the dice expression the roll is from and all of the individual rolled dice.
	/// If max_rolls is specified and there are more rolls than it, the output will be truncated and appended with
	/// "X more..." (where X is the remaining roll count past the max).
	/// Example output: `3d6[6, 2, 5]`
	fn describe(&self, max_rolls: Option<usize>) -> String {
		let max_rolls = max_rolls.unwrap_or(usize::MAX);
		let total_rolls = self.rolls.len() + self.explosions.as_ref().map(|explosions| explosions.len()).unwrap_or(0);
		let truncated_rolls = total_rolls.saturating_sub(max_rolls);

		format!(
			"{}[{}{}]",
			self.dice,
			self.rolls
				.iter()
				.chain(self.explosions.as_deref().unwrap_or(&[0; 0]))
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

impl fmt::Display for Roll<'_> {
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
}
