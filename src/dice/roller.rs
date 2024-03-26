//! Abstractions for rolling [`DieRoll`]s using various means.

use std::iter::Peekable;

use fastrand::Rng;

use super::DieRoll;

/// Trait to reliably roll [`DieRoll`]s
pub trait Roller {
	/// Rolls a single die.
	#[must_use]
	fn roll(&mut self, sides: u8) -> DieRoll;
}

/// Generates rolls with random values using [`fastrand`]. Requires the `fastrand` feature (enabled by default).
///
/// # Examples
///
/// ## Default fastrand roller
/// ```
/// use dicey::dice::{Dice, roller::FastRand};
///
/// let mut roller = FastRand::default();
/// let dice = Dice::new(4, 6);
/// let _ = dice.roll(&mut roller, true)?;
/// let _ = dice.roll(&mut roller, true)?;
/// # Ok::<(), dicey::dice::Error>(())
/// ```
///
/// ## Custom fastrand roller (seeded)
/// ```
/// use dicey::dice::{Dice, roller::FastRand};
/// use fastrand::Rng;
///
/// let mut roller = FastRand::new(Rng::with_seed(0xef6f79ed30ba75a));
/// let dice = Dice::new(4, 6);
/// let _ = dice.roll(&mut roller, true)?;
/// let _ = dice.roll(&mut roller, true)?;
/// # Ok::<(), dicey::dice::Error>(())
/// ```
#[cfg(feature = "fastrand")]
#[derive(Debug, Default, Clone)]
pub struct FastRand(Rng);

#[cfg(feature = "fastrand")]
impl FastRand {
	/// Creates a new [`fastrand`] roller that uses the given RNG instance to generate rolls.
	#[must_use]
	#[inline]
	pub const fn new(rng: Rng) -> Self {
		Self(rng)
	}
}

#[cfg(feature = "fastrand")]
impl Roller for FastRand {
	/// Rolls a single die using the [`fastrand::Rng`] the roller was created with.
	#[inline]
	fn roll(&mut self, sides: u8) -> DieRoll {
		DieRoll::new(self.0.u8(1..=sides))
	}
}

/// Generates rolls that always have a specific value.
///
/// # Examples
/// ```
/// use dicey::dice::{Dice, roller::Val};
///
/// let mut roller = Val(42);
///
/// let dice = Dice::new(4, 6);
/// let rolled = dice.roll(&mut roller, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 42));
///
/// let dice = Dice::new(2, 20);
/// let rolled = dice.roll(&mut roller, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 42));
/// # Ok::<(), dicey::dice::Error>(())
/// ```
#[derive(Debug, Default, Clone)]
#[allow(clippy::exhaustive_structs)]
pub struct Val(pub u8);

impl Roller for Val {
	/// Rolls a single die, always with one specific value.
	#[inline]
	fn roll(&mut self, _sides: u8) -> DieRoll {
		DieRoll::new(self.0)
	}
}

/// Generates rolls that always have their max value.
///
/// # Examples
/// ```
/// use dicey::dice::{Dice, roller::Max};
///
/// let mut roller = Max;
///
/// let dice = Dice::new(4, 6);
/// let rolled = dice.roll(&mut roller, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 6));
///
/// let dice = Dice::new(2, 20);
/// let rolled = dice.roll(&mut roller, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 20));
/// # Ok::<(), dicey::dice::Error>(())
/// ```
#[derive(Debug, Default, Clone)]
#[allow(clippy::exhaustive_structs)]
pub struct Max;

impl Roller for Max {
	/// Rolls a single die, always with the max value (same as the number of sides).
	#[inline]
	fn roll(&mut self, sides: u8) -> DieRoll {
		DieRoll::new(sides)
	}
}

/// Generates rolls from an iterator of values. Mainly useful for testing purposes.
#[derive(Debug, Clone)]
pub struct Iter<I: Iterator<Item = u8>>(Peekable<I>);

impl<I: Iterator<Item = u8>> Iter<I> {
	/// Checks whether the iterator still has values available.
	#[inline]
	pub fn can_roll(&mut self) -> bool {
		self.0.peek().is_some()
	}

	/// Creates a new roller that uses the given iterator to provide roll values.
	#[must_use]
	#[inline]
	pub fn new(iter: impl IntoIterator<IntoIter = I>) -> Self {
		Self(iter.into_iter().peekable())
	}
}

impl<I: Iterator<Item = u8>> Roller for Iter<I> {
	/// Rolls a die with the value from the next iteration.
	///
	/// # Panics
	/// If the iterator has finished, this will panic.
	#[inline]
	#[allow(clippy::expect_used)]
	fn roll(&mut self, _sides: u8) -> DieRoll {
		DieRoll::new(self.0.next().expect("iterator is finished"))
	}
}
