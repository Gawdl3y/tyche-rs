//! Abstractions for rolling [`DieRoll`]s using various means.

use alloc::{borrow::Cow, vec::Vec};
use core::iter::Peekable;

#[cfg(feature = "fastrand")]
use fastrand::Rng;

use super::{Dice, DieRoll, Error, Rolled};

/// Rolls dice - what else is there to say?
pub trait Roller {
	/// Rolls a single die.
	#[must_use]
	fn roll_die(&mut self, sides: u8) -> DieRoll;

	/// Rolls a set of dice and optionally applies all of its modifiers to the rolls.
	///
	/// # Errors
	/// If any errors are encountered while applying the dice's modifiers, an error variant is returned.
	fn roll<'d, 'r>(&mut self, dice: &'d Dice, apply_mods: bool) -> Result<Rolled<'r>, Error>
	where
		'd: 'r,
		Self: Sized,
	{
		// Roll the dice!
		let mut rolls = Vec::with_capacity(dice.count as usize);
		for _ in 0..dice.count {
			rolls.push(self.roll_die(dice.sides));
		}

		let mut rolled = Rolled {
			rolls,
			dice: Cow::Borrowed(dice),
		};

		// Apply all of the dice's modifiers
		if apply_mods {
			for modifier in &dice.modifiers {
				modifier.apply(&mut rolled, self)?;
			}
		}

		Ok(rolled)
	}
}

/// Generates rolls with random values using [fastrand]. Requires the `fastrand` feature (enabled by default).
///
/// # Examples
///
/// ## Default fastrand roller
/// ```
/// use tyche::dice::{roller::{FastRand as FastRandRoller, Roller}, Dice};
///
/// let mut roller = FastRandRoller::default();
///
/// let dice = Dice::new(4, 6);
/// let _ = roller.roll(&dice, true)?;
/// let _ = roller.roll(&dice, true)?;
/// # Ok::<(), tyche::dice::Error>(())
/// ```
///
/// ## Manually seeded fastrand roller
/// ```
/// use tyche::dice::{roller::{FastRand as FastRandRoller, Roller}, Dice};
///
/// let mut roller = FastRandRoller::with_seed(0x750c38d574400);
///
/// let dice = Dice::new(4, 6);
/// let _ = roller.roll(&dice, true)?;
/// let _ = roller.roll(&dice, true)?;
/// # Ok::<(), tyche::dice::Error>(())
/// ```
///
/// ## Custom fastrand roller
/// ```
/// use tyche::dice::{roller::{FastRand as FastRandRoller, Roller}, Dice};
/// use fastrand::Rng;
///
/// let rng = Rng::with_seed(0x750c38d574400);
/// let mut roller = FastRandRoller::new(rng);
///
/// let dice = Dice::new(4, 6);
/// let _ = roller.roll(&dice, true)?;
/// let _ = roller.roll(&dice, true)?;
/// # Ok::<(), tyche::dice::Error>(())
/// ```
#[cfg(feature = "fastrand")]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "std", derive(Default))]
pub struct FastRand(Rng);

#[cfg(feature = "fastrand")]
impl FastRand {
	/// Creates a new fastrand roller that uses the given RNG instance to generate rolls.
	#[must_use]
	#[inline]
	pub const fn new(rng: Rng) -> Self {
		Self(rng)
	}

	/// Creates a new fastrand roller that uses a pre-seeded RNG instance to generate rolls.
	#[must_use]
	#[inline]
	pub fn with_seed(seed: u64) -> Self {
		Self(Rng::with_seed(seed))
	}
}

#[cfg(feature = "fastrand")]
impl Roller for FastRand {
	/// Rolls a single die using the [`fastrand::Rng`] the roller was created with.
	#[inline]
	fn roll_die(&mut self, sides: u8) -> DieRoll {
		if sides > 0 {
			DieRoll::new(self.0.u8(1..=sides))
		} else {
			DieRoll::new(0)
		}
	}
}

/// Generates rolls that always have a specific value.
///
/// # Examples
/// ```
/// use tyche::dice::{roller::{Roller, Val as ValRoller}, Dice};
///
/// let mut roller = ValRoller(42);
///
/// let dice = Dice::new(4, 6);
/// let rolled = roller.roll(&dice, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 42));
///
/// let dice = Dice::new(2, 20);
/// let rolled = roller.roll(&dice, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 42));
/// # Ok::<(), tyche::dice::Error>(())
/// ```
#[derive(Debug, Default, Clone)]
#[expect(clippy::exhaustive_structs, reason = "Highly unlikely to change")]
pub struct Val(pub u8);

impl Roller for Val {
	/// Rolls a single die, always with one specific value.
	#[inline]
	fn roll_die(&mut self, _sides: u8) -> DieRoll {
		DieRoll::new(self.0)
	}
}

/// Generates rolls that always have their max value.
///
/// # Examples
/// ```
/// use tyche::dice::{roller::{Max as MaxRoller, Roller}, Dice};
///
/// let mut roller = MaxRoller;
///
/// let dice = Dice::new(4, 6);
/// let rolled = roller.roll(&dice, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 6));
///
/// let dice = Dice::new(2, 20);
/// let rolled = roller.roll(&dice, true)?;
/// assert!(rolled.rolls.iter().all(|roll| roll.val == 20));
/// # Ok::<(), tyche::dice::Error>(())
/// ```
#[derive(Debug, Default, Clone)]
#[expect(clippy::exhaustive_structs, reason = "Highly unlikely to change")]
pub struct Max;

impl Roller for Max {
	/// Rolls a single die, always with the max value (same as the number of sides).
	#[inline]
	fn roll_die(&mut self, sides: u8) -> DieRoll {
		DieRoll::new(sides)
	}
}

/// Generates rolls from an iterator of values. Mainly useful for testing purposes.
///
/// # Examples
/// ```
/// use tyche::dice::{roller::{Iter as IterRoller, Roller}, Dice, DieRoll};
///
/// let mut roller = IterRoller::new(vec![1, 2, 3, 4, 10]);
/// let dice = Dice::new(5, 6);
/// assert_eq!(
/// 	roller.roll(&dice, true)?.rolls,
/// 	vec![DieRoll::new(1), DieRoll::new(2), DieRoll::new(3), DieRoll::new(4), DieRoll::new(10)]
/// );
/// # Ok::<(), tyche::dice::Error>(())
/// ```
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
	#[expect(
		clippy::expect_used,
		reason = "Mostly for testing, otherwise manual checking of can_roll() is expected"
	)]
	fn roll_die(&mut self, _sides: u8) -> DieRoll {
		DieRoll::new(self.0.next().expect("iterator is finished"))
	}
}
