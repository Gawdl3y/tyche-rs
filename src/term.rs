//! AST-like data structures for evaluating full mathematical dice expressions and working with their results.

use crate::dice::{Dice, Error as DiceError, Rolled};

/// Generates an implementation of [TermType] and [DescribeBinaryTerm] for an enum type.
/// This is very tightly coupled with the expected variants (Val, Dice, Neg, Add, Sub, Mul, DivDown, DivUp).
macro_rules! term_type_impl {
	($name:ty) => {
		impl HasTermType for $name {
			fn term_type(&self) -> TermType {
				match self {
					Self::Num(..) | Self::Dice(..) => TermType::Value,
					Self::Neg(..) => TermType::Unary,
					Self::Add(..) | Self::Sub(..) => TermType::Additive,
					Self::Mul(..) | Self::DivDown(..) | Self::DivUp(..) => TermType::Multiplicative,
				}
			}

			#[must_use]
			fn is_value(&self) -> bool {
				matches!(self, Self::Num(..) | Self::Dice(..))
			}

			#[must_use]
			fn is_unary(&self) -> bool {
				matches!(self, Self::Neg(..))
			}

			#[must_use]
			fn is_additive(&self) -> bool {
				matches!(self, Self::Add(..) | Self::Sub(..))
			}

			#[must_use]
			fn is_multiplicative(&self) -> bool {
				matches!(self, Self::Mul(..) | Self::DivDown(..) | Self::DivUp(..))
			}
		}

		impl DescribeBinaryTerm for $name {}
	};
}

/// Individual terms usable in expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
	/// Standalone integer
	Num(i32),

	/// Dice literal
	Dice(Dice),

	/// Negation of a term (makes it negative)
	Neg(Box<Self>),

	/// Sum of two terms
	Add(Box<Self>, Box<Self>),

	/// Difference of two terms
	Sub(Box<Self>, Box<Self>),

	/// Product of two terms
	Mul(Box<Self>, Box<Self>),

	/// Integer quotient of two terms (rounded down)
	DivDown(Box<Self>, Box<Self>),

	/// Integer quotient of two terms (rounded up)
	DivUp(Box<Self>, Box<Self>),
}

term_type_impl!(Term);

impl Term {
	/// Evaluates the term. For most types of terms, this will directly result in a 1:1 equivalent [`EvaledTerm`],
	/// with the notable exception of [`Term::Dice`]. For dice terms, the dice they contain are rolled, resulting
	/// in an [`EvaledTerm::Dice`] with the [`Rolled`] set of dice.
	pub fn eval(&self) -> Result<EvaledTerm, Error> {
		Ok(match self {
			Self::Num(x) => EvaledTerm::Num(*x),
			Self::Dice(dice) => EvaledTerm::Dice(dice.roll()?),

			Self::Neg(x) => EvaledTerm::Neg(Box::new(x.eval()?)),

			Self::Add(a, b) => EvaledTerm::Add(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Sub(a, b) => EvaledTerm::Sub(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Mul(a, b) => EvaledTerm::Mul(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivDown(a, b) => EvaledTerm::DivDown(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivUp(a, b) => EvaledTerm::DivUp(Box::new(a.eval()?), Box::new(b.eval()?)),
		})
	}

	/// Checks whether the term is deterministic (will always yield the same value with every evaluation).
	/// A [`Term::Num`] will always return `true`, a [`Term::Dice`] will always return `false` unless the dice they
	/// contain only have one side, and all unary and binary terms forward the check to their children.
	#[must_use]
	pub fn is_deterministic(&self) -> bool {
		match self {
			Self::Num(..) => true,
			Self::Dice(dice) => dice.sides == 1,
			Self::Neg(x) => x.is_deterministic(),
			Self::Add(a, b) | Self::Sub(a, b) | Self::Mul(a, b) | Self::DivDown(a, b) | Self::DivUp(a, b) => {
				a.is_deterministic() && b.is_deterministic()
			}
		}
	}
}

impl Describe for Term {
	/// Builds a full usable expression from the terms. Operations are grouped with parentheses whenever the order of
	/// operations could be considered ambiguous, such as when mixing addition and multiplication together. All strings
	/// output from this should result in the exact same Term layout when re-parsing them.
	///
	/// `list_limit` does not affect the output of this implementation in any way since there are no possible lists of
	/// elements included, so it is always safe to pass `None`.
	#[inline]
	fn describe(&self, _list_limit: Option<usize>) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(dice) => dice.to_string(),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.describe(None)),
				_ => format!("-({})", x.describe(None)),
			},

			Self::Add(a, b) => self.describe_binary_term('+', a.as_ref(), b.as_ref(), None),
			Self::Sub(a, b) => self.describe_binary_term('-', a.as_ref(), b.as_ref(), None),
			Self::Mul(a, b) => self.describe_binary_term('*', a.as_ref(), b.as_ref(), None),
			Self::DivDown(a, b) => self.describe_binary_term('/', a.as_ref(), b.as_ref(), None),
			Self::DivUp(a, b) => self.describe_binary_term('\\', a.as_ref(), b.as_ref(), None),
		}
	}
}

impl std::fmt::Display for Term {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output of this implementation is equivalent to [`Term::describe(None)`].
	///
	/// [`Term::describe(None)`]: ./enum.Term.html#method.describe
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Individual evaluated terms from expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvaledTerm<'r> {
	/// Standalone integer
	Num(i32),

	/// Rolled dice
	Dice(Rolled<'r>),

	/// Negation of a term (makes it negative)
	Neg(Box<Self>),

	/// Sum of two terms
	Add(Box<Self>, Box<Self>),

	/// Difference of two terms
	Sub(Box<Self>, Box<Self>),

	/// Product of two terms
	Mul(Box<Self>, Box<Self>),

	/// Integer quotient of two terms (rounded down)
	DivDown(Box<Self>, Box<Self>),

	/// Integer quotient of two terms (rounded up)
	DivUp(Box<Self>, Box<Self>),
}

term_type_impl!(EvaledTerm<'_>);

impl EvaledTerm<'_> {
	/// Calculates the total (sum) of the evaluated term and all of its children (if any).
	pub fn calc(&self) -> Result<i32, Error> {
		match self {
			Self::Num(x) => Ok(*x),
			Self::Dice(roll) => Ok(roll.total()?.into()),

			Self::Neg(x) => Ok(-x.calc()?),

			Self::Add(a, b) => a.calc()?.checked_add(b.calc()?).ok_or(Error::Overflow),
			Self::Sub(a, b) => a.calc()?.checked_sub(b.calc()?).ok_or(Error::Overflow),
			Self::Mul(a, b) => a.calc()?.checked_mul(b.calc()?).ok_or(Error::Overflow),
			Self::DivDown(a, b) => a.calc()?.checked_div(b.calc()?).ok_or(Error::Division),
			Self::DivUp(a, b) => {
				let a_val = a.calc()?;
				let b_val = b.calc()?;
				let result = a_val.checked_div(b_val).ok_or(Error::Division)?;
				let remainder = a_val.checked_rem(b_val).ok_or(Error::Division)?;
				if remainder != 0 {
					Ok(result + 1)
				} else {
					Ok(result)
				}
			}
		}
	}
}

impl Describe for EvaledTerm<'_> {
	fn describe(&self, list_limit: Option<usize>) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(roll) => roll.describe(list_limit),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.describe(list_limit)),
				_ => format!("-({})", x.describe(list_limit)),
			},

			Self::Add(a, b) => self.describe_binary_term('+', a.as_ref(), b.as_ref(), list_limit),
			Self::Sub(a, b) => self.describe_binary_term('-', a.as_ref(), b.as_ref(), list_limit),
			Self::Mul(a, b) => self.describe_binary_term('*', a.as_ref(), b.as_ref(), list_limit),
			Self::DivDown(a, b) => self.describe_binary_term('/', a.as_ref(), b.as_ref(), list_limit),
			Self::DivUp(a, b) => self.describe_binary_term('\\', a.as_ref(), b.as_ref(), list_limit),
		}
	}
}

impl std::fmt::Display for EvaledTerm<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Error resulting from evaluating a [`Term`] or calculating an [`EvaledTerm`]
#[derive(thiserror::Error, Debug)]
pub enum Error {
	/// Dice-related error (likely during rolling)
	#[error("dice error: {0}")]
	Dice(#[from] DiceError),

	/// Integer overflow (likely during calculation of a sum or product)
	#[error("integer overflow")]
	Overflow,

	/// Division-related error (likely division by zero)
	#[error("division error")]
	Division,
}

/// Operation type for an individual term
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TermType {
	/// Single value, no operation
	Value,

	/// Unary operation
	Unary,

	/// Additive operation (sum or difference)
	Additive,

	/// Multiplicative operation (product or quotient)
	Multiplicative,
}

/// Trait that offers [`TermType`]-related information
pub trait HasTermType {
	/// Gets the type of this term.
	fn term_type(&self) -> TermType;

	/// Checks whether this term is a single value.
	fn is_value(&self) -> bool;

	/// Checks whether this term is a unary operation.
	fn is_unary(&self) -> bool;

	/// Checks whether this term is an additive operation.
	fn is_additive(&self) -> bool;

	/// Checks whether this term is a multiplicative operation.
	fn is_multiplicative(&self) -> bool;

	/// Checks whether this term is a binary (additive or multiplicative) operation.
	fn is_binary(&self) -> bool {
		self.is_additive() || self.is_multiplicative()
	}
}

/// Trait to allow creation of expanded descriptions with an optional max number of individual listed results where
/// applicable
pub trait Describe {
	/// Builds a detailed expression string with additional information about non-deterministic elements.
	/// Any elements of the expression that can have a different result between multiple evaluations or multiple results
	/// should list all of the specific individual results that occurred (ideally, up to `list_limit` of them).
	fn describe(&self, list_limit: Option<usize>) -> String;
}

/// Trait for describing binary expressions with influence from own type.
/// Used for, e.g. wrapping parentheses around parts of expressions based on [TermType] of self and the expression.
trait DescribeBinaryTerm: HasTermType + Describe {
	/// Builds a detailed description for a binary expression with parentheses added to disambiguate mixed
	/// additive/multiplicative operations.
	fn describe_binary_term(
		&self,
		op: char,
		a: &impl DescribeBinaryTerm,
		b: &impl DescribeBinaryTerm,
		list_limit: Option<usize>,
	) -> String {
		format!(
			"{} {} {}",
			match (self.term_type(), a.term_type()) {
				(TermType::Additive, TermType::Multiplicative)
				| (TermType::Multiplicative, TermType::Additive)
				| (TermType::Unary, TermType::Additive)
				| (TermType::Unary, TermType::Multiplicative)
				| (TermType::Unary, TermType::Unary) => paren_wrap(a.describe(list_limit)),
				_ => a.describe(list_limit),
			},
			op,
			match (self.term_type(), b.term_type()) {
				(TermType::Additive, TermType::Multiplicative)
				| (TermType::Multiplicative, TermType::Additive)
				| (TermType::Unary, TermType::Additive)
				| (TermType::Unary, TermType::Multiplicative)
				| (TermType::Unary, TermType::Unary) => paren_wrap(b.describe(list_limit)),
				_ => b.describe(list_limit),
			}
		)
	}
}

/// Wraps a string in parentheses.
fn paren_wrap(mut text: String) -> String {
	text.insert(0, '(');
	text.push(')');
	text
}
