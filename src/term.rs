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

			fn is_value(&self) -> bool {
				matches!(self, Self::Num(..) | Self::Dice(..))
			}

			fn is_unary(&self) -> bool {
				matches!(self, Self::Neg(..))
			}

			fn is_additive(&self) -> bool {
				matches!(self, Self::Add(..) | Self::Sub(..))
			}

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
	// Values
	Num(i32),
	Dice(Dice),

	// Unary operators
	Neg(Box<Self>),

	// Binary operators
	Add(Box<Self>, Box<Self>),
	Sub(Box<Self>, Box<Self>),
	Mul(Box<Self>, Box<Self>),
	DivDown(Box<Self>, Box<Self>),
	DivUp(Box<Self>, Box<Self>),
}

term_type_impl!(Term);

impl Term {
	/// Evaluates the term
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

	/// Checks whether the term is deterministic (will always yield the same value)
	pub fn is_deterministic(&self) -> bool {
		match self {
			Self::Num(..) => true,
			Self::Dice(dice) => dice.sides.get() == 1,
			Self::Neg(x) => x.is_deterministic(),
			Self::Add(a, b) | Self::Sub(a, b) | Self::Mul(a, b) | Self::DivDown(a, b) | Self::DivUp(a, b) => {
				a.is_deterministic() && b.is_deterministic()
			}
		}
	}

	/// Builds a full usable expression from the terms
	pub fn expression(&self) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(dice) => dice.to_string(),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.expression()),
				_ => format!("-({})", x.expression()),
			},

			Self::Add(a, b) => self.describe_binary_term('+', a.as_ref(), b.as_ref(), None),
			Self::Sub(a, b) => self.describe_binary_term('-', a.as_ref(), b.as_ref(), None),
			Self::Mul(a, b) => self.describe_binary_term('*', a.as_ref(), b.as_ref(), None),
			Self::DivDown(a, b) => self.describe_binary_term('/', a.as_ref(), b.as_ref(), None),
			Self::DivUp(a, b) => self.describe_binary_term('\\', a.as_ref(), b.as_ref(), None),
		}
	}
}

impl Describe for Term {
	fn describe(&self, _max_rolls: Option<usize>) -> String {
		self.expression()
	}
}

impl std::fmt::Display for Term {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.expression())
	}
}

/// Individual evaluated terms from expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvaledTerm<'a> {
	// Values
	Num(i32),
	Dice(Rolled<'a>),

	// Unary operators
	Neg(Box<Self>),

	// Binary operators
	Add(Box<Self>, Box<Self>),
	Sub(Box<Self>, Box<Self>),
	Mul(Box<Self>, Box<Self>),
	DivDown(Box<Self>, Box<Self>),
	DivUp(Box<Self>, Box<Self>),
}

term_type_impl!(EvaledTerm<'_>);

impl EvaledTerm<'_> {
	/// Calculates the total of the evaluated term
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
	fn describe(&self, max_rolls: Option<usize>) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(roll) => roll.describe(max_rolls),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.describe(max_rolls)),
				_ => format!("-({})", x.describe(max_rolls)),
			},

			Self::Add(a, b) => self.describe_binary_term('+', a.as_ref(), b.as_ref(), max_rolls),
			Self::Sub(a, b) => self.describe_binary_term('-', a.as_ref(), b.as_ref(), max_rolls),
			Self::Mul(a, b) => self.describe_binary_term('*', a.as_ref(), b.as_ref(), max_rolls),
			Self::DivDown(a, b) => self.describe_binary_term('/', a.as_ref(), b.as_ref(), max_rolls),
			Self::DivUp(a, b) => self.describe_binary_term('\\', a.as_ref(), b.as_ref(), max_rolls),
		}
	}
}

impl std::fmt::Display for EvaledTerm<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// An error resulting from a term operation
#[derive(thiserror::Error, Debug)]
pub enum Error {
	#[error("dice error: {0}")]
	Dice(#[from] DiceError),

	#[error("integer overflow")]
	Overflow,

	#[error("division error")]
	Division,
}

/// Operation type for an individual term
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TermType {
	Value,
	Unary,
	Additive,
	Multiplicative,
}

/// Trait for working with [TermType]s
pub trait HasTermType {
	/// Gets the type of this term
	fn term_type(&self) -> TermType;

	/// Checks whether this term is a value
	fn is_value(&self) -> bool;

	/// Checks whether this term is unary
	fn is_unary(&self) -> bool;

	/// Checks whether this term is additive
	fn is_additive(&self) -> bool;

	/// Checks whether this term is multiplicative
	fn is_multiplicative(&self) -> bool;

	/// Checks whether this term is binary (additive or multiplicative)
	fn is_binary(&self) -> bool {
		self.is_additive() || self.is_multiplicative()
	}
}

/// Trait to allow creation of expanded descriptions with an optional max number of rolls where applicable
pub trait Describe {
	/// Produces a detailed expression string with additional information about non-deterministic elements
	fn describe(&self, max_rolls: Option<usize>) -> String;
}

/// Trait for describing binary expressions with influence from own type.
/// Used for, e.g. wrapping parentheses around parts of expressions based on [TermType] of self and the expression.
trait DescribeBinaryTerm: HasTermType + Describe {
	/// Builds a detailed description for a binary expression with parentheses around parts of it if appropriate
	fn describe_binary_term(
		&self,
		op: char,
		a: &impl DescribeBinaryTerm,
		b: &impl DescribeBinaryTerm,
		max_rolls: Option<usize>,
	) -> String {
		format!(
			"{} {} {}",
			match (self.term_type(), a.term_type()) {
				(TermType::Additive, TermType::Multiplicative)
				| (TermType::Multiplicative, TermType::Additive)
				| (TermType::Unary, TermType::Additive)
				| (TermType::Unary, TermType::Multiplicative)
				| (TermType::Unary, TermType::Unary) => paren_wrap(a.describe(max_rolls)),
				_ => a.describe(max_rolls),
			},
			op,
			match (self.term_type(), b.term_type()) {
				(TermType::Additive, TermType::Multiplicative)
				| (TermType::Multiplicative, TermType::Additive)
				| (TermType::Unary, TermType::Additive)
				| (TermType::Unary, TermType::Multiplicative)
				| (TermType::Unary, TermType::Unary) => paren_wrap(b.describe(max_rolls)),
				_ => b.describe(max_rolls),
			}
		)
	}
}

/// Wraps a string in parentheses
fn paren_wrap(mut text: String) -> String {
	text.insert(0, '(');
	text.push(')');
	text
}
