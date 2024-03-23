//! AST-like data structures for evaluating full mathematical dice expressions and working with their results.

use std::fmt;

use fastrand::Rng;

use crate::dice::{Dice, Error as DiceError, Rolled};

/// Generates an implementation of [`HasOpType`] and [`DescribeBinaryExpr`] for an enum type.
/// This is very tightly coupled with the expected variants:
/// `Val`, `Dice`, `Neg`, `Add`, `Sub`, `Mul`, `DivDown`, and `DivUp`.
macro_rules! op_type_impl {
	($name:ty) => {
		impl HasOpType for $name {
			fn op_type(&self) -> OpType {
				match self {
					Self::Num(..) | Self::Dice(..) => OpType::Value,
					Self::Neg(..) => OpType::Unary,
					Self::Add(..) | Self::Sub(..) => OpType::Additive,
					Self::Mul(..) | Self::DivDown(..) | Self::DivUp(..) => OpType::Multiplicative,
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
	};
}

/// Individual elements of a full mathematical dice expression
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Expr {
	/// Standalone integer
	Num(i32),

	/// Dice literal
	Dice(Dice),

	/// Negation of an expression (makes the result of it negative)
	Neg(Box<Self>),

	/// Sum of two expressions
	Add(Box<Self>, Box<Self>),

	/// Difference of two expressions
	Sub(Box<Self>, Box<Self>),

	/// Product of two expressions
	Mul(Box<Self>, Box<Self>),

	/// Integer quotient of two expressions (rounded down)
	DivDown(Box<Self>, Box<Self>),

	/// Integer quotient of two expressions (rounded up)
	DivUp(Box<Self>, Box<Self>),
}

op_type_impl!(Expr);

impl Expr {
	/// Evaluates the expression. For most types of expressions, this will directly result in a 1:1 equivalent
	/// [`Evaled`], with the notable exception of [`Expr::Dice`]. For dice expressions, the dice they contain are
	/// rolled, resulting in an [`Evaled::Dice`] with the [`Rolled`] set of dice.
	///
	/// # Errors
	/// If there is an integer overflow or division error encountered during any operations, or if an error occurs
	/// during dice rolling, an error variant will be returned.
	pub fn eval(&self) -> Result<Evaled, EvalError> {
		Ok(match self {
			Self::Num(x) => Evaled::Num(*x),
			Self::Dice(dice) => Evaled::Dice(dice.roll().map_err(|err| EvalError::Dice(self.clone(), err))?),

			Self::Neg(x) => Evaled::Neg(Box::new(x.eval()?)),

			Self::Add(a, b) => Evaled::Add(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Sub(a, b) => Evaled::Sub(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Mul(a, b) => Evaled::Mul(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivDown(a, b) => Evaled::DivDown(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivUp(a, b) => Evaled::DivUp(Box::new(a.eval()?), Box::new(b.eval()?)),
		})
	}

	/// Evaluates the expression, passing along a specific Rng to use for any random number generation.
	/// See [`Self::eval()`] for more information.
	#[allow(clippy::missing_errors_doc)]
	pub fn eval_using_rng(&self, rng: &mut Rng) -> Result<Evaled, EvalError> {
		Ok(match self {
			Self::Dice(dice) => Evaled::Dice(
				dice.roll_using_rng(rng)
					.map_err(|err| EvalError::Dice(self.clone(), err))?,
			),
			_ => self.eval()?,
		})
	}

	/// Checks whether the expression is deterministic (will always yield the same value with every evaluation).
	/// A [`Self::Num`] will always return `true`, a [`Self::Dice`] will always return `false` unless the dice they
	/// contain only have one side, and all unary and binary expressions forward the check to their children.
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

impl Describe for Expr {
	/// Builds a full usable expression string from the expressions. Operations are grouped with parentheses whenever
	/// the order of operations could be considered ambiguous, such as when mixing addition and multiplication together.
	/// All strings output from this should result in the exact same expression layout when re-parsing them.
	///
	/// `list_limit` does not affect the output of this implementation in any way since there are no possible lists of
	/// elements included, so it is always safe to pass `None`.
	fn describe(&self, _list_limit: Option<usize>) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(dice) => dice.to_string(),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.describe(None)),
				_ => format!("-({})", x.describe(None)),
			},

			Self::Add(a, b) => self.describe_binary_expr('+', a.as_ref(), b.as_ref(), None),
			Self::Sub(a, b) => self.describe_binary_expr('-', a.as_ref(), b.as_ref(), None),
			Self::Mul(a, b) => self.describe_binary_expr('*', a.as_ref(), b.as_ref(), None),
			Self::DivDown(a, b) => self.describe_binary_expr('/', a.as_ref(), b.as_ref(), None),
			Self::DivUp(a, b) => self.describe_binary_expr('\\', a.as_ref(), b.as_ref(), None),
		}
	}
}

impl fmt::Display for Expr {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output of this implementation is equivalent to [`Self::describe(None)`].
	///
	/// [`Self::describe(None)`]: Self::describe()
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Individual elements of an evaluated mathematical dice expression
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Evaled<'a> {
	/// Standalone integer
	Num(i32),

	/// Rolled dice
	Dice(Rolled<'a>),

	/// Negation of an expression (makes the result of it negative)
	Neg(Box<Self>),

	/// Sum of two expressions
	Add(Box<Self>, Box<Self>),

	/// Difference of two expressions
	Sub(Box<Self>, Box<Self>),

	/// Product of two expressions
	Mul(Box<Self>, Box<Self>),

	/// Integer quotient of two expressions (rounded down)
	DivDown(Box<Self>, Box<Self>),

	/// Integer quotient of two expressions (rounded up)
	DivUp(Box<Self>, Box<Self>),
}

op_type_impl!(Evaled<'_>);

impl Evaled<'_> {
	/// Calculates the final result of the evaluated expression and all of its children (if any).
	///
	/// # Errors
	/// If there is an integer overflow or division error, or an error calculating the total of a set of dice rolls,an
	/// error variant will be returned.
	pub fn calc(&self) -> Result<i32, CalcError> {
		match self {
			Self::Num(x) => Ok(*x),
			Self::Dice(rolled) => Ok(rolled
				.total()
				.map_err(|err| CalcError::Dice(self.clone().into_owned(), err))?
				.into()),

			Self::Neg(x) => Ok(x
				.calc()?
				.checked_neg()
				.ok_or_else(|| CalcError::Overflow(self.clone().into_owned()))?),

			Self::Add(a, b) => a
				.calc()?
				.checked_add(b.calc()?)
				.ok_or_else(|| CalcError::Overflow(self.clone().into_owned())),
			Self::Sub(a, b) => a
				.calc()?
				.checked_sub(b.calc()?)
				.ok_or_else(|| CalcError::Overflow(self.clone().into_owned())),
			Self::Mul(a, b) => a
				.calc()?
				.checked_mul(b.calc()?)
				.ok_or_else(|| CalcError::Overflow(self.clone().into_owned())),
			Self::DivDown(a, b) => a
				.calc()?
				.checked_div(b.calc()?)
				.ok_or_else(|| CalcError::Division(self.clone().into_owned())),
			Self::DivUp(a, b) => {
				let a_val = a.calc()?;
				let b_val = b.calc()?;
				let result = a_val
					.checked_div(b_val)
					.ok_or_else(|| CalcError::Division(self.clone().into_owned()))?;
				let remainder = a_val
					.checked_rem(b_val)
					.ok_or_else(|| CalcError::Division(self.clone().into_owned()))?;
				if remainder != 0 {
					Ok(result
						.checked_add(1)
						.ok_or_else(|| CalcError::Overflow(self.clone().into_owned()))?)
				} else {
					Ok(result)
				}
			}
		}
	}

	/// Moves all of self's owned data into a new instance and clones any unowned data in order to create a `'static`
	/// instance of self.
	#[must_use]
	pub fn into_owned(self) -> Evaled<'static> {
		match self {
			Self::Num(x) => Evaled::Num(x),
			Self::Dice(rolled) => Evaled::Dice(rolled.into_owned()),
			Self::Neg(x) => Evaled::Neg(Box::new(x.into_owned())),
			Self::Add(a, b) => Evaled::Add(Box::new(a.into_owned()), Box::new(b.into_owned())),
			Self::Sub(a, b) => Evaled::Sub(Box::new(a.into_owned()), Box::new(b.into_owned())),
			Self::Mul(a, b) => Evaled::Mul(Box::new(a.into_owned()), Box::new(b.into_owned())),
			Self::DivDown(a, b) => Evaled::DivDown(Box::new(a.into_owned()), Box::new(b.into_owned())),
			Self::DivUp(a, b) => Evaled::DivUp(Box::new(a.into_owned()), Box::new(b.into_owned())),
		}
	}
}

impl Describe for Evaled<'_> {
	fn describe(&self, list_limit: Option<usize>) -> String {
		match self {
			Self::Num(x) => x.to_string(),
			Self::Dice(roll) => roll.describe(list_limit),

			Self::Neg(x) => match x.as_ref() {
				Self::Num(..) | Self::Dice(..) => format!("-{}", x.describe(list_limit)),
				_ => format!("-({})", x.describe(list_limit)),
			},

			Self::Add(a, b) => self.describe_binary_expr('+', a.as_ref(), b.as_ref(), list_limit),
			Self::Sub(a, b) => self.describe_binary_expr('-', a.as_ref(), b.as_ref(), list_limit),
			Self::Mul(a, b) => self.describe_binary_expr('*', a.as_ref(), b.as_ref(), list_limit),
			Self::DivDown(a, b) => self.describe_binary_expr('/', a.as_ref(), b.as_ref(), list_limit),
			Self::DivUp(a, b) => self.describe_binary_expr('\\', a.as_ref(), b.as_ref(), list_limit),
		}
	}
}

impl fmt::Display for Evaled<'_> {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output of this implementation is equivalent to [`Self::describe(None)`].
	///
	/// [`Self::describe(None)`]: Self::describe()
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Error that can occur during [`Expr::eval()`]
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum EvalError {
	/// Dice-related error (likely during rolling)
	#[error("dice error while evaluating \"{0}\": {1}")]
	Dice(Expr, #[source] DiceError),
}

/// Error that can occur during [`Evaled::calc()`]
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum CalcError {
	/// Dice-related error (likely during totalling)
	#[error("dice error while calculating ({0}): {1}")]
	Dice(Evaled<'static>, #[source] DiceError),

	/// Integer overflow (likely during calculation of a sum or product)
	#[error("integer overflow while calculating {0}")]
	Overflow(Evaled<'static>),

	/// Division-related error (likely division by zero)
	#[error("division error while calculating {0}")]
	Division(Evaled<'static>),
}

/// Operation type for an individual expression
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::exhaustive_enums)]
pub enum OpType {
	/// Single value, no operation
	Value,

	/// Unary operation
	Unary,

	/// Additive operation (sum or difference)
	Additive,

	/// Multiplicative operation (product or quotient)
	Multiplicative,
}

/// Trait that offers [`OpType`]-related information
pub trait HasOpType {
	/// Gets the type of this expression.
	fn op_type(&self) -> OpType;

	/// Checks whether this expression is a single value.
	fn is_value(&self) -> bool;

	/// Checks whether this expression is a unary operation.
	fn is_unary(&self) -> bool;

	/// Checks whether this expression is an additive operation.
	fn is_additive(&self) -> bool;

	/// Checks whether this expression is a multiplicative operation.
	fn is_multiplicative(&self) -> bool;

	/// Checks whether this expression is a binary (additive or multiplicative) operation.
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
	#[must_use]
	fn describe(&self, list_limit: Option<usize>) -> String;
}

/// Trait for describing binary expressions with influence from own type.
/// Used for, e.g. wrapping parentheses around parts of expressions based on [`OpType`] of self and the expression.
trait DescribeBinaryExpr: HasOpType + Describe {
	/// Builds a detailed description for a binary expression with parentheses added to disambiguate mixed
	/// additive/multiplicative operations.
	fn describe_binary_expr(
		&self,
		op: char,
		a: &impl DescribeBinaryExpr,
		b: &impl DescribeBinaryExpr,
		list_limit: Option<usize>,
	) -> String {
		format!(
			"{} {} {}",
			match (self.op_type(), a.op_type()) {
				(OpType::Additive | OpType::Unary, OpType::Multiplicative)
				| (OpType::Multiplicative | OpType::Unary, OpType::Additive)
				| (OpType::Unary, OpType::Unary) => paren_wrap(a.describe(list_limit)),
				_ => a.describe(list_limit),
			},
			op,
			match (self.op_type(), b.op_type()) {
				(OpType::Additive | OpType::Unary, OpType::Multiplicative)
				| (OpType::Multiplicative | OpType::Unary, OpType::Additive)
				| (OpType::Unary, OpType::Unary) => paren_wrap(b.describe(list_limit)),
				_ => b.describe(list_limit),
			}
		)
	}
}

impl<T: HasOpType + Describe> DescribeBinaryExpr for T {}

/// Wraps a string in parentheses.
#[must_use]
fn paren_wrap(mut text: String) -> String {
	text.insert(0, '(');
	text.push(')');
	text
}
