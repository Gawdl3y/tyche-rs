//! AST-like data structures for evaluating full mathematical dice expressions and working with their results.

use fastrand::Rng;

use crate::dice::{Dice, Error as DiceError, Rolled};

/// Generates an implementation of [`ExprType`] and [`DescribeBinaryExpr`] for an enum type.
/// This is very tightly coupled with the expected variants (Val, Dice, Neg, Add, Sub, Mul, DivDown, DivUp).
macro_rules! expr_type_impl {
	($name:ty) => {
		impl HasExprType for $name {
			fn expr_type(&self) -> ExprType {
				match self {
					Self::Num(..) | Self::Dice(..) => ExprType::Value,
					Self::Neg(..) => ExprType::Unary,
					Self::Add(..) | Self::Sub(..) => ExprType::Additive,
					Self::Mul(..) | Self::DivDown(..) | Self::DivUp(..) => ExprType::Multiplicative,
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

		impl DescribeBinaryExpr for $name {}
	};
}

/// Individual elements of a full mathematical dice expression
#[derive(Debug, Clone, PartialEq, Eq)]
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

expr_type_impl!(Expr);

impl Expr {
	/// Evaluates the expression. For most types of expressions, this will directly result in a 1:1 equivalent
	/// [`EvaledExpr`], with the notable exception of [`Expr::Dice`]. For dice expressions, the dice they contain are
	/// rolled, resulting in an [`EvaledExpr::Dice`] with the [`Rolled`] set of dice.
	pub fn eval(&self) -> Result<EvaledExpr, Error> {
		Ok(match self {
			Self::Num(x) => EvaledExpr::Num(*x),
			Self::Dice(dice) => EvaledExpr::Dice(dice.roll()?),

			Self::Neg(x) => EvaledExpr::Neg(Box::new(x.eval()?)),

			Self::Add(a, b) => EvaledExpr::Add(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Sub(a, b) => EvaledExpr::Sub(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::Mul(a, b) => EvaledExpr::Mul(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivDown(a, b) => EvaledExpr::DivDown(Box::new(a.eval()?), Box::new(b.eval()?)),
			Self::DivUp(a, b) => EvaledExpr::DivUp(Box::new(a.eval()?), Box::new(b.eval()?)),
		})
	}

	/// Evaluates the expression, passing along a specific Rng to use for any random number generation.
	/// See [`Self::eval()`] for more information.
	pub fn eval_using_rng(&self, rng: &mut Rng) -> Result<EvaledExpr, Error> {
		Ok(match self {
			Self::Dice(dice) => EvaledExpr::Dice(dice.roll_using_rng(rng)?),
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
	#[inline]
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

impl std::fmt::Display for Expr {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output of this implementation is equivalent to [`Self::describe(None)`].
	///
	/// [`Self::describe(None)`]: Self::describe()
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Individual elements of an evaluated mathematical dice expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvaledExpr<'r> {
	/// Standalone integer
	Num(i32),

	/// Rolled dice
	Dice(Rolled<'r>),

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

expr_type_impl!(EvaledExpr<'_>);

impl EvaledExpr<'_> {
	/// Calculates the final result of the evaluated expression and all of its children (if any).
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

impl Describe for EvaledExpr<'_> {
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

impl std::fmt::Display for EvaledExpr<'_> {
	/// Formats the value using the given formatter. [Read more][core::fmt::Debug::fmt()]
	///
	/// The output of this implementation is equivalent to [`Self::describe(None)`].
	///
	/// [`Self::describe(None)`]: Self::describe()
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.describe(None))
	}
}

/// Error resulting from evaluating a [`Expr`] or calculating an [`EvaledExpr`]
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

/// Operation type for an individual expression
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprType {
	/// Single value, no operation
	Value,

	/// Unary operation
	Unary,

	/// Additive operation (sum or difference)
	Additive,

	/// Multiplicative operation (product or quotient)
	Multiplicative,
}

/// Trait that offers [`ExprType`]-related information
pub trait HasExprType {
	/// Gets the type of this expression.
	fn expr_type(&self) -> ExprType;

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
	fn describe(&self, list_limit: Option<usize>) -> String;
}

/// Trait for describing binary expressions with influence from own type.
/// Used for, e.g. wrapping parentheses around parts of expressions based on [`ExprType`] of self and the expression.
trait DescribeBinaryExpr: HasExprType + Describe {
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
			match (self.expr_type(), a.expr_type()) {
				(ExprType::Additive, ExprType::Multiplicative)
				| (ExprType::Multiplicative, ExprType::Additive)
				| (ExprType::Unary, ExprType::Additive)
				| (ExprType::Unary, ExprType::Multiplicative)
				| (ExprType::Unary, ExprType::Unary) => paren_wrap(a.describe(list_limit)),
				_ => a.describe(list_limit),
			},
			op,
			match (self.expr_type(), b.expr_type()) {
				(ExprType::Additive, ExprType::Multiplicative)
				| (ExprType::Multiplicative, ExprType::Additive)
				| (ExprType::Unary, ExprType::Additive)
				| (ExprType::Unary, ExprType::Multiplicative)
				| (ExprType::Unary, ExprType::Unary) => paren_wrap(b.describe(list_limit)),
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
