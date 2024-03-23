#![cfg(feature = "parse")]

//! Parser generator functions and implementations of [`str::FromStr`] for all dice and expression data structures.
//! Requires the `parse` feature (enabled by default).
//!
//! The parser generators generate parsers for parsing dice, dice modifiers, modifier conditions, and full mathematical
//! dice expressions (such as `4d8 + 2d6x - 3`) from strings. They're all made with [chumsky] and are almost entirely
//! zero-copy. A parser can be used directly by calling [`Parser::parse()`] on it.
//!
//! # Examples
//!
//! ## Parsing Dice
//! ```
//! use dicey::dice::Dice;
//!
//! let dice: Dice = "6d8x".parse()?;
//! assert_eq!(dice, Dice::builder().count(6).sides(8).explode(None, true).build());
//! # Ok::<(), dicey::parse::Error>(())
//! ```
//!
//! ## Parsing expressions
//! ```
//! use dicey::{dice::Dice, expr::Expr};
//!
//! let expr: Expr = "6d8x + 4d6 - 3".parse()?;
//! assert_eq!(
//! 	expr,
//! 	Expr::Sub(
//! 		Box::new(Expr::Add(
//! 			Box::new(Expr::Dice(
//! 				Dice::builder().count(6).sides(8).explode(None, true).build()
//! 			)),
//! 			Box::new(Expr::Dice(Dice::new(4, 6))),
//! 		)),
//! 		Box::new(Expr::Num(3)),
//! 	)
//! );
//! # Ok::<(), dicey::parse::Error>(())
//! ```

use std::{fmt, str};

use chumsky::prelude::*;

use crate::{
	dice::{Condition, Dice, Modifier},
	expr::Expr,
};

/// Generates a parser that specifically handles dice with or without modifiers like "d20", "2d20kh", "8d6x", etc.
#[must_use]
pub fn dice_part<'src>() -> impl Parser<'src, &'src str, Dice, extra::Err<Rich<'src, char>>> + Copy {
	// Parser for dice literals
	text::int(10)
		.labelled("dice count")
		.or_not()
		.then_ignore(just('d'))
		.then(text::int(10).labelled("dice sides"))
		.then(modifier_list_part())
		.try_map(|((count, sides), modifiers), span| {
			let count = count
				.unwrap_or("1")
				.parse()
				.map_err(|err| Rich::custom(span, format!("Dice count: {err}")))?;
			let sides = sides
				.parse()
				.map_err(|err| Rich::custom(span, format!("Dice sides: {err}")))?;

			Ok(Dice {
				count,
				sides,
				modifiers,
			})
		})
		.labelled("dice set")
}

/// Generates a parser that specifically handles dice with or without modifiers like "d20", "2d20kh", "8d6x", etc.
/// and expects end of input
#[must_use]
pub fn dice<'src>() -> impl Parser<'src, &'src str, Dice, extra::Err<Rich<'src, char>>> + Copy {
	dice_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifiers with conditions like "r1", "xo>4", "kh", etc.
#[must_use]
pub fn modifier_part<'src>() -> impl Parser<'src, &'src str, Modifier, extra::Err<Rich<'src, char>>> + Copy {
	// Parser for dice modifier conditions
	let condition = condition_part();

	// Parser for dice modifiers
	choice((
		// Reroll dice (e.g. r1, rr1, r<=2)
		just('r')
			.ignored()
			.then(just('r').ignored().or_not().map(|r| r.is_some()))
			.then(condition)
			.map(|(((), recurse), cond)| Modifier::Reroll { cond, recurse }),
		// Exploding dice (e.g. x, xo, x>4)
		just('x')
			.ignored()
			.then(just('o').ignored().or_not().map(|o| o.is_none()))
			.then(condition.or_not())
			.map(|(((), recurse), cond)| Modifier::Explode { cond, recurse }),
		// Keep lowest (e.g. kl, kl2)
		just("kl")
			.ignored()
			.then(text::int(10).labelled("keep lowest count").or_not())
			.try_map(|((), count), span| {
				let count = count
					.unwrap_or("1")
					.parse()
					.map_err(|err| Rich::custom(span, format!("Keep lowest count: {err}")))?;
				Ok(Modifier::KeepLow(count))
			}),
		// Keep highest (e.g. k, kh, kh2)
		just('k')
			.ignored()
			.then_ignore(just('h').or_not())
			.then(text::int(10).labelled("keep highest count").or_not())
			.try_map(|((), count), span| {
				let count = count
					.unwrap_or("1")
					.parse()
					.map_err(|err| Rich::custom(span, format!("Keep highest count: {err}")))?;
				Ok(Modifier::KeepHigh(count))
			}),
		// Min (e.g. min3)
		just("min")
			.ignored()
			.then(text::int(10).labelled("min roll value"))
			.try_map(|((), min): ((), &str), span| {
				let min = min
					.parse()
					.map_err(|err| Rich::custom(span, format!("Minimum: {err}")))?;
				Ok(Modifier::Min(min))
			}),
		// Max (e.g. max4)
		just("max")
			.ignored()
			.then(text::int(10).labelled("max roll value"))
			.try_map(|((), max): ((), &str), span| {
				let max = max
					.parse()
					.map_err(|err| Rich::custom(span, format!("Maximum: {err}")))?;
				Ok(Modifier::Max(max))
			}),
	))
	.labelled("dice modifier")
}

/// Generates a parser that specifically handles dice modifiers with conditions like "r1", "xo>4", "kh", etc.
/// and expects end of input
#[must_use]
pub fn modifier<'src>() -> impl Parser<'src, &'src str, Modifier, extra::Err<Rich<'src, char>>> + Copy {
	modifier_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifier lists with conditions like "r1kh4", "r1xo>4kh4", etc.
#[must_use]
pub fn modifier_list_part<'src>() -> impl Parser<'src, &'src str, Vec<Modifier>, extra::Err<Rich<'src, char>>> + Copy {
	modifier_part().repeated().collect()
}

/// Generates a parser that specifically handles dice modifier lists with conditions like "r1kh4", "r1xo>4kh4", etc.
/// and expects end of input
#[must_use]
pub fn modifier_list<'src>() -> impl Parser<'src, &'src str, Vec<Modifier>, extra::Err<Rich<'src, char>>> + Copy {
	modifier_list_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifier conditions like "<3", ">=5", "=1", "1", etc.
#[must_use]
pub fn condition_part<'src>() -> impl Parser<'src, &'src str, Condition, extra::Err<Rich<'src, char>>> + Copy {
	choice((
		just(">=").to(Condition::Gte as fn(u8) -> _),
		just("<=").to(Condition::Lte as fn(u8) -> _),
		just('>').to(Condition::Gt as fn(u8) -> _),
		just('<').to(Condition::Lt as fn(u8) -> _),
		just('=').to(Condition::Eq as fn(u8) -> _),
	))
	.labelled("condition symbol")
	.or_not()
	.then(text::int::<&'src str, _, _>(10).labelled("condition number"))
	.try_map(|(condfn, val), span| {
		let val = val
			.parse()
			.map_err(|err| Rich::custom(span, format!("Modifier condition: {err}")))?;
		Ok(condfn.map_or_else(|| Condition::Eq(val), |condfn| condfn(val)))
	})
	.labelled("modifier condition")
}

/// Generates a parser that specifically handles dice modifier conditions like "<3", ">=5", "=1", "1", etc.
/// and expects end of input
#[must_use]
pub fn condition<'src>() -> impl Parser<'src, &'src str, Condition, extra::Err<Rich<'src, char>>> + Copy {
	condition_part().then_ignore(end())
}

/// Generates a parser that handles full expressions including mathematical operations, grouping with parentheses,
/// dice, etc.
#[must_use]
pub fn expr_part<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> + Clone {
	// Helper function for operators
	let op = |c| just(c).padded();

	recursive(|expr| {
		// Parser for numbers
		let int = text::int(10)
			.try_map(|s: &str, span| {
				s.parse()
					.map(Expr::Num)
					.map_err(|err| Rich::custom(span, format!("{err}")))
			})
			.labelled("number");

		// Parser for dice literals
		let dice = dice_part().map(Expr::Dice);

		// Parser for expressions enclosed in parentheses
		let atom = dice
			.or(int)
			.or(expr.delimited_by(just('('), just(')')).labelled("group"))
			.padded();

		// Parser for negative sign
		let unary = op('-').repeated().foldr(atom, |_op, rhs| Expr::Neg(Box::new(rhs)));

		// Parser for multiplication and division (round up or down)
		let product = unary.clone().foldl(
			choice((
				op('*').to(Expr::Mul as fn(_, _) -> _),
				op('/').to(Expr::DivDown as fn(_, _) -> _),
				op('\\').to(Expr::DivUp as fn(_, _) -> _),
			))
			.then(unary)
			.repeated(),
			|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
		);

		// Parser for addition and subtraction operators
		product.clone().foldl(
			choice((
				op('+').to(Expr::Add as fn(_, _) -> _),
				op('-').to(Expr::Sub as fn(_, _) -> _),
			))
			.then(product)
			.repeated(),
			|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
		)
	})
}

/// Generates a parser that handles full expressions including mathematical operations, grouping with parentheses,
/// dice, etc. and expects end of input
#[must_use]
pub fn expr<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> + Clone {
	expr_part().then_ignore(end())
}

/// Error that can occur while parsing a string into a dice or expression-related structure via [`std::str::FromStr`].
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct Error {
	/// Details of the originating one or more [`Rich`]s that occurred during parsing
	pub details: String,
}

#[allow(clippy::absolute_paths)]
impl std::error::Error for Error {
	fn description(&self) -> &str {
		&self.details
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.details)
	}
}

impl str::FromStr for Dice {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = dice().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(ToString::to_string).collect::<Vec<_>>().join("; "),
		});
		result
	}
}

impl str::FromStr for Modifier {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = modifier().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(ToString::to_string).collect::<Vec<_>>().join("; "),
		});
		result
	}
}

impl str::FromStr for Condition {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		condition().parse(s).into_result().map_err(|errs| Error {
			details: errs.iter().map(ToString::to_string).collect::<Vec<_>>().join("; "),
		})
	}
}

impl str::FromStr for Expr {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = expr().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(ToString::to_string).collect::<Vec<_>>().join("; "),
		});
		result
	}
}

/// Trait to allow convenient access to a parser generator for any implementing type
pub trait GenParser<T> {
	/// Generates a parser for this type that expects end of input. Requires the `parse` feature (enabled by default).
	#[must_use]
	fn parser<'src>() -> impl Parser<'src, &'src str, T, extra::Err<Rich<'src, char>>> + Clone;

	/// Generates a parser for this type that parses up to the end of valid input. Requires the `parse` feature
	/// (enabled by default).
	#[must_use]
	fn part_parser<'src>() -> impl Parser<'src, &'src str, T, extra::Err<Rich<'src, char>>> + Clone;
}

impl GenParser<Dice> for Dice {
	#[inline]
	#[allow(refining_impl_trait)]
	fn parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		dice()
	}

	#[inline]
	#[allow(refining_impl_trait)]
	fn part_parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		dice_part()
	}
}

impl GenParser<Modifier> for Modifier {
	#[inline]
	#[allow(refining_impl_trait)]
	fn parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		modifier()
	}

	#[inline]
	#[allow(refining_impl_trait)]
	fn part_parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		modifier_part()
	}
}

impl GenParser<Condition> for Condition {
	#[inline]
	#[allow(refining_impl_trait)]
	fn parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		condition()
	}

	#[inline]
	#[allow(refining_impl_trait)]
	fn part_parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Copy {
		condition_part()
	}
}

impl GenParser<Expr> for Expr {
	#[inline]
	fn parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Clone {
		expr()
	}

	#[inline]
	fn part_parser<'src>() -> impl Parser<'src, &'src str, Self, extra::Err<Rich<'src, char>>> + Clone {
		expr_part()
	}
}
