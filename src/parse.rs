#![cfg(feature = "parse")]
#![allow(clippy::tabs_in_doc_comments)]

//! Parser generator functions and implementations of [`std::str::FromStr`] for all dice and term data structures.
//! Requires the `parse` feature (enabled by default).
//!
//! The parser generators generate parsers for parsing dice, dice modifiers, modifier conditions, and full mathematical
//! dice expressions (such as `4d8 + 2d6x - 3`) from strings. They're all made with [chumsky] and are almost entirely
//! zero-copy. A parser can be used directly by calling [`chumsky::Parser::parse()`] on it.
//!
//! # Examples
//!
//! ## Parsing Dice
//! ```
//! use dicey::dice::Dice;
//!
//! let dice: Dice = "6d8x".parse().expect("unable to parse dice");
//! assert_eq!(dice, Dice::builder().count(6).sides(8).explode(None, true).build());
//! ```
//!
//! ## Parsing Terms (mathematical dice expressions)
//! ```
//! use dicey::{dice::Dice, term::Term};
//!
//! let term: Term = "6d8x + 4d6 - 3".parse().expect("unable to parse term");
//! assert_eq!(
//! 	term,
//! 	Term::Sub(
//! 		Box::new(Term::Add(
//! 			Box::new(Term::Dice(
//! 				Dice::builder().count(6).sides(8).explode(None, true).build()
//! 			)),
//! 			Box::new(Term::Dice(Dice::builder().count(4).sides(6).build())),
//! 		)),
//! 		Box::new(Term::Num(3)),
//! 	)
//! );
//! ```

use chumsky::prelude::*;

use crate::{
	dice::{Condition, Dice, Modifier},
	term::Term,
};

/// Generates a parser that specifically handles dice terms like "d20", "2d20kh", "8d6x", etc.
pub fn dice_part<'src>() -> impl Parser<'src, &'src str, Dice, extra::Err<Rich<'src, char>>> + Copy {
	// Parser for dice expressions
	text::int(10)
		.or_not()
		.then_ignore(just('d'))
		.then(text::int(10))
		.then(modifier_list_part())
		.try_map(|((count, sides), modifiers), span| {
			let count = count
				.unwrap_or("1")
				.parse()
				.map_err(|err| Rich::custom(span, format!("Dice count: {}", err)))?;
			let sides = sides
				.parse()
				.map_err(|err| Rich::custom(span, format!("Dice sides: {}", err)))?;

			Ok(Dice {
				count,
				sides,
				modifiers,
			})
		})
}

/// Generates a parser that specifically handles dice terms like "d20", "2d20kh", "8d6x", etc.
/// and expects end of input
pub fn dice<'src>() -> impl Parser<'src, &'src str, Dice, extra::Err<Rich<'src, char>>> + Copy {
	dice_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifiers with conditions like "r1", "xo>4", "kh", etc.
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
			.map(|((_, recurse), cond)| Modifier::Reroll { cond, recurse }),
		// Exploding dice (e.g. x, xo, x>4)
		just('x')
			.ignored()
			.then(just('o').ignored().or_not().map(|o| o.is_none()))
			.then(condition.or_not())
			.map(|((_, recurse), cond)| Modifier::Explode { cond, recurse }),
		// Keep lowest (e.g. kl, kl2)
		just("kl")
			.ignored()
			.then(text::int(10).or_not())
			.try_map(|(_, count), span| {
				let count = count
					.unwrap_or("1")
					.parse()
					.map_err(|err| Rich::custom(span, format!("Keep lowest count: {}", err)))?;
				Ok(Modifier::KeepLow(count))
			}),
		// Keep highest (e.g. k, kh, kh2)
		just('k')
			.ignored()
			.then_ignore(just('h').or_not())
			.then(text::int(10).or_not())
			.try_map(|(_, count), span| {
				let count = count
					.unwrap_or("1")
					.parse()
					.map_err(|err| Rich::custom(span, format!("Keep highest count: {}", err)))?;
				Ok(Modifier::KeepHigh(count))
			}),
	))
}

/// Generates a parser that specifically handles dice modifiers with conditions like "r1", "xo>4", "kh", etc.
/// and expects end of input
pub fn modifier<'src>() -> impl Parser<'src, &'src str, Modifier, extra::Err<Rich<'src, char>>> + Copy {
	modifier_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifier lists with conditions like "r1kh4", "r1xo>4kh4", etc.
pub fn modifier_list_part<'src>() -> impl Parser<'src, &'src str, Vec<Modifier>, extra::Err<Rich<'src, char>>> + Copy {
	modifier_part().repeated().collect()
}

/// Generates a parser that specifically handles dice modifier lists with conditions like "r1kh4", "r1xo>4kh4", etc.
/// and expects end of input
pub fn modifier_list<'src>() -> impl Parser<'src, &'src str, Vec<Modifier>, extra::Err<Rich<'src, char>>> + Copy {
	modifier_list_part().then_ignore(end())
}

/// Generates a parser that specifically handles dice modifier conditions like "<3", ">=5", "=1", "1", etc.
pub fn condition_part<'src>() -> impl Parser<'src, &'src str, Condition, extra::Err<Rich<'src, char>>> + Copy {
	choice((
		just(">=").to(Condition::Gte as fn(u8) -> _),
		just("<=").to(Condition::Lte as fn(u8) -> _),
		just('>').to(Condition::Gt as fn(u8) -> _),
		just('<').to(Condition::Lt as fn(u8) -> _),
		just('=').to(Condition::Eq as fn(u8) -> _),
	))
	.or_not()
	.then(text::int::<&'src str, _, _>(10))
	.try_map(|(condfn, val), span| {
		let val = val
			.parse()
			.map_err(|err| Rich::custom(span, format!("Modifier condition: {}", err)))?;
		Ok(match condfn {
			Some(condfn) => condfn(val),
			None => Condition::Eq(val),
		})
	})
}

/// Generates a parser that specifically handles dice modifier conditions like "<3", ">=5", "=1", "1", etc.
/// and expects end of input
pub fn condition<'src>() -> impl Parser<'src, &'src str, Condition, extra::Err<Rich<'src, char>>> + Copy {
	condition_part().then_ignore(end())
}

/// Generates a parser that handles full expressions including mathematical operations, grouping with parentheses,
/// dice expressions, etc.
pub fn term_part<'src>() -> impl Parser<'src, &'src str, Term, extra::Err<Rich<'src, char>>> + Clone {
	// Helper function for operators
	let op = |c| just(c).padded();

	recursive(|term| {
		// Parser for numbers
		let int = text::int(10).try_map(|s: &str, span| {
			s.parse()
				.map(Term::Num)
				.map_err(|e| Rich::custom(span, format!("{}", e)))
		});

		// Parser for dice expressions
		let dice = dice_part().map(Term::Dice);

		// Parser for expressions enclosed in parentheses
		let atom = dice.or(int).or(term.delimited_by(just('('), just(')'))).padded();

		// Parser for negative sign
		let unary = op('-').repeated().foldr(atom, |_op, rhs| Term::Neg(Box::new(rhs)));

		// Parser for multiplication and division (round up or down)
		let product = unary.clone().foldl(
			choice((
				op('*').to(Term::Mul as fn(_, _) -> _),
				op('/').to(Term::DivDown as fn(_, _) -> _),
				op('\\').to(Term::DivUp as fn(_, _) -> _),
			))
			.then(unary)
			.repeated(),
			|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
		);

		// Parser for addition and subtraction operators
		product.clone().foldl(
			choice((
				op('+').to(Term::Add as fn(_, _) -> _),
				op('-').to(Term::Sub as fn(_, _) -> _),
			))
			.then(product)
			.repeated(),
			|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
		)
	})
}

/// Generates a parser that handles full expressions including mathematical operations, grouping with parentheses,
/// dice expressions, etc. and expects end of input
pub fn term<'src>() -> impl Parser<'src, &'src str, Term, extra::Err<Rich<'src, char>>> + Clone {
	term_part().then_ignore(end())
}

/// Error that can occur while parsing a string into a dice or term-related structure via [`std::str::FromStr`].
#[derive(Debug, Clone)]
pub struct Error {
	/// Details of the originating one or more [`chumsky::error::Rich`]s that occurred during parsing
	pub details: String,
}

impl std::error::Error for Error {
	fn description(&self) -> &str {
		&self.details
	}
}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.details)
	}
}

impl std::str::FromStr for Dice {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = dice().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(|err| err.to_string()).collect::<Vec<_>>().join("; "),
		});
		result
	}
}

impl std::str::FromStr for Modifier {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = modifier().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(|err| err.to_string()).collect::<Vec<_>>().join("; "),
		});
		result
	}
}

impl std::str::FromStr for Condition {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		condition().parse(s).into_result().map_err(|errs| Error {
			details: errs.iter().map(|err| err.to_string()).collect::<Vec<_>>().join("; "),
		})
	}
}

impl std::str::FromStr for Term {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let lc = s.to_lowercase();
		let result = term().parse(&lc).into_result().map_err(|errs| Error {
			details: errs.iter().map(|err| err.to_string()).collect::<Vec<_>>().join("; "),
		});
		result
	}
}
