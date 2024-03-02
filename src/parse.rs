#![cfg(feature = "parse")]

use std::num::NonZeroU8;

use chumsky::prelude::*;

use crate::{
	dice::{Condition, Dice, Modifier},
	expr::Term,
};

/// Generates a parser that specifically handles dice terms like "d20", "2d20kh", "8d6x", etc.
pub fn dice<'src>() -> impl Parser<'src, &'src str, Dice, extra::Err<Rich<'src, char>>> + Clone {
	// Parser for dice modifier conditions
	let condition = choice((
		just('=').to(Condition::Eq as fn(NonZeroU8) -> _),
		just('>').to(Condition::Gt as fn(NonZeroU8) -> _),
		just(">=").to(Condition::Gte as fn(NonZeroU8) -> _),
		just('<').to(Condition::Lt as fn(NonZeroU8) -> _),
		just(">=").to(Condition::Lte as fn(NonZeroU8) -> _),
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
	});

	// Parser for dice expressions
	text::int(10)
		.or_not()
		.then_ignore(just('d'))
		.then(text::int(10))
		.then(
			choice((
				// Exploding dice (e.g. x, xo, x>4)
				just('x')
					.ignored()
					.then(just('o').ignored().or_not().map(|o| o.is_none()))
					.then(condition.or_not())
					.map(|((_, once), cond)| Modifier::Explode(cond, once)),
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
			.repeated()
			.collect(),
		)
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

/// Generates a parser that handles full expressions including mathematical operations, grouping with parentheses,
/// dice terms, etc.
pub fn expr<'src>() -> impl Parser<'src, &'src str, Term, extra::Err<Rich<'src, char>>> {
	// Helper function for operators
	let op = |c| just(c).padded();

	recursive(|expr| {
		// Parser for numbers
		let int = text::int(10).try_map(|s: &str, span| {
			s.parse()
				.map(Term::Num)
				.map_err(|e| Rich::custom(span, format!("{}", e)))
		});

		// Parser for dice expressions
		let dice = dice().map(Term::Dice);

		// Parser for expressions enclosed in parentheses
		let atom = dice.or(int).or(expr.delimited_by(just('('), just(')'))).padded();

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
	.then_ignore(end())
}
