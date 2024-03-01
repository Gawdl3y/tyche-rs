#![cfg(feature = "parse")]

use chumsky::prelude::*;

use crate::dice::{Dice, Modifier};
use crate::expr::Term;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Term, extra::Err<Rich<'a, char>>> {
	// Helper functions for operators
	let op = |c| just(c).padded();

	recursive(|expr| {
		// Parser for numbers
		let int = text::int(10).try_map(|s: &str, span| {
			s.parse()
				.map(Term::Num)
				.map_err(|e| Rich::custom(span, format!("{}", e)))
		});

		// Parser for dice expressions
		let dice = text::int(10)
			.or_not()
			.then_ignore(just('d').or(just('D')))
			.then(text::int::<_, _, extra::Err<Rich<char>>>(10))
			.then((just('x').or(just('X'))).or_not())
			.try_map(|vals: ((Option<&str>, &str), Option<char>), span| {
				let count = vals
					.0
					 .0
					.map(|v| v.parse())
					.unwrap_or(Ok(1))
					.map_err(|e| Rich::custom(span, format!("Error in dice count: {}", e)))?;
				let sides = vals
					.0
					 .1
					.parse()
					.map_err(|e| Rich::custom(span, format!("Error in dice sides: {}", e)))?;
				let explode = vals.1.is_some();

				Ok(Term::Dice(Dice {
					count,
					sides,
					modifiers: if explode {
						vec![Modifier::Explode(None, true)]
					} else {
						Vec::new()
					},
				}))
			});

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
