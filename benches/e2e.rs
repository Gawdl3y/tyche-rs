#![feature(test)]

extern crate test;

use test::Bencher;

use chumsky::Parser;

use tyche::dice::roller::FastRand as FastRandRoller;

#[bench]
fn e2e_basic(b: &mut Bencher) {
	let parser = tyche::parser();
	let mut rng = FastRandRoller::default();
	b.iter(|| parser.parse("4d8 + 4").unwrap().eval(&mut rng).unwrap().calc().unwrap());
}

#[bench]
fn e2e_complex(b: &mut Bencher) {
	let parser = tyche::parser();
	let mut rng = FastRandRoller::default();
	b.iter(|| {
		parser
			.parse("4d8x + 2d10 * (-3d6 - 6 / 2 \\ 4)")
			.unwrap()
			.eval(&mut rng)
			.unwrap()
			.calc()
			.unwrap()
	});
}

#[bench]
fn e2e_absurd(b: &mut Bencher) {
	let parser = tyche::parser();
	let mut rng = FastRandRoller::default();
	let expr = include_str!("absurd_dice_expr.txt");
	b.iter(|| parser.parse(expr).unwrap().eval(&mut rng).unwrap().calc().unwrap())
}
