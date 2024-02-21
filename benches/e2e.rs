#![feature(test)]

extern crate test;

use test::Bencher;

use chumsky::Parser;

#[bench]
fn e2e_basic(b: &mut Bencher) {
	let parser = dicey::parser();
	b.iter(|| parser.parse("4d8 + 4").unwrap().eval().unwrap().calc().unwrap());
}

#[bench]
fn e2e_complex(b: &mut Bencher) {
	let parser = dicey::parser();
	b.iter(|| {
		parser
			.parse("4d8x + 2d10 * (-3d6 - 6 / 2 \\ 4)")
			.unwrap()
			.eval()
			.unwrap()
			.calc()
			.unwrap()
	});
}
