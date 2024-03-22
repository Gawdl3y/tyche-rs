#![feature(test)]

extern crate test;

use chumsky::Parser;
use test::Bencher;

#[bench]
fn parse_basic(b: &mut Bencher) {
	let parser = dicey::parser();
	b.iter(|| parser.parse("4d8 + 4").unwrap());
}

#[bench]
fn parse_complex(b: &mut Bencher) {
	let parser = dicey::parser();
	b.iter(|| parser.parse("4d8x + 2d10 * (-3d6 - 6 / 2 \\ 4)").unwrap());
}

#[bench]
fn parse_absurd(b: &mut Bencher) {
	let parser = dicey::parser();
	b.iter(|| parser.parse(include_str!("absurd_dice_expr.txt")))
}

#[bench]
fn parser_creation(b: &mut Bencher) {
	b.iter(|| dicey::parser());
}
