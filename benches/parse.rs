#![feature(test)]

extern crate test;

use chumsky::Parser;
use dicey::expr::Expr;
use test::Bencher;

const ABSURD_EXPR: &str = include_str!("absurd_dice_expr.txt");

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
	b.iter(|| parser.parse(ABSURD_EXPR).unwrap())
}

#[bench]
fn parser_creation(b: &mut Bencher) {
	b.iter(dicey::parser);
}

#[bench]
fn fromstr_basic(b: &mut Bencher) {
	b.iter(|| "4d8 + 4".parse::<Expr>().unwrap());
}

#[bench]
fn fromstr_complex(b: &mut Bencher) {
	b.iter(|| "4d8x + 2d10 * (-3d6 - 6 / 2 \\ 4)".parse::<Expr>().unwrap());
}

#[bench]
fn fromstr_absurd(b: &mut Bencher) {
	b.iter(|| ABSURD_EXPR.parse::<Expr>().unwrap())
}
