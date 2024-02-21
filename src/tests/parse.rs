use chumsky::Parser;

use crate::*;

// #[test]
// fn basic_addition() {
// 	let ast = parser().parse("42 + 69").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, 111);
// }

// #[test]
// fn basic_subtraction() {
// 	let ast = parser().parse("42 - 69").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, -27);
// }

// #[test]
// fn basic_multiplication() {
// 	let ast = parser().parse("42 * 69").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, 2898);
// }

// #[test]
// fn basic_division_rounded_down() {
// 	let ast = parser().parse("50 / 11").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, 4);
// }

// #[test]
// fn basic_division_rounded_up() {
// 	let ast = parser().parse("50 \\ 11").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, 5);
// }

// #[test]
// fn complex_math() {
// 	let ast = parser().parse("-5 * (3 + 1) - -4 / 2").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert_eq!(result.val, -18);
// }

// #[test]
// fn basic_dice_math() {
// 	let ast = parser().parse("4d5 + 20").unwrap();
// 	let result = ast.eval().unwrap();
// 	assert!(result.val > 20);
// }

// #[test]
// fn eval_overflow() {
// 	let ast = parser().parse(&format!("{} + 1", i32::MAX)).unwrap();
// 	let result = ast.eval();
// 	assert!(result.is_err());
// 	assert!(result.unwrap_err().to_string().contains("overflow"));
// }

// #[test]
// fn invalid_token() {
// 	let bigboi = i32::MAX as i64 + 1;
// 	let ast = parser().parse(&format!("{}", bigboi));
// 	assert!(ast.has_errors());
// }
