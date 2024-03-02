#[cfg(feature = "parse")]
fn main() {
	use std::env;
	use std::io::{self, Write};

	use chumsky::Parser;

	let args = env::args();
	let input = if args.len() > 1 {
		// Obtain the expression by combining all args passed to the executable, so that it can be left unquoted
		// even with spaces. The first argument is ignored since it is typically the name of the executable itself.
		args.skip(1).collect::<Vec<String>>().join(" ")
	} else {
		let mut lines = io::stdin().lines();

		// If there isn't already input available in stdin, display a prompt for it
		if lines.size_hint().1.is_none() {
			print!("Enter dice expression: ");
			io::stdout().flush().unwrap();
		}

		// Grab the first line available from stdin
		lines.next().unwrap().unwrap()
	};

	println!("Input: {}", input);

	match dicey::parse::expr().parse(&input).into_result() {
		Ok(ast) => {
			println!("Parsed: {:?}", ast);
			println!("Deterministic: {}", ast.is_deterministic());

			match ast.eval() {
				Ok(evaled) => {
					println!();
					println!("Evaluated: {:?}", evaled);
					println!("Described: {}", evaled);
					println!(
						"Total: {}",
						evaled
							.calc()
							.map(|total| total.to_string())
							.or_else(|err| Ok::<_, dicey::term::Error>(err.to_string()))
							.unwrap()
					);
				}
				Err(eval_err) => eprintln!("Evaluation error: {}", eval_err),
			}
		}
		Err(parse_errs) => parse_errs.into_iter().for_each(|e| eprintln!("Parse error: {}", e)),
	};
}

#[cfg(not(feature = "parse"))]
fn main() {
	println!("Nothing to do since the parse feature is disabled.")
}
