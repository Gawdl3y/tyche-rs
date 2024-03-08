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
			print!("\x1b[1m\x1b[36mEnter dice expression:\x1b[0m ");
			io::stdout().flush().unwrap();
		}

		// Grab the first line available from stdin
		lines.next().unwrap().unwrap()
	};

	println!("\x1b[1m\x1b[36mInput:\x1b[0m {}", input);

	match dicey::parse::term().parse(&input).into_result() {
		Ok(ast) => {
			println!("\x1b[1m\x1b[36mParsed:\x1b[0m {:#?}", ast);
			println!("\x1b[1m\x1b[36mDeterministic:\x1b[0m {}", ast.is_deterministic());

			match ast.eval() {
				Ok(evaled) => {
					println!("\x1b[1m\x1b[36mEvaluated:\x1b[0m {:#?}", evaled);
					println!("\x1b[1m\x1b[36mDescribed:\x1b[0m {}", evaled);
					println!(
						"\x1b[1m\x1b[36mTotal:\x1b[0m {}",
						evaled
							.calc()
							.map(|total| total.to_string())
							.or_else(|err| Ok::<_, dicey::term::Error>(err.to_string()))
							.unwrap()
					);
				}
				Err(eval_err) => eprintln!("\x1b[1m\x1b[31mEvaluation error:\x1b[0m {}", eval_err),
			}
		}
		Err(parse_errs) => parse_errs
			.into_iter()
			.for_each(|e| eprintln!("\x1b[1m\x1b[31mParse error:\x1b[0m {}", e)),
	};
}

#[cfg(not(feature = "parse"))]
fn main() {
	println!("Nothing to do since the parse feature is disabled.")
}
