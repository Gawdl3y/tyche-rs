#[cfg(feature = "parse")]
fn main() {
	use std::{
		env,
		io::{self, Write},
		process::exit,
	};

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

	// Parse the input
	let expr = match dicey::parser().parse(&input).into_result() {
		Ok(expr) => expr,
		Err(errs) => {
			errs.into_iter()
				.for_each(|err| eprintln!("\x1b[1m\x1b[31mParse error:\x1b[0m {}", err));
			exit(1);
		}
	};

	// Evaluate the expression
	let evaled = match expr.eval() {
		Ok(evaled) => evaled,
		Err(err) => {
			eprintln!("\x1b[1m\x1b[31mEvaluation error:\x1b[0m {}", err);
			exit(2);
		}
	};

	println!("\x1b[1m\x1b[36mEvaluated:\x1b[0m {evaled:#?}");
	println!("\x1b[1m\x1b[36mDescribed:\x1b[0m {evaled}");
	println!(
		"\x1b[1m\x1b[36mTotal:\x1b[0m {}",
		evaled
			.calc()
			.map(|total| total.to_string())
			.or_else(|err| Ok::<_, dicey::expr::Error>(err.to_string()))
			.unwrap()
	);
}

#[cfg(not(feature = "parse"))]
fn main() -> Result<(), &'static str> {
	Err("Nothing to do since the parse feature is disabled.")
}
