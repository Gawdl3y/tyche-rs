#[cfg(feature = "build-binary")]
fn main() {
	use std::{
		env,
		io::{self, Write},
		process::exit,
	};

	use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
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
	let parsed = dicey::parser().parse(&input);
	let expr = if !parsed.has_errors() {
		parsed.into_output().expect("no output from parser without errors")
	} else {
		let mut colors = ColorGenerator::new();
		let mut report = Report::build(ReportKind::Error, "input", 0).with_message("Unable to parse dice expression");

		for err in parsed.errors() {
			report.add_label(
				Label::new(("input", err.span().into_range()))
					.with_color(colors.next())
					.with_message(err.reason()),
			);
			println!("{err}");
		}

		report.finish().eprint(("input", Source::from(&input))).unwrap();
		exit(1);
	};

	// Evaluate the expression
	let evaled = match expr.eval() {
		Ok(evaled) => evaled,
		Err(err) => {
			eprintln!("\x1b[1m\x1b[31mEvaluation error:\x1b[0m {err}");
			exit(2);
		}
	};

	println!("\x1b[1m\x1b[36mEvaluated:\x1b[0m {evaled:#?}");
	println!("\x1b[1m\x1b[36mDescribed:\x1b[0m {evaled}");

	// Calculate the total
	let total = match evaled.calc() {
		Ok(total) => total,
		Err(err) => {
			eprintln!("\x1b[1m\x1b[31mCalculation error:\x1b[0m {err}");
			exit(3);
		}
	};

	println!("\x1b[1m\x1b[36mTotal:\x1b[0m {total}");
}

#[cfg(not(feature = "build-binary"))]
fn main() -> Result<(), &'static str> {
	Err("Nothing to do since the build-binary feature is disabled.")
}
