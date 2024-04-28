# Tyche

[![Crates.io Version](https://img.shields.io/crates/v/tyche)](https://crates.io/crates/tyche)
[![docs.rs](https://img.shields.io/docsrs/tyche)](https://docs.rs/tyche)

Tyche is a library for parsing, rolling, and explaining the results of tabletop dice.  
It also has a simple CLI app binary that evaluates a given expression.

The eventual goal is full compatibility with [FoundryVTT's dice syntax](https://foundryvtt.com/article/dice/) with some
convenient extensions.

## Features

- Parsing dice expressions, simple or complex
- Arithmetic
  - Addition: `2d6 + 2`
  - Subtraction: `2d6 - 2`
  - Multiplication: `2d6 * 2`
  - Division (integer, rounded down): `2d6 / 2`
  - Division (integer, rounded up): `2d6 \ 2`
  - Standard mathematical order of operations
  - Parenthetical grouping: `(2d6 + 2) * 2`
- Dice modifiers
  - Keep highest (advantage): `2d20kh`, `2d20k`
    - With specific amount to keep: `3d20kh2`
  - Keep lowest (disadvantage): `2d20kl`
    - With specific amount to keep: `3d20kl2`
  - Reroll (once): `4d6r`
    - With specific condition: `4d6r>4`, `4d6r>=5`, `4d6r<3`, `4d6r<=2`, `4d6r4`
  - Reroll (recursive): `4d6rr`
    - With specific condition: `4d6rr>4`, `4d6rr>=5`, `4d6rr<3`, `4d6rr<=2`, `4d6rr4`
  - Explode (recursive): `4d6x`
    - With specific condition: `4d6x>4`, `4d6x>=5`, `4d6x<3`, `4d6x<=2`, `4d6x4`
  - Explode (once): `4d6xo`
    - With specific condition: `4d6xo>4`, `4d6xo>=5`, `4d6xo<3`, `4d6xo<=2`, `4d6xo4`
  - Minimum: `4d8min3`
  - Maximum: `4d8max6`
- Dice modifier chaining (applied in the order they're specified): `4d6rr1x>4`, `8d8min2kh4xo`
- Roller abstractions, allowing custom die rolling behavior

## Installation

### Library

Run `cargo add tyche` or add the following to your project's Cargo.toml file:

```toml
[dependencies]
tyche = "0.1.0"
```

### Binary (CLI app)

Run `cargo install tyche --features build-binary`.  
Assuming Cargo's bin directory is in your `$PATH`, use the app with `tyche` or `tyche <dice expression>`.

## Library usage

There are three main types that you'll start with while using Tyche:

- [`Dice`]: a struct containing a dice count, number of sides for each die, and modifiers that should be applied to any
  resulting rolls, representing a set of dice, e.g. `4d8x`.
- [`Expr`]: an AST-like tree structure enum of individual components of a dice expression, capable of representing
  complex sets of mathematical operations including dice, e.g. `(2d6 + 2) * 2`.
- [`Roller`]: a trait for rolling individual die values and entire sets of `Dice`. Sometimes referred to as simply "RNG"
  for the sake of brevity. There are a few `Roller` implementations available out of the box:
  - [`FastRand`]: uses the [fastrand] crate for random number generation
  - [`Max`]: always rolls the max possible value for each die
  - [`Val`]: always rolls one specific value, ignoring dice sides
  - [`Iter`]: rolls a specific sequence of die values from an iterator, ignoring dice sides
    (useful for testing purposes)

### Parsing

All parsing requires the `parse` feature of the crate to be enabled (which it is by default).

Tyche uses the [chumsky] parser generator to parse all strings in a _nearly_ zero-copy and wicked-fast fashion.

Most conveniently, parsing can be done by utilizing the standard [`FromStr`] implementations for the relevant types
([`Dice`], [`Expr`], [`Modifier`], and [`Condition`]):

```rust
use tyche::{
	dice::modifier::{Condition, Modifier},
	Dice, Expr,
};

let expr: Expr = "4d6rr<3 + 2d8 - 4".parse()?;
let dice: Dice = "4d6rr<3".parse()?;
let modifier: Modifier = "rr<3".parse()?;
let cond: Condition = "<3".parse()?;
```

Alternatively, you can directly use the parsers for each type via its associated [`GenParser`] implementation
or the functions in the [`tyche::parse`] module.

### Manually creating Dice

Programmatically constructing [`Dice`] to roll is painless, even with lots of chained modifiers,
thanks to its use of the builder pattern.

```rust
use tyche::{dice::modifier::Condition, Dice};

// Simple set of dice, no modifiers: 2d20
let d2d20 = Dice::new(2, 20);

// Exploding dice: 4d6x
let d4d6x = Dice::builder()
	.count(4)
	.sides(6)
	.explode(None, true)
	.build();

// Chained modifiers: 6d6rr1x
let d6d6rr1x = Dice::builder()
	.count(6)
	.sides(6)
	.reroll(Condition::Eq(1), true)
	.explode(None, true)
	.build();
```

### Rolling Dice

All rolling of dice is performed by a [`Roller`] implementation.  
The most suitable "default" roller implementation is [`FastRand`], which generates random numbers for die values using a
[`fastrand::Rng`] instance.

```rust
use tyche::dice::roller::FastRand as FastRandRoller;

// Create a FastRand roller with the default thread-local fastrand::Rng
let mut roller = FastRandRoller::default();

// Create a FastRand roller with a custom-seeded fastrand::Rng
let rng = fastrand::Rng::with_seed(0x750c38d574400);
let mut roller = FastRandRoller::new(rng);
```

Once you have a roller, you can roll a single die at a time or a whole set of [`Dice`] with it:

```rust
use tyche::dice::{roller::FastRand as FastRandRoller, Dice, Roller};

let mut roller = FastRandRoller::default();

// Roll a single 20-sided die
let die = roller.roll_die(20);

// Roll two six-sided dice
let dice = Dice::new(2, 6);
let rolled = roller.roll(&dice, true)?;
```

Rolling a single die results in a [`DieRoll`] instance, whereas rolling a set of `Dice` returns a [`Rolled`] instance.

#### Working with rolled dice sets

[`Rolled`] is a struct that ties multiple [`DieRoll`]s together with the [`Dice`] that were rolled to generate them so
it can accurately describe what happened during the roll and application of any modifiers that were on the dice.

Using a `Rolled` result, you can easily total the results of all rolled dice and/or build a string that contains the
original dice set along with a list of each individual die roll.

```rust
use tyche::{
	dice::{roller::FastRand as FastRandRoller, Dice, Roller},
	expr::Describe,
};

// Build and roll 4d6kh2
let mut roller = FastRandRoller::default();
let dice = Dice::builder()
	.count(4)
	.sides(6)
	.keep_high(2)
	.build();
let rolled = roller.roll(&dice, true)?;

// Total the results
let total = rolled.total()?;
assert!((2..=12).contains(&total));

// Build a detailed string about the rolls
// The resulting string will look like "4d6kh2[2 (d), 5, 6, 4 (d)]"
let described = rolled.to_string();

// This is the same as the to_string() call above, except only two die rolls will be listed.
// The resulting string will look like "4d6kh2[2 (d), 5, 2 more...]"
let limited = rolled.describe(Some(2));
```

#### Working with individual die rolls

A [`DieRoll`] contains the final value of the die alongside information about any changes that were made to it and the
source of said changes.

##### Added rolls

When a modifier (rather than the original dice set) causes the addition of a new [`DieRoll`] to a [`Rolled`] set,
the roll's `added_by` field is set to `Some(source_modifier)`. An added roll will be marked as such in strings.  
Modifiers that can result in additional die rolls are:

- Explode
- Reroll

##### Dropped rolls

When a modifier causes the removal of a [`DieRoll`] from a [`Rolled`] set,
the roll's `dropped_by` field is set to `Some(source_modifier)`. A dropped roll will not be affected by any further
modifiers, is not included when totaling the rolled set, and will be marked as dropped in strings.  
Modifiers that can result in dropped die rolls are:

- Reroll
- Keep highest
- Keep lowest

##### Changed rolls

When a modifier directly manipulates the value of a [`DieRoll`] in a [`Rolled`] set, the roll's `changes` field has a
[`ValChange`] item added to it that describes the change made and the modifier that caused it.  
Modifiers that can result in changed die rolls are:

- Minimum
- Maximum

### Working with expressions

[`Expr`] trees are essentially always obtained from parsing an expression string, as manually creating them would be
quite cumbersome.

Once you have an `Expr` variant, it can be evaluated to produce an [`Evaled`] expression tree.
`Evaled` expression trees are nearly identical to their originating `Expr` tree, except any Dice variants have their
dice rolled. This separation allows for describing an expression in a detailed way both before and after rolling dice it
contains, in addition to a few other utilities.

```rust
use tyche::{
	dice::roller::FastRand as FastRandRoller,
	expr::{Describe, Expr},
};

// Parse a nice dice expression
let expr: Expr = "4d6 + 2d8 - 2".parse()?;

// This expression is definitely not deterministic (it contains dice sets)
assert!(!expr.is_deterministic());

// Build a string for the expression - in this case, it'll be identical to the original parsed string
assert_eq!(expr.to_string(), "4d6 + 2d8 - 2");

// Evaluate the expression, rolling any dice sets it contains
let mut roller = FastRandRoller::default();
let evaled = expr.eval(&mut roller)?;

// Build a detailed string about the evaluated expression
// The resulting string will look like "4d6[3, 2, 6, 2] + 2d8[5, 4] - 2"
let described = evaled.to_string();

// This is the same as the to_string() call above, except only two die rolls in each set will be listed.
// The resulting string will look like "4d6[3, 2, 2 more...] + 2d8[5, 4] - 2"
let limited = evaled.describe(Some(2));

// Calculate the final result of the expression
let total = evaled.calc()?;
assert!((4..=38).contains(&total));
```

## Contributing

All contributions are welcome! Try to keep PRs relatively small in scope (single feature/fix/refactor at a time)
and word your commits descriptively.

## License

Tyche is licensed under the [LGPLv3](https://www.gnu.org/licenses/lgpl-3.0) license.

[`Dice`]: https://docs.rs/tyche/latest/tyche/dice/struct.Dice.html
[`Expr`]: https://docs.rs/tyche/latest/tyche/expr/enum.Expr.html
[`Evaled`]: https://docs.rs/tyche/latest/tyche/expr/enum.Evaled.html
[`Roller`]: https://docs.rs/tyche/latest/tyche/dice/roller/trait.Roller.html
[`FastRand`]: https://docs.rs/tyche/latest/tyche/dice/roller/struct.FastRand.html
[`Max`]: https://docs.rs/tyche/latest/tyche/dice/roller/struct.Max.html
[`Val`]: https://docs.rs/tyche/latest/tyche/dice/roller/struct.Val.html
[`Iter`]: https://docs.rs/tyche/latest/tyche/dice/roller/struct.Iter.html
[`Modifier`]: https://docs.rs/tyche/latest/tyche/dice/modifier/enum.Modifier.html
[`Condition`]: https://docs.rs/tyche/latest/tyche/dice/modifier/enum.Condition.html
[`DieRoll`]: https://docs.rs/tyche/latest/tyche/dice/struct.DieRoll.html
[`Rolled`]: https://docs.rs/tyche/latest/tyche/dice/struct.Rolled.html
[`ValChange`]: https://docs.rs/tyche/latest/tyche/dice/struct.ValChange.html
[`GenParser`]: https://docs.rs/tyche/latest/tyche/parse/trait.GenParser.html
[`tyche::parse`]: https://docs.rs/tyche/latest/tyche/parse/index.html
[`FromStr`]: https://doc.rust-lang.org/std/str/trait.FromStr.html
[fastrand]: https://github.com/smol-rs/fastrand
[`fastrand::Rng`]: https://docs.rs/fastrand/latest/fastrand/struct.Rng.html
[chumsky]: https://github.com/zesterer/chumsky
