#![allow(clippy::expect_used, clippy::unwrap_used, reason = "Tests should panic")]

mod dice;
mod expr;

#[cfg(feature = "parse")]
mod parse;
