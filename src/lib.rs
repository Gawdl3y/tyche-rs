#![allow(clippy::tabs_in_doc_comments)]

//! A library for parsing, rolling, and explaining the results of tabletop dice. Dice modifiers use a similar syntax to
//! [FoundryVTT's modifiers].
//!
//! [FoundryVTT's modifiers]: https://foundryvtt.com/article/dice-modifiers/

pub mod dice;
pub mod expr;

#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "parse")]
pub use parse::expr as parser;

#[cfg(test)]
mod tests;
