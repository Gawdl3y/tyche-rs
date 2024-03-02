pub mod dice;
pub mod term;

#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "parse")]
pub use parse::term as parser;

#[cfg(test)]
mod tests;
