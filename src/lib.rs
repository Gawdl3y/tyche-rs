#![allow(clippy::tabs_in_doc_comments)]

//! A library for parsing, rolling, and explaining the results of tabletop dice. Dice modifiers use a similar syntax to
//! [FoundryVTT's modifiers].
//!
//! [FoundryVTT's modifiers]: https://foundryvtt.com/article/dice-modifiers/

#![warn(explicit_outlives_requirements)]
#![warn(missing_docs)]
#![deny(macro_use_extern_crate)]
#![deny(meta_variable_misuse)]
#![warn(missing_debug_implementations)]
#![deny(unit_bindings)]
#![warn(unreachable_pub)]
#![warn(unused_crate_dependencies)]
#![warn(unused_qualifications)]
#![warn(clippy::pedantic)]
#![warn(clippy::absolute_paths)]
#![warn(clippy::arithmetic_side_effects)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::cognitive_complexity)]
#![warn(clippy::empty_enum_variants_with_brackets)]
#![warn(clippy::empty_structs_with_brackets)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::expect_used)]
#![warn(clippy::filetype_is_file)]
#![warn(clippy::missing_const_for_fn)]
#![warn(clippy::fn_to_numeric_cast_any)]
#![warn(clippy::format_push_string)]
#![warn(clippy::get_unwrap)]
#![warn(clippy::if_then_some_else_none)]
#![warn(clippy::lossy_float_literal)]
#![warn(clippy::map_err_ignore)]
#![warn(clippy::missing_docs_in_private_items)]
#![warn(clippy::multiple_inherent_impl)]
#![warn(clippy::mutex_atomic)]
#![warn(clippy::panic_in_result_fn)]
#![warn(clippy::print_stderr)]
#![warn(clippy::print_stdout)]
#![warn(clippy::pub_without_shorthand)]
#![warn(clippy::rc_buffer)]
#![warn(clippy::rc_mutex)]
#![warn(clippy::redundant_type_annotations)]
#![warn(clippy::ref_patterns)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
#![warn(clippy::same_name_method)]
#![warn(clippy::self_named_module_files)]
#![warn(clippy::str_to_string)]
#![warn(clippy::string_to_string)]
#![warn(clippy::suspicious_xor_used_as_pow)]
#![warn(clippy::tests_outside_test_module)]
#![warn(clippy::try_err)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unnecessary_safety_comment)]
#![warn(clippy::unnecessary_safety_doc)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::unneeded_field_pattern)]
#![warn(clippy::unwrap_in_result)]
#![warn(clippy::unwrap_used)]
#![warn(clippy::verbose_file_reads)]

pub mod dice;
pub mod expr;

#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "parse")]
pub use parse::expr as parser;

#[cfg(test)]
mod tests;
