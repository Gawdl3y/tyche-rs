#![doc = pretty_readme::docify!("README.md", "https://docs.rs/tyche/latest/tyche/", "./")]
#![cfg_attr(not(any(doc, test)), no_std)]
#![expect(
	clippy::tabs_in_doc_comments,
	reason = "Consistency with source, user-configurability & accessibility"
)]
#![deny(macro_use_extern_crate, meta_variable_misuse, unit_bindings)]
#![warn(
	explicit_outlives_requirements,
	missing_docs,
	missing_debug_implementations,
	unreachable_pub,
	unused_crate_dependencies,
	unused_qualifications,
	clippy::pedantic,
	clippy::absolute_paths,
	clippy::alloc_instead_of_core,
	clippy::allow_attributes,
	clippy::allow_attributes_without_reason,
	clippy::arithmetic_side_effects,
	clippy::cfg_not_test,
	clippy::clone_on_ref_ptr,
	clippy::cognitive_complexity,
	clippy::dbg_macro,
	clippy::doc_include_without_cfg,
	clippy::empty_enum_variants_with_brackets,
	clippy::empty_structs_with_brackets,
	clippy::exhaustive_enums,
	clippy::exhaustive_structs,
	clippy::exit,
	clippy::expect_used,
	clippy::field_scoped_visibility_modifiers,
	clippy::filetype_is_file,
	clippy::fn_to_numeric_cast_any,
	clippy::get_unwrap,
	clippy::if_then_some_else_none,
	clippy::infinite_loop,
	clippy::lossy_float_literal,
	clippy::map_err_ignore,
	clippy::map_with_unused_argument_over_ranges,
	clippy::missing_const_for_fn,
	clippy::missing_docs_in_private_items,
	clippy::multiple_inherent_impl,
	clippy::mutex_atomic,
	clippy::needless_raw_strings,
	clippy::non_zero_suggestions,
	clippy::panic_in_result_fn,
	clippy::pathbuf_init_then_push,
	clippy::pointer_format,
	clippy::precedence_bits,
	clippy::print_stderr,
	clippy::print_stdout,
	clippy::pub_without_shorthand,
	clippy::rc_buffer,
	clippy::rc_mutex,
	clippy::redundant_test_prefix,
	clippy::redundant_type_annotations,
	clippy::ref_patterns,
	clippy::renamed_function_params,
	clippy::rest_pat_in_fully_bound_structs,
	clippy::return_and_then,
	clippy::same_name_method,
	clippy::self_named_module_files,
	clippy::semicolon_inside_block,
	clippy::std_instead_of_alloc,
	clippy::std_instead_of_core,
	clippy::str_to_string,
	clippy::string_lit_chars_any,
	clippy::suspicious_xor_used_as_pow,
	clippy::tests_outside_test_module,
	clippy::try_err,
	clippy::undocumented_unsafe_blocks,
	clippy::unnecessary_safety_comment,
	clippy::unnecessary_safety_doc,
	clippy::unnecessary_self_imports,
	clippy::unneeded_field_pattern,
	clippy::unused_result_ok,
	clippy::unwrap_in_result,
	clippy::unwrap_used,
	clippy::verbose_file_reads
)]

extern crate alloc;
extern crate core;

pub mod dice;
pub mod expr;
#[cfg(feature = "parse")]
pub mod parse;

pub use dice::Dice;
pub use expr::Expr;
#[cfg(feature = "parse")]
pub use parse::expr as parser;

#[cfg(test)]
mod tests;

#[cfg(feature = "build-binary")]
use ariadne as _;
