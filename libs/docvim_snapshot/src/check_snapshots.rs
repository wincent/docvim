use proc_macro::TokenStream;
use std::fs;
use std::str::FromStr;

/// Convenience macro for checking all snapshots relative to the current package.
///
/// Brittle assumptions:
///
/// - Every usage of the macro contains a package name attribute (eg. `#[check_snapshots(docvim_parser)]`).
/// - Every usage of the macro is attached to a function named `transform`.
/// - Snapshots live at "test/snapshots".
/// - Snapshots are named "$something.shap".
/// - Filenames are well-formed (eg. valid Unicode) with no spaces(!!!).
///
#[proc_macro_attribute]
pub fn check_snapshots(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut base = std::env::current_dir().expect("Could not get current directory");
    base.push("libs");
    base.push(attr.to_string());
    base.push("tests/snapshots");

    let mut tests = item.to_string();
    for entry in fs::read_dir(base).expect(&format!("Could not read directory {}", std::env!("CARGO_MANIFEST_DIR"))) {
        let entry = entry.expect("Could not read file");
        let snapshot = String::from(entry.path().to_str().unwrap());
        let snapshot_name = String::from(entry.path().file_stem().unwrap().to_str().unwrap());

        tests.push_str("\n");
        tests.push_str("#[test]\n");
        tests.push_str(&format!("fn test_{}() -> Result<(), Box<dyn std::error::Error>> {{\n", snapshot_name));
        tests.push_str(&format!("  assert!(docvim_snapshot::check_snapshot(\"{}\", &transform));\n", snapshot));
        tests.push_str("  Ok(())\n");
        tests.push_str("}\n");
    }

    TokenStream::from_str(&tests).expect("Could not generate token stream")
}
