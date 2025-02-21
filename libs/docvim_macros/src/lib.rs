use proc_macro::TokenStream;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

/// Convenience macro for checking all snapshots relative to the current package.
///
/// Brittle assumptions:
///
/// - Every usage of the macro contains a root-relative path attribute to find the snapshots
///   (eg. `#[check_snapshots("libs/docvim_parser/tests/snapshots")]`).
/// - Every usage of the macro is attached to a function named `transform`.
/// - Snapshots live in the specified directory or its subdirectories.
/// - There are no symlinks under this directory.
/// - Snapshots are named "$something.snap".
/// - File and directory names are well-formed (eg. valid Unicode) with no spaces(!!!).
///
#[proc_macro_attribute]
pub fn check_snapshots(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut base = std::env::current_dir().expect("Could not get current directory");
    base.push(attr.to_string());

    let mut tests = item.to_string();

    fn walk(dir: &PathBuf, base: &PathBuf, tests: &mut String) {
        for entry in fs::read_dir(dir).expect(&format!("Could not read directory {:?}", dir)) {
            let entry = entry.expect("Could not access file");
            let file_type = entry.file_type().expect("Could not get file type");
            if file_type.is_symlink() {
                panic!("Found symlink");
            } else if file_type.is_dir() {
                walk(&entry.path().to_path_buf(), &base, tests);
            } else {
                let snapshot = String::from(entry.path().to_str().expect("Invalid UTF-8 string"));
                let snapshot_name = String::from(
                    entry
                        .path()
                        .strip_prefix(base)
                        .expect("Base is not prefix of path")
                        .to_str()
                        .expect("Invalid UTF-8 string"),
                );
                let snapshot_name = snapshot_name.replace("/", "_");
                let snapshot_name = snapshot_name.trim_end_matches(".snap");
                // TODO: panic if snapshot name isn't a valid function identifier; the trouble is, have to wade
                // deep into Unicode to actually determine that - see:
                // https://doc.rust-lang.org/reference/identifiers.html

                tests.push_str("\n");
                tests.push_str("#[test]\n");
                tests.push_str(&format!(
                    "fn test_{}() -> Result<(), Box<dyn std::error::Error>> {{\n",
                    snapshot_name
                ));
                tests.push_str(&format!("  assert!(docvim_snapshot::check_snapshot(std::path::Path::new(r####\"{}\"####), &transform, false)?);\n", snapshot));
                tests.push_str("  Ok(())\n");
                tests.push_str("}\n");
            }
        }
    }

    walk(&base, &base, &mut tests);

    TokenStream::from_str(&tests).expect("Could not generate token stream")
}
