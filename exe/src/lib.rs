use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

// TODO[future]: docvim_parser::vimscript::Parser;
use docvim_parser::lua::Parser;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

#[derive(Debug)]
struct Options {
    debug: bool,
    directory: PathBuf,
    generate_output: bool,
    outfiles: Vec<PathBuf>,
    show_help: bool,
    show_version: bool,
    verbose: bool,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            debug: false,
            directory: Path::new(".").to_path_buf(),
            generate_output: true,
            outfiles: vec![],
            show_help: false,
            show_version: false,
            verbose: false,
        }
    }
}

#[derive(Clone, Debug)]
enum OptionsErrorKind {
    IllegalArgument,
    MissingRequiredArgument,
    UnrecognizedOption,
}

impl OptionsErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            OptionsErrorKind::IllegalArgument => "illegal argument",
            OptionsErrorKind::MissingRequiredArgument => "missing required argument",
            OptionsErrorKind::UnrecognizedOption => "unrecognized option",
        }
    }
}

#[derive(Debug, Clone)]
struct OptionsError {
    kind: OptionsErrorKind,
}

impl fmt::Display for OptionsError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}

impl Error for OptionsError {}

fn usage() {
    println!(
        r#"docvim - a documentation generator for Neovim and Vim plug-ins

Usage: docvim [-d|--debug] [-c|--directory DIRECTORY] [-v|--verbose] [OUTFILES...]
       docvim [-h|--help]
       docvim [--version]

Available options:
  OUTFILES...               Target file(s) for generated output (default: standard output)
  -c,--directory DIRECTORY  Change to DIRECTORY before processing (default: ".")
  -d,--debug                Print debug information during processing
  -h,--help                 Show this help text
  -v,--verbose              Be verbose during processing
  --version                 Print version information
"#
    );
}

fn version() {
    println!("docvim v{}", VERSION.unwrap_or("unknown"));
}

fn parse_args(args: Vec<String>) -> Result<Options, OptionsError> {
    let mut options = Options {
        ..Default::default()
    };
    let mut iter = args.iter();
    let mut seen_executable = false;
    while let Some(arg) = iter.next() {
        if !seen_executable {
            seen_executable = true;
            continue;
        }
        if arg == "-c" || arg == "--directory" {
            match iter.next() {
                Some(directory) => {
                    if directory.starts_with("-") {
                        return Err(OptionsError {
                            kind: OptionsErrorKind::IllegalArgument,
                        });
                    } else {
                        options.directory = Path::new(directory).to_path_buf();
                    }
                }
                None => {
                    return Err(OptionsError {
                        kind: OptionsErrorKind::MissingRequiredArgument,
                    });
                }
            };
        } else if arg == "-d" || arg == "--debug" {
            options.debug = true;
        } else if arg == "-h" || arg == "--help" {
            options.generate_output = false;
            options.show_help = true;
        } else if arg == "-v" || arg == "--verbose" {
            options.verbose = true;
        } else if arg == "--version" {
            options.generate_output = false;
            options.show_version = true;
        } else if arg.starts_with("-") {
            return Err(OptionsError {
                kind: OptionsErrorKind::UnrecognizedOption,
            });
        } else {
            options.outfiles.push(Path::new(arg).to_path_buf());
        }
    }
    Ok(options)
}

fn files(directory: PathBuf) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut paths = Vec::new();
    let mut queue = vec![directory];

    while queue.len() > 0 {
        let directory = queue.pop().unwrap();
        for entry in fs::read_dir(directory)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                queue.push(entry.path());
            } else if let Some(extension) = entry.path().extension() {
                if extension == "lua" {
                    paths.push(entry.path());
                }
            }
        }
    }

    Ok(paths)
}

pub fn run(args: Vec<String>) -> Result<(), Box<dyn Error>> {
    let options = parse_args(args)?;

    if options.show_help {
        usage();
    }

    if options.show_version {
        version();
    }

    if options.generate_output {
        for input in files(options.directory)? {
            // let input = "sample/init.lua";
            let contents = fs::read_to_string(input).expect("unable to read file");

            // println!("Text:\n{}", contents);

            let mut parser = Parser::new(&contents);

            // TODO: pretty print this error
            parser.parse().expect("Failed to parse");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn blinking_light() {
        assert!(true);
    }
}
