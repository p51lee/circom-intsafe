extern crate num_bigint_dig as num_bigint;
use interval_analysis::analysis::IntervalAnalyzer;
use interval_analysis::analysis_representation::ALStmt;
use num_bigint::BigInt;
use parser::parser_logic::parse_file;
use program_structure::ast::{Definition::*, AST};
use program_structure::file_definition::FileLibrary;
use std::env;
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    // Collect the command-line arguments into a vector.
    let args: Vec<String> = env::args().collect();

    // Variables to store the -p value and file path
    let mut p_value: Option<i64> = None; // Use i64 for larger numbers
    let mut file_path: Option<String> = None;

    // Iterate over the arguments and parse them.
    let mut i = 1; // Start after the program name
    while i < args.len() {
        if args[i] == "-p" {
            // Ensure there's a value after -p
            if i + 1 >= args.len() {
                eprintln!("Error: -p option requires a value.");
                std::process::exit(1);
            }

            // Try to parse the value following -p
            match args[i + 1].parse::<i64>() {
                // Parse as i64
                Ok(num) => p_value = Some(num),
                Err(_) => {
                    eprintln!("Error: '{}' is not a valid number.", args[i + 1]);
                    std::process::exit(1);
                }
            }

            // Skip the value we just processed
            i += 2;
        } else {
            // If it's not a -p option, it's the file path
            file_path = Some(args[i].clone());
            i += 1;
        }
    }

    // Ensure we have a file path
    if file_path.is_none() {
        eprintln!("Error: File path is required.");
        eprintln!("Usage: {} [-p <prime number>] <file_path>", args[0]);
        std::process::exit(1);
    }

    // Ensure the -p option is provided
    if p_value.is_none() {
        eprintln!("Error: -p option is mandatory.");
        eprintln!("Usage: {} -p <prime number> <file_path>", args[0]);
        std::process::exit(1);
    }

    // Extract the file path and p_value (if present)
    let file_path = file_path.unwrap();
    let prime = p_value.unwrap();

    // Open the file.
    let mut file = File::open(&file_path)?;

    // Read the file contents into a string.
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut file_library = FileLibrary::new();
    let file_id = file_library.add_file(file_path.to_string(), contents.clone());
    let parsed_opt = parse_file(&contents, file_id).ok();

    if let Some(AST {
        custom_gates,
        custom_gates_declared,
        includes,
        definitions,
        ..
    }) = parsed_opt
    {
        if custom_gates || custom_gates_declared {
            eprintln!("Custom gates are not supported.");
        } else if !includes.is_empty() {
            eprintln!("Includes are not supported.");
        } else if let [template] = definitions.as_slice() {
            match template {
                Template {
                    name,
                    args,
                    arg_location,
                    body,
                    parallel,
                    is_custom_gate,
                    ..
                } => {
                    println!("Template: {}", name);
                    println!("Args: {:?}", args);
                    // println!("Arg location: {:?}", arg_location);
                    // println!("Body: {:#?}", body);
                    // println!("Parallel: {:?}", parallel);
                    // println!("Is custom gate: {:?}", is_custom_gate);
                    let al_stmt = ALStmt::from_stmt(body);
                    println!("{:#?}", al_stmt);
                    println!();
                    let mut analyzer =
                        IntervalAnalyzer::new(al_stmt, args.clone(), BigInt::from(prime));
                    analyzer.widen();
                    analyzer.narrow();
                    analyzer.check_all();
                    // println!("{:#?}", analyzer.table);
                    // for m in al_stmt.all_metas() {
                    //     println!("{:#?}", m);
                    // }
                    // for s in al_stmt.all_instrs() {
                    //     println!("{:#?}: {:?}", s.meta(), s);
                    // }
                }
                _ => eprintln!("Only accept templates."),
            }
        } else {
            eprintln!("Only accept one template.");
        }
        std::process::exit(1);
    } else {
        eprintln!("Error parsing the file.");
        std::process::exit(1);
    }
}
