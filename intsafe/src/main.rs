use interval_analysis::analysis_representation::ALStmt;
use parser::parser_logic::parse_file;
use program_structure::ast::{Definition::*, AST};
use program_structure::file_definition::FileLibrary;
use std::env;
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    // Collect the command-line arguments into a vector.
    let args: Vec<String> = env::args().collect();

    // Check if the correct number of arguments is provided.
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }

    // Get the file path from the arguments.
    let file_path = &args[1];

    // Open the file.
    let mut file = File::open(file_path)?;

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
                    // println!("Template: {}", name);
                    // println!("Args: {:?}", args);
                    // println!("Arg location: {:?}", arg_location);
                    // println!("Body: {:#?}", body);
                    // println!("Parallel: {:?}", parallel);
                    // println!("Is custom gate: {:?}", is_custom_gate);
                    let al_stmt = ALStmt::from_stmt(body);
                    println!("{:#?}", al_stmt);
                    // println!();
                    // println!();
                    // println!();
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
