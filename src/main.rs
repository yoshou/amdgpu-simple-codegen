use std::env;
use std::fs;
use std::io::Read;
use std::path;

use amdgpu_simple_codegen::bitcode::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Missing input");
        return;
    }

    let path = path::Path::new(&args[1]);
    let mut file = match fs::File::open(&path) {
        Ok(f) => f,
        Err(err) => panic!("File error: {}", err),
    };

    let mut data = Vec::<u8>::new();
    file.read_to_end(&mut data).ok();

    if !is_bitcode(&data) {
        panic!("Invalid format")
    }

    let bitcode = Bitcode::new(&data);
    let module = match bitcode.decode() {
        Ok(value) => value,
        Err(err) => panic!("Decode bitcode error: {}", err.message),
    };
}
