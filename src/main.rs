mod assemble;
mod parse;

use std::env;

fn main() {
    let args: Vec<_> = env::args().collect(); 
    assemble::assemble_to_file(&args[1].to_owned()[..], &args[2][..]);
}
