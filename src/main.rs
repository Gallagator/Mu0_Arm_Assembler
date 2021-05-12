mod assembler;

use std::env;

use assembler::assemble;

fn main() {
    let args: Vec<_> = env::args().collect(); 
    assemble::assemble_to_file(&args[1].to_owned()[..], &args[2][..]);
}
