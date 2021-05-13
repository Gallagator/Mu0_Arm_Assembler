mod assembler;

use std::env;

use assembler::assemble;

fn main() {

    let args: Vec<_> = env::args().collect();
    if args.len() < 3 {
        println!(
            "Please provide an input assembly file as well as an output file.
Eg: ./mu0_arm_assembler asm.s out.txt"
        );
    }
    else {
        assemble::assemble_to_file(&args[1].to_owned()[..], &args[2][..]);
    }
}
