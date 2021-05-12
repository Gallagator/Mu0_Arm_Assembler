mod assemble;
mod parse;

fn main() {
    assemble::assemble_to_file("test.s");
}
