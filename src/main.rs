mod assemble;
mod parse;
use std::{fs::File, io::Read};

use parse::ast;

use combine::stream::position::SourcePosition;
use combine::EasyParser;
use combine::Parser;
use combine::{position, stream::position};

fn main() {
    let mut file = File::open("test.s").unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input);

    let res = parse::parse::program().easy_parse(position::Stream::new(input.as_str()));
    //print!("{:?}", res);
}
