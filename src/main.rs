mod parse;
use parse::ast;

use combine::{position, stream::position};
use combine::EasyParser;
use combine::stream::position::SourcePosition;
use combine::Parser;

fn main() {
    let res = parse::parse::op2().easy_parse(position::Stream::new("R0               ROR   $123"));
    println!("{:?}", res);
}
