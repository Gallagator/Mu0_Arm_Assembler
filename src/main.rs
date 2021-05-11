mod parse;
use parse::ast;

use combine::{position, stream::position};
use combine::EasyParser;
use combine::stream::position::SourcePosition;
use combine::Parser;

const INPUT: &str = ".label1
                     STP 
                     JMP $736  
                     JMI .label1 
                     JEQ .helooTHer 
                     ADD R2, $12 ROR $213
                     SUB R0, R0 LSL $13   
                     ADC R2, R3 ASR $12 
                     LDR R0  , [R3, $123]! 
                     STR R2, [R1], $213 
                     .label43";

fn main() {
    if let res = parse::parse::program().easy_parse(position::Stream::new(INPUT))
    {
        println!("{:?}", res);
    }
}
