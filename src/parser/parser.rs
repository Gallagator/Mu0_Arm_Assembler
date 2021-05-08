use nom::{self, IResult};

use super::ast::*;

/* A parser can have any output type T. It has a &str for input and for output
 * message. The error also contains a line number */
type ParseResult<T> = IResult<&str, T, (&str, u32)>; 

impl Ast {
    
    fn reg(input: &str) -> ParseResult<Reg> {
        
    }

}
