use nom::{self, IResult};

use super::ast::*;

/* A parser can have any output type T. It has a &str for input and for output
 * message*/
type ParseResult<T> = IResult<&str, T, &str>; 

impl Ast {
    
    fn reg(input: &str) {
        
    }
}
