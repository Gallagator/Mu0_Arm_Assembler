/* File for parsing. Check ../../bnf.txt for more information */

use combine::{
    attempt, choice, from_str, many, many1, optional,
    parser::char::{alpha_num, digit, hex_digit, letter},
    token, tokens, ParseError, Parser, Stream,
};
use std::str::Chars;

use super::ast::*;

fn tag<Input>(tok: &'static str) -> impl Parser<Input, Output = Chars>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    tokens(|l, r| l.eq(&r), tok, tok.chars())
}

fn reg<Input>() -> impl Parser<Input, Output = Box<Reg>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let r0_parser = attempt(tag("R0").map(|_| Box::new(Reg::R0)));
    let r1_parser = attempt(tag("R1").map(|_| Box::new(Reg::R1)));
    let r2_parser = attempt(tag("R2").map(|_| Box::new(Reg::R2)));
    let r3_parser = attempt(tag("R3").map(|_| Box::new(Reg::R3)));
    choice((r0_parser, r1_parser, r2_parser, r3_parser))
}

fn shift<Input>() -> impl Parser<Input, Output = Box<Shift>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let lsl_parser = attempt(tag("LSL").map(|_| Box::new(Shift::LSL)));
    let lsr_parser = attempt(tag("LSR").map(|_| Box::new(Shift::LSR)));
    let asr_parser = attempt(tag("ASR").map(|_| Box::new(Shift::ASR)));
    let ror_parser = attempt(tag("ROR").map(|_| Box::new(Shift::ROR)));
    choice((lsl_parser, lsr_parser, asr_parser, ror_parser))
}

fn label<Input>() -> impl Parser<Input, Output = Label>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        token('.'),
        choice((letter(), token('_'))),
        many(choice((alpha_num(), token('_')))),
    )
        .map(|(_, start, rest): (_, char, String)| {
            let mut label = rest.clone();
            label.insert(0, start);
            label
        })
}

/* TODO parse hexadecimal numbers */
fn int_lit<Input>() -> impl Parser<Input, Output = i16>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        optional(token('-')),
        token('$'),
        from_str(many1::<String, _, _>(digit())),
    )
        .map(|(neg, _, num): (Option<_>, _, i16)| if let None = neg { num } else { -num })
    //(
    //    tag("0x"),
    //    many1::<String, _, _>(hex_digit()),
    //).map(|(_, hex)| i16::from_str_radix(&hex[..], 16).())
}

use combine::EasyParser;

#[test]
fn test_tag() {
    if let Err(_) = tag("ROR").easy_parse("ROR") {
        assert!(false);
    }

    if let Ok(_) = tag("ROR").easy_parse("RO_R") {
        assert!(false);
    }
}

#[test]
fn test_reg() {
    match reg().easy_parse("R0") {
        Ok((reg, _)) => assert_eq!(reg, Box::new(Reg::R0)),
        Err(_) => assert!(false),
    }
    match reg().easy_parse("R1") {
        Ok((reg, _)) => assert_eq!(reg, Box::new(Reg::R1)),
        Err(_) => assert!(false),
    }
    match reg().easy_parse("R2") {
        Ok((reg, _)) => assert_eq!(reg, Box::new(Reg::R2)),
        Err(_) => assert!(false),
    }
    match reg().easy_parse("R3") {
        Ok((reg, _)) => assert_eq!(reg, Box::new(Reg::R3)),
        Err(_) => assert!(false),
    }
    match reg().easy_parse("r0") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }
}

#[test]
fn test_shift() {
    match shift().easy_parse("LSL") {
        Ok((shift, _)) => assert_eq!(shift, Box::new(Shift::LSL)),
        Err(_) => assert!(false),
    }
    match shift().easy_parse("LSR") {
        Ok((shift, _)) => assert_eq!(shift, Box::new(Shift::LSR)),
        Err(_) => assert!(false),
    }
    match shift().easy_parse("ASR") {
        Ok((shift, _)) => assert_eq!(shift, Box::new(Shift::ASR)),
        Err(_) => assert!(false),
    }
    match shift().easy_parse("ROR") {
        Ok((shift, _)) => assert_eq!(shift, Box::new(Shift::ROR)),
        Err(_) => assert!(false),
    }
    match shift().easy_parse("lsl") {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
    }
}

#[test]
fn test_label() {
    match label().easy_parse(".hello") {
        Ok((lab, _)) => assert_eq!(lab, "hello"),
        Err(_) => assert!(false),
    }

    match label().easy_parse("._1_la2_52d_4s21j5v2ns_2n3sh_3917d_ncj224b") {
        Ok((lab, _)) => assert_eq!(lab, "_1_la2_52d_4s21j5v2ns_2n3sh_3917d_ncj224b"),
        Err(_) => assert!(false),
    }

    match label().easy_parse(".a") {
        Ok((lab, _)) => assert_eq!(lab, "a"),
        Err(_) => assert!(false),
    }

    match label().easy_parse("._") {
        Ok((lab, _)) => assert_eq!(lab, "_"),
        Err(_) => assert!(false),
    }

    match label().easy_parse(".9") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }

    match label().easy_parse(".this_is_a_very_long_label21343_dhe3423b") {
        Ok((lab, _)) => assert_eq!(lab, "this_is_a_very_long_label21343_dhe3423b"),
        Err(_) => assert!(false),
    }

    match label().easy_parse("this isnt a label") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }

    match label().easy_parse(". hello no label here!") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }
}

#[test]
fn test_int_lit() {
    match int_lit().easy_parse("$1234") {
        Ok((num, _)) => assert_eq!(num, 1234i16),
        Err(_) => assert!(false),
    }
    match int_lit().easy_parse("-$1234") {
        Ok((num, _)) => assert_eq!(num, -1234i16),
        Err(_) => assert!(false),
    }
    /* Overflow */
    match int_lit().easy_parse("$1234343298473984") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }

    match int_lit().easy_parse("-$1234343298473984") {
        Ok(_) => assert!(false),
        Err(_) => {}
    }
}
