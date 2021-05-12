/* File for parsing. Check ../../bnf.txt for more information */

use combine::{
    attempt, between, choice, from_str, many, many1, optional,
    parser::char::{alpha_num, digit, letter, spaces},
    satisfy, skip_count, skip_many, token, tokens, ParseError, Parser, Stream,
};

use std::str::Chars;

use super::ast::*;

fn spaces_or_tabs<Input>() -> impl Parser<Input, Output = ()>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    skip_many(satisfy(|c| c == ' ' || c == '\t'))
}

fn tag<Input>(tok: &'static str) -> impl Parser<Input, Output = Chars>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    tokens(|l, r| l.eq(&r), tok, tok.chars())
}

fn space_tag<Input>(tok: &'static str) -> impl Parser<Input, Output = Chars>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(
        spaces_or_tabs::<Input>(),
        spaces_or_tabs::<Input>(),
        tag::<Input>(tok),
    )
}

fn space_token<Input>(c: char) -> impl Parser<Input, Output = char>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        spaces_or_tabs::<Input>(),
        token(c),
        spaces_or_tabs::<Input>(),
    )
        .map(|(_, tok, _)| tok)
}

fn reg<Input>() -> impl Parser<Input, Output = Reg>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let r0_parser = attempt(space_tag("R0").map(|_| Reg::R0));
    let r1_parser = attempt(space_tag("R1").map(|_| Reg::R1));
    let r2_parser = attempt(space_tag("R2").map(|_| Reg::R2));
    let r3_parser = attempt(space_tag("R3").map(|_| Reg::R3));
    choice((r0_parser, r1_parser, r2_parser, r3_parser))
}

fn shift<Input>() -> impl Parser<Input, Output = Shift>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let lsl_parser = attempt(space_tag("LSL").map(|_| Shift::LSL));
    let lsr_parser = attempt(space_tag("LSR").map(|_| Shift::LSR));
    let asr_parser = attempt(space_tag("ASR").map(|_| Shift::ASR));
    let ror_parser = attempt(space_tag("ROR").map(|_| Shift::ROR));
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

fn jumpable<Input>() -> impl Parser<Input, Output = Jumpable>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let int_lit_parser = int_lit().map(|num| Jumpable::Absolute(num as u16, (0, 0)));
    let label_parser = label().map(|lab| Jumpable::Label(lab, (0, 0)));
    choice((int_lit_parser, label_parser))
}

fn op2<Input>() -> impl Parser<Input, Output = Op2>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let immediate_parser =
        (int_lit(), space_tag("ROR"), int_lit()).map(|(k, _, b)| Op2::Immediate(k, b, (0, 0)));
    let shifted_reg_parser = (reg::<Input>(), shift(), int_lit())
        .map(|(reg, shift, num)| Op2::ShifedReg(reg, shift, num, (0, 0)));
    choice((immediate_parser, shifted_reg_parser))
}

pub fn index<Input>() -> impl Parser<Input, Output = AsmIndex>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    /* looks for a reg inside [] optionally with an int lit inside*/
    let pre_write = (
        between(
            space_token::<Input>('['),
            space_token::<Input>(']'),
            (
                reg(),
                optional((skip_count(1, space_token(',')), int_lit()).map(|(_, n)| n)),
            ),
        ),
        optional(space_token('!')),
    )
        .map(|((reg, offs), is_write_back)| {
            let itype = is_write_back.map_or(IndexType::PRE, |_| IndexType::PREWRITE);
            (reg, offs.unwrap_or(0), itype, (0, 0))
        });
    /* Am very disapointed that I couldn't include the
     * '['〈reg〉']' ','〈int_lit〉rule in the above parser ;'(*/
    let post_write = (
        between(space_token::<Input>('['), space_token::<Input>(']'), reg()),
        (skip_count(1, space_token(',')), int_lit()).map(|(_, n)| n),
    )
        .map(|(reg, offs)| (reg, offs, IndexType::POSTWRITE, (0, 0)));
    choice((attempt(post_write), pre_write))
}

pub fn instr<Input>() -> impl Parser<Input, Output = Instruction>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let jmp = (space_tag("JMP"), jumpable()).map(|instr| Instruction::JMP(instr.1));
    let jmi = (space_tag("JMI"), jumpable()).map(|instr| Instruction::JMI(instr.1));
    let jeq = (space_tag("JEQ"), jumpable()).map(|instr| Instruction::JEQ(instr.1));
    let stp = space_tag::<Input>("STP").map(|instr| Instruction::STP);

    let add = (
        space_tag::<Input>("ADD"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::ADD(reg, op));
    let sub = (
        space_tag::<Input>("SUB"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::SUB(reg, op));

    let adc = (
        space_tag::<Input>("ADC"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::ADC(reg, op));

    let sbc = (
        space_tag::<Input>("SBC"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::SBC(reg, op));
    let mov = (
        space_tag::<Input>("MOV"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::MOV(reg, op));

    let cmp = (
        space_tag::<Input>("CMP"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::CMP(reg, op));

    let and = (
        space_tag::<Input>("AND"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::AND(reg, op));

    let tst = (
        space_tag::<Input>("TST"),
        reg::<Input>(),
        space_token::<Input>(','),
        op2::<Input>(),
    )
        .map(|(_, reg, _, op)| Instruction::TST(reg, op));

    let ldr = (
        space_tag::<Input>("LDR"),
        reg::<Input>(),
        space_token::<Input>(','),
        index(),
    )
        .map(|(_, reg, _, index)| Instruction::LDR(reg, index));

    let store = (
        space_tag::<Input>("STR"),
        reg::<Input>(),
        space_token::<Input>(','),
        index(),
    )
        .map(|(_, reg, _, index)| Instruction::STR(reg, index));

    choice((
        attempt(jmp),
        attempt(jmi),
        attempt(jeq),
        attempt(stp),
        attempt(add),
        attempt(sub),
        attempt(adc),
        attempt(sbc),
        attempt(mov),
        attempt(cmp),
        attempt(and),
        attempt(tst),
        attempt(ldr),
        attempt(store),
    ))
}

pub fn program<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let statement = (
        spaces::<Input>(),
        choice((
            instr().map(|instr| Statement::Instruction(instr)),
            label().map(|lab| Statement::Label(lab)),
        )),
    )
        .map(|(_, stat)| stat);
    many::<Vec<_>, _, _>(attempt(statement)).map(|stats| Ast { statements: stats })
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::stream::position;
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
    fn test_space_token() {
        match space_token('h').easy_parse("   h  ") {
            Ok((tok, _)) => assert_eq!(tok, 'h'),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn test_space_tag() {
        match space_tag("hdwk").easy_parse("   hdwk  ") {
            Ok((tag, _)) => assert_eq!(tag.as_str(), "hdwk"),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn test_reg() {
        match reg().easy_parse("R0") {
            Ok((reg, _)) => assert_eq!(reg, Reg::R0),
            Err(_) => assert!(false),
        }
        match reg().easy_parse("R1") {
            Ok((reg, _)) => assert_eq!(reg, Reg::R1),
            Err(_) => assert!(false),
        }
        match reg().easy_parse("R2") {
            Ok((reg, _)) => assert_eq!(reg, Reg::R2),
            Err(_) => assert!(false),
        }
        match reg().easy_parse("R3") {
            Ok((reg, _)) => assert_eq!(reg, Reg::R3),
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
            Ok((shift, _)) => assert_eq!(shift, Shift::LSL),
            Err(_) => assert!(false),
        }
        match shift().easy_parse("LSR") {
            Ok((shift, _)) => assert_eq!(shift, Shift::LSR),
            Err(_) => assert!(false),
        }
        match shift().easy_parse("ASR") {
            Ok((shift, _)) => assert_eq!(shift, Shift::ASR),
            Err(_) => assert!(false),
        }
        match shift().easy_parse("ROR") {
            Ok((shift, _)) => assert_eq!(shift, Shift::ROR),
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
            Ok((lab, _)) => assert_eq!(lab, String::from("hello")),
            Err(_) => assert!(false),
        }

        match label().easy_parse("._1_la2_52d_4s21j5v2ns_2n3sh_3917d_ncj224b") {
            Ok((lab, _)) => assert_eq!(
                lab,
                String::from("_1_la2_52d_4s21j5v2ns_2n3sh_3917d_ncj224b")
            ),
            Err(_) => assert!(false),
        }

        match label().easy_parse(".a") {
            Ok((lab, _)) => assert_eq!(lab, String::from("a")),
            Err(_) => assert!(false),
        }

        match label().easy_parse("._") {
            Ok((lab, _)) => assert_eq!(lab, String::from("_")),
            Err(_) => assert!(false),
        }

        match label().easy_parse(".9") {
            Ok(_) => assert!(false),
            Err(_) => {}
        }

        match label().easy_parse(".this_is_a_very_long_label21343_dhe3423b") {
            Ok((lab, _)) => {
                assert_eq!(lab, String::from("this_is_a_very_long_label21343_dhe3423b"))
            }
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

    #[test]
    fn test_jumpable() {
        match jumpable()
            .easy_parse(position::Stream::new(".hello"))
            .unwrap()
            .0
        {
            Jumpable::Absolute(_, _) => assert!(false),
            Jumpable::Label(lab, _) => assert_eq!(lab, String::from("hello")),
        }
        match jumpable()
            .easy_parse(position::Stream::new(".hel_lo"))
            .unwrap()
            .0
        {
            Jumpable::Absolute(_, _) => assert!(false),
            Jumpable::Label(lab, _) => assert_eq!(lab, String::from("hel_lo")),
        }
        match jumpable()
            .easy_parse(position::Stream::new("$1324"))
            .unwrap()
            .0
        {
            Jumpable::Absolute(num, _) => assert_eq!(num, 1324),
            Jumpable::Label(_, _) => assert!(false),
        }
    }

    #[test]
    fn test_op2() {
        let _ = op2()
            .easy_parse(position::Stream::new("R0               ROR   $123"))
            .map(|(op, _)| {
                assert_eq!(Op2::ShifedReg(Reg::R0, Shift::ROR, 123, (0, 0)), op);
            })
            .map_err(|_| assert!(false));
        let _ = op2()
            .easy_parse(position::Stream::new(
                "  \n R0 \n  \n            ROR   $123",
            ))
            .map(|_| assert!(false));
        let _ = op2()
            .easy_parse(position::Stream::new("$123 ROR        $12      "))
            .map(|(op, _)| {
                assert_eq!(Op2::Immediate(123, 12, (0, 0)), op);
            })
            .map_err(|_| assert!(false));
    }

    #[test]
    fn test_index() {
        match index().easy_parse("[     R2  ]") {
            Ok((index, _)) => assert_eq!(index, (Reg::R2, 0, IndexType::PRE, (0, 0))),
            Err(_) => assert!(false),
        }
        match index().easy_parse("[     R1  ,    $3262   ]") {
            Ok((index, _)) => assert_eq!(index, (Reg::R1, 3262, IndexType::PRE, (0, 0))),
            Err(_) => assert!(false),
        }
        match index().easy_parse("[     R3  ,    -$62   ]!") {
            Ok((index, _)) => assert_eq!(index, (Reg::R3, -62, IndexType::PREWRITE, (0, 0))),
            Err(_) => assert!(false),
        }
        match index().easy_parse("[     R1             ], $0") {
            Ok((index, _)) => assert_eq!(index, (Reg::R1, 0, IndexType::POSTWRITE, (0, 0))),
            Err(_) => assert!(false),
        }
        match index().easy_parse("[     R1             ], $123") {
            Ok((index, _)) => assert_eq!(index, (Reg::R1, 123, IndexType::POSTWRITE, (0, 0))),
            Err(_) => assert!(false),
        }
        match index().easy_parse("[     R4  ,    $3262   ]") {
            Ok((_, _)) => assert!(false),
            Err(_) => {}
        }
    }

    #[test]
    fn test_instr() {
        match instr().easy_parse("JMP $23") {
            Ok((Instruction::JMP(jumpable), _)) => {
                assert_eq!(jumpable, Jumpable::Absolute(23, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("JMP .hello_there21 \n") {
            Ok((Instruction::JMP(jumpable), _)) => assert_eq!(
                jumpable,
                Jumpable::Label(String::from("hello_there21"), (0, 0))
            ),
            _ => assert!(false),
        }
        match instr().easy_parse("JMP .9hello_there21 \n") {
            Ok(_) => assert!(false),
            _ => {}
        }

        match instr().easy_parse("JMI $23") {
            Ok((Instruction::JMI(jumpable), _)) => {
                assert_eq!(jumpable, Jumpable::Absolute(23, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("JEQ .hello_there21 \n") {
            Ok((Instruction::JEQ(jumpable), _)) => assert_eq!(
                jumpable,
                Jumpable::Label(String::from("hello_there21"), (0, 0))
            ),
            _ => assert!(false),
        }
        match instr().easy_parse("STP \n") {
            Ok((Instruction::STP, _)) => {}
            _ => assert!(false),
        }

        match instr().easy_parse("ADD R3, R1 ASR $12") {
            Ok((Instruction::ADD(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("ADD R3, $13 ROR $24") {
            Ok((Instruction::ADD(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::Immediate(13, 24, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("SUB R3, R1 ASR $12") {
            Ok((Instruction::SUB(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("SUB R3, $13 ROR $24") {
            Ok((Instruction::SUB(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::Immediate(13, 24, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("ADC R3, R1 ASR $12") {
            Ok((Instruction::ADC(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("ADC R3, $13 ROR $24") {
            Ok((Instruction::ADC(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::Immediate(13, 24, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("SBC R3, R1 ASR $12") {
            Ok((Instruction::SBC(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("MOV R3, $13 ROR $24") {
            Ok((Instruction::MOV(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::Immediate(13, 24, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("CMP R3, R1 ASR $12") {
            Ok((Instruction::CMP(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }
        match instr().easy_parse("AND R3, $13 ROR $24") {
            Ok((Instruction::AND(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::Immediate(13, 24, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("TST R3, R1 ASR $12") {
            Ok((Instruction::TST(Reg::R3, op), _)) => {
                assert_eq!(op, Op2::ShifedReg(Reg::R1, Shift::ASR, 12, (0, 0)))
            }
            _ => assert!(false),
        }

        match instr().easy_parse("LDR R1  , [R2]") {
            Ok((Instruction::LDR(Reg::R1, (Reg::R2, 0, IndexType::PRE, _)), _)) => {}
            _ => assert!(false),
        }

        match instr().easy_parse("LDR R2  , [R0, $1245]") {
            Ok((Instruction::LDR(Reg::R2, (Reg::R0, 1245, IndexType::PRE, _)), _)) => {}
            _ => assert!(false),
        }
        match instr().easy_parse("LDR R1 , [R3, $9378]!") {
            Ok((Instruction::LDR(Reg::R1, (Reg::R3, 9378, IndexType::PREWRITE, _)), _)) => {}
            _ => assert!(false),
        }

        match instr().easy_parse("LDR R0 , [R2], $1234") {
            Ok((Instruction::LDR(Reg::R0, (Reg::R2, 1234, IndexType::POSTWRITE, _)), _)) => {}
            _ => assert!(false),
        }
        match instr().easy_parse("STR R1  , [R2]") {
            Ok((Instruction::STR(Reg::R1, (Reg::R2, 0, IndexType::PRE, _)), _)) => {}
            _ => assert!(false),
        }

        match instr().easy_parse("STR R2  , [R0, $1245]") {
            Ok((Instruction::STR(Reg::R2, (Reg::R0, 1245, IndexType::PRE, _)), _)) => {}
            _ => assert!(false),
        }
        match instr().easy_parse("STR R2 , [R3, $9378]!") {
            Ok((Instruction::STR(Reg::R2, (Reg::R3, 9378, IndexType::PREWRITE, _)), _)) => {}
            _ => assert!(false),
        }

        match instr().easy_parse("STR R0 , [R2], $10234") {
            Ok((Instruction::STR(Reg::R0, (Reg::R2, 10234, IndexType::POSTWRITE, _)), _)) => {}
            _ => assert!(false),
        }
    }
}
