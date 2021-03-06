#![allow(dead_code)]
mod assembler;

#[cfg(test)]
mod tests {
    use super::assembler::assemble::SemanticError;
    use super::assembler::ast::*;

    use std::collections::HashMap;

    use combine::stream::position;
    use combine::EasyParser;

    use super::assembler::assemble::Encode;

    #[test]
    fn test_jump_assemble() {
        let test_vec = vec![
            ("JMI $128", 0x5080, None),
            (
                "JEQ $4096",
                0,
                Some(SemanticError::IMMEDIATEOVERSIZE((1, 10))),
            ),
            ("JMP $4095", 0x4FFF, None),
            ("STP", 0x7000, None),
            ("ADD R0, $12 ROR $8", 0x838C, None),
            ("SUB R3, R2 ASR $15", 0x9DFA, None),
            ("TST R3, R3 ROR $15", 0xFCFF, None),
            ("LDR R2, [R3]", 0x0AC3, None),
            ("LDR R3, [R3, $15]!", 0x0FFF, None),
            ("STR R0, [R0]", 0x00C0, None),
            ("STR R3, [R3], -$10", 0x0D2B, None),
        ];

        test_vec
            .iter()
            .for_each(|(instr, res, err)| test_assemble(instr, *res, err));
    }

    fn test_assemble(instruction: &str, encoding: u16, err: &Option<SemanticError>) {
        println!("testing: {}", instruction);
        let mut prog = super::assembler::parse::program()
            .easy_parse(position::Stream::new(instruction))
            .unwrap()
            .0;
        if let Statement::Instruction(instr) = &mut prog.statements[0] {
            match Encode::instruction(instr, &HashMap::new()) {
                Ok(instr) => assert_eq!(instr, encoding),
                Err(e) => assert_eq!(&Some(e), err),
            }
        }
    }
}
