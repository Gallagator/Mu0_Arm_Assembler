use std::collections::HashMap;

use super::parse::ast::*;

const LSL: u16 = 0b00 << 2;
const LSR: u16 = 0b01 << 2;
const ASR: u16 = 0b10 << 2;
const ROR: u16 = 0b11 << 2;

const DPR: u16 = 0b1 << 15;

const S_BIT: u16 = 0b1 << 8;
const C_BIT: u16 = 0b1 << 9;

const MAX_JMP: u16 = (0b1 << 12) - 1;

pub struct encode {
    val: u16,
}

impl encode {
    /* dpr instruction encoding functions */
    fn set_shift(&mut self, shift: Shift) {
        self.val |= match shift {
            Shift::LSL => LSL,
            Shift::LSR => LSR,
            Shift::ASR => ASR,
            Shift::ROR => ROR,
        }
    }

    fn set_b(&mut self, b: u16) {
        self.val |= b << 4
    }

    fn set_s(&mut self) {
        self.val |= S_BIT
    }

    fn set_c(&mut self) {
        self.val |= C_BIT
    }

    fn set_k(&mut self, k: u16) {
        self.val |= k
    }

    fn set_rot(&mut self, rot: u16) {
        self.val |= rot << 4
    }

    fn set_op(&mut self, operation: &Instruction) {
        let op_val = match *operation {
            Instruction::JMP(_) => 0b100,
            Instruction::JMI(_) => 0b101,
            Instruction::JEQ(_) => 0b110,
            Instruction::STP => 0b111,
            Instruction::ADD(_, _) => 0b000,
            Instruction::SUB(_, _) => 0b001,
            Instruction::ADC(_, _) => 0b010,
            Instruction::SBC(_, _) => 0b011,
            Instruction::MOV(_, _) => 0b100,
            Instruction::CMP(_, _) => 0b101,
            Instruction::AND(_, _) => 0b110,
            Instruction::TST(_, _) => 0b111,
            _ => {
                debug_assert!(false);
                0b000
            }
        };
        self.val |= op_val << 12;
    }

    fn set_dpr(&mut self) {
        self.val |= DPR;
    }

    /*  load/store encoding functions */
    fn set_u(&mut self) {
        self.val |= 1 << 6
    }

    fn set_p(&mut self) {
        self.val |= 1 << 7
    }

    fn set_w(&mut self) {
        self.val |= 1 << 8
    }

    fn set_l(&mut self) {
        self.val |= 1 << 9
    }

    fn set_n(&mut self, n: u16) {
        self.val |= n << 2
    }

    /* common */
    fn set_d(&mut self, d: u16) {
        self.val |= d << 10
    }

    fn set_m(&mut self, m: u16) {
        self.val |= m
    }

    pub fn instruction( instr: &mut Instruction, label_map: HashMap<&str, u16>,) 
        -> Result<u16, SemanticError> {
        let mut encoding = encode { val: 0 };
        match instr {
            Instruction::JMP(jumpable) => {
                encoding.jumps(jumpable, label_map)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            },
            //Instruction::JMI() => ,
            //Instruction::JEQ() => ,
            //Instruction::STP   => ,
            //Instruction::ADD() => ,
            //Instruction::SUB() => ,
            //Instruction::ADC() => ,
            //Instruction::SBC() => ,
            //Instruction::MOV() => ,
            //Instruction::CMP() => ,
            //Instruction::AND() => ,
            //Instruction::TST() => ,
            //Instruction::LDR() => ,
            //Instruction::STR() => ,
            _ => Ok(0),
        }
    }

    /* translator functions */
    fn jumps(&mut self, 
        jumpable: &Jumpable,
        label_map: HashMap<&str, u16>,
    ) -> Result<(), SemanticError> {
        let position;
        let n = match *jumpable {
            Jumpable::Absolute(n, pos) => {
                position = pos;
                n
            }
            Jumpable::Label(lab, pos) => {
                position = pos;
                *label_map
                    .get(&lab[..])
                    .map_or(Err(SemanticError::UNDEFINEDLABEL(pos)), |n| Ok(n))?
            }
        };
        if n > MAX_JMP {
            Err(SemanticError::IMMEDIATEOVERSIZE(position))
        } else {
            self.val |= n;
            Ok(())
        }
    }
}

enum SemanticError {
    UNDEFINEDLABEL(Position),
    ODDROR(Position),
    IMMEDIATEOVERSIZE(Position),
}
