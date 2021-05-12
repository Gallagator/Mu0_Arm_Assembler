use std::fmt;
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
};

use combine::stream::position;
use combine::EasyParser;

use super::ast::*;
use super::parse;

const LSL: u16 = 0b00 << 2;
const LSR: u16 = 0b01 << 2;
const ASR: u16 = 0b10 << 2;
const ROR: u16 = 0b11 << 2;

const DPR: u16 = 0b1 << 15;

const S_BIT: u16 = 0b1 << 8;
const C_BIT: u16 = 0b1 << 9;

const MAX_JMP: u16 = (0b1 << 12) - 1;
const MAX_K: i16 = (0b1 << 5) - 1;
const MAX_ROT: i16 = 15;
const MAX_BY: i16 = (0b1 << 4) - 1;
const MAX_OFFS: i16 = (0b1 << 4) - 1;

pub struct Encode {
    val: u16,
}

#[derive(Debug, PartialEq)]
pub enum SemanticError {
    UNDEFINEDLABEL(Position),
    REDEFINEDLABEL(Position),
    ODDROR(Position),
    IMMEDIATEOVERSIZE(Position),
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pos;
        let msg = match self {
            SemanticError::ODDROR(p) => { pos = p; "Immediate ROR'd by an even value." },
            SemanticError::UNDEFINEDLABEL(p) => { pos = p; "Jump to undefined label." },
            SemanticError::IMMEDIATEOVERSIZE(p) => {
                pos = p;
                "You have used an immediate value too large for the instruction"
            }
            SemanticError::REDEFINEDLABEL(p) => { pos = p; "Label already defined" } ,
        };
        write!(f, "Semantic error: {}\nLine: {}\nCol:{}", msg, pos.0, pos.1)
    }
}

impl Encode {
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
        self.val |= rot << 5
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
    fn set_d(&mut self, reg: Reg) {
        let d = match reg {
            Reg::R0 => 0b00,
            Reg::R1 => 0b01,
            Reg::R2 => 0b10,
            Reg::R3 => 0b11,
        };
        self.val |= d << 10
    }

    fn set_m(&mut self, reg: Reg) {
        let m = match reg {
            Reg::R0 => 0b00,
            Reg::R1 => 0b01,
            Reg::R2 => 0b10,
            Reg::R3 => 0b11,
        };
        self.val |= m
    }

    pub fn instruction(
        instr: &Instruction,
        label_map: &HashMap<&str, u16>,
    ) -> Result<u16, SemanticError> {
        let mut encoding = Encode { val: 0 };
        match instr {
            Instruction::JMP(jumpable) => {
                encoding.jumps(jumpable, label_map)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::JMI(jumpable) => {
                encoding.jumps(jumpable, label_map)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::JEQ(jumpable) => {
                encoding.jumps(jumpable, label_map)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::STP => {
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::ADD(reg, op2) => {
                encoding.set_s();
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::SUB(reg, op2) => {
                encoding.set_s();
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::ADC(reg, op2) => {
                encoding.set_s();
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::SBC(reg, op2) => {
                encoding.set_s();
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::MOV(reg, op2) => {
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::CMP(reg, op2) => {
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::AND(reg, op2) => {
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::TST(reg, op2) => {
                encoding.dprs(reg.clone(), op2)?;
                encoding.set_op(&instr);
                Ok(encoding.val)
            }
            Instruction::LDR(regd, asm_index) => {
                encoding.set_l();
                encoding.mems(regd.clone(), asm_index)?;
                Ok(encoding.val)
            }
            Instruction::STR(regd, asm_index) => {
                encoding.mems(regd.clone(), asm_index)?;
                Ok(encoding.val)
            }
        }
    }

    /* translator functions */
    fn jumps(
        &mut self,
        jumpable: &Jumpable,
        label_map: &HashMap<&str, u16>,
    ) -> Result<(), SemanticError> {
        let position;
        let n = match jumpable {
            Jumpable::Absolute(n, pos) => {
                position = pos;
                n.clone()
            }
            Jumpable::Label(lab, pos) => {
                position = pos;
                *label_map
                    .get(&lab[..])
                    .map_or(Err(SemanticError::UNDEFINEDLABEL(pos.clone())), |n| Ok(n))?
            }
        };
        if n > MAX_JMP {
            Err(SemanticError::IMMEDIATEOVERSIZE(position.clone()))
        } else {
            self.val |= n;
            Ok(())
        }
    }
    fn dprs(&mut self, reg: Reg, op: &Op2) -> Result<(), SemanticError> {
        self.set_dpr();
        self.set_d(reg.clone());
        match op {
            Op2::Immediate(k, r, pos) => {
                let k = k.clone();
                let r = r.clone();
                let pos = pos.clone();
                if k > MAX_K || k < 0 || r > MAX_ROT || r < 0 {
                    Err(SemanticError::IMMEDIATEOVERSIZE(pos))
                } else if r % 2 == 1 {
                    Err(SemanticError::ODDROR(pos))
                } else {
                    self.set_c();
                    self.set_k(k as u16);
                    self.set_rot((r as u16) / 2);
                    Ok(())
                }
            }
            Op2::ShifedReg(regm, shift, by, pos) => {
                let by = by.clone();
                if by > MAX_BY || by < 0 {
                    Err(SemanticError::IMMEDIATEOVERSIZE(pos.clone()))
                } else {
                    self.set_m(regm.clone());
                    self.set_shift(shift.clone());
                    self.set_b(by as u16);
                    Ok(())
                }
            }
        }
    }
    fn mems(&mut self, regd: Reg, asm_index: &AsmIndex) -> Result<(), SemanticError> {
        let (regm, mut offs, index_type, pos) = asm_index.clone();
        self.set_d(regd);
        if offs >= 0 {
            self.set_u();
        } else {
            offs = -offs;
        }

        if offs > MAX_OFFS {
            Err(SemanticError::IMMEDIATEOVERSIZE(pos.clone()))
        } else {
            self.set_m(regm);
            self.set_n(offs as u16);
            match index_type {
                IndexType::PRE => self.set_p(),
                IndexType::PREWRITE => {
                    self.set_p();
                    self.set_w()
                }
                IndexType::POSTWRITE => self.set_w(),
            }
            Ok(())
        }
    }
}

fn assemble(Ast { statements: stats }: &Ast) -> Vec<Result<u16, SemanticError>> {
    let mut assembled: Vec<Result<u16, SemanticError>> = Vec::new();
    let mut label_map = HashMap::new();
    let mut index = 0;
    /* 1st pass to determine the location of labels */
    for stat in stats.iter() {
        match stat {
            Statement::Label(lab, pos) => {
                let err = Err(SemanticError::REDEFINEDLABEL(pos.clone()));
                label_map
                    .insert(&lab[..], index)
                    .map(|_| assembled.push(err));
            }
            _ => {
                index += 1;
            }
        }
    }
    /* 2nd pass for the assembling */
    for stat in stats.iter() {
        match stat {
            Statement::Instruction(instr) => assembled.push(Encode::instruction(instr, &label_map)),
            _ => {}
        }
    }
    assembled
}

pub fn assemble_to_file(input_filename: &str, output: &str) {
    /* Read assembly file */
    let mut file =
        File::open(input_filename).expect(&format!("{} file doesn't exist.", input_filename)[..]);
    let mut assembly = String::new();
    file.read_to_string(&mut assembly)
        .expect("Unable to read entire file sorry!");
    /* parse assembly file */
    let assembly = position::Stream::new(assembly.as_str());
    let result = parse::program().easy_parse(assembly);
    let ast;
    match result {
        Ok(parse_output) => ast = parse_output.0,
        Err(err) => {
            println!("Parse error :{}", err);
            return;
        }
    }
    /* Assemble the file */
    let instrs = assemble(&ast);
    let mut out = File::create(output).expect("I couldn't create out.txt. Maybe you should?");
    for instr in instrs.iter() {
        match instr {
            Ok(encoding) => {
                out.write_all(format!("{:#06x}\n", encoding).as_bytes())
                    .expect("Couldn't write to the output file.");
            }
            Err(sem_err) => println!("{}", sem_err),
        }
    }
}
