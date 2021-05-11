#[derive(Debug)]
#[derive(PartialEq)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

pub type Position = (u32, u32); /* Line followed by col */

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Statement {
    Instruction(Instruction),
    Label(Label),
}

pub type Label = String;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Instruction {
    JMP(Jumpable),
    JMI(Jumpable),
    JEQ(Jumpable),
    STP,
    ADD(Reg, Op2),
    SUB(Reg, Op2),
    ADC(Reg, Op2),
    SBC(Reg, Op2),
    MOV(Reg, Op2),
    CMP(Reg, Op2),
    AND(Reg, Op2),
    TST(Reg, Op2),
    LDR(Reg, AsmIndex),
    STR(Reg, AsmIndex),
}

pub type AsmIndex = (Reg, i16, IndexType, Position);

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Jumpable {
    Absolute(u16, Position),
    Label(Label, Position),
}

#[derive(Debug)]
#[derive(PartialEq)] 
pub enum Op2 {
    Immediate(i16, i16, Position), // this if for ROR of an immedate value K
    ShifedReg(Reg, Shift, i16, Position),
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum IndexType {
    PRE,
    PREWRITE,
    POSTWRITE,
}

#[derive(Debug, PartialEq)]
pub enum Reg {
    R0,
    R1,
    R2,
    R3,
}

#[derive(Debug, PartialEq)]
pub enum Shift {
    LSL,
    LSR,
    ASR,
    ROR,
}
