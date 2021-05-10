pub struct Ast {
    statments: Vec<Statement>,
}

pub type Position = (u32, u32); /* Row followed by col */

pub enum Statement {
    Instruction(Instruction),
    Label(Label),
}

pub type Label = String;

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
    LDR(Reg, Reg, i8, IndexType, Position),
    STR(Reg, Reg, i8, IndexType, Position),
}

pub enum Jumpable {
    Absolute(u16, Position),
    Label(Label, Position),
}

pub enum Op2 {
    Immediate(u8, u8, Position), // this if for ROR of an immedate value K
    ShifedReg(Reg, Shift, u8, Position),
}

pub enum IndexType {
    PRE,
    POST,
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
