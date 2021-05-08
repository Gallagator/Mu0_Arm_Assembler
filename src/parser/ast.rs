pub struct Ast {
    statments: Vec<Statement>,
}

pub enum Statement {
    instruction(Instruction),
    label(Label),
}

pub type Label = String;

pub enum Instruction {
    JMP(jumpable), 
    JMI(jumpable),
    JEQ(jumpable),
    STP,
    ADD(Reg, Op2),
    SUB(Reg, Op2),
    ADC(Reg, Op2),
    SBC(Reg, Op2),
    MOV(Reg, Op2),
    CMP(Reg, Op2),
    AND(Reg, Op2),
    TST(Reg, Op2),
    LDR(Reg, Reg, i8, IndexType),
    STR(Reg, Reg, i8, IndexType),
    
}

pub enum Jumpable {
    absolute(u16),
    label(Label),
}

pub enum Op2 {
    immediate(u8, u8),        // this if for ROR of an immedate value K
    shifed_reg(Reg, Shift, u8),
}

pub enum IndexType {
   PRE,
   POST,
   PRE_WRITE,
   POST_WRITE,
}

pub enum Reg {
    R0,
    R1,
    R2,
    R3,
}

pub enum Shift {
    LSL,
    LSR,
    ASR,
    ROR,
}

