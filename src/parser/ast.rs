pub struct Ast {
    statments: Vec<Statement>,
}

pub enum Statement {
    instruction(Instruction),
    label(Label),
}

pub type Label = String;

pub struct Instruction {
    opcode: Opcode,
    args  : Args,
}

pub enum Args {
    None,
    jmp(Jumpable),
    regOp(Reg, Op2),
    regIndex(Reg, Reg, i8, IndexType),
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

pub enum Opcode {
    JMP, 
    JMI,
    JEQ,
    STP,
    ADD,
    SUB,
    ADC,
    SBC,
    MOV,
    CMP,
    AND,
    TST,
    LDR,
    STR, 
}
