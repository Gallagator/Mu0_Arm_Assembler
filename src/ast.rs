struct Ast {
    statments: Vec<Statement>,
}

enum Statement {
    instruction(Instruction),
    label(Label),
}

type Label = String;

struct Instruction {
    opcode: Opcode,
    args  : Args,
}

enum Args {
    None,
    jmp(Jumpable),
    regOp(Reg, Op2),
    regIndex(Reg, Reg, i8, IndexType),
}

enum Jumpable {
    absolute(u16),
    label(Label),
}

enum Op2 {
    immediate(u8, u8),        // this if for ROR of an immedate value K
    shifed_reg(Reg, Shift, u8),
}

enum IndexType {
   PRE,
   POST,
   PRE_WRITE,
   POST_WRITE,
}

enum Reg {
    R0,
    R1,
    R2,
    R3,
}

enum Shift {
    LSL,
    LSR,
    ASR,
    ROR,
}

enum Opcode {
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
