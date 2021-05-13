A simple assembler written in rust for the simple Mu0-ARM architecture.

Line and column numbers are provided for syntax and semantic errors as well
as helpful(ish) error messages.
Assembly labels are supported so code like below can be written:

MOV R1, $1 ROR $0
MOV R2, $10 ROR $0
.loop
ADD R1, $1 LSL $0
CMP R1, R2 LSL $0
JMI .loop

This is a for loop from 1 to 10 in the assembly language.

