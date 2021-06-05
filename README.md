A simple assembler written in rust for the simple Mu0-ARM architecture.

Line and column numbers are provided for syntax and semantic errors as well
as helpful(ish) error messages.
Assembly labels are supported so code like below can be written:

MOV R1, $1 ROR $0

MOV R2, $10 ROR $0

.loop

ADD R1, $1 ROR $0

CMP R1, R2 ROR $0

JMI .loop

This is a for loop from 1 to 10 in the assembly language.
----------------------------------------------------------------------------

Floating point instructions have been added.
They take the form of:

ADDF <REG> <REG>
SUBF <REG> <REG>
MULF <REG> <REG>

Their encoding is: 0001 RD MUL S XXXXXX RM
where RD the 2 bits specifying the destination register, MUL is a flag to
specify that the operation is a multiplication, S specifies that its a subtraction
X is don't care and RM is the 2 bits specifying other register.

----------------------------------------------------------------------

RELEASE: https://github.com/Gallagator/Mu0_Arm_Assembler/releases

----------------------------------------------------------------------------

BUILD:

To build, navigate to the project directory and type:
"cargo build --release" 

You must have the rust programming language installed to use cargo.

The executable will be in /target/release

---------------------------------------------------------------------------------

RUN:

Linux: 

navigate to the directory that the executable is installed with "cd [directory]"

run: "./mu0_arm_assembler test.s out.txt"

where test.s is the assembly file and out.txt is the destination file for
the assembly output.

-------------------------------------------------------------------------------------
Windows:

1 (recomended)
  Install a linux subsystem. I reccomend the ubuntu one from the microsoft store.
  I suggest you use this guide: https://itsfoss.com/install-bash-on-windows/
  Follow the linux guide above.
  
2
  Alternatively Open the CMD
  Navigate to the directory that the executable is in using "cd [directory]"
  type: "start mu0_arm_assembler test.s out.txt"
  Unfortunately using the CMD doesn't allow error messages to be printed.
 
 

