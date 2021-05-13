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


BUILD:

To build, navigate to the project directory and type:
"cargo build --release" 

You must have the rust programming language installed to use cargo.

The executable will be in /target/release

RUN:
Linux: "./mu0_arm_assembler test.s out.txt"

Windows:
1 (reccomended)
  Install a linux subsystem. I reccomend the ubuntu one from the microsoft store.
  I reccomend you use this guide.
2
  Alternatively Open the CMD
  Navigate to the directory that the executable is in using "cd [directory]"
  type: "start mu0_arm_assembler test.s out.txt"
  Unfortunately using the CMD doesn't allow error messages to be printed.
 
  
where test.s is the assembly file and out.txt is the destination file for
the assembly output.

