// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

 

    @R0
    M=0
    @R0 // if R0 is 0 goto END
    D=M
    @END	
    D;JEQ
    @R1 // if R1 is 0 goto END
    D=M
    @END	
    D;JEQ
    @3
    M=0	
	
(LOOP)
    @R1 // if (R3-R1>0), goto END
    D=M
    @4
    M=D	
    @3
    D=M
    @4
    M=D-M	
    @4
    D=M	
    @END
    D;JEQ
    // Here we do R2+=R0, R3+=1	since we failed the break test
    @R0
    D=M
    @R2
    M=D+M
    @3
    M=M+1
    @LOOP
    0;JMP
(END)
    @END
    0;JMP
