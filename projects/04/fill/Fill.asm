// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.

@16384 //screen starts here
D=A
@0 //current pixel address
M=D
@1 // temp register


(main)
    @24576 //keyboard input register
    D=M

    @fill
    D;JNE

    @erase
    D;JEQ

    (fill)
	// cheack to see if we can write further
	@0
	D=M
	@24353
	D=D-A
	@main
	D;JGE

	// else continue to fill
        @0 // current pixel address register
	A=M // A = current pixel address
	M=1 // set RAM[currentPixelAddress]=1
	@0
	M=M+1 // increase current pixel addrss by 1

	// decide which loop to go to
	@main
	0;JMP
	
    (erase)
	// check to see if we can't erase any further
	@0
	D=M
	@16384
	D=D-A
	@eraseLast
	D;JEQ

	// else continue to erase
        @0 // current pixel address register
    	A=M // D =  current pixel address 
	M=0 //set current pixel to 0
	@0
	M=M-1 //decrement current pixel

	//decide which loop to go back to
	@main
        0;JMP

    (eraseLast)
        @0 // current pixel address register
    	A=M // D =  current pixel address 
	M=0 //set current pixel to 0
	@main
	0;JMP
    


//when holding down a key, the fill-in loop should be activated,
//the indexing shoudl increase and the pixels changed to black
//when no key is held, the erase loop shoudl be activated, the index should
//decrease and the pixels should turn white along the way. 
