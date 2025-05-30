@
@   FORTH Compiler for the Raspberry Pi Pico 2 W (RP2350)
@
@   This is a simple FORTH compiler for the Raspberry Pi Pico 2 W.
@   Strongly based on the Jones FORTH by Richard W.M. Jones <rich@annexia.org>
@
@       http://annexia.org/forth
@
@   This compiler is implemented in ARM assembly.
@
@   A second major influce is codescribe's updated version of Jones FORTH
@
@       https://github.com/codescribe/jonesforth
@

    .syntax unified
    .cpu cortex-m33
    .thumb_func

    .set LEDUC_VERSION, 1

@ MARK: Translation notes
@
@   Registers:
@   r5 - instruction pointer (%esi)
@   r6 - return stack pointer (%ebp)
@   rP - parameter stack pointer (%esp)
@
@   Instructions:
@   LOSL:
@       ldr r0, [r5], #4
@
@   JMP *(%eax):
@       ldr r0, [r0]
@       orr r0, #1    @ set thumb bit
@       blx r0



@   This macro is called NEXT.  That's a FORTH-ism.  It expands to those two instructions.
@
@   Every FORTH primitive that we write has to be ended by NEXT.  Think of it kind of like
@   a return.
@
@   We are using indirect threaded code.
@
@   We compress our function calls down to a list of addresses and use a somewhat
@   magical macro to act as a "jump to next function in the list".  We also use one register (r5)
@   to act as a kind of instruction pointer, pointing to the next function in the list.
@
@   NOTE: r0 contains the address of the codeword. This is important becase DOCOL (the interpreter)
@   is expecting r0 to contain the address of the codeword.

    .macro NEXT
    ldr r0, [r5], #4                    @ r5 points to the next instruction, the codeword
    ldr r1, [r0]                        @ get the interpreter address stored in the codeword
    orr r1, #1                          @ set the thumb bit (make sure we stay in thumb mode)
    blx r1                              @ branch to the interpreter referenced by the codeword
    .endm



@   In this FORTH, we are using the normal stack pointer (sp) for the parameter stack.
@   We will use another register (r6) for our return stack.
@
@   These two macros which just wrap up the details of using r6 for the return stack.
@   You use them as for example "PUSHRSP r0" (push r0 on the return stack) or "POPRSP r1"
@   (pop top of return stack into r1).

    .macro PUSHRSP reg
    str \reg, [r6, #-4]!                @ push reg on to return stack
    .endm

    .macro POPRSP reg
    ldr \reg, [r6], #4                  @ pop reg from return stack
    .endm


    .text
    .global main
@ Assembler entry point.
main:
    bl set_up_data_space
    bl sdk_init 

    movw r0, :lower16:var_s0 
    movt r0, :upper16:var_s0
    str sp, [r0]                        @ save the initial data stack pointer in FORTH variable S0

    movw r6, :lower16:return_stack_top
    movt r6, :upper16:return_stack_top  @ initialise the return stack

    movw r5, :lower16:cold_start
    movt r5, :upper16:cold_start        @ Initialise interpreter

    NEXT			                    @ Run interpreter!
    
    .section .rodata
cold_start:
    .word QUIT



@   In FORTH the interpreter function is often called DOCOL. I think it means "DO COLON" because
@   all FORTH definitions start with a colon, as in : DOUBLE DUP + ;
@
@   The "interpreter" (it's not really "interpreting") just needs to push the old r5 on the
@   stack and set r5 to the first word in the definition.  Remember that we jumped to the
@   function using the NEXT macro?  Well a consequence of that is that conveniently r0 contains
@   the address of this codeword, so just by adding 4 to it we get the address of the first
@   data word.  Finally after setting up r5, it just does NEXT which causes that first word
@   to run.

@ DOCOL - the interpreter!
@   r0 is expected to point to the codeword
    .text
    .align 2
DOCOL:
    PUSHRSP r5		                    @ push r5 on to the return stack
    add r5, r0, #4		                @ r0 points to the code word, so make r5 point to first data word
    NEXT




@
@   MARK: Built-in Words
@
@   The dictionary entries (headers) look like the following in memory for 
@   : DOUBLE DUP + ; 
@
@     pointer to previous word
@      ^
@      |
@   +--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@   | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@   +---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
@      ^       len                         pad  codeword      |
@      |                                                      V
@     LINK in next word                             points to codeword of DUP
@
@   Initially we can't just write ": DOUBLE DUP + ;" (ie. that literal string) here because we
@   don't yet have anything to read the string, break it up at spaces, parse each word, etc. etc.
@   So instead we will have to define built-in words using the GNU assembler data constructors
@   (like .word, .byte, .string, .ascii and so on -- look them up in the gas info page if you are
@   unsure of them).
@
@   The long way would be:
@
@           .word <link to previous word>
@           .byte 6             // len
@           .ascii "DOUBLE"     // string
@           .byte 0	            // padding
@   DOUBLE: .word DOCOL         // codeword
@           .word DUP           // pointer to codeword of DUP
@           .word PLUS          // pointer to codeword of +
@           .word EXIT          // pointer to codeword of EXIT
@
@   That's going to get quite tedious rather quickly, so here I define an assembler macro
@   so that I can just write:
@
@   defword "DOUBLE",6,,DOUBLE
@   .word DUP,PLUS,EXIT
@
@   and I'll get exactly the same effect.

@   Flags - these are discussed later.
    .set F_IMMED, 0x80
    .set F_HIDDEN, 0x20
    .set F_LENMASK, 0x1f	            @ length mask

@   Store the chain of links.
    .set link, 0

    .macro defword name, flags=0, label
    .section .rodata
    .align 2
    .globl name_\label
name_\label:
1:
    .word link                          @ link
    .byte 3f - 2f + \flags              @ flags + length byte
2:
    .ascii "\name"                      @ the name
3:
    .align 2                            @ pad with 0's to next 4 byte boundary
    .globl \label                             
\label:
    .word DOCOL                         @ codeword - the interpreter
    @ list of word pointers follow
    .set link, 1b
    .endm

@   Similarly I want a way to write words written in assembly language.  There will quite a few
@   of these to start with because, well, everything has to start in assembly before there's
@   enough "infrastructure" to be able to start writing FORTH words, but also I want to define
@   some common FORTH words in assembly language for speed, even though I could write them in FORTH.
@
@   This is what DUP looks like in memory:
@
@     pointer to previous word
@      ^
@      |
@   +--|------+---+---+---+---+------------+
@   | LINK    | 3 | D | U | P | code_DUP ---------------------> points to the assembly
@   +---------+---+---+---+---+------------+                    code used to write DUP,
@      ^       len              codeword                        which ends with NEXT.
@      |
@     LINK in next word
@
@   Again, for brevity in writing the header I'm going to write an assembler macro called defcode.
@   As with defword above, don't worry about the complicated details of the macro.

    .macro defcode name, flags=0, label
    .section .rodata
    .align 2
    .globl name_\label
name_\label:
1:
    .word link		                    @ link
    .byte 3f - 2f + \flags	            @ flags + length byte
2:
    .ascii "\name"	                    @ the name
3:
    .align 2                            @ pad with 0's to 4 byte boundary
    .globl \label
\label:
    .word code_\label	                // codeword
    .text
    .globl code_\label
code_\label:			                // assembler code follows
    .set link, 1b
    .endm

@   MARK: FORTH Primitive Words
@   Now some easy FORTH primitives.  These are written in assembly for speed.

    defcode "drop",,DROP
    pop {r0}		                    @ drop top of stack
    NEXT

    defcode "print",,PRINT
    pop {r0}                            @ get top of stack
    NEXT

    defcode "swap",,SWAP
    pop {r0}		                    @ swap top two elements on stack
    pop {r1}
    push {r0}
    push {r1}
    NEXT

    defcode "dup",,DUP
    ldr r0, [sp]                        @ duplicate top of stack
    push {r0}
    NEXT

    defcode "over",,OVER
    ldr r0, [sp, #4]	                @ get the top element of stack
    push {r0}		                    @ and push it on top
    NEXT

    defcode "rot",,ROT
    pop {r0}
    pop {r1}
    pop {r2}
    push {r1}
    push {r0}
    push {r2}
    NEXT

    defcode "-rot",,NROT
    pop {r0}
    pop {r1}
    pop {r2}
    push {r0}
    push {r2}
    push {r1}
    NEXT

    defcode "2drop",,TWODROP                @ drop top two elements of stack
    pop {r0}
    pop {r0}
    NEXT

    defcode "2dup",,TWODUP                  @ duplicate top two elements of stack
    ldr r0, [sp]
    ldr r1, [sp, #4]
    push {r1}
    push {r0}
    NEXT

    defcode "2swap",,TWOSWAP                @ swap top two pairs of elements of stack
    pop {r0}
    pop {r1}
    pop {r2}
    pop {r3}
    push {r1}
    push {r0}
    push {r3}
    push {r2}
    NEXT

    defcode "?dup",,QDUP	                @ duplicate top of stack if non-zero
    ldr r0, [sp]
    cmp r0, #0
    beq 1f
    push {r0}
1:	NEXT

    defcode "1+",,INCR
    pop {r0}		                    @ increment top of stack
    add r0, #1
    push {r0}		                    @ push it back on to the stack
    NEXT

    defcode "1-",,DECR
    pop {r0}    		                @ decrement top of stack
    sub r0, #1
    push {r0}		                    @ push it back on to the stack
    NEXT

    defcode "4+",,INCR4
    pop {r0}		                    @ increment top of stack
    add r0, #4
    push {r0}		                    @ push it back on to the stack
    NEXT

    defcode "4-",,DECR4
    pop {r0}		                    @ decrement top of stack
    sub r0, #4
    push {r0}		                    @ push it back on to the stack
    NEXT

    defcode "+",,ADD
    pop {r0}        		            @ get top of stack
    pop {r1}        		            @ get next word on stack
    add r0, r1                          @ add them together
    push {r0}                           @ and push the result back on to the stack
    NEXT

    defcode "-",,SUB
    pop {r0}        		            @ get top of stack
    pop {r1}        		            @ get next word on stack
    sub r0, r1, r0                      @ subtract them
    push {r0}                           @ and push the result back on to the stack
    NEXT

    defcode "*",,MUL
    pop {r0}
    pop {r1}        		            @ get top two words on stack
    mul r0, r1, r0                      @ multiply them
    push {r0}                           @ and push the result back on to the stack
    NEXT

@   In this FORTH, only /MOD is primitive.  Later we will define the / and MOD words in
@   terms of the primitive /MOD.

    defcode "/mod",,DIVMOD
    pop {r0}                            @ get top of stack (divisor)
    pop {r1}                            @ get next word on stack (dividend)
    cmp r0, #0                          @ check for divide by zero
    beq 1f                              @ if zero, branch to error

    sdiv r2, r1, r0                     @ signed divide, quotient, r2 = (r1 / r0)
    mul r3, r2, r0
    cmp r1, r3
    ite lt
    sublt r3, r3, r1
    subge r3, r1, r3                    @ remainder, r3 = (r1 - (r2 * r0))

    push {r3}                           @ push the remainder on to the stack
    push {r2}                           @ push the quotient on to the stack
    NEXT

1:                                      @ Divide by zero error
    mov r0, #0                          @ set r0 to zero
    push {r0}                           @ push zero for remainder
    push {r0}                           @ push zero for quotient 
    NEXT

    defcode "u/mod",,UDIVMOD
    pop {r0}                            @ get top of stack (divisor)
    pop {r1}                            @ get next word on stack (dividend)
    cmp r0, #0                          @ check for divide by zero
    beq 1f                              @ if zero, branch to error

    udiv r2, r1, r0                     @ unsigned divide
    mul r3, r2, r0                      @ multiply
    sub r3, r1, r3                      @ subtract to get remainder

    push {r3}                           @ push the remainder on to the stack
    push {r2}                           @ push the quotient on to the stack
    NEXT

1:                                      @ Divide by zero error  
    mov r0, #0                          @ set r0 to zero
    push {r0}                           @ push zero for remainder
    push {r0}                           @ push zero for quotient
    NEXT

@   MARK: Comparison Operations
@   Lots of comparison operations like =, <, >, etc..
@
@   ANS FORTH says that the comparison words should return all (binary) 1's for
@   TRUE and all 0's for FALSE.

    defcode "=",,EQU                        @ top two words are equal?
    pop {r0}
    pop {r1}
    cmp r0, r1
    ite eq
    moveq r0, #-1
    movne r0, #0
    push {r0}  
    NEXT

    defcode "<>",,NEQU                      @ top two words are not equal?
    pop {r0}
    pop {r1}
    cmp r0, r1
    ite ne
    movne r0, #-1
    moveq r0, #0
    push {r0}
    NEXT

    defcode "<",,LT
    pop {r0}
    pop {r1}
    cmp r1, r0
    ite lt
    movlt r0, #-1
    movge r0, #0
    push {r0}
    NEXT

    defcode ">",,GT
    pop {r0}
    pop {r1}
    cmp r1, r0
    ite gt
    movgt r0, #-1
    movle r0, #0
    push {r0}
    NEXT

    defcode "<=",,LE
    pop {r0}
    pop {r1}
    cmp r1, r0
    ite le
    movle r0, #-1
    movgt r0, #0
    push {r0}
    NEXT

    defcode ">=",,GE
    pop {r0}
    pop {r1}
    cmp r1, r0
    ite ge
    movge r0, #-1
    movlt r0, #0
    push {r0}
    NEXT

    defcode "0=",,ZEQU                      @ top of stack equals 0?
    pop {r0}
    cmp r0, #0
    ite eq
    moveq r0, #-1
    movne r0, #0
    push {r0}
    NEXT

    defcode "0<>",,ZNEQU                    @ top of stack not 0?
    pop {r0}
    cmp r0, #0
    ite ne
    movne r0, #-1
    moveq r0, #0
    push {r0}
    NEXT

    defcode "0<",,ZLT                       @ comparisons with 0
    pop {r0}
    cmp r0, #0
    ite lt
    movlt r0, #-1
    movge r0, #0
    push {r0}
    NEXT

    defcode "0>",,ZGT
    pop {r0}
    cmp r0, #0
    ite gt
    movgt r0, #-1
    movle r0, #0
    push {r0}
    NEXT

    defcode "0<=",,ZLE
    pop {r0}
    cmp r0, #0
    ite le
    movle r0, #-1
    movgt r0, #0
    push {r0}
    NEXT

    defcode "0>=",,ZGE
    pop {r0}
    cmp r0, #0
    ite ge
    movge r0, #-1
    movlt r0, #0
    push {r0}
    NEXT

    defcode "and",,AND                      @ bitwise AND
    pop {r0}
    pop {r1}
    and r0, r1
    push {r0}
    NEXT

    defcode "or",,OR                        @ bitwise OR
    pop {r0}
    pop {r1}
    orr r0, r1
    push {r0}
    NEXT

    defcode "xor",,XOR                       @ bitwise XOR
    pop {r0}
    pop {r1}
    eor r0, r1
    push {r0}
    NEXT

    defcode "invert",,INVERT                @ this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)
    pop {r0}
    mvn r0, r0
    push {r0}
    NEXT



@
@   MARK: Returning from FORTH words
@
@   Time to talk about what happens when we EXIT a function.  In this diagram QUADRUPLE has called
@   DOUBLE, and DOUBLE is about to exit (look at where r5 is pointing):
@
@           QUADRUPLE
@           +------------------+
@           | codeword         |
@           +------------------+                DOUBLE
@           | addr of DOUBLE  ----------------> +------------------+
@           +------------------+                | codeword         |
@           | addr of DOUBLE   |                +------------------+
@           +------------------+                | addr of DUP      |
@           | addr of EXIT	   |                +------------------+
@           +------------------+                | addr of +        |
@                                               +------------------+
@                                         r5 -> | addr of EXIT     |
@                                               +------------------+
@
@   What happens when the + function does NEXT?  Well, the following code is executed.

    defcode "exit",,EXIT
    POPRSP r5		                    @ pop return stack into instruction pointer
    NEXT

@   EXIT gets the old r5 which we saved from before on the return stack, and puts it in r5.
@   So after this (but just before NEXT) we get:
@
@           QUADRUPLE
@           +------------------+
@           | codeword         |
@           +------------------+                DOUBLE
@           | addr of DOUBLE  ----------------> +------------------+
@           +------------------+                | codeword         |
@     r5 -> | addr of DOUBLE   |                +------------------+
@           +------------------+                | addr of DUP      |
@           | addr of EXIT	   |                +------------------+
@           +------------------+                | addr of +        |
@                                               +------------------+
@                                               | addr of EXIT     |
@                                               +------------------+
@
@   And NEXT just completes the job by, well, in this case just by calling DOUBLE again



@
@   MARK: Literals
@
@   The final point I "glossed over" before was how to deal with functions that do anything
@   apart from calling other functions.  For example, suppose that DOUBLE was defined like this:
@
@   : DOUBLE 2 * ;
@
@   It does the same thing, but how do we compile it since it contains the literal 2?  One way
@   would be to have a function called "2" (which you'd have to write in assembler), but you'd need
@   a function for every single literal that you wanted to use.
@
@   FORTH solves this by compiling the function using a special word called LIT:
@
@   +---------------------------+-------+-------+-------+-------+-------+
@   | (usual header of DOUBLE)  | DOCOL | LIT   | 2     | *     | EXIT  |
@   +---------------------------+-------+-------+-------+-------+-------+
@
@   LIT is executed in the normal way, but what it does next is definitely not normal.  It
@   looks at r5 (which now points to the number 2), grabs it, pushes it on the stack, then
@   manipulates r5 in order to skip the number as if it had never been there.

    defcode "lit",,LIT
    @ r5 points to the next command, but in this case it points to the next
    @ literal 32 bit integer.  Get that literal into r0 and increment r5.
    ldr r0, [r5], #4
    push {r0}                           @ push the literal number on to the stack
    NEXT



@
@   MARK: Memory
@
@   As important point about FORTH is that it gives you direct access to the lowest levels
@   of the machine.  Manipulating memory directly is done frequently in FORTH, and these are
@   the primitive words for doing it.

    defcode "!",,STORE
    pop {r0}                            @ address to store at
    pop {r1}                            @ data to store there
    str r1, [r0]                        @ store it
    NEXT

    defcode "@",,FETCH
    pop {r0}                            @ address to fetch
    ldr r1, [r0]                        @ fetch it
    push {r1}                           @ push value onto stack
    NEXT

    defcode "+!",,ADDSTORE
    pop {r0}                            @ address
    pop {r1}                            @ the amount to add
    ldr r2, [r0]                        @ load the value at the address
    add r2, r1                          @ add it
    str r2, [r0]                        @ store it
    NEXT


    defcode "-!",,SUBSTORE
    pop {r0}                            @ address
    pop {r1}                            @ the amount to subtract
    ldr r2, [r0]                        @ load the value at the address
    sub r0, r0, r1                      @ subtract it
    str r2, [r0]                        @ store it
    NEXT

@   ! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
@   so we also define standard words C@ and C!.
@
@   Byte-oriented operations only work on architectures which permit them (arm is one of those).

    defcode "c!",,STOREBYTE
    pop {r0}                            @ address to store at
    pop {r1}                            @ data to store there
    strb r1, [r0]                       @ store it
    NEXT

    defcode "c@",,FETCHBYTE
    pop {r0}                            @ address to fetch
    ldrb r1, [r0]                       @ fetch it
    push {r1}                           @ push value onto stack
    NEXT



@
@   MARK: Built-In Variables
@
@   These are some built-in variables and related standard FORTH words.  Of these, the only one that we
@   have discussed so far was LATEST, which points to the last (most recently defined) word in the
@   FORTH dictionary.  LATEST is also a FORTH word which pushes the address of LATEST (the variable)
@   on to the stack, so you can read or write it using @ and ! operators.  For example, to print
@   the current value of LATEST (and this can apply to any FORTH variable) you would do:
@
@   LATEST @ . CR
@
@   To make defining variables shorter, I'm using a macro called defvar, similar to defword and
@   defcode above.  (In fact the defvar macro uses defcode to do the dictionary header).

    .macro defvar name, flags=0, label, initial=0
    defcode \name, \flags, \label
    ldr r0, =var_\name
    push {r0}
    NEXT
    .data
    .align 2
var_\name:
    .word \initial
    .endm

@   The built-in variables are:
@
@   STATE       Is the interpreter executing code (0) or compiling a word (non-zero)?
@   LATEST      Points to the latest (most recently defined) word in the dictionary.
@   DP          Points to the next free byte of memory.  When compiling, compiled words go here.
@   S0          Stores the address of the top of the parameter stack.
@   BASE        The current base for printing and reading numbers.

    defvar "state",,STATE
    defvar "dp",,DP
    defvar "latest",,LATEST,name_EXECUTE    @ EXECUTE must be last in built-in dictionary
    defvar "s0",,SZ
    defvar "base",,BASE,10



@
@   MARK: Built-in Constants
@
@   It's also useful to expose a few constants to FORTH.  When the word is executed it pushes a
@   constant value on the stack.
@
@   The built-in constants are:
@
@   VERSION     Is the current version of this FORTH.
@   R0          The address of the top of the return stack.
@   DOCOL       Pointer to DOCOL.
@   F_IMMED     The IMMEDIATE flag's actual value.
@   F_HIDDEN    The HIDDEN flag's actual value.
@   F_LENMASK   The length mask in the flags/len byte.
@
@   In Arm, a reference to a label is done using the ldr instruction, which loads the address of
@   the label into a register, so we have the defref macro for that.

    .macro defconst name, flags=0, label, value
    defcode \name, \flags, \label
    movw r0, :lower16:\value
    movt r0, :upper16:\value
    push {r0}
    NEXT
    .endm

    .macro defref name, flags=0, label, value
    defcode \name,  \flags, \label
    ldr r0, =\value
    push {r0}
    NEXT
    .endm

    defconst "version",,VERSION,LEDUC_VERSION
    defref "r0",,RZ,return_stack_top
    defref "docol",,__DOCOL,DOCOL
    defconst "f_immed",,__F_IMMED,F_IMMED
    defconst "f_hidden",,__F_HIDDEN,F_HIDDEN
    defconst "f_lenmask",,__F_LENMASK,F_LENMASK



@
@   MARK: Return Stack
@
@   These words allow you to access the return stack.  Recall that the register r6 always points to
@   the top of the return stack.

    defcode ">r",,TOR
    pop {r0}                            @ pop parameter stack into r0
    PUSHRSP r0                          @ push it on to the return stack
    NEXT

    defcode "r>",,FROMR
    POPRSP r0                           @ pop return stack on to r0
    push {r0}                           @ and push on to parameter stack
    NEXT

    defcode "rsp@",,RSPFETCH
    push {r6}                           @ push the return stack pointer
    NEXT

    defcode "rsp!",,RSPSTORE
    pop {r6}                            @ pop the return stack pointer
    NEXT

    defcode "rdrop",,RDROP
    add r6, #4                          @ pop return stack and throw away
    NEXT



@
@   MARK: Parameter (Data) Stack
@
@   These functions allow you to manipulate the parameter stack that is access throught sp.

    defcode "dsp@",,DSPFETCH
    mov r0, sp
    push {r0}
    NEXT

    defcode "dsp!",,DSPSTORE
    pop {r0}
    mov sp, r0
    NEXT



@
@   MARK: Input and Output
@
@   These are our first really meaty/complicated FORTH primitives.  I have chosen to write them in
@   C/assembler, but surprisingly in "real" FORTH implementations these are often written in terms
@   of more fundamental FORTH primitives.  I chose to avoid that because I think that just obscures
@   the implementation. 
@
@   The FORTH word KEY reads the next byte from stdin (and pushes it on the parameter stack).
@   So if KEY is called and someone hits the space key, then the number 32 (ASCII code of space)
@   is pushed on the stack.
@
@   In FORTH there is no distinction between reading code and reading input.  We might be reading
@   and compiling code, we might be reading words to execute, we might be asking for the user
@   to type their name -- ultimately it all comes in through KEY.
@
@   The FORTH word EMIT writes out a single byte to stdout.
@
@   The implementation of KEY and EMIT can be found in LeducForth.c

    defcode "key",,KEY
    bl _key
    push {r0}                           @ push the character on to the stack
    NEXT

    defcode "emit",,EMIT
    pop {r0}                            @ get the character to emit
    bl _emit
    NEXT

@   WORD is a FORTH word which reads the next full word of input.
@
@   What it does in detail is that it first skips any blanks (spaces, tabs, newlines and so on).
@   Then it calls KEY to read characters into an internal buffer until it hits a blank.  Then it
@   calculates the length of the word it read and returns the address and the length as
@   two words on the stack (with the length at the top of stack).
@
@   Notice that WORD has a single internal buffer which it overwrites each time (rather like
@   a static C string).  Also notice that WORD's internal buffer is just 32 bytes long and
@   there is NO checking for overflow.  31 bytes happens to be the maximum length of a
@   FORTH word that we support, and that is what WORD is used for: to read FORTH words when
@   we are compiling and executing code.  The returned strings are not NUL-terminated.
@
@   Start address+length is the normal way to represent strings in FORTH (not ending in an
@   ASCII NUL character as in C), and so FORTH strings can contain any character including NULs
@   and can be any length.
@
@   WORD is not suitable for just reading strings (eg. user input) because of all the above
@   peculiarities and limitations.
@
@   Note that when executing, you'll see:
@   WORD FOO
@   which puts "FOO" and length 3 on the stack, but when compiling:
@   : BAR WORD FOO ;
@   is an error (or at least it doesn't do what you might expect).  Later we'll talk about compiling
@   and immediate mode, and you'll understand why.

    defcode "word",,WORD
    bl _word
    push {r1}                           @ push the address of the word on to the stack
    push {r0}                           @ push the length of the word on to the stack
    NEXT

_word:
    push {r4-r5, lr}
    eor r5, r5                          @ r5 = length, set to 0

    @ Search for first non-blank character.  Also skip \ comments.
1:
    bl _key		                        @ get next key, returned in r0
    cmp r0, #'\\'   		            @ start of a comment?
    beq 4f			                    @ if so, skip the comment
    cmp r0, #' '                        @ is it a blank?
    beq 1b			                    @ if so, keep looking
    cmp r0, #0x0D                       @ is it a CR?
    beq 1b			                    @ if so, keep looking

    @ Search for the end of the word, storing chars as we go.
    ldr r4, =word_buffer	            @ r4 = pointer to return buffer
2:
    strb r0, [r4], #1	                @ store character in return buffer
    add r5, #1                          @ increment length
    bl _key 	                        @ get next key, returned in r0
    cmp r0, #' '    		            @ is blank?
    beq 3f			                    		                    
    cmp r0, #0x0D                       @ is it a CR?
    beq 3f			                    
    b 2b                                @ keep reading characters

    @ Return the word (well, the static buffer) and length. 
3:
    mov r0, r5                          @ length of word
    ldr r1, =word_buffer                @ pointer to return buffer
    pop {r4-r5, pc}

    @ Code to skip \ comments to end of the current line.
4:
    bl _key
    cmp r0, #13   		                @ end of line yet?
    bne 4b
    b 1b

	.data                               @ NB: easier to fit in the .data section
	@ A static buffer where WORD returns.  Subsequent calls
	@ overwrite this buffer.  Maximum word length is 32 chars.
word_buffer:
	.space 32


@   As well as reading in words we'll need to read in numbers and for that we are using a function
@   called NUMBER.  This parses a numeric string such as one returned by WORD and pushes the
@   number on the parameter stack.
@
@   The function uses the variable BASE as the base (radix) for conversion, so for example if
@   BASE is 2 then we expect a binary number.  Normally BASE is 10.
@
@   If the word starts with a '-' character then the returned value is negative.
@
@   If the string can't be parsed as a number (or contains characters outside the current BASE)
@   then we need to return an error indication.  So NUMBER actually returns two items on the stack.
@   At the top of stack we return the number of unconverted characters (ie. if 0 then all characters
@   were converted, so there is no error).  Second from top of stack is the parsed number or a
@   partial value if there was an error.

    defcode "number",,NUMBER
    pop {r0}                            @ length of string
    pop {r1}                            @ start address of c-string
    bl _number
    push {r0}                           @ parsed number
    push {r1}                           @ number of unparsed characters (0 = no error)
    NEXT

_number:
    push {r4-r5}
    mov r2, r1                          @ r2 = start address of string
    mov r1, r0                          @ r1 = length of string
    eor r0, r0                          @ r0 = parsed number (cleared)
    eor r5, r5                          @ r5 = negative flag (cleared)

    cmp r1, #0		                    @ trying to parse a zero-length string is an error, but will return 0.
    beq 5f

    ldr r3, =var_base	                @ pointer to BASE
    ldrb r3, [r3]                       @ r3 = BASE

    @ Check if first character is '-'.
    ldrb r4, [r2], #1                   @ r5 = first character in string
    cmp r4, #'-'    		            @ negative number?
    bne 2f
    mov r5, #1                          @ set flag to indicate negative number
    subs r1, #1                         @ decrement length of string
    bne 1f
    mov r1, #1
    pop {r4-r5, pc}

    @ Loop reading digits.
1:  cmp r1, #0                          @ end of string?   
    beq 4f
    mul r0, r3, r0                      @ r0 *= BASE
    ldrb r4, [r2], #1                   @ r5 = next character in string

    @ Convert 0-9, A-Z to a number 0-35.
2:  sub r4, #'0'		                @ < '0'?
    bmi 4f
    cmp r4, #10		                    @ <= '9'?
    blt 3f
    sub r4, #17		                    @ < 'A'? (17 is 'A'-'0')
    blt 4f
    add r4, #10

3:	cmp r4, r3		                    @ >= BASE?
    bge 4f

    @ OK, so add it to r0 and loop.
    add r0, r4
    sub r1, #1
    bne 1b

    @ Negate the result if first character was '-' (saved on the stack).
4:	cmp r5, #0
    beq 5f
    neg r0, r0

5:	pop {r4-r5}
    bx lr



@
@   MARK: Dictionary Look ups
@
@   We're building up to our prelude on how FORTH code is compiled, but first we need yet more infrastructure.
@
@   The FORTH word (FIND) takes a string (a word as parsed by WORD -- see above) and looks it up in the
@   dictionary.  What it actually returns is the address of the dictionary header, if it finds it,
@   or 0 if it didn't.
@
@   This word is called (FIND) but not simply FIND because FIND is a standard ANS Forth word which does a little
@   more than (FIND).
@
@   So if DOUBLE is defined in the dictionary, then WORD DOUBLE FIND returns the following pointer:
@
@   pointer to this
@       |
@       |
@       V
@       +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@       +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@
@   See also >CFA and >DFA.
@
@   FIND doesn't find dictionary entries which are flagged as HIDDEN.  See below for why.

defcode "(find)",,PAREN_FIND
    pop {r0}                            @ r0 = the length of the search string
    pop {r1}                            @ r1 = address of the search string
    bl _find
    push {r0}                           @ r0 = address of dictionary entry (or NULL)
    NEXT

_find: 
    push {r4-r7}
                                        @ r0 = length of the search string
                                        @ r1 = address of the search string
    @ TODO: convert the search word to lowercase

    @ Now we start searching backwards through the dictionary for this word.
    ldr r2, =var_latest                 @ r2 = points to current dictionary entry
 1: ldr r7, [r2]
    cmp r7, #0                          @ end of dictionary?
    beq 6f

    mov r6, r1                          @ r6 = address of the search string
    add r7, r7, #4                      @ r2 = address of the current word's name
    @ Compare the length expected and the length of the word.
    @ Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
    @ this won't pick the word (the length will appear to be wrong).
    ldrb r3, [r7], #1                   @ r3 = flags+length field of current entry
    and r3, #F_HIDDEN|F_LENMASK         @ r3 = name length + hidden flag
    cmp r3, r0		                    @ Length is the same?
    bne 3f                              @ No, so skip to next entry.   

    @ Compare the strings in detail.
2:  ldrb r4, [r6], #1                   @ r4 = load char from the search string
    ldrb r5, [r7], #1                   @ r5 = load char from the current entry name
    
    @ Convert r4 to lowercase if it's uppercase
    cmp r4, #'A'
    blt 3f
    cmp r4, #'Z'
    bgt 3f
    add r4, #32                         @ Convert to lowercase by adding 32

    @ Convert r5 to lowercase if it's uppercase 
3:  cmp r5, #'A'
    blt 4f
    cmp r5, #'Z'
    bgt 4f
    add r5, #32                         @ Convert to lowercase by adding 32

4:  cmp r4, r5                          @ Compare characters
    bne 5f                              @ Not the same, so skip to next entry
    subs r3, #1                         @ Decrement length
    bne 2b         
    
    @ The strings are the same - return the header pointer in r0
    ldr r0, [r2]
    pop {r4-r7}                         @ return to caller
    bx lr

    @ Not the same, so check the next entry.
5:	ldr r2, [r2]                        @ Move back through the link field to the previous word
    b 1b                                @ .. and loop.

6:	// Not found.
    eor r0, r0                          @ Return zero to indicate not found.
    pop {r4-r7}
    bx lr

@   (FIND) returns the dictionary pointer, but when compiling we need the codeword pointer (recall
@   that FORTH definitions are compiled into lists of codeword pointers).  The standard FORTH
@   word >CFA turns a dictionary pointer into a codeword pointer.
@
@   The example below shows the result of:
@
@       WORD DOUBLE (FIND) >CFA
@
@   (FIND) returns a pointer to this
@   |                                  >CFA converts it to a pointer to this
@   |                                          |
@   V                                          V
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@   | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@                                              codeword
@
@	Notes:
@
@	Because names vary in length, this isn't just a simple increment.
@
@	In this FORTH you cannot easily turn a codeword pointer back into a dictionary entry pointer, but
@	that is not true in most FORTH implementations where they store a back pointer in the definition
@	(with an obvious memory/complexity cost).  The reason they do this is that it is useful to be
@	able to go backwards (codeword -> dictionary entry) in order to decompile FORTH definitions
@	quickly.
@
@	What does CFA stand for?  My best guess is "Code Field Address".

defcode ">cfa",,TCFA
    pop {r0}                            @ r0 = address of the word
    bl _tcfa
    push {r0}                           @ r0 = address of the code field
    NEXT

_tcfa:
    add r0, #4                          @ r0 = address of the name field   
    ldrb r1, [r0], #1                   @ r1 = flags+length field of current entry
    and r1, #F_LENMASK                  @ r1 = length of name (remove fields)
    add r0, r1                          @ r0 = address of the code field
	add r0, #3                          @ The codeword is 4-byte aligned.
	and r0, #~3
    blx lr                              @ return to caller


@   Related to >CFA is >DFA which takes a dictionary entry address as returned by (FIND) and
@   returns a pointer to the first data field.
@
@   (FIND) returns a pointer to this
@   |                                  >CFA converts it to a pointer to this
@   |                                          |
@   |                                          |    >DFA converts it to a pointer to this
@   |                                          |            |
@   V                                          V            V
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@   | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@	                                           codeword
@
@   (Note to those following the source of FIG-FORTH / ciforth: My >DFA definition is
@   different from theirs, because they have an extra indirection).
@
@   You can see that >DFA is easily defined in FORTH just by adding 4 to the result of >CFA.

    defword ">dfa",,TDFA
    .word TCFA                          @ >CFA  (get code field address)
    .word INCR4                         @ 4+    (add 4 to it to get to next word)
    .word EXIT                          @ EXIT  (return from FORTH word)


@
@   MARK: Compiling
@
@   Now we'll talk about how FORTH compiles words.  Recall that a word definition looks like this:
@
@       : DOUBLE DUP + ;
@
@   and we have to turn this into:
@
@     pointer to previous word
@      ^
@      |
@   +--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@   | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@   +---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
@      ^       len                         pad  codeword      |
@      |                                                      V
@     LATEST points here                                    points to codeword of DUP
@
@   There are several problems to solve.  Where to put the new word?  How do we read words?  How
@   do we define the words : (COLON) and ; (SEMICOLON)?
@
@   FORTH solves this rather elegantly and as you might expect in a very low-level way which
@   allows you to change how the compiler works on your own code.
@
@   FORTH has an INTERPRET function (a true interpreter this time, not DOCOL) which runs in a
@   loop, reading words (using WORD), looking them up (using (FIND)), turning them into codeword
@   pointers (using >CFA) and deciding what to do with them.
@
@   What it does depends on the mode of the interpreter (in variable STATE).
@
@   When STATE is zero, the interpreter just runs each word as it looks them up.  This is known as
@   immediate mode.
@
@   The interesting stuff happens when STATE is non-zero -- compiling mode.  In this mode the
@   interpreter appends the codeword pointer to user memory (the DP variable points to the next
@   free byte of user memory -- see DATA SEGMENT section below).
@
@   So you may be able to see how we could define : (COLON).  The general plan is:
@
@   (1) Use WORD to read the name of the function being defined.
@
@   (2) Construct the dictionary entry -- just the header part -- in user memory:
@
@        pointer to previous word (from LATEST)    +-- Afterwards, DP points here, where
@          ^                                       |   the interpreter will start appending
@          |                                       V   codewords.
@       +--|------+---+---+---+---+---+---+---+---+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
@       +---------+---+---+---+---+---+---+---+---+------------+
@                  len                         pad  codeword
@
@   (3) Set LATEST to point to the newly defined word, ...
@
@   (4) .. and most importantly leave DP pointing just after the new codeword.  This is where
@       the interpreter will append codewords.
@
@   (5) Set STATE to 1.  This goes into compile mode so the interpreter starts appending codewords to
@       our partially-formed header.
@
@   After : has run, our input is here:
@
@   : DOUBLE DUP + ;
@            ^
@            |
@           Next byte returned by KEY will be the 'D' character of DUP
@
@   so the interpreter (now it's in compile mode, so I guess it's really the compiler) reads "DUP",
@   looks it up in the dictionary, gets its codeword pointer, and appends it:
@
@                                                                            +-- DP updated to point here.
@                                                                            |
@                                                                            V
@       +---------+---+---+---+---+---+---+---+---+------------+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        |
@       +---------+---+---+---+---+---+---+---+---+------------+------------+
@                  len                         pad  codeword
@
@   Next we read +, get the codeword pointer, and append it:
@
@                                                                                         +-- DP updated to point here.
@                                                                                         |
@                                                                                         V
@       +---------+---+---+---+---+---+---+---+---+------------+------------+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          |
@       +---------+---+---+---+---+---+---+---+---+------------+------------+------------+
@                  len                         pad  codeword
@
@   The issue is what happens next.  Obviously what we _don't_ want to happen is that we
@   read ";" and compile it and go on compiling everything afterwards.
@
@   At this point, FORTH uses a trick.  Remember the length byte in the dictionary definition
@   isn't just a plain length byte, but can also contain flags.  One flag is called the
@   IMMEDIATE flag (F_IMMED in this code).  If a word in the dictionary is flagged as
@   IMMEDIATE then the interpreter runs it immediately _even if it's in compile mode_.
@
@   This is how the word ; (SEMICOLON) works -- as a word flagged in the dictionary as IMMEDIATE.
@
@   And all it does is append the codeword for EXIT on to the current definition and switch
@   back to immediate mode (set STATE back to 0).  Shortly we'll see the actual definition
@   of ; and we'll see that it's really a very simple definition, declared IMMEDIATE.
@
@   After the interpreter reads ; and executes it 'immediately', we get this:
@
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@   | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
@   +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
@              len                         pad  codeword                                           ^
@                                                                                                  |
@                                                                                                  DP
@   STATE is set to 0.
@
@   And that's it, job done, our new definition is compiled, and we're back in immediate mode
@   just reading and executing words, perhaps including a call to test our new word DOUBLE.
@
@   The only last wrinkle in this is that while our word was being compiled, it was in a
@   half-finished state.  We certainly wouldn't want DOUBLE to be called somehow during
@   this time.  There are several ways to stop this from happening, but in FORTH what we
@   do is flag the word with the HIDDEN flag (F_HIDDEN in this code) just while it is
@   being compiled.  This prevents (FIND) from finding it, and thus in theory stops any
@   chance of it being called.
@
@	The above explains how compiling, : (COLON) and ; (SEMICOLON) works and in a moment I'm
@	going to define them.  The : (COLON) function can be made a little bit more general by writing
@	it in two parts.  The first part, called HEADER, (HEADER_COMMA), makes just the header:
@
@                                                  +-- Afterwards, DP points here.
@                                                  |
@                                                  V
@       +---------+---+---+---+---+---+---+---+---+
@       | LINK    | 6 | D | O | U | B | L | E | 0 |
@       +---------+---+---+---+---+---+---+---+---+
@                  len                         pad
@
@   and the second part, the actual definition of : (COLON), calls HEADER, (HEADER_COMMA) and appends the
@   DOCOL codeword, so leaving:
@
@                                                               +-- Afterwards, DP points here.
@                                                               |
@                                                               V
@       +---------+---+---+---+---+---+---+---+---+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
@       +---------+---+---+---+---+---+---+---+---+------------+
@                  len                         pad  codeword
@
@   HEADER, (HEADER_COMMA) is a FORTH word and the advantage of this split is that we can reuse it to
@   create other types of words (not just ones which contain code, but words which contain variables,
@   constants and other data).

    defcode "header,",,HEADER_COMMA
    // Get the name length and address.
    pop {r0}                            @ r0 = length of the name
    pop {r1}                            @ r1 = address of the name
    
    // Link pointer.
    ldr r2, =var_dp                     @ r2 = address of variable HERE 
    ldr r2, [r2]                        @ r2 = the address of the new header
    ldr r3, =var_latest                 @ r3 = address of variable LATEST
    ldr r3, [r3]                        @ r4 = the address of the next entry
    str r3, [r2], #4                    @ store it in the header, increment
    and r0, #F_LENMASK                  @ mask length of name to valid range
    strb r0, [r2], #1                   @ store the length of the name in the header
1:  ldrb r3, [r1], #1                   @ r3 = character from the name, increment
    strb r3, [r2], #1                   @ store the name, increment
    subs r0, #1                         @ decrement the length of the name
    bne 1b                              @ if not, loop
    add r2, #3                          @ align to next 4 byte boundary
    and r2, #~3

    // Update LATEST and DP
    ldr r0, =var_dp                     @ r0 = address of HERE
    ldr r0, [r0]                        @ r0 = the value of HERE
    ldr r1, =var_latest                 @ r1 = address of LATEST
    str r0, [r1]                        @ store the address of the new header in LATEST
    ldr r0, =var_dp                     @ r0 = address of HERE
    str r2, [r0]                        @ store the address of the new header in HERE
    NEXT


@   Because I want to define : (COLON) in FORTH, not assembler, we need a few more FORTH words
@   to use.
@
@   The first is , (COMMA) which is a standard FORTH word which appends a 32 bit integer to the user
@   memory pointed to by DP, and adds 4 to DP.  So the action of , (COMMA) is:
@
@                                                       previous value of DP
@                                                                |
@                                                                V
@       +---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
@       | LINK    | 6 | D | O | U | B | L | E | 0 |             |  <data>    |
@       +---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
@                  len                         pad                            ^
@                                                                             |
@                                                                       new value of DP
@
@   and <data> is whatever 32 bit integer was at the top of the stack.
@
@   , (COMMA) is quite a fundamental operation when compiling.  It is used to append codewords
@   to the current word that is being compiled.

    defcode ",",,COMMA
    pop {r0}                            @ r0 = data to store
    bl _comma
    NEXT

_comma:
    ldr r1, =var_dp                     @ r1 = address of the HERE variable
    ldr r2, [r1]                        @ r2 = the address of HERE (stored in the HERE variable)
    str r0, [r2], #4                    @ store the data at HERE
    str r2, [r1]                        @ update HERE
    bx lr                               @ return to caller


@   Our definitions of : (COLON) and ; (SEMICOLON) will need to switch to and from compile mode.
@
@   Immediate mode vs. compile mode is stored in the global variable STATE, and by updating this
@   variable we can switch between the two modes.
@
@   For various reasons which may become apparent later, FORTH defines two standard words called
@   [ and ] (LBRAC and RBRAC) which switch between modes:
@
@       Word    Assembler   Action      Effect
@       [       LBRAC       STATE := 0  Switch to immediate mode.
@       ]       RBRAC       STATE := 1  Switch to compile mode.
@
@   [ (LBRAC) is an IMMEDIATE word.  The reason is as follows: If we are in compile mode and the
@   interpreter saw [ then it would compile it rather than running it.  We would never be able to
@   switch back to immediate mode!  So we flag the word as IMMEDIATE so that even in compile mode
@   the word runs immediately, switching us back to immediate mode.

    defcode "[",F_IMMED,LBRAC
    eor r0, r0                          @ clear r0
    ldr r1, =var_state                  @ r1 = address of STATE
    str r0, [r1]                        @ set STATE to 0
    NEXT

    defcode "]",,RBRAC
    mov r0, #1
    ldr r1, =var_state                  @ r1 = address of STATE
    str r0, [r1]                        @ set STATE to 1.
    NEXT


@   Now we can define : (COLON) using HEADER, (HEADER_COMMA).  It just calls HEADER, (HEADER_COMMA),
@   appends DOCOL (the codeword), sets the word HIDDEN and goes into compile mode.

    defword ":",,COLON
    .word WORD                          @ Get the name of the new word
    .word HEADER_COMMA                  @ CREATE the dictionary entry / header
    .word LIT, DOCOL, COMMA             @ Append DOCOL  (the codeword).
    .word LATEST, FETCH, HIDDEN         @ Make the word hidden (see below for definition).
    .word RBRAC                         @ Go into compile mode.
    .word EXIT                          @ Return from the function.


@   ; (SEMICOLON) is also elegantly simple.  Notice the F_IMMED flag.

    defword ";",F_IMMED,SEMICOLON
    .word LIT, EXIT, COMMA              @ Append EXIT (so the word will return).
    .word LATEST, FETCH, HIDDEN         @ Toggle hidden flag -- unhide the word (see below for definition).
    .word LBRAC                         @ Go back to IMMEDIATE mode.
    .word EXIT                          @ Return from the function.



@
@   MARK: Extending the compiler
@
@   Words flagged with IMMEDIATE (F_IMMED) aren't just for the FORTH compiler to use.  You can define
@   your own IMMEDIATE words too, and this is a crucial aspect when extending basic FORTH, because
@   it allows you in effect to extend the compiler itself.  Does gcc let you do that?
@
@   Standard FORTH words like IF, WHILE, ." and so on are all written as extensions to the basic
@   compiler, and are all IMMEDIATE words.
@
@   The IMMEDIATE word toggles the F_IMMED (IMMEDIATE flag) on the most recently defined word,
@   or on the current word if you call it in the middle of a definition.
@
@   Typical usage is:
@
@       : MYIMMEDWORD IMMEDIATE
@           ...definition...
@       ;
@
@   but some FORTH programmers write this instead:
@
@       : MYIMMEDWORD
@           ...definition...
@       ; IMMEDIATE
@
@   The two usages are equivalent, to a first approximation.

    defcode "immediate",F_IMMED,IMMEDIATE
    ldr r0, =var_latest                 @ r0 = address of LATEST
    ldr r0, [r0]                        @ r0 = the address of the new header
    add r0, #4                          @ Point to the name/flags byte.
    ldrb r1, [r0]                       @ r1 = flags byte
    eor r1, #F_IMMED                    @ toggle the IMMEDIATE bit
    strb r1, [r0]                       @ store it back
    NEXT


@   'addr HIDDEN' toggles the hidden flag (F_HIDDEN) of the word defined at addr.  To hide the
@   most recently defined word (used above in : and ; definitions) you would do:
@
@       LATEST @ HIDDEN
@
@   'HIDE word' toggles the flag on a named 'word'.
@
@   Setting this flag stops the word from being found by (FIND), and so can be used to make 'private'
@   words.  For example, to break up a large word into smaller parts you might do:
@
@       : SUB1 ... subword ... ;
@       : SUB2 ... subword ... ;
@       : SUB3 ... subword ... ;
@       : MAIN ... defined in terms of SUB1, SUB2, SUB3 ... ;
@       HIDE SUB1
@       HIDE SUB2
@       HIDE SUB3
@
@   After this, only MAIN is 'exported' or seen by the rest of the program.

    defcode "hidden",,HIDDEN
    pop {r0}                            @ r0 = dictionary entry
    add r0, #4                          @ Point to name/flags byte.
    ldrb r1, [r0]                       @ r1 = flags byte
    eor r1, #F_HIDDEN                   @ toggle the HIDDEN bit.
    strb r1, [r0]                       @ store it back    
    NEXT

    defword "hide",,HIDE
    .word WORD                          @ Get the word (after HIDE).
    .word PAREN_FIND                    @ Look up in the dictionary.
    .word HIDDEN                        @ Set F_HIDDEN flag.
    .word EXIT                          @ Return.


@   ['] (BRACKET_TICK) is a standard FORTH word which returns the codeword pointer of the next word.
@
@   The common usage is:
@
@       ['] FOO ,
@
@   which appends the codeword of FOO to the current word we are defining (this only works in compiled code).
@
@   You tend to use ['] in IMMEDIATE words.  For example an alternate (and rather useless) way to define
@   a literal 2 might be:
@
@       : LIT2 IMMEDIATE
@           ['] LIT ,   \ Appends LIT to the currently-being-defined word
@           2 ,         \ Appends the number 2 to the currently-being-defined word
@       ;
@
@   So you could do:
@
@       : DOUBLE LIT2 * ;
@
@   (If you don't understand how LIT2 works, then you should review the material about compiling words
@   and immediate mode).
@
@   This definition of ['] uses a cheat which I copied from buzzard92.  As a result it only works in
@   compiled code.  It is possible to write a version of [']' based on WORD, (FIND), >CFA which works in
@   immediate mode too.

@   This definition of ['] uses a cheat which I copied from buzzard92.  As a result it only works in
@   compilied and when the word after it is not an immediate word. It is possible to write a version
@   of ['] which works on immediate word, or to write another version based on WORD, (FIND), >CFA
@   which works in immediate mode too.

    defcode "[']",,BRACKET_TICK
    ldr r0, [r5], #4			        // Get the address of the next word and skip it.
    push {r0}		                    // Push it on the stack.
    NEXT



@
@   MARK: Branching
@
@   It turns out that all you need in order to define looping constructs, IF-statements, etc.
@   are two primitives.
@
@   BRANCH is an unconditional branch. 0BRANCH is a conditional branch (it only branches if the
@   top of stack is zero).
@
@   The diagram below shows how BRANCH works in some imaginary compiled word.  When BRANCH executes,
@   r5 starts by pointing to the offset field (compare to LIT above):
@
@   +---------------------+-------+---- - - ---+------------+------------+---- - - - ----+------------+
@   | (Dictionary header) | DOCOL |            | BRANCH     | offset     | (skipped)     | word       |
@   +---------------------+-------+---- - - ---+------------+-----|------+---- - - - ----+------------+
@                                                              ^  |                       ^
@                                                              |  |                       |
@                                                              |  +-----------------------+
@                                                             r5 added to offset
@
@   The offset is added to r5 to make the new r5, and the result is that when NEXT runs, execution
@   continues at the branch target.  Negative offsets work as expected.
@
@   0BRANCH is the same except the branch happens conditionally.
@
@   Now standard FORTH words such as IF, THEN, ELSE, WHILE, REPEAT, etc. can be implemented entirely
@   in FORTH.  They are IMMEDIATE words which append various combinations of BRANCH or 0BRANCH
@   into the word currently being compiled.
@
@   As an example, code written like this:
@
@       condition-code IF true-part THEN rest-code
@
@   compiles to:
@
@       condition-code 0BRANCH OFFSET true-part rest-code
@                                 |             ^
@                                 |             |
@                                 +-------------+

    defcode "branch",,BRANCH
    ldr r0, [r5]                        @ Get the address of the next word and skip it.
    add r5, r0                          @ Add the offset to the instruction pointer.
    NEXT

    defcode "0branch",,ZBRANCH
    pop {r0}
    cmp r0, #0                          @ top of stack is zero?
    beq code_BRANCH                     @ if so, jump back to the branch function above
    ldr r0, [r5], #4
    NEXT



@
@   MARK: Literal Strings
@
@   LITSTRING is a primitive used to implement the ." and S" operators (which are written in
@   FORTH).  See the definition of those operators later.
@
@   TELL just prints a string.  It's more efficient to define this in assembly because we
@   can make it a single call into our C code.

    defcode "litstring",,LITSTRING
    ldr r0, [r5], #4                    @ get the length of the string
    push {r5}                           @ push the address of the string
    push {r0}                           @ push the length of the string
    add r5, r0                          @ skip past the string
    add r5, #3                          @ but round up to next 4 byte boundary
    and r5, #~3
    NEXT

    defcode "tell",,TELL
    pop {r1}                            @ r1 = length of the string
    pop {r0}                            @ r0 = address of the string
    bl _emit_string                      @ call the C function to print the string
    NEXT



@
@   MARK: Combining Data and Behavior
@
@   A FORTH word with codeword DOCOL defines the word's behavior with other FORTH words.
@   On the other hand, a built-in variable's behavior is simply push the pointer to its
@   data onto the stack.
@
@   One will wonder how can a FORTH word which owns complicated data specify its behavior on
@   these data using other FORTH words. The magic word DODOES serves this purpose. 
@
@   The diagram below shows how DODOES works.  When DODOES executes, it pushes the pointer to
@   its data onto the stack and execute the behavior words pointed to by the behavior pointer.
@
@   +---------------------+--------+---------+------+----------------+
@   | (Dictionary header) | DODOES |         | data | behavior words |
@   +---------------------+--------+---------+------+----------------+
@                        behavior pointer |           ^
@                                         |           |
@                                         +-----------+
@
@   When the behavior pointer is zero, there is no behavior words, DODOES simply pushes the pointer to
@   its data onto the stack.
@
@   +---------------------+--------+--------+------+
@   | (Dictionary header) | DODOES |   0    | data |
@   +---------------------+--------+--------+------+

    .text
    .align 2
DODOES:
    ldr r1, [r0, #4]                    @ r1 = behavior pointer
    cmp r1, #0                          @ Is it zero?
    beq 1f                              @ If so, skip the behavior words
    PUSHRSP r5
    add r5, r0, #4
1:  add r0, #8
    push {r0}                           @ Push the pointer to its data
    NEXT

    defconst "dodoes",,__DODOES,DODOES



@
@	MARK: Quit and Interpret
@
@   QUIT is the first FORTH function called, almost immediately after the FORTH system "boots".
@   As explained before, QUIT doesn't "quit" anything.  It does some initialisation (in particular
@   it clears the return stack) and it calls INTERPRET in a loop to interpret commands.  The
@   reason it is called QUIT is because you can call it from your own FORTH words in order to
@   "quit" your program and start again at the user prompt.
@
@   INTERPRET is the FORTH interpreter ("toploop", "toplevel" or "REPL" might be a more accurate
@   description -- see: http://en.wikipedia.org/wiki/REPL).

    @ QUIT must not return (ie. must not call EXIT).
    defword "quit",,QUIT
    .word RZ,RSPSTORE                   @ R0 RSP!, clear the return stack
    .word INTERPRET                     @ interpret the next word
    .word BRANCH,-8                     @ and loop (indefinitely)


@   This interpreter is pretty simple, but remember that in FORTH you can always override
@   it later with a more powerful one!

    defcode "interpret",,INTERPRET
    push {r4-r7}                        @ save registers
    bl _word                            @ Returns r0 = length, r1 = pointer to word.

    // Is it in the dictionary?
    mov r7, r0                          @ r7 = length of the word
    mov r5, r1                          @ r5 = address of the word 
    eor r4, r4                          @ clear literal number
    ldr r6, =interpret_is_lit           @ r6 = address of the global variable
    str r4, [r6]                        @ Not a literal number (not yet anyway ...)
    bl _find                            @ Returns r0 = pointer to header or 0 if not found.
    cmp r0, #0                          @ Found?
    beq 1f                              @ No, so assume it's a number.

    @ In the dictionary.  Is it an IMMEDIATE codeword?
    ldrb r5, [r0, #4]                   @ Get the flags byte.
    bl _tcfa                            @ Convert dictionary entry (in r0) to codeword pointer.
    ands r5, #F_IMMED                   @ Is IMMED flag set?
    bne 4f                              @ If IMMED, jump straight to executing.
    b 2f

1:	@ Not in the dictionary (not a word) so assume it's a literal number.
    add r4, #1                          @ Increment the literal number
    str r4, [r6]                        @ Store the literal number in the global variable
    mov r0, r7                          @ r0 = length of the word
    mov r1, r5                          @ r1 = address of the word
    bl _number                          @ Returns the parsed number in r0, r1 > 0 if error
    cmp r1, #0                          @ Is it a number?
    bne 6f                              @ No, so error.  
    mov r7, r0
    ldr r0, =LIT                        @ The word is LIT

2:	@ Are we compiling or executing?
    ldr r2, =var_state                  @ r2 = address of STATE
    ldr r2, [r2]                        @ r2 = the value of STATE
    cmp r2, #0                          @ Is STATE = 0?
    beq 4f                              @ Jump if executing.

    @ Compiling - just append the word to the current dictionary definition.
    bl _comma
    cmp r4, #0                          @ Is it a literal number?
    beq 3f
    mov r0, r7                          @ Yes, so LIT is followed by a number.
    bl _comma
3:  pop {r4-r7}
    NEXT

4:	@ Executing - run it!
    cmp r4, #0                          @ Is it a literal number?
    bne 5f

    @ Not a literal, execute it now.  This never returns, but the codeword will
    @ eventually call NEXT which will reenter the loop in QUIT.
    pop {r4-r7}                         @ pop the parameters off the stack
    ldr r1, [r0]
    orr r1, #1                          @ set the thumb bit   
    blx r1                              @ execute the codeword

5:	@ Executing a literal, which means push it on the stack.
    mov r0, r7                          @ r0 = the literal number
    pop {r4-r7}                         @ pop the parameters off the stack
    push {r0}
    NEXT

6:	@ Parse error (not a known word or a number in the current BASE).
    @ Print an error message followed by up to 40 characters of context.
    ldr r0, =errmsg
    mov r1, #12
    bl _emit_string

    pop {r4-r7}                         // pop the parameters off the stack
    NEXT

    .data			                    // NB: easier to fit in the .data section
    .align 2
errmsg:
    .asciz "parse error\n"
    .align 2
interpret_is_lit:
    .word 0			                    // Flag used to record if reading a literal



@
@   MARK: ANS Forth Core Words
@
@   Some lower level ANS Forth CORE words are not presented in the orignal jonesforth. They are
@   included here without explaination.

    @ Macros to deal with the return stack.
	.macro PUSH2RSP reg1, reg2
    str \reg1, [r6, #-4]!               @ push reg on to return stack
    str \reg2, [r6, #-4]!               @ push reg on to return stack
    .endm

	.macro POP2RSP reg1, reg2
    ldr \reg2, [r6], #4                 @ pop reg from return stack
    ldr \reg1, [r6], #4                 @ pop reg from return stack
	.endm

	defcode "2*",,TWOMUL
    pop {r0}
	lsl r0, #1
    push {r0}                            @ push the result on to the stack
    NEXT

	defcode "2/",,TWODIV
    pop {r0}
	asr r0, #1
    push {r0}
	NEXT

	defcode "(do)",,PAREN_DO
	pop {r0}                            @ pop parameter stack into %eax and %edx
	pop {r1}
	PUSH2RSP r1, r0                     @ push it on to the return stack
	NEXT

	defcode "(loop)",,PAREN_LOOP
	POP2RSP r1, r0
	add r0, #1
    cmp r1, r0
    beq 1f
	PUSH2RSP r1, r0
    ldr r1, [r5]                        @ Get the offset of the next instruction
    add r5, r1                          @ Add the offset to the instruction pointer
	NEXT
1:
	ldr r0, [r5], #4                    @ Get the next instruction
	NEXT

	defcode "(+loop)",,PAREN_PLUS_LOOP
	POP2RSP r3, r0                      @ index in r0, limit in r3
	sub r0, r3                          @ index-limit in r0
	pop {r1}                            @ n in r1
	add r1, r0                          @ index-limit+n in %ebx
	eor r0, r1                          @ (index-limit) and (index-limit+n) have different sign?
	bmi 1f
	add r1, r3                          @ index+n in r1
	PUSH2RSP r3, r1
    ldr r0, [r5]                        @ Load value from memory pointed to by r5
    add r5, r0                      @ Add that value to r5	NEXT
1:
	ldr r0, [r5], #4
	NEXT

	defcode "unloop",,UNLOOP
    add r6, #8
	NEXT

	defcode "i",,I
	push {r6}
	NEXT

	defcode "j",,J
    mov r0, r6
    add r0, #8
	push {r0}
	NEXT

   defword "/",,DIVIDE
    .word DIVMOD
    .word SWAP
    .word DROP
    .word EXIT

    defword "mod",,MOD
    .word DIVMOD
    .word DROP
    .word EXIT

    defcode "'\\n'",,NL
    mov r0, #0x0D                       @ Load the ASCII code for newline
    push {r0}
    NEXT

    @ bl (BLank) is a standard FORTH word for space.
    defcode "bl",,BL
    mov r0, #0x20                       @ Load the ASCII code for space
    push {r0}
    NEXT

    defcode "cr",,CR
    mov r0, #0x0D                       @ Load the ASCII code for carriage return
    bl _emit
    NEXT

    defcode "space",,SPACE
    mov r0, #0x20                       @ Load the ASCII code for space
    bl _emit                            @ Call the C function to print the character
    NEXT

    defcode "negate",,NEGATE
    pop {r0}
    rsb r0, r0, #0                      @ Negate the number
    push {r0}
    NEXT

    defcode "true",,TRUE
    mov r0, #-1
    push {r0}
    NEXT

    defcode "false",,FALSE
    mov r0, #0
    push {r0}
    NEXT

    defcode "not",,NOT
    b code_ZEQU
 

    defword "literal",F_IMMED,LITERAL
    .word BRACKET_TICK, LIT, COMMA      @ compile lit
    .word COMMA                         @ compile the literal itself (from the stack)
    .word EXIT

    defcode "':'",,CHAR_COLON
    mov r0, #0x3A                       @ Load the ASCII code for ':'
    push {r0}
    NEXT

    @ here fetch the data stack pointer. => : here dp @ ;
    defcode "here",,HERE
    ldr r0, =var_dp
    ldr r0, [r0]
    push {r0}
    NEXT

    defcode "';'",,CHAR_SEMICOLON
    mov r0, #0x3B                       @ Load the ASCII code for ';'
    push {r0}
    NEXT

    defcode "'('",,CHAR_LPAREN
    mov r0, #0x28                       @ Load the ASCII code for '('
    push {r0}
    NEXT

    defcode "')'",,CHAR_RPAREN
    mov r0, #0x29                       @ Load the ASCII code for ')'
    push {r0}
    NEXT

    defcode "'\"'",,CHAR_QUOTE
    mov r0, #0x22                       @ Load the ASCII code for '"'
    push {r0}
    NEXT

    defcode "'A'",,CHAR_A
    mov r0, #0x41                       @ Load the ASCII code for 'A'
    push {r0}
    NEXT

    defcode "'0'",,CHAR_0
    mov r0, #0x30                       @ Load the ASCII code for '0'
    push {r0}
    NEXT

    defcode "'-'",,CHAR_MINUS
    mov r0, #0x2D                       @ Load the ASCII code for '-'
    push {r0}
    NEXT

    defcode "'.'",,CHAR_DOT
    mov r0, #0x2E                       @ Load the ASCII code for '.'
    push {r0}
    NEXT

    defword "[compile]",F_IMMED,BRACKET_COMPILE
    .word WORD                          @ get the next word
    .word PAREN_FIND                    @ find it in the dictionary
    .word TCFA                          @ get its code field 
    .word COMMA                         @ and compile that
    .word EXIT

    @ : recurse immediate latest @ >cfa , ;
    defword "recurse",F_IMMED,RECURSE
    .word LATEST, FETCH                 @ latest points to the word being compiled at the moment
    .word TCFA                          @ get the code field
    .word COMMA                         @ compile it
    .word EXIT

    @ : if immediate ['] 0branch , here 0 , ;
    defword "if",F_IMMED,IF
    .word BRACKET_TICK, ZBRANCH, COMMA  @ compile 0branch
    .word HERE                          @ save location of the offset on the stack
    .word LIT, 0, COMMA                 @ compile a dummy offset
    .word EXIT

    @ : then immediate dup here swap - swap ! ;
    defword "then",F_IMMED,THEN
    .word DUP
    .word HERE, SWAP, SUB               @ calculate the offset from the address saved on the stack
    .word SWAP, STORE                   @ store the offset in the back-filled location
    .word EXIT

    @ : else immediate ['] branch , here 0 , swap dup here swap - swap ! ;
    defword "else",F_IMMED,ELSE
    .word BRACKET_TICK, BRANCH, COMMA   @ definite branch to just over the false-part
    .word HERE                          @ save location of the offset on the stack
    .word LIT, 0, COMMA                 @ compile a dummy offset
    .word SWAP                          @ now back-fill the original (if) offset
    .word DUP                           @ same as for then word above
    .word HERE, SWAP, SUB
    .word SWAP, STORE
    .word EXIT

    @ : begin immediate here ;
    defword "begin",F_IMMED,BEGIN
    .word HERE                          @ save location on the stack
    .word EXIT

    @ : until immediate ['] 0branch , here - , ;
    defword "until",F_IMMED,UNTIL
    .word BRACKET_TICK, ZBRANCH, COMMA  @ compile 0branch
    .word HERE, SUB                     @  calculate the offset from the address saved on the stack
    .word COMMA                         @ compile the offset here
    .word EXIT

    @ : again immediate ['] branch , here - , ;
    defword "again",F_IMMED,AGAIN
    .word BRACKET_TICK, BRANCH, COMMA   @ compile branch
    .word HERE, SUB                     @ calculate the offset back
    .word COMMA                         @ compile the offset here
    .word EXIT

    @ : while immediate ['] 0branch , here swap 0 , ;
    defword "while",F_IMMED,WHILE
    .word BRACKET_TICK, ZBRANCH, COMMA  @ compile 0branch
    .word HERE                          @ save location of the offset2 on the stack
    .word SWAP                          @ get the original offset (from begin)
    .word LIT, 0, COMMA                 @ compile a dummy offset2
    .word EXIT

    @ : repeat immediate ['] branch , here - , dup here swap - swap ! ;
    defword "repeat",F_IMMED,REPEAT
    .word BRACKET_TICK, BRANCH, COMMA   @ compile branch
    .word HERE, SUB, COMMA              @ and compile it after branch
    .word DUP
    .word HERE, SWAP, SUB               @ calculate the offset2
    .word SWAP, STORE                   @ and back-fill it in the original location
    .word EXIT

    @ : unless immediate ['] not , if ;
    defword "unless",F_IMMED,UNLESS
    .word BRACKET_TICK, NOT, COMMA      @ compile not (to reverse the test)
    .word IF                            @ continue by calling the normal if
    .word EXIT

    @ : ( immediate 1 key dup '(' = 0branch ( 20 ) drop 1+ branch ( 24 ) ')' = 0branch ( 8 ) 1- dup 0= 0branch ( -72 ) drop ;
    defword "(",F_IMMED,comment
    .word LIT, 1                        @ allowed nested parens by keeping track of depth
    .word KEY                           @ read next character
    .word DUP, CHAR_LPAREN, EQU, ZBRANCH, 20 @ open paren?
    .word DROP                          @ drop the open paren
    .word INCR                          @ depth increases
    .word BRANCH, 24                    @ branch to the end of the comment
    .word CHAR_RPAREN, EQU, ZBRANCH, 8  @ close paren?
    .word DECR                          @ depth decreases
    .word DUP, ZEQU, ZBRANCH, -72       @ continue until we reach matching close paren, depth 0
    .word DROP                          @ drop the depth counter
    .word EXIT

    @ : nip ( x y -- y ) swap drop ;
    defword "nip",,NIP
    .word SWAP
    .word DROP
    .word EXIT

    @ : tuck ( x y -- y x y ) swap over ;
    defword "tuck",,TUCK
    .word SWAP
    .word OVER
    .word EXIT

    @ : pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u ) 1+ 4 * DSP@ + @ ;
    defword "pick",,PICK
    .word INCR                          @ add one because of 'u' on the stack
    .word LIT, 4, MUL                   @ multiply by the word size
    .word DSPFETCH, ADD                @ add to the stack pointer
    .word FETCH                         @ and fetch
    .word EXIT

    @ : spaces ( n -- ) dup 0> 0branch ( 20 ) space 1- branch ( -28 ) drop ;
    defword "spaces",,SPACES
    .word DUP, ZGT, ZBRANCH, 20         @ is n > 0?
    .word SPACE                         @ if so, print a space
    .word DECR                          @ decrement n
    .word BRANCH, -28                   @ branch back to the start
    .word DROP                          @ drop n
    .word EXIT

    @ : decimal ( -- ) 10 base ! ;
    defword "decimal",,DECIMAL
    .word LIT, 10                       @ load the decimal base
    .word BASE
    .word STORE                         @ store it in the BASE variable
    .word EXIT

    @ : hex ( -- ) 16 base ! ;
    defword "hex",,HEX
    .word LIT, 16                       @ load the hex base
    .word BASE
    .word STORE                         @ store it in the BASE variable
    .word EXIT

    @ : u. ( u -- ) base @ u/mod ?dup 0branch ( 8 ) u. dup 10 < 0branch ( 16 ) '0' branch ( 20 ) 10 - 'A' + emit ;
    defword "u.",,U_DOT
    .word BASE, FETCH, UDIVMOD          @ width rem quot
    .word QDUP, ZBRANCH, 8              @ if quotient <> 0 then
    .word U_DOT                         @ recurse to print the quotient
    .word DUP, LIT, 10, LT, ZBRANCH, 16 @ print the remainder
    .word CHAR_0                        @ decimal digits 0..9 
    .word BRANCH, 20
    .word LIT, 10, SUB                  @ decimal digits 0..9 
    .word CHAR_A
    .word ADD
    .word EMIT
    .word EXIT

    @ : .S ( -- ) DSP@ 0 s0 @ + 4- over over <= 0branch ( 32 ) dup @ u. space 4- branch ( -44 ) drop drop ;
    defword ".s",,DOT_S
    .word DSPFETCH                      @ get current stack pointer
    .word LIT, 0, SZ, FETCH, ADD, DECR4 @ pointer to the stack element
    .word OVER, OVER, LE, ZBRANCH, 32   @ compare to current stack pointer
    .word DUP, FETCH, U_DOT             @ print the stack element
    .word SPACE
    .word DECR4                         @ move down
    .word BRANCH, -44
    .word DROP, DROP
    .word EXIT

    @ : uwidth ( u -- width ) base @ / ?dup 0branch ( 20 ) uwidth 1+ branch ( 12 ) 1 ;
    defword "uwidth",,UWIDTH
    .word BASE, FETCH, DIVIDE           @ rem quot
    .word QDUP, ZBRANCH, 20             @ if quotient <> 0 then
    .word UWIDTH, INCR                  @ return 1+recursive call
    .word BRANCH, 12
    .word LIT, 1                        @ return 1
    .word EXIT

    @ : u.r ( u width -- ) swap dup uwidth rot swap - spaces u. ;
    defword "u.r",,U_R
    .word SWAP                         @ width u
    .word DUP                          @ width u u
    .word UWIDTH                       @ width u uwidth
    .word ROT                          @ u uwidth width
    .word SWAP, SUB                    @ u width-uwidth
    .word SPACES
    .word U_DOT
    .word EXIT

    @ : .r ( n width -- ) swap dup 0< 0branch ( 36 ) negate 1 swap rot 1- branch ( 20 ) 0 swap rot
    @      swap dup uwidth rot swap - spaces swap 0branch ( 12 ) '-' emit u. ;
    defword ".r",,DOT_R
    .word SWAP                          @ width n
    .word DUP, ZLT, ZBRANCH, 36
    .word NEGATE                        @ width u
    .word LIT, 1                        @ save a flag to remember that it was negative | width n 1 
    .word SWAP                          @ width 1 u
    .word ROT                           @ 1 u width
    .word DECR                          @ 1 u width-1
    .word BRANCH, 20
    .word LIT, 0                        @ width u 0
    .word SWAP                          @ width 0 u
    .word ROT                           @ 0 u width
    .word SWAP                          @ flag width u
    .word DUP                           @ flag width u u
    .word UWIDTH                        @ flag width u uwidth
    .word ROT                           @ flag u uwidth width
    .word SWAP, SUB                     @ flag u width-uwidth
    .word SPACES                        @ flag u
    .word SWAP                          @ u flag
    .word ZBRANCH, 12                   @ was it negative? print the - character
    .word CHAR_MINUS, EMIT              @ if so, print the minus sign
    .word U_DOT
    .word EXIT

    @ : . 0 .r space ;
    defword ".",,DOT
    .word LIT, 0
    .word DOT_R
    .word SPACE
    .word EXIT

    @ : u. u. space ;
    defword "u.",,U_DOT_SPACE
    .word U_DOT
    .word SPACE
    .word EXIT

    @ : ? ( addr -- ) @ . ;
    defword "?",,QUESTION
    .word FETCH                         @ fetch the value at addr
    .word DOT
    .word EXIT
    
    @ : within -rot over <= 0branch ( 40 ) > 0branch ( 16 ) true branch ( 8 ) false branch ( 12 ) 2drop false ;
    defword "within",,WITHIN
    .word NROT                          @  b c a
    .word OVER                          @  b c a c
    .word LE, ZBRANCH, 40
    .word GT, ZBRANCH, 16               @  b c --
    .word TRUE
    .word BRANCH, 8                     @  else
    .word FALSE
    .word BRANCH, 12                    @ then
    .word TWODROP
    .word FALSE
    .word EXIT

    @ : depth ( -- n ) s0 @ DSP@ - 4- 4 / ;
    defword "depth",,DEPTH
    .word SZ, FETCH, DSPFETCH, SUB
    .word DECR4                         @ adjust because s0 was on the stack when we pushed DSP
    .word LIT, 4, DIVIDE                @ A cell has four bytes
    .word EXIT

    @ : aligned ( addr -- addr ) 3 + 3 invert and ;
    defword "aligned",,ALIGNED
    .word LIT, 3, ADD
    .word LIT, 3, INVERT
    .word AND
    .word EXIT

    @ : align here aligned dp ! ;
    defword "align",,ALIGN
    .word HERE
    .word ALIGNED
    .word DP
    .word STORE
    .word EXIT

    @  : c, here c! 1 dp +! ;
    defword "c,",,C_COMMA
    .word HERE, STOREBYTE               @ store the character in the compiled image
    .word LIT, 1, DP, ADDSTORE          @ increment dp pointer by 1 byte
    .word EXIT

    @ : s" ( -- addr len ) immediate state @ 0branch ( 112 ) ['] litstring , here 0 , key dup '"' <> 0branch ( 16 ) 
    @      c, branch ( -32 ) drop dup here swap - 4- swap ! align branch ( 72 ) here key dup '"' <> 0branch ( 24 )
    @      over c! 1+ branch ( -40 ) drop here - here swap ;
    defword "s\"",F_IMMED,S_QUOTE      @ s"
    .word STATE, FETCH, ZBRANCH, 112    @ compiling?
    .word BRACKET_TICK, LITSTRING, COMMA @ compile litstring
    .word HERE                          @ save the address of the length word on the stack
    .word LIT, 0, COMMA                      @ dummy length - we don't know what it is yet
    .word KEY                           @ get next character of the string
    .word DUP, CHAR_QUOTE, NEQU, ZBRANCH, 16
    .word C_COMMA                       @ copy character
    .word BRANCH, -32                   @ repeat
    .word DROP                          @ drop the double quote character at the end 
    .word DUP                           @ get the saved address of the length word
    .word HERE, SWAP, SUB               @ calculate the length
    .word DECR4                         @ subtract 4 (because we measured from the start of the length word)
    .word SWAP, STORE                   @ and back-fill the length location
    .word ALIGN                         @ round up to next multiple of 4 bytes for the remaining code
    .word BRANCH, 72                    @ immediate mode
    .word HERE                          @ get the start address of the temporary space
    .word KEY
    .word DUP, CHAR_QUOTE, NEQU
    .word ZBRANCH, 24                   @ while
    .word OVER, STOREBYTE               @ copy character
    .word INCR                          @ increment address
    .word BRANCH, -40                   @ repeat
    .word DROP                          @ drop the final " character
    .word HERE, SUB                     @ calculate the length
    .word HERE                          @ push the start address
    .word SWAP                          @ ( addr len )
    .word EXIT
    
    @ @ : ." immediate state @ 0branch ( 28 ) s" ['] tell , branch ( 48 ) key dup '"' = 0branch ( 12 ) drop exit emit branch ( -40 ) ;
    defword ".\"",F_IMMED,DOT_QUOTE     @ ."
    .word STATE, FETCH, ZBRANCH, 28     @ compiling?
    .word S_QUOTE                       @ read the string, and compile litstring, etc.
    .word BRACKET_TICK, TELL, COMMA     @ compile the final tell
    .word BRANCH, 48                    @ In immediate mode, just read characters and print them until we get
                                        @   to the ending double quote.
    .word KEY
    .word DUP, CHAR_QUOTE, EQU, ZBRANCH, 12
    .word DROP                          @ drop the double quote character
    .word EXIT                          @ return from this function
    .word EMIT
    .word BRANCH, -40                   @ repeat until we get to the double quote character
    .word EXIT

    @ : constant word header, docol , ['] lit , , ['] exit , ;
    defword "constant",,CONSTANT
    .word WORD                          @ get the name (the name follows constant)
    .word HEADER_COMMA                  @ make the dictionary entry
    .word DOCOL, COMMA                  @ append docol (the code field of this word)
    .word BRACKET_TICK, LIT, COMMA      @ append the code field lit
    .word COMMA                         @ append the value on the top of the stack
    .word BRACKET_TICK, EXIT, COMMA     @ append the code field exit
    .word EXIT

    @ : allot ( n -- addr ) dp +! ;
    defword "allot",,ALLOT
    .word DP, ADDSTORE                  @ adds n to here, after this the old value of here is still on the stack
    .word EXIT

    @ : cells ( n -- n ) 4 * ;
    defword "cells",,CELLS
    .word LIT, 4                        @ load the cell size
    .word MUL                           @ multiply by the cell size
    .word EXIT

    @ : variable word header, dodoes , 0 , 1 cells allot , ;
    defword "variable",,VARIABLE
    .word WORD, HEADER_COMMA          @ make the dictionary entry (the name follows variable)
    .word __DODOES, COMMA, LIT, 0, COMMA @ append docol (the code field of this word)
    .word LIT, 1, CELLS, ALLOT, COMMA @ allocate one cell of memory
    .word EXIT

    @ : create word header, dodoes , 0 , ;
    defword "create",,CREATE
    .word WORD, HEADER_COMMA            @ make the dictionary entry (the name follows VARIABLE)
    .word __DODOES, COMMA, LIT, 0, COMMA  @ ppend docol (the code field of this word)
    .word EXIT

    @ : does> r> latest @ >dfa ! ;
    defword "does>",,FROMDOES
    .word FROMR, LATEST, FETCH          @ get the latest word
    .word TDFA, STORE                   @ store the code field of the latest word
    .word EXIT

    @ : value word header, docol , ['] lit , , ['] exit , ;
    defword "value",,VALUE
    .word WORD, HEADER_COMMA            @ make the dictionary entry (the name follows value)
    .word __DOCOL, COMMA                  @ append docol
    .word BRACKET_TICK, LIT, COMMA      @ append the code field lit
    .word COMMA                         @ append the initial value
    .word BRACKET_TICK, EXIT, COMMA     @ append the code field exit
    .word EXIT

    @ : to ( n -- ) immediate word (find) >dfa 4+ state @ 0branch ( 40 ) ['] lit , , ['] ! , branch ( 8 ) ! ;
    defword "to",F_IMMED,TO
    .word WORD                          @ get the name of the value
    .word PAREN_FIND                    @ look it up in the dictionary
    .word TDFA                          @ get a pointer to the first data field (the 'lit')
    .word INCR4                         @ increment to point at the value
    .word STATE, FETCH, ZBRANCH, 40     @ compiling?
    .word BRACKET_TICK, LIT, COMMA      @ compile lit
    .word COMMA                         @ compile the address of the value
    .word BRACKET_TICK, STORE, COMMA    @ compile !
    .word BRANCH, 8                     @ immediate mode
    .word STORE                         @ update it straightaway
    .word EXIT

    @ : +to immediate word (find) >dfa 4+ state @ 0branch ( 40 ) ['] lit , , ['] +! , branch ( 8 ) +! ;
    defword "+to",F_IMMED,ADDTO
    .word WORD                          @ get the name of the value
    .word PAREN_FIND                    @ look it up in the dictionary
    .word TDFA                          @ get a pointer to the first data field (the 'lit')
    .word INCR4                         @ increment to point at the value
    .word STATE, FETCH, ZBRANCH, 40     @ compiling?
    .word BRACKET_TICK, LIT, COMMA      @ compile lit
    .word COMMA                         @ compile the address of the value
    .word BRACKET_TICK, STORE, COMMA    @ compile !
    .word BRANCH, 8                     @ immediate mode
    .word ADDSTORE                      @ update it straightaway
    .word EXIT

    @ : id. 4+ dup c@ f_lenmask and dup 0> 0branch ( 40 ) swap 1+ dup c@ emit swap 1- branch ( -48 ) 2drop ;
    defword "id.",,ID_DOT
    .word INCR4                         @ skip over the link pointer
    .word DUP, FETCHBYTE                @ get the flags/length byte
    .word __F_LENMASK, AND              @ mask out the flags - just want the length
    .word DUP, ZGT, ZBRANCH, 40         @ length > 0?
    .word SWAP, INCR                    @ addr len -- len addr+1
    .word DUP, FETCHBYTE                @ len addr -- len addr char | get the next character
    .word EMIT                          @ len addr char -- len addr | and print it
    .word SWAP, DECR                    @ len addr -- addr len-1    | subtract one from length
    .word BRANCH, -48                   @ repeat until we reach the end of the word
    .word TWODROP                       @ len addr --
    .word EXIT

    @ : ?hidden 4+ c@ f_hidden and ;
    defword "?hidden",,QHIDDEN
    .word INCR4                         @ skip over the link pointer
    .word FETCHBYTE                     @ get the flags/length byte
    .word __F_HIDDEN, AND               @ mask out the flags - just want the hidden flag
    .word EXIT

    @ : ?immediate 4+ c@ f_immed and ;
    defword "?immediate",,QIMMED
    .word INCR4                         @ skip over the link pointer
    .word FETCHBYTE                     @ get the flags/length byte
    .word __F_IMMED, AND                @ mask out the flags - just want the immediate flag
    .word EXIT

    @ : words latest @ ?dup 0branch ( 48 ) dup ?hidden not 0branch ( 16 ) dup id. space @ branch ( -52 ) cr ;
    defword "words",,WORDS
    .word LATEST, FETCH                 @ start at latest dictionary entry
    .word QDUP                          @ while link pointer is not null
    .word ZBRANCH, 48
    .word DUP, QHIDDEN, NOT, ZBRANCH, 16 @ ignore hidden words
    .word DUP, ID_DOT, SPACE            @ print the word
    .word FETCH, BRANCH, -52            @ dereference the link pointer - go to previous word
    .word CR
    .word EXIT

    @ : forget word (find) dup @ latest ! dp ! ;
    defword "forget",,FORGET
    .word WORD, PAREN_FIND              @ find the word, gets the dictionary entry address
    .word DUP, FETCH, LATEST, STORE     @ set latest to point to the previous word
    .word DP, STORE                     @ and store here with the dictionary address
    .word EXIT

    @





@   MARK: Odds and ends
@
@   CHAR puts the ASCII code of the first character of the following word on the stack.  For example
@   CHAR A puts 65 on the stack.
@
@   EXECUTE is used to run execution tokens.  See the discussion of execution tokens in the
@   FORTH code for more details.
@
@
@	In this FORTH, ECECUTE must be the last word in the built-in (assembler) dictionary because we
@   initialise the LATEST variable to point to it.  This means that if you want to extend the assembler
@   part, you must put new words before SEXECUTE, or else change how LATEST is initialised.

    defcode "char",,CHAR
    bl _word                            @ Returns r0 = length, r1 = pointer to word
    ldrb r0, [r1]                       @ Get the first character of the word
    push {r0}                           @ Push it on to the stack
    NEXT

    defcode "execute",,EXECUTE
    pop {r0}                            @ Get xt into r0
    ldr r0, [r0]                        @ Get the address of the codeword
    orr r0, #1                          @ set the thumb bit
    blx r0                              @ execute
                                        @ After xt runs its NEXT will continue executing the current word.



@
@   MARK: Data Space
@
@   Here we set up the data space, used for user definitions and variously known as just
@   the 'data space', 'user memory' or 'user definitions area'.  It is an area of memory which
@   grows upwards and stores both newly-defined FORTH words and global variables of various
@   sorts.
@
@   It is completely analogous to the C heap, except there is no generalised 'malloc' and 'free'
@   (but as with everything in FORTH, writing such functions would just be a Simple Matter
@   Of Programming).  Instead in normal use the data space just grows upwards as new FORTH
@   words are defined/appended to it.
@
@   There are various "features" of the GNU toolchain which make setting up the data space
@   more complicated than it really needs to be.  One is the GNU linker which inserts a random
@   "build ID" space.
@
@   Therefore writing this set_up_data_space assembler routine is a little more complicated
@   than it really needs to be.

    .text
set_up_data_space:
    ldr r0, =var_dp                     @ Initialise HERE to point at beginning of data space.
    ldr r1, =data_space                 @ r1 = address of data space
    str r1, [r0]                        @ store the address of the data space in HERE
    bx lr

.data
    .align 2
    .set RETURN_STACK_SIZE,8192
    .set INITIAL_DATA_SPACE_SIZE, 262144
    .bss
@ FORTH return stack
    .align 2
return_stack:
    .space RETURN_STACK_SIZE
return_stack_top:                       @ Initial top of return stack.

@ FORTH data space
    .align 2
data_space:
    .space INITIAL_DATA_SPACE_SIZE
