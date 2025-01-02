# LiteRISC
The LiteRISC processor

The idea was to create a compact instruction set like the early 8-bit
processor (6502, 6809) but with 32-bit operations and a large general purpose
register bank. It should also be efficient for compiling high-level
languages which basically means efficient stack handling.
Variable length instructions is choosen to get a memory efficient
instruction set. Basically only immediate data is variable length.
All instructions are encoded in one byte with the exception of immediate
data which is variable length and follows the first byte in the instruction.

To make instructions 8 bits and still being able to address a larger set
of registers, each instructions can only address one register. Each
instruction implicitly addresses an accumulator register so that
there can still be two operands and on result ( A = A op Rx ).

A wider instruction set would make it possible to address several registers
but this also makes the average instruction length longer. Two 8-bit instructions
can address two registers plus the accumulator compared two a single 16 bit
instruction that could address three registers.

## LiteRISC instruction set v 0.4

```
  +------------+
  |    R0      |
  +------------+
  |    R1      |
  +------------+
  |    ...     |
  +------------+
  |  R14 ( SP) |
  +------------+
  |  R15 (SRP) |
  +------------+
 
 [ o o o o  r r r r ]
 
 0   mv  A,Rx                Rx = A
 1   mv  Rx,A                A = Rx
 
 2   ld   A+#nn,Rx           Rx = M[A+nn].l
 3   ld   A,Rx               Rx = M[A].l
 4   ld   Rx,A               A = M[Rx].l
 5   st   Rx,A+#nn           M[A+nn].l = Rx
 6   st   Rx,A               M[A].l = Rx
 7   st   A,Rx               M[Rx].l = A
 
 ! One opcode for all. Size is determined by bit in each following byte. See below.
 8   mvi #nn,Rx              Rx = sex(nn)
     mvi #nnnn,Rx            Rx = sex(nnnn)
     mvi #nnnnnn,Rx          Rx = sex(nnnnnn)
 
 [ o o o o  n n n n ]
 9   mvi #n,A                A = sex(n)
 
 ! All jumps relative, offset variable size as in mvi.
 [ o o o o  f f f f ]
 10 0        j   #nn                 jump always
    1        jlt #nn                 jump <   signed      n ^ v
    2        jge #nn                 jump >=  signed    !(n ^ v)
    3        jlo #nn                 jump <   unsigned    c
    4        jhs #nn                 jump >=  unsigned   !c
    5        jz  #nn                 jump on zero         z
    6        jnz #nn                 jump on not zero    !z
 
    7        jlo.b #nn               jump <   unsigned    c8
    8        jhs.b #nn               jump >=  unsigned   !c8
    9        jz.b  #nn               jump on zero         z8
    10       jnz.b #nn               jump on not zero    !z8
 
    11       jlo.w #nn               jump <   unsigned    c16
    12       jhs.w #nn               jump >=  unsigned   !c16
    13       jz.w  #nn               jump on zero         z16
    14       jnz.w #nn               jump on not zero    !z16
 
    15       jsr #nn                 SRP = PC; PC = PC + sex(nn)
 
 11  add Rx,A                A = A + Rx
 12  sub Rx,A                A = A - Rx      sets CC = n v z c8 z8 c16 z16
 13  and Rx,A                A = A & Rx
 14  or  Rx,A                A = A | Rx
 
 [ o o o o  f f f f ]
 o  ffff
 15 0        not   A                 A = ~A
    1        lsl   A                 c = A, A = A << 1, A<0> = 0
    2        lsr   A                 c = A, A = A >> 1, A<31> = 0
    3        asr   A                 c = A, A = A >> 1, A<31> = A<30>
    4 see below
    5 see below
    6        push srp                sp = sp - 4, M[sp].l = srp
    7        popa                    a = M[sp].l, sp = sp + 4
    8 
       [ o o o o  r r r r ]
       oooo
       0-1 Unused
       2    ld   A+#nn,Rx            Rx = M[A+nn].b
       3    ld   A,Rx                Rx = M[A].b
       4    ld   Rx,A                A = M[Rx].b
       5    st   Rx,A+#nn            M[A+nn].b = Rx
       6    st   Rx,A                M[A].b = Rx
       7    st   A,Rx                M[Rx].b = A
       8-15 Unused
      
    9 Unused
    10       maskb A                 A = A & 0xff
    11       maskw A                 A = A & 0xffff
    12 sexb  A                       A<31:8>  = A<7>
    13 sexw  A                       A<31:16> = A<15>
    14       j     A                 PC = A
    15 Unused
 
 [ o o o o  f f f f ]
 [ 0 0 0 0  r r r r ]
 15 4    push  R0..Rn            for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
    5    pop   R0..Rn            for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
 
 
 Variable size immediate data:
 
 7-bits
   [    instr        ]
   [ 0 n n n n n n n ]
 
 14-bits
   [    instr        ]
   [ 1 n n n n n n n ]
   [ 0 m m m m m m m ] -> 0bnnnnnnnmmmmmmm
 
 21-bits
   [    instr        ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 0 n n n n n n n ]
 
 28-bits
   [    instr        ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 0 n n n n n n n ]
 
 32-bits
   [    instr        ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 1 n n n n n n n ]
   [ 0 0 0 0 n n n n ]
```
## Function calling convention

Leaf functions are called with "JSR func" and return is then done
with the "func: ... SRP->A; J A" sequence.

Non-leaf functions have to save the PC on the stack and then the
sequence is "JSR func; func: PUSH-SRP; ... POP A; J A"

The PUSH Rx and POP Rx instructions are primarily for function calls
(note that they push a sequence of registers R0 to Rn).

Parameters P0-P3 are passed in R10-R13. Return value in R10.
Parameters can be clobbered but registers R0-R9 are not allowed
to be clobbered.

A function should use registers R0 and up for temporaries and
save on stack at start of function.

func: PUSH-SRP; PUSH R4; ...use R0-4... POP R4; POP A; J A

Passing parameters on the stack.

  PUSH R0 ; push parameter
  JSR func
  POP R0 ; deallocate parameter

func:
  PUSH-SRP
  PUSH R2 ; save temporaries
  A=M[SP-4] ; get the parameter
...


The boundary R0-R9 / R10-R13 is not required by the instruction
and can be choosen differently.


## Issues
  - CC is not part of the register bank. Awkward to save CC on interrupt,
  - no XOR instruction
  - should mvi really sign extend? Loading constants with upper zeros but
    with highest immediate bit set is not possible. Instead must add another
    immediate byte with all zeros.
  - document this:
    But since ld/st can't do byte operations perhaps all should use word addresses.
    - all memory accesses are 32-bit long
    - memory addresses are byte addresses
    - byte order is little endian (lsb is a lowest address)
    - unaligned 32-bit access is never done. The lower 2 address bits
      are masked before memory access, resulting in aligned access.
  - jump offset is relative the first byte after the jump instruction.
    - j #0 is therefore a NOP
    - j #1 skips one byte after the jump instruction
    - j #-1 jumps to last byte in jump instruction
    - j #-2 jumps to first byte in jump insruction, i.e. an endless loop.
  - byte operations like string ops seems inefficient
    - have added byte ld/st to solve this
  - no wait for interrupt instruction
  - shouldn't STST SRP be PUSH SRP?

## Screenshot

Here is a screenshot of the emulator while running the hello world
program.

```
  ┌--------------------------------------┐
  |i: j #-2           160                |     A = M[R0].b     248
  |                                      |     maskb A         250
  |PC  00000004          4               |     R1 = A          1
  |A   00000004          4               |     R2 = 127        130 127
  |R0  0000000C         12               |     A = sex(0)      144
  |R1  00000000          0               |     A = A - R1      193
  |R2  FFFFFFFF 4294967295               |     jz #7           165 7
  |R3  00000000          0               |     A = R1          17
  |R4  00000000          0               |     M[R2].l = A     114
  |R5  00000000          0               |     A = sex(1)      145
  |R6  00000000          0               |     A = A + R0      176
  |R7  00000000          0               |     R0 = A          0
  |R8  00000000          0               |     j #-17          160 111
  |R9  00000000          0               |     A = M[R0].b     248
  |R10 00000000          0               |     maskb A         250
  |R11 00000000          0               |     R1 = A          1
  |R12 00000000          0               |     R2 = 127        130 127
  |R13 00000000          0               |     A = sex(0)      144
  |R14 00000000          0               |     A = A - R1      193
  |R15 00000004          4               |     jz #7           165 7
  |                                      |     A = R1          17
  |N V C Z8 C8 Z16 C16                   |     M[R2].l = A     114
  |0 0 0 1  0  1   0                     |     A = sex(1)      145
  |                                      |     A = A + R0      176
  |                                      |     R0 = A          0
  |                                      |     j #-17          160 111
  |                                      |     A = M[R0].b     248
  |                                      |     maskb A         250
  └--------------------------------------┘     R1 = A          1
                                               R2 = 127        130 127
                                               A = sex(0)      144
  Hello World!                                 A = A - R1      193
                                               jz #7           165 7
                                               A = R15         31
                                               j A             254
                                               j #-2           160 126
                                               j #-2           160 126
                                               j #-2           160 126
                                               j #-2           160 126
```

## Installation

On a fresh Ubuntu install you would need to do the following to install
SBCL Common Lisp and then run the LiteRISC emulator.

```
sudo apt install sbcl
sudo apt install -y git
sudo snap install curl
sudo apt install ncurses-dev
curl -O https://beta.quicklisp.org/quicklisp.lisp
sudo apt install build-essential
sbcl --load quicklisp.lisp 
git clone git@github.com:kranerup/literisc.git liteRISC
cd liteRISC
sbcl --load run.lisp
```
