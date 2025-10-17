# LiteRISC
The LiteRISC processor - by Kenny Ranerup

This is a small RISC processor with a novel instruction set that has
been implemented in Verilog and ran in an FPGA. There is an assembler,
an emulator, a serial boot loader as well as an, incomplete, Lisp interpreter.

The idea was to create a compact instruction set like the early 8-bit
processor (6502, 6809) but with 32-bit operations and a large general purpose
register bank. The target systems is small embedded systems where the
memory space is constrained. For example as auxiliary processors in SoCs.

One big issue with the early 8-bit processors is that it is very hard to make
an efficient compiler. Therefore the instruction set should also be efficient
for compiling high-level languages which basically means efficient stack
handling and general purpose register.

Variable length instructions is choosen to get a memory efficient
instruction set. Basically only immediate data is variable length.
All instructions are encoded in one byte with the exception of immediate
data which is variable length and follows the first byte in the instruction.

To make instructions 8 bits and still being able to address a larger set
of registers, each instructions can only address one register. Each
instruction implicitly addresses an accumulator register so that
there can still be two operands and one result ( A = A op Rx ).

A wider instruction set would make it possible to address several registers
but this also makes the average instruction length longer. Two 8-bit instructions
can address two registers plus the accumulator compared to a single 16 bit
instruction that could address three registers.

Another inspiration from the 8-bit instruction sets is to have jump
instructions combined with condition codes. Use of condition codes
is unusual in RISC because it creates extra instruction dependencies
making pipelineing and parallelism more difficult. With this instruction
set there are no unnecessary instruction dependencies because only one
instruction changes condition code values. Also the instruction set
is not targeted for high performance making pipelining and parallel speculative
execution less interesting. Instead the instruction set can utilize the
sequential non-pipelined execution to make more efficient instructions
(for example the multi-register push/pop instructions).

The instruction set is pretty extensive so why call it RISC? Well, the
RISC concept was never really about having few instructions. It was rather
about optimizing the instruction set for compilers and only include
instructions that make the compiled programs faster or smaller. In that
sense this instruction set is RISC.

I'm not claiming this instruction set is better than others as that
would require a lot of more work to measure and compare with other instruction
sets. I view this more as an experiment that merges the 8-bit variable
length instruction sets with the RISC ideas of compiler friendly symmetric
instructions.


## LiteRISC instruction set v 0.5

### Register Set
```
The register set consist of 16 general purpose, 32 bit wide registers.
Most instructions can select any of the registers as operand but in some
instructions the R15 and R14 registers are implied operand. The R14 register
can act as a stack pointer (SP) and the R15 register can be used as a
subroutine return pointer (SRP).

There is also a condition code register (CC) that can not be accessed directly,
only a few special instructions implicitly uses this register.

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
  |    PC      |
  +------------+------------+
  | CC: n v z c8 z8 c16 z16 |
  +-------------------------+
```
### First level instructions

First instruction byte is divided into two fields, opcode and register.
```
      opcode    Rx 
     7     4  3     0
   [ o o o o  r r r r ]
```

```
   | opcode |  instruction operation
   +--------+----------------------------
   |   0    |   Rx = A
   |   1    |   A = Rx
   |   2    |   Rx = M[A+nn].l, nn is 1 to 5 following bytes
   |   3    |   Rx = M[A].l
   |   4    |   A = M[Rx].l
   |   5    |   M[A+nn].l = Rx
   |   6    |   M[A].l = Rx
   |   7    |   M[Rx].l = A
   |   8    |   Rx = sex(nn), nn is 1 to 5 following bytes
   |   9    |   A = sex(n), where n is the value of the register field
   |  10    |   jump, register field is additional jump opcode
   |  11    |   A = A + Rx
   |  12    |   A = A - Rx, sets CC = n v z c8 z8 c16 z16
   |  13    |   A = A & Rx
   |  14    |   A = A | Rx
   |  15    |   second level opcodes
   +--------+----------------------------
```
### Jump instructions

The conditional jump instructions are intended to be used after a
`A-=Rx` subtraction instruction since that instruction sets the flags.
This instruction can be regarded as a comparison.

```
A < R3 (unsigned 32-bit compare)
if true jump to xxx
```
is equivalent to
```
A -= R3
jlt xxx
```

There are signed and unsigned jumps and size of operands can be 8/16 or 32
bits.

All jumps have an offset that is relative current PC. The offset is variable size
with the same format as in immediate move instruction "Rx = nn".

The jump offset is relative the first byte after the jump instruction.
- j #0 is therefore a NOP
- j #1 skips one byte after the jump instruction
- j #-1 jumps to last byte in jump instruction
- j #-2 jumps to first byte in jump insruction, i.e. an endless loop.

The jump instruction naming is inspired by the 6809 processor instruction set.

```
               jump
              opcode
     7     4  3     0
   [ 1 0 1 0  f f f f ]
```

```
     jump                                        flag
   | opcode |  instruction operation           | condition
   +--------+----------------------------------+--------
   |   0    |   j   #nn   - jump always        |  -
   |   1    |   jlt #nn   - jump <   signed    |   n ^ v
   |   2    |   jge #nn   - jump >=  signed    | !(n ^ v)
   |   3    |   jlo #nn   - jump <   unsigned  |   c
   |   4    |   jhs #nn   - jump >=  unsigned  |  !c
   |   5    |   jz  #nn   - jump on zero       |   z
   |   6    |   jnz #nn   - jump on not zero   |  !z
   |   7    |   jlo.b #nn - jump <   unsigned  |   c8
   |   8    |   jhs.b #nn - jump >=  unsigned  |  !c8
   |   9    |   jz.b  #nn - jump on zero       |   z8
   |   10   |   jnz.b #nn - jump on not zero   |  !z8
   |   11   |   jlo.w #nn - jump <   unsigned  |   c16
   |   12   |   jhs.w #nn - jump >=  unsigned  |  !c16
   |   13   |   jz.w  #nn - jump on zero       |   z16
   |   14   |   jnz.w #nn - jump on not zero   |  !z16
   |   15   |   jsr #nn   - SRP = PC; PC = PC + sex(nn)
   +--------+----------------------------------+--------
``` 

### Second level instructions
```
              second
              opcode
     7     4  3     0
   [ 1 1 1 1  f f f f ]
```
```
     second
   | opcode |  instruction operation
   +--------+----------------------------
   |   0    | not  A,    A = ~A
   |   1    | lsl  A,    c = A, A = A << 1, A<0> = 0
   |   2    | lsr  A,    c = A, A = A >> 1, A<31> = 0
   |   3    | asr  A,    c = A, A = A >> 1, A<31> = A<30>
   |   4    | multi push, see below
   |   5    | multi pop, see below
   |   6    | push srp,  sp = sp - 4, M[sp].l = srp
   |   7    | pop-a,     a = M[sp].l, sp = sp + 4
   |   8    | third level opcodes
   |   9    | reti,      pop pc, cc, A, r14-r0 and ei
   |   10   | maskb A,   A = A & 0xff
   |   11   | maskw A,   A = A & 0xffff
   |   12   | sexb A,    A<31:8>  = A<7>
   |   13   | sexw A,    A<31:16> = A<15>
   |   14   | j-A,       PC = A
   |   15   | unused
   +--------+----------------------------
```

### Multiple register push and pop
Two byte long push instruction. The second byte holds a register
field that determines which registers to push.
```
            7     4  3     0
   byte 0 [ 1 1 1 1  0 1 0 0 ]
   byte 1 [ 0 0 0 0  r r r r ]

     second
   | opcode |  instruction operation
   +--------+----------------------------------------------------------
   |  4     | push R0..Rn,  for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
   +--------+----------------------------------------------------------
```
Two byte long pop instruction. The second byte holds a register
field that determines which registers to pop
```
            7     4  3     0
   byte 0 [ 1 1 1 1  0 1 0 1 ]
   byte 1 [ 0 0 0 0  r r r r ]

     second
   | opcode |  instruction operation
   +--------+----------------------------------------------------------
   |  5     | pop R0..Rn,  for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
   +--------+----------------------------------------------------------
```

### Third level instructions
```
            7     4  3     0
   byte 0 [ 1 1 1 1  1 0 0 0 ]
   byte 1 [ o o o o  r r r r ]  third opcode, register field
```
```
     third
   | opcode |  instruction operation
   +--------+----------------------------
   |    0   | adc,               c,A = A + Rx + c
   |    1   | unused
   |    2   | Rx = M[A+nn].b
   |    3   | Rx = M[A].b
   |    4   | A = M[Rx].b
   |    5   | M[A+nn].b = Rx
   |    6   | M[A].b = Rx
   |    7   | M[Rx].b = A
   |    8   | fourth level opcodes
   |    9   | A = A xor Rx
   |   10   | Rx = M[A+nn].w
   |   11   | Rx = M[A].w
   |   12   | A = M[Rx].w
   |   13   | M[A+nn].w = Rx
   |   14   | M[A].w = Rx
   |   15   | M[Rx].w = A
   +--------+----------------------------
```

### Fourth level instructions
```
            7     4  3     0
   byte 0 [ 1 1 1 1  1 0 0 0 ]
   byte 1 [ 1 0 0 0  o o o o ]  fourth opcode
```
```
     fourth
   | opcode |  instruction operation
   +--------+----------------------------
   |  0     | ei
   |  1     | di
   |  2-15  | unused

```
 
### Variable size immediate data

The msb bit in each byte encodes if the immediate value ends in this byte, when
0, or continues in the following byte, when 1. 

This can encode immediate values in sizes of 7, 14, 21, 28 or 32 bits.

The value is constructed by, for each byte, left shifting previous value
and inserting the 7 bits from current byte into lsb of the new value.

Note that the immediate value is sign extended to allow for jump and offsets
to be negative. This has the consequence that loading a value shorter than 32 bits
with the upper bits being 0 will might require and additional byte with all
zeros to avoid sign extension setting the upper bits to 1.
```
 7 bits
   byte 0 [    instr        ]
   byte 1 [ 0 n n n n n n n ]
 
 14-bits
   byte 0 [    instr        ]
   byte 1 [ 1 n n n n n n n ]
   byte 2 [ 0 m m m m m m m ] -> 0bnnnnnnnmmmmmmm
 
 21-bits
   byte 0 [    instr        ]
   byte 1 [ 1 n n n n n n n ]
   byte 2 [ 1 n n n n n n n ]
   byte 3 [ 0 n n n n n n n ]
 
 28-bits
   byte 0 [    instr        ]
   byte 1 [ 1 n n n n n n n ]
   byte 2 [ 1 n n n n n n n ]
   byte 3 [ 1 n n n n n n n ]
   byte 4 [ 0 n n n n n n n ]
 
 32-bits
   byte 0 [    instr        ]
   byte 1 [ 1 n n n n n n n ]
   byte 2 [ 1 n n n n n n n ]
   byte 3 [ 1 n n n n n n n ]
   byte 4 [ 1 n n n n n n n ]
   byte 5 [ 0 0 0 0 n n n n ]

```
### Load/store instructions

The addresses are always byte addresses but the the addresses must always
be aligned to the storage size of the load/store instruction. There are
byte, word and double word storage sizes (1,2 and 4 bytes wide data).
The word instructions require word aligned addresses (address bit 0 == 0)
and double word instructions require address bit 1,0 == 0.
If an unaligned address is used the lowest bits will be used as 0.

When writing integer data from a register into memory the byte order is little
endian, meaning that the least significant byte is at the lowest address.

## Interrupts

After reset interrupts are disabled. The ei/di instructions are used
to enable/disable interrupts. When an interrupt occurs and interrupts
are enabled the processor will complete current instruction and then
save the complete state onto the stack.

The registers R0-R14 are pushed followed by A, CC and last PC. Each
word pushed is 4 bytes. Interrupts are then disabled.

To return from interrupt the `reti` instruction is used. This will
restore the processor state py poping from the stack in reverser order,
i.e. PC, CC, A, R14 - R0. Finally interrupts are enabled.


## Function calling convention

Leaf functions are called with `JSR` instruction and return is then done
with the instruction sequence:
```
  SRP=A
  J-A
```

Non-leaf functions have to save the PC on the stack and then the
sequence is
```
  JSR func

  func:
   PUSH-SRP
   ...
   POP-A
   J-A
```

The `PUSH Rx` and `POP Rx` instructions are primarily for function calls
(note that they push a sequence of registers R0 to Rn).

Parameters (P0-P3) are passed in register R10-R13. Return value is in R10.
Parameters can be clobbered but registers R0-R9 are not allowed
to be clobbered.

A function should use registers R0 and up for temporaries and
but must save them on stack at start of function.

```
  func:
    PUSH-SRP
    PUSH R4
    ...use R0-4...
    POP R4
    POP-A
    J-A
```

If number of parameters are more then 4 then they need to be passed
on the stack in addition to the register R10-R13.

```
  PUSH R0 ; push parameter
  JSR func
  POP R0 ; deallocate parameter

func:
  PUSH-SRP
  PUSH R2 ; save temporaries
  A=M[SP-4] ; get the parameter
```

The choice of using R0-R9 as temporary registers and  R10-R13 for parameters
is just a convention. There is nothing in the instruction set that treats
the registers differently.

## Miscellaneous

There is no halt or wait for interrupt instruction. As halt the `jmp #-2` can
be used.

There is no instruction to push an arbitrary register to the stack. The
shortest sequence is to use the push-r instruction:
```
  A=Rx RN
  Rx=A R0
  push-r R0
```
This creates an assymetry making the register R0 special. The need for pushing
arbitrary register should be limited but it might be needed in subroutine calls
with more than four parameters.

## Instruction Set Table

Here is a complete summary of all instructions and their instruction format.
The instruction format consists of one or two bytes and each byte is subdivided
into a most significant nybble (msn) and a least significant nybble (lsn).
Instructions can have variable sized immediate data as well.

```
   [  byte 0 ]   [ byte 1  ] |
   [ msn| lsn]   [ msn| lsn] | instruction description
   ------------+-------------+--------------------------------------------
   [  0 | Rx ]   -----------  Rx = A
   [  1 | Rx ]   -----------  A = Rx
   [  2 | Rx ]   immediate    Rx = M[A+nn].l, nn is 1 to 5 following bytes
   [  3 | Rx ]   -----------  Rx = M[A].l
   [  4 | Rx ]   -----------  A = M[Rx].l
   [  5 | Rx ]   immediate    M[A+nn].l = Rx
   [  6 | Rx ]   -----------  M[A].l = Rx
   [  7 | Rx ]   -----------  M[Rx].l = A
   [  8 | Rx ]   immediate    Rx = sex(nn), nn is 1 to 5 following bytes
   [  9 | n  ]   -----------  A = sex(n), where n is the value of the register field
   [ 10 | 0  ]   immediate    j   #nn   - jump always        |  -
   [ 10 | 1  ]   immediate    jlt #nn   - jump <   signed    |   n ^ v
   [ 10 | 2  ]   immediate    jge #nn   - jump >=  signed    | !(n ^ v)
   [ 10 | 3  ]   immediate    jlo #nn   - jump <   unsigned  |   c
   [ 10 | 4  ]   immediate    jhs #nn   - jump >=  unsigned  |  !c
   [ 10 | 5  ]   immediate    jz  #nn   - jump on zero       |   z
   [ 10 | 6  ]   immediate    jnz #nn   - jump on not zero   |  !z
   [ 10 | 7  ]   immediate    jlo.b #nn - jump <   unsigned  |   c8
   [ 10 | 8  ]   immediate    jhs.b #nn - jump >=  unsigned  |  !c8
   [ 10 | 9  ]   immediate    jz.b  #nn - jump on zero       |   z8
   [ 10 | 10 ]   immediate    jnz.b #nn - jump on not zero   |  !z8
   [ 10 | 11 ]   immediate    jlo.w #nn - jump <   unsigned  |   c16
   [ 10 | 12 ]   immediate    jhs.w #nn - jump >=  unsigned  |  !c16
   [ 10 | 13 ]   immediate    jz.w  #nn - jump on zero       |   z16
   [ 10 | 14 ]   immediate    jnz.w #nn - jump on not zero   |  !z16
   [ 10 | 15 ]   immediate    jsr #nn   - SRP = PC; PC = PC + sex(nn)
   [ 11 | Rx ]   -----------  A = A + Rx
   [ 12 | Rx ]   -----------  A = A - Rx, sets CC = n v z c8 z8 c16 z16
   [ 13 | Rx ]   -----------  A = A & Rx
   [ 14 | Rx ]   -----------  A = A | Rx
   [ 15 | 0  ]   -----------  not  A,    A = ~A
   [ 15 | 1  ]   -----------  lsl  A,    c = A, A = A << 1, A<0> = 0
   [ 15 | 2  ]   -----------  lsr  A,    c = A, A = A >> 1, A<31> = 0
   [ 15 | 3  ]   -----------  asr  A,    c = A, A = A >> 1, A<31> = A<30>
   [ 15 | 4  ]   [  0 | Rx ]  push R0..Rn,  for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
   [ 15 | 5  ]   [  0 | Rx ]  pop R0..Rn,  for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
   [ 15 | 6  ]   -----------  push srp,  sp = sp - 4, M[sp].l = srp
   [ 15 | 7  ]   -----------  pop-a,     a = M[sp].l, sp = sp + 4
   [ 15 | 8  ]   [  0 | Rx ]  adc,               c,A = A + Rx + c
   [ 15 | 8  ]   [  1 | Rx ]  unused
   [ 15 | 8  ]   [  2 | Rx ]  Rx = M[A+nn].b
   [ 15 | 8  ]   [  3 | Rx ]  Rx = M[A].b
   [ 15 | 8  ]   [  4 | Rx ]  A = M[Rx].b
   [ 15 | 8  ]   [  5 | Rx ]  M[A+nn].b = Rx
   [ 15 | 8  ]   [  6 | Rx ]  M[A].b = Rx
   [ 15 | 8  ]   [  7 | Rx ]  M[Rx].b = A
   [ 15 | 8  ]   [  8 | 0  ]  ei
   [ 15 | 8  ]   [  8 | 1  ]  di
   [ 15 | 8  ]   [  8 | 2-15 ]  unused
   [ 15 | 8  ]   [  9 | Rx ]  A = A xor Rx
   [ 15 | 8  ]   [ 10 | Rx ]  Rx = M[A+nn].w
   [ 15 | 8  ]   [ 11 | Rx ]  Rx = M[A].w
   [ 15 | 8  ]   [ 12 | Rx ]  A = M[Rx].w
   [ 15 | 8  ]   [ 13 | Rx ]  M[A+nn].w = Rx
   [ 15 | 8  ]   [ 14 | Rx ]  M[A].w = Rx
   [ 15 | 8  ]   [ 15 | Rx ]  M[Rx].w = A
   [ 15 | 9  ]   -----------  reti,      pop pc, cc, A, r14-r0 and ei
   [ 15 | 10 ]   -----------  maskb A,   A = A & 0xff
   [ 15 | 11 ]   -----------  maskw A,   A = A & 0xffff
   [ 15 | 12 ]   -----------  sexb A,    A<31:8>  = A<7>
   [ 15 | 13 ]   -----------  sexw A,    A<31:16> = A<15>
   [ 15 | 14 ]   -----------  j-A,       PC = A
   [ 15 | 15 ]   -----------  unused
```

## Screenshot

Here is a screenshot of the emulator while running a lisp interpreter. The
emulator view has processor register content, breakpoints, symbolic disassember
with labels, memory viewer.

```
 ┌----------------------------┐                                                        ┌-------------------------------------------------------------------------┐
 |i: jsr #-57        175      |             1076: j #-25          160 103              |0000: 28 31 32 33 20 27 73 79 6D 30 20 27 28 73 79 6D (123 'sym0 '(sym   |
 |                            |     L-PR-LIST-LO: A = R0          16                   |0010: 32 20 73 79 6D 31 29 29 00 00 00 00 00 00 00 00 2 sym1))........   |
 |PC  00000422       1058     |             1054: R10 = A         10                   |0020: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |A   00000422       1058     |             1055: jsr #-662       175 250 106          |0030: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R0  00000023         35     |     L-CAR       : A = R10         26                   |0040: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R1  00000004          4     |              397: A = A << 1      241                  |0050: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R2  00000000          0     |              398: A = A << 1      241                  |0060: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R3  00000000          0     |              399: R10 = M[A+572].w 248 170 132         |0070: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R4  00000000          0     |              403: A = R14         30                   |0080: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R5  00000000          0     |              404: j A             254                  |0090: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R6  00000000          0     |             1058: jsr #-57        175 71               |00A0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R7  00000000          0     |     L-PRINT     : push srp        246                  |00B0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R8  00000000          0     |             1004: push R0..R1     244 1                |00C0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R9  00000000          0     |             1006: R1 = 892        129 134 124          |00D0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R10 00000003          3     |             1009: A = R10         26                   |00E0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R11 00000002          2     |             1010: R0 = A          0                    |00F0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R12 00000000          0     |             1011: A = A + R1      177                  |0100: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R13 00000000          0     |             1012: R1 = M[A].b     248 49               |0110: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |R14 00000422       1058     |             1014: A = sex(2)      146                  |0120: 0D 0A 3E 20 00 73 79 6D 31 00 00 00 00 00 00 00 ..> .sym1.......   |
 |R15 0000085C       2140     |             1015: A = A - R1      193                  |0130: 00 00 00 00 00 00 00 00 00 00 00 00 00 6E 69 6C .............nil   |
 |                            |             1016: jnz #4          166 4                |0140: 00 74 00 65 72 72 6F 72 00 71 75 6F 74 65 00 6E .t.error.quote.n   |
 |N V C Z8 C8 Z16 C16         |     L-NOT-SYM   : A = sex(4)      148                  |0150: 6F 74 00 2B 00 64 65 66 76 61 72 00 6C 61 6D 62 ot.+.defvar.lamb   |
 |0 0 0 1  0  1   0           |             1023: A = A - R1      193                  |0160: 64 61 00 64 65 66 75 6E 00 73 79 6D 30 00 73 79 da.defun.sym0.sy   |
 |                            |             1024: jnz #4          166 4                |0170: 6D 32 00 73 79 6D 31 00 00 00 00 00 00 00 00 00 m2.sym1.........   |
 |                            |             1026: jsr #15         175 15               |0180: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |                            |     L-PRINT-LIST: push srp        246                  |0190: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |                            |             1044: push R0..R1     244 1                |01A0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |                            |             1046: A = R10         26                   |01B0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 └----------------------------┘             1047: R0 = A          0                    |01C0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                            1048: R10 = 40        138 40               |01D0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                            1050: jsr #-734       175 250 34           |01E0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 (123 (quote sym0) (                L-PUTCHAR   : push R0..R0     244 0                |01F0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                             321: R0 = 127        128 127              |0200: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                             323: A = R10         26                   |0210: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                             324: M[R0].b = A     248 112              |0220: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
                                             326: pop R0..R0      245 0                |0230: 00 00 00 00 00 00 3B 00 25 00 00 00 00 00 00 00 ......;.%.......   |
                                             328: A = R14         30                   |0240: 01 00 04 00 00 00 06 00 04 00 0C 00 45 07 00 00 ............E...   |
                                             329: j A             254                  |0250: 00 00 00 00 01 00 05 00 02 00 06 00 03 00 07 00 ................   |
                                    L-PR-LIST-LO: A = R0          16                   |0260: 69 06 00 00 09 00 12 00 0A 00 08 00 7B 06 00 00 i...........{...   |
                                            1054: R10 = A         10                   |0270: 0C 00 16 00 0D 00 0B 00 C2 06 00 00 0F 00 18 00 ................   |
                                            1055: jsr #-662       175 250 106          |0280: 10 00 0E 00 EF 06 00 00 12 00 1F 00 13 00 11 00 ................   |
                                    L-CAR       : A = R10         26                   |0290: 17 07 00 00 15 00 26 00 16 00 14 00 7B 00 00 00 ......&.....{...   |
                                             397: A = A << 1      241                  |02A0: 18 00 1D 00 00 00 2C 00 1A 00 00 00 03 00 1B 00 ......,.........   |
                                             398: A = A << 1      241                  |02B0: 1C 00 24 00 00 00 31 00 1E 00 21 00 00 00 36 00 ..$...1...!...6.   |
 ┌----------------------------┐              399: R10 = M[A+572].w 248 170 132         |02C0: 20 00 00 00 1F 00 00 00 03 00 22 00 23 00 00 00  .........".#...   |
 |                            |              403: A = R14         30                   |02D0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |                            |              404: j A             254                  |02E0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 |                            |             1058: jsr #-57        175 71               |02F0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................   |
 └----------------------------┘                                                        └-------------------------------------------------------------------------┘

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
  (quicklisp-quickstart:install)
  (ql:add-to-init-file)
  (exit)
git clone git@github.com:kranerup/literisc.git liteRISC
alternatively: git clone https://github.com/kranerup/literisc.git liteRISC
cd ~/quicklisp/local-projects
ln -s ~/liteRISC # or the path where you ran git clone
sbcl
  (ql:register-local-projects)
  (ql:quickload "literisc")
  (exit)
cd ~/liteRISC # or the path where you ran git clone
sbcl --load run.lisp
```

## Running Lisp

To run the emulator with a emulated terminal you need to configure
lisp.lisp to use emulated io and setup which pty to use.

The create sockets with `socat` and connect one end to a terminal that supports 
connecting to a pty, e.g. `screen`. The other end of the sockets will be connected
to the emulator.

```
socat -d -d pty,raw,echo=0 pty,raw,echo=0
screen /dev/pts/2
```
