from myhdl import *
from myhdl import Struct, unpack_struct

from modules.common.memory import memory
from modules.common.Common import copySignal
from cpu_common import flop
from sub_always import sub3_w_sig

# byte 1: [ o o o o  r r r r ]
# opcode field (upper 4 bits) in byte 1:
OPC_A_RX      = 0  # Rx = A
OPC_RX_A      = 1  # A = Rx
OPC_LD_A_OFFS = 2  # Rx = M[A+nn].l
OPC_LD_A      = 3  # Rx = M[A].l
OPC_LD_RX     = 4  # A = M[Rx].l
OPC_ST_A_OFFS = 5  # M[A+nn].l = Rx
OPC_ST_A      = 6  # M[A].l = Rx
OPC_ST_RX     = 7  # M[Rx].l = A
OPC_MVI       = 8  # Rx = sex(nn)
OPC_MVIA      = 9  # A = sex(n)
OPC_JMP       = 10 # j #nn, uses the r-field to encode type of jump
OPC_ADD       = 11 # A = A + Rx
OPC_SUB       = 12 # A = A - Rx
OPC_AND       = 13 # A = A & Rx
OPC_OR        = 14 # A = A | Rx
OPC_NEXT      = 15 # -> r-field is an additional opcode field

# op code jump
OPCJ_J        = 0  # jump always
OPCJ_JLT      = 1  # jump <   signed      n ^ v
OPCJ_JGE      = 2  # jump >=  signed    !(n ^ v)
OPCJ_JLO      = 3  # jump <   unsigned    c
OPCJ_JHS      = 4  # jump >=  unsigned   !c
OPCJ_JZ       = 5  # jump on zero         z
OPCJ_JNZ      = 6  # jump on not zero    !z
OPCJ_JLO8     = 7  # jump <   unsigned    c8
OPCJ_JHS8     = 8  # jump >=  unsigned   !c8
OPCJ_JZ8      = 9  # jump on zero         z8
OPCJ_JNZ8     = 10 # jump on not zero    !z8
OPCJ_JLO16    = 11 # jump <   unsigned    c16
OPCJ_JHS16    = 12 # jump >=  unsigned   !c16
OPCJ_JZ16     = 13 # jump on zero         z16
OPCJ_JNZ16    = 14 # jump on not zero    !z16
OPCJ_JSR      = 15 # SRP = PC; PC = PC + sex(nn)

# op code inner, the r-field is used as additional opcodes
# byte 1: [ o o o o  i i i i ]
# oooo = OPC_NEXT iiii = OPCI_...
OPCI_NOT     = 0  # A = ~A
OPCI_LSL     = 1  # c = A, A = A << 1, A<0> = 0
OPCI_LSR     = 2  # c = A, A = A >> 1, A<31> = 0
OPCI_ASR     = 3  # c = A, A = A >> 1, A<31> = A<30>
OPCI_PUSH_R  = 4  # for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
OPCI_POP_R   = 5  # for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
OPCI_PUSH_SRP= 6  # sp = sp -4, M[sp] = srp
OPCI_POP_A   = 7  # A = M[sp].l, sp = sp + 4
OPCI_NEXT    = 8 
OPCI_RETI    = 9 
OPCI_MASKB   = 10 # A = A & 0xff
OPCI_MASKW   = 11 # A = A & 0xffff
OPCI_SEXB    = 12 # A<31:8>  = A<7>
OPCI_SEXW    = 13 # A<31:16> = A<15>
OPCI_J_A     = 14 # PC = A
OPCI_NOP     = 15 # NOP

# op code inner 2
# byte 1: [ f f f f  i i i i ], ffff=OPC_NEXT iiii=OPCI_NEXT
# byte 2: [ o o o o  r r r r ]
# oooo = OPCI2_*
OPCI2_ADC        = 0  # c,A = A + Rx + c
OPCI2_UNUSED1    = 1
OPCI2_LDB_A_OFFS = 2  # Rx = M[A+nn].b
OPCI2_LDB_A      = 3  # Rx = M[A].b
OPCI2_LDB_RX     = 4  # A = M[Rx].b
OPCI2_STB_A_OFFS = 5  # M[A+nn].b = Rx
OPCI2_STB_A      = 6  # M[A].b = Rx
OPCI2_STB_RX     = 7  # M[Rx].b = A
OPCI2_NEXT       = 8  # -> r-field is an additional opcode field
OPCI2_XOR        = 9  # A = A xor Rx
OPCI2_LDW_A_OFFS = 10 # Rx = M[A+nn].w
OPCI2_LDW_A      = 11 # Rx = M[A].w
OPCI2_LDW_RX     = 12 # A = M[Rx].w
OPCI2_STW_A_OFFS = 13 # M[A+nn].w = Rx
OPCI2_STW_A      = 14 # M[A].w = Rx
OPCI2_STW_RX     = 15 # M[Rx].w = A

# op code inner 3
# byte 1: [ f f f f  i i i i ], ffff=OPC_NEXT iiii=OPCI_NEXT
# byte 2: [ g g g g  o o o o ], gggg=OPCI2_NEXT, oooo = OPCI3_*
OPCI3_EI         = 0
OPCI3_DI         = 1

INTR_START = 6 # reset starts at 0, allows jump past interrupt handler

# state
ST_RESET = 0
ST_NEXT_INSTR = 1
ST_DECODE_INSTR = 2
ST_READ_IMM = 3
ST_READ_PART2 = 4
ST_REG_CNT = 5
ST_INTERRUPT = 6
ST_RETI = 7

# ALU operations
ALU_PASS_X = 0
ALU_PASS_Y = 1
ALU_LSL    = 2
ALU_LSR    = 3
ALU_ASR    = 4
ALU_ADD    = 5
ALU_AND    = 6
ALU_OR     = 7
ALU_NOT    = 8
ALU_SUB    = 9
ALU_MASKB  = 10
ALU_MASKW  = 11
ALU_SEXB   = 12
ALU_SEXW   = 13
ALU_XOR    = 14
ALU_ADC    = 15
ALU_ADDC   = 16

# register numbers
SRP = 14
SP = 15

# reg cnt
A = 15
CC = 16
PCR = 17 

# create symbolic names mapping from the constants
curr_globs = dict(globals())

state_to_sym = {}
sym_to_op = {}
op_to_sym = {}
alu_to_sym = {}
inner_to_sym = {}
inner2_to_sym = {}
jmp_to_sym = {}

for s,v in curr_globs.items():
    if s.startswith("OPC_"):
        op_to_sym[v] = s
        sym_to_op[s] = v
    if s.startswith("OPCJ_"):
        jmp_to_sym[v] = s
        sym_to_op[s] = v
    if s.startswith("OPCI_"):
        inner_to_sym[v] = s
        sym_to_op[s] = v
    if s.startswith("OPCI2_"):
        inner2_to_sym[v] = s
        sym_to_op[s] = v
    if s.startswith("ST_"):
        state_to_sym[v] = s
    if s.startswith("ALU_"):
        alu_to_sym[v] = s

#----------------------
# dmem interface:
# dmem_adr: byte addresses but:
#  - dword accesses are always word aligned [1:0]==0
#  - byte accesses can be any alignment
# dmem_wr_sz:  0 - byte, 1 - 16-bit word, 2 - 32-bit word
# dmem_dout: read data from memory
# dmem_din:  write data to memory
#            when byte write the lowest bits [7:0] are valid
#            when word write the lowest bits [15:0] are valid
#----------------------
# About dmem. There are several possibilities. The physical memory
# is 8-bits, 16-bits or 32-bits wide.
# The physical memory has write mask or not.
# Write mask allows 8/16-bit writes to 16/32-bit memory in one cycle.
# Without write mask a 16/32-bit memory requires a read/modify/write
# sequence for byte/word write.
#----------------------

@module
def cpu( clk, clk_en, sync_rstn,
         imem_dout,
         imem_adr,
         imem_rd,
         dmem_din,
         dmem_dout,
         dmem_adr,
         dmem_rd,
         dmem_wr,
         dmem_wr_sz, # 0 - byte, 1 - 16-bit word, 2 - 32-bit word
         halt,
         intr,
         enable_obs,
         obs_regs,
         obs_acc,
         obs_cc,
         obs_op,
         sim_print):


    pc = Signal(modbv(0)[16:])
    pc_next = Signal(modbv(0)[16:])
    pc_inc = Signal(modbv(0)[16:])
    reg_bank = [ Signal(modbv(0)[32:]) for _ in range(16) ]
    acc = Signal(modbv(0)[32:])
    acc_ff = Signal(modbv(0)[32:])
    acc_next = Signal(modbv(0)[32:])

    n_n = Signal(modbv(0)[1:]) 
    n_c = Signal(modbv(0)[1:]) 
    n_z = Signal(modbv(0)[1:]) 
    n_v = Signal(modbv(0)[1:]) 
    n_c8 = Signal(modbv(0)[1:]) 
    n_z8 = Signal(modbv(0)[1:]) 
    n_c16 = Signal(modbv(0)[1:]) 
    n_z16 = Signal(modbv(0)[1:]) 

    n_cc = Signal(modbv(0)[8:])

    cc_n = Signal(modbv(0)[1:]) 
    cc_c = Signal(modbv(0)[1:]) 
    cc_z = Signal(modbv(0)[1:]) 
    cc_v = Signal(modbv(0)[1:]) 
    cc_c8 = Signal(modbv(0)[1:]) 
    cc_z8 = Signal(modbv(0)[1:]) 
    cc_c16 = Signal(modbv(0)[1:]) 
    cc_z16 = Signal(modbv(0)[1:]) 

    cc = Signal(modbv(0)[8:])

    intr_enabled = Signal(modbv(1)[1:])
    n_intr_enabled = Signal(modbv(0)[1:])

    # IR - first instruction word, held till loading next instruction
    #   the ir fields are extracted into (available during the whole
    #   instruction):
    #   - op
    #   - r_field
    #   if op == OPC_NEXT then r_field is secondary op code
    # IR2 - some OPC_NEXT instructions has a second instruction word loaded here
    #   the ir2 fields are extracted into (available when ir2 loaded until end
    #   of instruction):
    #   - op2
    #   - r_field2

    n_ir = Signal(modbv(0)[8:])
    curr_ir = Signal(modbv(0)[8:])
    ir = Signal(modbv(0)[8:])
    n_ir2 = Signal(modbv(0)[8:])
    ir2 = Signal(modbv(0)[8:])
    imm = Signal(modbv(0)[32:])
    imm_next = Signal(modbv(0)[32:])
    op = Signal(modbv(0)[4:])
    op2 = Signal(modbv(0)[4:])
    rx = Signal(modbv(0)[32:])
    r_field  = Signal(modbv(0)[4:])
    r_field2  = Signal(modbv(0)[4:])
    rx_idx  = Signal(modbv(0)[4:])
    state = Signal(modbv(0)[4:])
    next_state = Signal(modbv(0)[4:])
    alu_oper = Signal(modbv(0)[5:])

    sel_imem = Signal(modbv(0)[4:])
    n_load_ir = Signal(modbv(0)[1:])
    load_ir = Signal(modbv(0)[1:])
    n_load_ir2 = Signal(modbv(0)[1:])
    load_ir2 = Signal(modbv(0)[1:])
    n_op2_valid = Signal(modbv(0)[1:])
    op2_valid = Signal(modbv(0)[1:])
    inc_pc = Signal(modbv(0)[1:])
    inc_sp = Signal(modbv(0)[1:])
    dec_sp = Signal(modbv(0)[1:])
    load_pc = Signal(modbv(0)[1:])
    load_pc_intr = Signal(modbv(0)[1:])
    deferred_load_pc = Signal(modbv(0)[1:])
    n_deferred_load_pc = Signal(modbv(0)[1:])
    direct_load_imm = Signal(modbv(0)[1:])
    load_imm = Signal(modbv(0)[1:])
    n_load_imm = Signal(modbv(0)[1:])
    imm_more = Signal(modbv(0)[1:])
    alu_x_imm = Signal(modbv(0)[1:])
    alu_imm_width = Signal(modbv(0)[2:])
    alu_y_pc = Signal(modbv(0)[1:])
    alu_op_x = Signal(modbv(0)[32:])
    alu_op_y = Signal(modbv(0)[32:])
    alu_out = Signal(modbv(0)[32:])
    reg_wr_op = Signal(modbv(0)[32:])
    reg_wr_alu = Signal(modbv(0)[1:])
    reg_wr_pc = Signal(modbv(0)[1:])
    reg_wr_dmem = Signal(modbv(0)[1:])
    n_reg_wr_deferred = Signal(modbv(0)[1:])
    reg_wr_deferred = Signal(modbv(0)[1:])
    n_acc_wr_deferred = Signal(modbv(0)[1:])
    acc_wr_deferred = Signal(modbv(0)[1:])
    n_reg_ld_rx = Signal(modbv(0)[4:])
    n_reg_ld_maskb = Signal(modbv(0)[4:])
    n_reg_ld_maskw = Signal(modbv(0)[4:])
    n_acc_ld_maskb = Signal(modbv(0)[4:])
    n_acc_ld_maskw = Signal(modbv(0)[4:])
    reg_ld_rx = Signal(modbv(0)[4:])
    reg_ld_maskb = Signal(modbv(0)[4:])
    reg_ld_maskw = Signal(modbv(0)[4:])
    acc_ld_maskb = Signal(modbv(0)[4:])
    acc_ld_maskw = Signal(modbv(0)[4:])
    wr_acc = Signal(modbv(0)[1:])
    wr_reg = Signal(modbv(0)[1:])
    reg_dest  = Signal(modbv(0)[4:])
    reg_dest_srp  = Signal(modbv(0)[1:])
    dmem_adr_sel = Signal(modbv(0)[2:])
    dmem_wr_acc = Signal(modbv(0)[1:])
    sel_reg_cnt = Signal(modbv(0)[1:])
    load_reg_cnt = Signal(modbv(0)[1:])
    load_reti_reg_cnt = Signal(modbv(0)[1:])
    clear_reg_cnt = Signal(modbv(0)[1:])
    inc_reg_cnt = Signal(modbv(0)[1:])
    n_reg_cnt  = Signal(modbv(0)[5:])
    reg_cnt  = Signal(modbv(0)[5:])
    n_sp = Signal(modbv(0)[32:])
    sel_srp = Signal(modbv(0)[1:])
    rx_field2 = Signal(modbv(0)[1:])
    intr_push = Signal(modbv(0)[1:])
    n_load_cc = Signal(modbv(0)[1:])
    load_cc = Signal(modbv(0)[1:])
    set_i = Signal(modbv(0)[1:])
    clr_i = Signal(modbv(0)[1:])
    sub_res = Signal(modbv(0)[32:])
    add_sub = Signal(modbv(0)[1:])
    borrow_in = Signal(modbv(0)[1:])

    ln   = Signal(modbv(0)[1:])
    lz   = Signal(modbv(0)[1:])
    lv   = Signal(modbv(0)[1:])
    lc   = Signal(modbv(0)[1:])
    lc8  = Signal(modbv(0)[1:])
    lz8  = Signal(modbv(0)[1:])
    lc16 = Signal(modbv(0)[1:])
    lz16 = Signal(modbv(0)[1:])

    v_mid = Signal(modbv(0)[1:]) # unused
    n_mid = Signal(modbv(0)[1:]) # unused
    v_lo  = Signal(modbv(0)[1:]) # unused
    n_lo  = Signal(modbv(0)[1:])  # unused


    # sel_imem
    PC = 0

    # op code outer

    ist   = flop( next_state,         state,            clk_en, clk, sync_rstn, reset_value=ST_RESET )
    iir   = flop( n_ir,               ir,               clk_en, clk, sync_rstn  )
    iir2  = flop( n_ir2,              ir2,              clk_en, clk, sync_rstn  )
    irc   = flop( n_load_ir,          load_ir,          clk_en, clk, sync_rstn  )
    irc2  = flop( n_load_ir2,         load_ir2,         clk_en, clk, sync_rstn  )
    iop2  = flop( n_op2_valid,        op2_valid,        clk_en, clk, sync_rstn  )
    idf   = flop( n_reg_wr_deferred,  reg_wr_deferred,  clk_en, clk, sync_rstn  )
    ida   = flop( n_acc_wr_deferred,  acc_wr_deferred,  clk_en, clk, sync_rstn  )
    idpc  = flop( n_deferred_load_pc, deferred_load_pc, clk_en, clk, sync_rstn  )
    idr   = flop( n_reg_ld_rx,        reg_ld_rx,        clk_en, clk, sync_rstn  )
    idrm  = flop( n_reg_ld_maskb,     reg_ld_maskb,     clk_en, clk, sync_rstn  )
    idrmw = flop( n_reg_ld_maskw,     reg_ld_maskw,     clk_en, clk, sync_rstn  )
    idam  = flop( n_acc_ld_maskb,     acc_ld_maskb,     clk_en, clk, sync_rstn  )
    idamw = flop( n_acc_ld_maskw,     acc_ld_maskw,     clk_en, clk, sync_rstn  )
    ili   = flop( n_load_imm,         load_imm,         clk_en, clk, sync_rstn  )
    ii    = flop( n_intr_enabled,     intr_enabled,     clk_en, clk, sync_rstn, reset_value=1        )
    icc   = flop( n_load_cc,          load_cc,          clk_en, clk, sync_rstn, reset_value=1        )

    
    @always_comb
    def sub_ctrl():
        add = modbv(0)[1:]
        b_in = modbv(0)[1:]
        if alu_oper != ALU_SUB:
            add[:] = 1
            if alu_oper == ALU_ADC:
                b_in[:] = cc_c
        else:
            b_in[:] = 1
        add_sub.next = add
        borrow_in.next = b_in

    isub = sub3_w_sig(
        alu_op_y,  #i
        alu_op_x, #i
        sub_res, #o
        add_sub, #i
        borrow_in, #i
        lc,   lv,    ln,    lz, #o
        lc16, v_mid, n_mid, lz16, #o
        lc8,  v_lo,  n_lo,  lz8, #o
        32, 16, 8 )

    @always_comb
    def ctrl():
        load_pc_intr.next = 0
        n_load_ir.next = 0
        n_load_imm.next = 0
        n_load_ir2.next = 0
        n_op2_valid.next = 0
        next_state.next = ST_NEXT_INSTR
        sel_imem.next = 0
        load_reg_cnt.next = 0
        clear_reg_cnt.next = 0
        n_intr_enabled.next = 0
        intr_push.next = 0
        inc_pc.next = 0
    
        goto_next_instr = modbv(0)[1:]
        intr_active = modbv(0)[1:]

        if clr_i == 1:
            n_intr_enabled.next = 0
        elif set_i == 1:
            n_intr_enabled.next = 1
        else:
            n_intr_enabled.next = intr_enabled


        if halt == 1:
            goto_next_instr[:] = 1
            n_load_ir.next = 1
            n_load_imm.next = 1
            inc_pc.next = 1
        else:
            if state == ST_RESET:
                n_load_ir.next = 1
                n_load_imm.next = 0
                inc_pc.next = 0
                sel_imem.next = PC
                goto_next_instr[:] = 1
            elif state == ST_NEXT_INSTR:
                if ( op == OPC_MVI or
                     op == OPC_JMP or
                     op == OPC_LD_A_OFFS or
                     op == OPC_ST_A_OFFS ):
                    n_load_ir.next = 0
                    n_load_imm.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    next_state.next = ST_READ_IMM
                elif op == OPC_NEXT and (
                        r_field == OPCI_POP_R or
                        r_field == OPCI_PUSH_R or
                        r_field == OPCI_NEXT):
                    n_load_ir.next = 0
                    n_load_ir2.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    n_op2_valid.next = r_field == OPCI_NEXT
                    next_state.next = ST_READ_PART2
                elif op == OPC_NEXT and r_field == OPCI_RETI:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    next_state.next = ST_RETI
                else:
                    n_load_ir.next = 1
                    n_load_imm.next = 0
                    inc_pc.next = 1
                    sel_imem.next = PC
                    goto_next_instr[:] = 1
            elif state == ST_READ_IMM:
                if imm_more:
                    n_load_ir.next = 0
                    n_load_imm.next = 0
                    inc_pc.next = 1
                    sel_imem.next = PC
                    n_op2_valid.next = op2_valid
                    next_state.next = ST_READ_IMM
                else:
                    n_load_ir.next = 1
                    n_load_imm.next = 0
                    inc_pc.next = 1
                    sel_imem.next = PC
                    goto_next_instr[:] = 1
            elif state == ST_READ_PART2: # TODO: remove duplicate code below
                if op == OPC_NEXT and r_field == OPCI_POP_R:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    load_reg_cnt.next = 1
                    next_state.next = ST_REG_CNT 
                elif op == OPC_NEXT and r_field == OPCI_PUSH_R:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    clear_reg_cnt.next = 1
                    next_state.next = ST_REG_CNT 
                elif op == OPC_NEXT and r_field == OPCI_NEXT:
                    if op2 == OPCI2_LDB_A_OFFS or op2 == OPCI2_LDW_A_OFFS :
                        n_load_ir.next = 0
                        n_load_imm.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        n_op2_valid.next = 1
                        next_state.next = ST_READ_IMM
                    elif op2 == OPCI2_STB_A_OFFS or op2 == OPCI2_STW_A_OFFS :
                        n_load_ir.next = 0
                        n_load_imm.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        n_op2_valid.next = 1
                        next_state.next = ST_READ_IMM
                    else:
                        n_load_ir.next = 1
                        n_load_imm.next = 0
                        inc_pc.next = 1
                        sel_imem.next = PC
                        goto_next_instr[:] = 1
            elif state == ST_READ_PART2:
                if op == OPC_NEXT and r_field == OPCI_POP_R:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    load_reg_cnt.next = 1
                    next_state.next = ST_REG_CNT 
                elif op == OPC_NEXT and r_field == OPCI_PUSH_R:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    clear_reg_cnt.next = 1
                    next_state.next = ST_REG_CNT 
                elif op == OPC_NEXT and r_field == OPCI_NEXT:
                    if op2 == OPCI2_LDB_A_OFFS or op2 == OPCI2_LDW_A_OFFS :
                        n_load_ir.next = 0
                        n_load_imm.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        n_op2_valid.next = 1
                        next_state.next = ST_READ_IMM
                    elif op2 == OPCI2_STB_A_OFFS or op2 == OPCI2_STW_A_OFFS :
                        n_load_ir.next = 0
                        n_load_imm.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        n_op2_valid.next = 1
                        next_state.next = ST_READ_IMM
                    else:
                        n_load_ir.next = 1
                        n_load_imm.next = 0
                        inc_pc.next = 1
                        sel_imem.next = PC
                        goto_next_instr[:] = 1
            elif state == ST_REG_CNT:
                if r_field == OPCI_POP_R:
                    if n_reg_cnt == 0:
                        n_load_ir.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        goto_next_instr[:] = 1
                    else:
                        n_load_ir.next = 0
                        inc_pc.next = 0
                        next_state.next = ST_REG_CNT
                elif r_field == OPCI_PUSH_R:
                    if n_reg_cnt == ir2[4:]:
                        n_load_ir.next = 1
                        inc_pc.next = 1
                        sel_imem.next = PC
                        goto_next_instr[:] = 1
                    else:
                        n_load_ir.next = 0
                        inc_pc.next = 0
                        next_state.next = ST_REG_CNT
            elif state == ST_INTERRUPT:
                n_load_ir.next = 0
                inc_pc.next = 0
                if n_reg_cnt == PCR:
                    goto_next_instr[:] = 1
                    intr_push.next = 1
                    load_pc_intr.next = 1
                else:
                    next_state.next = ST_INTERRUPT
                    intr_push.next = 1
            elif state == ST_RETI:
                n_load_ir.next = 0
                inc_pc.next = 0
                if n_reg_cnt == 0:
                    n_load_ir.next = 1
                    sel_imem.next = PC
                    goto_next_instr[:] = 1
                else:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    next_state.next = ST_RETI

            intr_active[:] = intr==1 and intr_enabled==1
            if goto_next_instr == 1:
                if intr_active == 1:
                    n_load_ir.next = 0
                    inc_pc.next = 0
                    clear_reg_cnt.next = 1
                    n_intr_enabled.next = 0
                    intr_push.next = 1
                    next_state.next = ST_INTERRUPT
                else:
                    next_state.next = ST_NEXT_INSTR

    if sim_print:
        @always(clk.negedge)
        def prt_state():
            print("state:",state)
            if state == ST_NEXT_INSTR:
                print("ST_NEXT_INSTR")
                if op == OPC_MVI or op == OPC_JMP:
                    print("- OPC_MVI/JMP")
            elif state == ST_READ_IMM:
                print("ST_READ_IMM")
                if imm_more:
                    print("- imm_more")

    @always_comb
    def decoder():
        alu_imm_width.next = 0
        alu_oper.next = 0
        alu_x_imm.next = 0
        alu_y_pc.next = 0
        dec_sp.next = 0
        dmem_rd.next = 0
        dmem_wr.next = 0
        dmem_wr_sz.next = 2
        inc_sp.next = 0
        load_pc.next = 0
        n_deferred_load_pc.next = 0
        n_reg_ld_rx.next = 0
        n_reg_ld_maskb.next = 0
        n_reg_ld_maskw.next = 0
        n_reg_wr_deferred.next = 0
        reg_dest_srp.next = 0
        reg_wr_alu.next = 0
        reg_wr_pc.next = 0
        wr_acc.next = 0
        wr_reg.next = 0
        dmem_wr_acc.next = 0
        dmem_adr_sel.next = 0
        direct_load_imm.next = 0
        n_acc_wr_deferred.next = 0
        sel_srp.next = 0
        inc_reg_cnt.next = 0
        sel_reg_cnt.next = 0
        rx_field2.next = 0
        n_load_cc.next = 0
        load_reti_reg_cnt.next = 0
        n_acc_ld_maskb.next = 0
        n_acc_ld_maskw.next = 0
        set_i.next = 0
        clr_i.next = 0

        take_jump = modbv(0)[1:]
        take_jump[:] = 0

        if state == ST_INTERRUPT or intr_push == 1:
            inc_reg_cnt.next = 1
            dec_sp.next = 1
            dmem_wr.next = 1
            dmem_adr_sel.next = 2 # SP
            sel_reg_cnt.next = 1
        elif op == OPC_A_RX:
            alu_oper.next = ALU_PASS_Y
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
            wr_reg.next = 1
        elif op == OPC_RX_A:
            alu_oper.next = ALU_PASS_X
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_LD_A: # Rx = M[A].l
            alu_oper.next = ALU_PASS_Y
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            n_reg_wr_deferred.next = 1
            n_reg_ld_rx.next = r_field
            dmem_rd.next = 1
            dmem_adr_sel.next = 0 # ALU
        elif op == OPC_LD_RX: # A = M[Rx].l
            alu_oper.next = ALU_PASS_X
            alu_x_imm.next = 0
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            n_acc_wr_deferred.next = 1
            dmem_rd.next = 1
            dmem_adr_sel.next = 0 # ALU
        elif op == OPC_ST_A:
            alu_oper.next = ALU_PASS_Y
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            dmem_wr.next = 1
            dmem_adr_sel.next = 0 # ALU
            dmem_wr_acc.next = 0 # Rx
        elif op == OPC_ST_RX: # M[Rx].l = A
            alu_oper.next = ALU_PASS_X # Rx
            alu_x_imm.next = 0 # D.C.
            dmem_wr.next = 1
            dmem_adr_sel.next = 0 # ALU
            dmem_wr_acc.next = 1 # Acc
        elif op == OPC_MVIA:
            alu_oper.next = ALU_PASS_X
            alu_x_imm.next = 1
            direct_load_imm.next = 1
            alu_imm_width.next = 0 # 4 bits from first instr byte
            wr_acc.next = 1
            alu_y_pc.next = 0 # D.C.
            reg_wr_alu.next = 1
        elif op == OPC_JMP or op == OPCJ_JNZ:
            if state == ST_NEXT_INSTR:
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0
            elif state == ST_READ_IMM:
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0

                take_jump[:] = 1
                if imm_more == 0:
                    if r_field == OPCJ_JNZ:
                        take_jump[:] = cc_z == 0
                    elif r_field == OPCJ_JNZ8:
                        take_jump[:] = cc_z8 == 0
                    elif r_field == OPCJ_JNZ16:
                        take_jump[:] = cc_z16 == 0
                    elif r_field == OPCJ_JZ:
                        take_jump[:] = cc_z == 1
                    elif r_field == OPCJ_JZ8:
                        take_jump[:] = cc_z8 == 1
                    elif r_field == OPCJ_JZ16:
                        take_jump[:] = cc_z16 == 1
                    elif r_field == OPCJ_JHS:
                        take_jump[:] = cc_c == 0
                    elif r_field == OPCJ_JHS8:
                        take_jump[:] = cc_c8 == 0
                    elif r_field == OPCJ_JHS16:
                        take_jump[:] = cc_c16 == 0
                    elif r_field == OPCJ_JLO:
                        take_jump[:] = cc_c == 1
                    elif r_field == OPCJ_JLO8:
                        take_jump[:] = cc_c8 == 1
                    elif r_field == OPCJ_JLO16:
                        take_jump[:] = cc_c16 == 1
                    elif r_field == OPCJ_JLT:
                        take_jump[:] = cc_n ^ cc_v
                    elif r_field == OPCJ_JGE:
                        take_jump[:] = (cc_n ^ cc_v) == 0
                    elif r_field == OPCJ_J:
                        take_jump[:] = 1

                if imm_more:
                    alu_oper.next = ALU_PASS_X
                    load_pc.next = 0
                else:
                    alu_oper.next = ALU_ADD
                    if take_jump == 1:
                        load_pc.next = 1

                if r_field == OPCJ_JSR:
                    reg_dest_srp.next = 1
                    reg_wr_pc.next = 1
                    wr_reg.next = 1

        elif op == OPC_MVI: # Rx = sex(nn)
            if state == ST_NEXT_INSTR:
                alu_oper.next = ALU_PASS_X
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
            elif state == ST_READ_IMM:
                alu_oper.next = ALU_PASS_X
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
                wr_reg.next = 1

        elif op == OPC_LD_A_OFFS: # Rx = M[A+nn].l
            if state == ST_NEXT_INSTR:
                alu_oper.next = ALU_PASS_X
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
            elif state == ST_READ_IMM:
                if imm_more == 0:
                    alu_oper.next = ALU_ADD
                    alu_x_imm.next = 1
                    alu_imm_width.next = 1 # 7 bits from instr byte
                    n_reg_wr_deferred.next = 1
                    n_reg_ld_rx.next = r_field
                    dmem_rd.next = 1
                    dmem_adr_sel.next = 0 # ALU
                else:
                    alu_oper.next = ALU_PASS_X
                    alu_x_imm.next = 1
                    alu_imm_width.next = 1 # 7 bits from instr byte

        elif op == OPC_ST_A_OFFS: # M[A+nn].l = Rx
            if state == ST_NEXT_INSTR:
                alu_oper.next = ALU_PASS_X
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
            elif state == ST_READ_IMM:
                if imm_more == 0:
                    alu_oper.next = ALU_ADD
                    alu_x_imm.next = 1
                    alu_imm_width.next = 1 # 7 bits from instr byte
                    alu_y_pc.next = 0 # acc
                    dmem_wr.next = 1
                    dmem_adr_sel.next = 0 # ALU
                    dmem_wr_acc.next = 0 # Rx
                else:
                    alu_oper.next = ALU_PASS_X
                    alu_x_imm.next = 1
                    alu_imm_width.next = 1 # 7 bits from instr byte
        elif op == OPC_RX_A:
            alu_oper.next = ALU_PASS_X
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_ADD:
            alu_oper.next = ALU_ADDC
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_AND:
            alu_oper.next = ALU_AND
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_OR:
            alu_oper.next = ALU_OR
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_SUB:
            alu_oper.next = ALU_SUB
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_NEXT:
            # the reg field is secondary opcode
            if r_field == OPCI_NOP:
                alu_oper.next = ALU_PASS_Y
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_NOT:
                alu_oper.next = ALU_NOT
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_LSL:
                alu_oper.next = ALU_LSL
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_LSR:
                alu_oper.next = ALU_LSR
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_ASR:
                alu_oper.next = ALU_ASR
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_RETI:
                dmem_adr_sel.next = 1 # SP
                inc_sp.next = 1
                dmem_rd.next = 1
                if state != ST_RETI:
                    load_reti_reg_cnt.next = 1
                    # trick to make selrx take dmem_dout
                    # this will write PC to R9 as a side effect
                    n_reg_ld_rx.next = OPCI_RETI
                    n_deferred_load_pc.next = 1
                    n_reg_wr_deferred.next = 1
                else:
                    alu_oper.next = ALU_PASS_X # pass dmem_dout through alu to pc and cc
                    if reg_cnt == PCR: # next dout will be CC
                        # trick to make selrx take dmem_dout
                        # this will write CC to R9 as a side effect
                        n_reg_ld_rx.next = OPCI_RETI
                        n_reg_wr_deferred.next = 1
                        n_load_cc.next = 1
                    elif reg_cnt == CC: # next dout will be A
                        n_acc_wr_deferred.next = 1
                    else:
                        n_reg_wr_deferred.next = 1
                        n_reg_ld_rx.next = n_reg_cnt

            elif r_field == OPCI_MASKB:
                alu_oper.next = ALU_MASKB
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_MASKW:
                alu_oper.next = ALU_MASKW
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_SEXB:
                alu_oper.next = ALU_SEXB
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_SEXW:
                alu_oper.next = ALU_SEXW
                wr_acc.next = 1
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_J_A:
                load_pc.next = 1
                alu_oper.next = ALU_PASS_Y
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 0
            elif r_field == OPCI_PUSH_SRP: # sp = sp -4, M[sp] = srp
                dmem_wr.next = 1
                dmem_adr_sel.next = 2 # next SP
                dmem_wr_acc.next = 0 # Rx
                dec_sp.next = 1
                sel_srp.next = 1
            elif r_field == OPCI_POP_A: # A = M[sp].l, sp = sp + 4
                alu_oper.next = ALU_PASS_X
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                n_acc_wr_deferred.next = 1
                dmem_rd.next = 1
                dmem_adr_sel.next = 1 # SP
                inc_sp.next = 1
            elif r_field == OPCI_POP_R: # for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
                alu_oper.next = ALU_PASS_X
                dmem_adr_sel.next = 1 # SP
                if state == ST_REG_CNT or state == ST_READ_PART2:
                    inc_sp.next = 1
                    dmem_rd.next = 1
                    n_reg_wr_deferred.next = 1
                    n_reg_ld_rx.next = n_reg_cnt
            elif r_field == OPCI_PUSH_R:  # for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
                dmem_adr_sel.next = 2 # next SP
                inc_reg_cnt.next = 1
                if state == ST_REG_CNT or state == ST_READ_PART2:
                    dec_sp.next = 1
                    dmem_wr.next = 1
                    sel_reg_cnt.next = 1
            elif r_field == OPCI_NEXT and op2_valid == 1:
                rx_field2.next = 1
                if op2 == OPCI2_LDB_A or op2 == OPCI2_LDW_A: # Rx = M[A].b/w
                    alu_oper.next = ALU_PASS_Y
                    alu_x_imm.next = 0 # D.C.
                    wr_acc.next = 0
                    alu_y_pc.next = 0 # acc
                    n_reg_wr_deferred.next = 1
                    n_reg_ld_rx.next = r_field2
                    n_reg_ld_maskb.next = op2 == OPCI2_LDB_A
                    n_reg_ld_maskw.next = op2 == OPCI2_LDW_A
                    dmem_rd.next = 1
                    dmem_adr_sel.next = 0 # ALU
                elif op2 == OPCI2_LDB_A_OFFS or op2 == OPCI2_LDW_A_OFFS: # Rx = M[A+imm].b
                    if state == ST_NEXT_INSTR:
                        alu_oper.next = ALU_PASS_X
                        alu_x_imm.next = 1
                        alu_imm_width.next = 1 # 7 bits from instr byte
                    elif state == ST_READ_IMM:
                        if imm_more == 0:
                            alu_oper.next = ALU_ADD
                            alu_x_imm.next = 1
                            alu_imm_width.next = 1 # 7 bits from instr byte
                            n_reg_wr_deferred.next = 1
                            n_reg_ld_rx.next = r_field2
                            n_reg_ld_maskb.next = op2 == OPCI2_LDB_A_OFFS
                            n_reg_ld_maskw.next = op2 == OPCI2_LDW_A_OFFS
                            dmem_rd.next = 1
                            dmem_adr_sel.next = 0 # ALU
                        else:
                            alu_oper.next = ALU_PASS_X
                            alu_x_imm.next = 1
                            alu_imm_width.next = 1 # 7 bits from instr byte
                elif op2 == OPCI2_STB_A_OFFS or op2 == OPCI2_STW_A_OFFS : # M[A+nn].b/w = Rx
                    if state == ST_NEXT_INSTR:
                        alu_oper.next = ALU_PASS_X
                        alu_x_imm.next = 1
                        alu_imm_width.next = 1 # 7 bits from instr byte
                    elif state == ST_READ_IMM:
                        if imm_more == 0:
                            alu_oper.next = ALU_ADD
                            alu_x_imm.next = 1
                            alu_imm_width.next = 1 # 7 bits from instr byte
                            alu_y_pc.next = 0 # acc
                            dmem_wr.next = 1
                            if op2 == OPCI2_STB_A_OFFS:
                                dmem_wr_sz.next = 0
                            elif op2 == OPCI2_STW_A_OFFS:
                                dmem_wr_sz.next = 1
                            dmem_adr_sel.next = 0 # ALU
                            dmem_wr_acc.next = 0 # Rx
                        else:
                            alu_oper.next = ALU_PASS_X
                            alu_x_imm.next = 1
                            alu_imm_width.next = 1 # 7 bits from instr byte
                elif op2 == OPCI2_LDB_RX or op2 == OPCI2_LDW_RX: # A = M[Rx].b/w
                    alu_oper.next = ALU_PASS_X
                    alu_x_imm.next = 0
                    wr_acc.next = 0
                    alu_y_pc.next = 0 # acc
                    n_acc_wr_deferred.next = 1
                    n_acc_ld_maskb.next = op2 == OPCI2_LDB_RX
                    n_acc_ld_maskw.next = op2 == OPCI2_LDW_RX
                    dmem_rd.next = 1
                    dmem_adr_sel.next = 0 # ALU
                elif op2 == OPCI2_STB_A:
                    alu_oper.next = ALU_PASS_Y
                    alu_x_imm.next = 0 # D.C.
                    wr_acc.next = 0
                    alu_y_pc.next = 0 # acc
                    dmem_wr.next = 1
                    dmem_adr_sel.next = 0 # ALU
                    dmem_wr_acc.next = 0 # Rx
                    dmem_wr_sz.next = 0 # byte
                elif op2 == OPCI2_STB_RX: # M[Rx].b = A
                    alu_oper.next = ALU_PASS_X # Rx
                    alu_x_imm.next = 0 # D.C.
                    dmem_wr.next = 1
                    dmem_adr_sel.next = 0 # ALU
                    dmem_wr_acc.next = 1 # Acc
                    dmem_wr_sz.next = 0 # byte
                elif op2 == OPCI2_STW_RX: # M[Rx].w = A
                    alu_oper.next = ALU_PASS_X # Rx
                    alu_x_imm.next = 0 # D.C.
                    dmem_wr.next = 1
                    dmem_adr_sel.next = 0 # ALU
                    dmem_wr_acc.next = 1 # Acc
                    dmem_wr_sz.next = 1 # word
                elif op2 == OPCI2_XOR:
                    alu_oper.next = ALU_XOR
                    alu_x_imm.next = 0
                    wr_acc.next = 1
                    alu_y_pc.next = 0 # acc
                    reg_wr_alu.next = 1
                elif op2 == OPCI2_ADC:
                    alu_oper.next = ALU_ADC
                    alu_x_imm.next = 0
                    wr_acc.next = 1
                    alu_y_pc.next = 0 # acc
                    reg_wr_alu.next = 1
                elif op2 == OPCI2_NEXT:
                    if r_field2 == OPCI3_EI: # enable interrupt
                        set_i.next = 1;
                    elif r_field2 == OPCI3_DI: # enable interrupt
                        clr_i.next = 1;

    if sim_print:
        @always(clk.negedge)
        def dbgprint():
            if halt == 0:
                print("ir:", ir, "op:", ir[8:] >> 4, "r:", ir[4:], "ir2:", ir2, "op2:", op2,"r2:",r_field2)
                if op == OPC_A_RX:
                    print("EXE RX",rx,"=A",acc)
                elif op == OPC_MVIA:
                    print("EXE A=",imm_next)
                elif op == OPC_MVI:
                    print("EXE Rx",rx,"=",imm_next)
                elif op == OPC_RX_A:
                    print("EXE A=RX",rx)
                elif op == OPC_ADD:
                    print("EXE A=A+Rx",rx)
                elif op == OPC_SUB:
                    print("EXE A=A-Rx",rx)
                elif op == OPC_NEXT:
                    if r_field == OPCI_NOP:
                        print("EXE NOP")
                    else:
                        print("EXE ?",r_field)
                elif op == OPC_JMP:
                    print("EXE jmp #")
                print("ctrl:",
                    "alu_oper",alu_oper,
                    "alu_x_imm",alu_x_imm,
                    "alu_y_pc",alu_y_pc,
                    "reg_wr_alu",reg_wr_alu,
                    "alu_imm_width", alu_imm_width)
                print(
                    "wr_reg",wr_reg,
                    "inc_sp",inc_sp,
                    "dec_sp",dec_sp,
                    "inc_pc",inc_pc,
                    "sel_imem",sel_imem)
                print(
                    "load_pc",load_pc,
                    "n_load_ir",n_load_ir,
                    "n_load_ir2",n_load_ir2,
                    "load_imm",load_imm)

                print("alu_op_x",alu_op_x)
                print("alu_op_y",alu_op_y)

                if alu_oper == ALU_PASS_X:
                    print("alu PASS_X",alu_op_x)
                elif alu_oper == ALU_PASS_Y:
                    print("alu PASS_Y",alu_op_y)
                elif alu_oper == ALU_ADD:
                    print("alu ADD",alu_op_x,alu_op_y,alu_op_x+alu_op_y)
                elif alu_oper == ALU_SUB:
                    print("alu SUB",alu_op_x,alu_op_y,alu_op_x+alu_op_y)

                print("rx",r_field, reg_bank[r_field])
                print("imm:",imm,"imm_next",imm_next)
                print("imem_dout:",imem_dout)
                print("pc_next",pc_next)

    @always_comb
    def pcctrl():
        pc_inc.next = pc + 1
        if halt:
            pc_next.next = pc
        elif load_pc == 1 or deferred_load_pc == 1:
            pc_next.next = alu_out
        elif inc_pc == 1:
            pc_next.next = pc_inc
        elif load_pc_intr == 1:
            pc_next.next = INTR_START
        else:
            pc_next.next = pc

    ipc = flop( pc_next, pc, clk_en, clk, sync_rstn )

    @always_comb
    def imem():
        if sel_imem == PC:
            imem_adr.next = pc_next
            imem_rd.next = 1
        else:
            imem_adr.next = pc_next
            imem_rd.next = 0

    @always_comb
    def irctrl():
        if load_ir == 1:
            curr_ir.next = imem_dout[8:]
        else:
            curr_ir.next = ir
        n_ir.next = curr_ir

        if load_ir2 == 1:
            n_ir2.next = imem_dout[8:]
        else:
            n_ir2.next = ir2

    @always_comb
    def rxidx():
        if sel_reg_cnt == 1:
            rx_idx.next = n_reg_cnt
        elif sel_srp == 1:
            rx_idx.next = SRP
        elif op2_valid == 1 and rx_field2 == 1:
            rx_idx.next = r_field2
        else:
            rx_idx.next = r_field

    @always_comb
    def selrx():
        # - duplicated code from reg_wr, should be merged
        masked_dout = modbv(0)[32:]
        if reg_ld_maskb == 1:
            masked_dout[:] = dmem_dout[8:]
        elif reg_ld_maskw == 1:
            masked_dout[:] = dmem_dout[16:]
        else:
            masked_dout[:] = dmem_dout

        if reg_wr_deferred==1 and reg_ld_rx == r_field:
            rx.next = masked_dout
        else:
            rx.next = reg_bank[ rx_idx ]

    @always_comb
    def accforw():
        masked_dout = modbv(0)[32:]
        if acc_ld_maskb == 1:
            masked_dout[:] = dmem_dout[8:]
        elif acc_ld_maskw == 1:
            masked_dout[:] = dmem_dout[16:]
        else:
            masked_dout[:] = dmem_dout

        if acc_wr_deferred==1:
            acc.next = masked_dout
        else:
            acc.next = acc_ff

    # 7-bits
    #   [    instr        ]
    #   [ 0 n n n n n n n ]
    # 
    # 14-bits
    #   [    instr        ]
    #   [ 1 n n n n n n n ]
    #   [ 0 m m m m m m m ] -> 0bnnnnnnnmmmmmmm
    @always_comb
    def ld_imm():
        imm_next.next = 0
        ext = modbv(0)[32:]
        ext[:] = 0
        if load_imm==1 or direct_load_imm==1:
            if imem_dout[6]:
                ext[:] = 0xffffffff
            ext[7:] = imem_dout[7:]
            imm_next.next = ext
        else:
            imm_next.next = imm << 7 | imem_dout[7:]
        imm_more.next = imem_dout[7]

    imm_ff = flop( imm_next, imm, clk_en, clk, sync_rstn )

    rc_ff = flop( n_reg_cnt, reg_cnt, clk_en, clk, sync_rstn )

    @always_comb
    def rcnt():
        if load_reg_cnt == 1:
            n_reg_cnt.next = n_ir2[4:]
        elif clear_reg_cnt == 1:
            n_reg_cnt.next = 0
        elif load_reti_reg_cnt == 1:
            n_reg_cnt.next = PCR
        elif inc_reg_cnt == 1:
            n_reg_cnt.next = reg_cnt + 1
        else:
            n_reg_cnt.next = reg_cnt - 1


    @always_comb
    def selalu():
        ext = modbv(0)[32:]
        ext[:] = 0
        # alu op X - immediate / Rx
        alu_op_x.next = 0
        if alu_x_imm:
            if alu_imm_width == 0:
                if imm_next[3]:
                    ext[:] = 0xffffffff
                ext[4:] = imm_next[4:]
                alu_op_x.next = ext
            elif alu_imm_width == 1:
                alu_op_x.next = imm_next # TBD sex
        else:
            alu_op_x.next = rx
        # alu op Y (ACC) - A / PC+1
        alu_op_y.next = 0
        if alu_y_pc:
            alu_op_y.next = pc + 1
        else:
            alu_op_y.next = acc

    @inline
    def sub( x, y, res, extended_x, extended_y, width=32 ) :
        extended_x[:] = x
        extended_x[width] = extended_x[width-1]

        extended_y[:] = y
        extended_y[width] = extended_y[width-1]

        res[:] = extended_x - extended_y;

    @inline
    def flags( x, y, ext_res, n, z, v, c, width ):
        # subtract with carry:
        # a - b -> a + (~b) + 1 -> a + ~b + carry-in
        # carry in = 1, unless we implement chaining
        # carry out:
        c[:] = ext_res[width]
        # Overflow occurs when the sign of the operands are different and
        # the sign of the result is different from the sign of operand x
        v[:] = ((x[width-1] ^ y[width-1]) & (x[width-1] ^ ext_res[width-1]));
        n[:] = ext_res[width-1]
        z[:] = ext_res[width-1:] == 0

    

    @always_comb
    def alu():
        extended_x = modbv(0)[32+1:]
        extended_y = modbv(0)[32+1:]

        n_n.next   = cc_n
        n_z.next   = cc_z
        n_v.next   = cc_v
        n_c.next   = cc_c
        n_c8.next  = cc_c8
        n_z8.next  = cc_z8
        n_c16.next = cc_c16
        n_z16.next = cc_z16

        alu_out.next = ALU_PASS_X
        tmp = modbv(0)[33:]
        if alu_oper == ALU_PASS_X:
            alu_out.next = alu_op_x
        elif alu_oper == ALU_PASS_Y:
            alu_out.next = alu_op_y
        elif alu_oper == ALU_LSL:
            alu_out.next = alu_op_y << 1
        elif alu_oper == ALU_LSR:
            alu_out.next = alu_op_y >> 1
        elif alu_oper == ALU_ASR:
            tmp[:] = alu_op_y >> 1
            tmp[31] = tmp[30]
            alu_out.next = tmp
        elif alu_oper == ALU_SUB or alu_oper == ALU_ADD or alu_oper == ALU_ADC or alu_oper == ALU_ADDC:

            alu_out.next = sub_res

            if alu_oper == ALU_SUB:
                n_n.next = ln
                n_z.next = lz
                n_v.next = lv
                n_c.next = lc
                n_c8.next = lc8
                n_z8.next = lz8
                n_c16.next = lc16
                n_z16.next = lz16
            elif alu_oper == ALU_ADDC or alu_oper == ALU_ADC:
                n_c.next = lc

        elif alu_oper == ALU_AND:
            alu_out.next = alu_op_x & alu_op_y
        elif alu_oper == ALU_OR:
            alu_out.next = alu_op_x | alu_op_y
        elif alu_oper == ALU_NOT:
            alu_out.next = ~alu_op_y
        elif alu_oper == ALU_XOR:
            alu_out.next = alu_op_x ^ alu_op_y
        elif alu_oper == ALU_MASKB:
            alu_out.next = alu_op_y[8:]
        elif alu_oper == ALU_MASKW:
            alu_out.next = alu_op_y[16:]
        elif alu_oper == ALU_SEXB:
            tmp[:] = alu_op_y[8:]
            if alu_op_y[7] == 1:
                tmp[32:8] = 0xffffff
            else:
                tmp[32:8] = 0
            alu_out.next = tmp
        elif alu_oper == ALU_SEXW:
            tmp[:] = alu_op_y[16:]
            if alu_op_y[15] == 1:
                tmp[32:16] = 0xffff
            else:
                tmp[32:16] = 0
            alu_out.next = tmp

    @always_comb
    def ccreg():
        if load_cc == 1:
            n_cc.next = alu_out
        else:
            n_cc.next = concat( n_n,n_c,n_z,n_v,n_c8,n_z8,n_c16,n_z16 )

    @always_comb
    def ccsplit():
        cc_n.next = cc[7]
        cc_c.next = cc[6]
        cc_z.next = cc[5]
        cc_v.next = cc[4]
        cc_c8.next = cc[3]
        cc_z8.next = cc[2]
        cc_c16.next = cc[1]
        cc_z16.next = cc[0]

    cc_ff = flop( n_cc, cc, clk_en, clk, sync_rstn )

    @always_comb
    def wrback():
        if reg_wr_alu:
            reg_wr_op.next = alu_out
        elif reg_wr_pc:
            reg_wr_op.next = pc_inc
        else:
            reg_wr_op.next = imem_dout

    @always_comb
    def dmema():
        dmem_adr.next = 0
        if dmem_adr_sel == 0: # ALU
            dmem_adr.next = alu_out
        elif dmem_adr_sel == 1: # SP
            dmem_adr.next = reg_bank[ SP ]
        elif dmem_adr_sel == 2: # next SP
            dmem_adr.next = n_sp

    @always_comb
    def dmemd():
        push_acc = modbv(0)[1:]
        push_acc[:] = intr_push == 1 and n_reg_cnt == A
        if dmem_wr_acc == 1 or push_acc == 1:
            dmem_din.next = acc
        elif intr_push == 1 and n_reg_cnt == CC:
            dmem_din.next = cc
        elif intr_push == 1 and n_reg_cnt == PCR:
            dmem_din.next = pc
        else:
            dmem_din.next = rx

    @always_comb
    def to_acc():
        masked_dout = modbv(0)[32:]
        if acc_ld_maskb == 1:
            masked_dout[:] = dmem_dout[8:]
        elif acc_ld_maskw == 1:
            masked_dout[:] = dmem_dout[16:]
        else:
            masked_dout[:] = dmem_dout

        if wr_acc:
            acc_next.next = reg_wr_op
        elif acc_wr_deferred==1:
            acc_next.next = masked_dout
        else:
            acc_next.next = acc

    if sim_print:
        @always(clk.negedge)
        def dbgprintmx():
            if reg_wr_alu:
                print("reg_wr_op alu",alu_out)
            else:
                print("reg_wr_op imem",imem_dout)
            if wr_acc:
                print("wr_acc",reg_wr_op)


    iacc_ff = flop( acc_next, acc_ff, clk_en, clk, sync_rstn )

    @always_comb
    def regd():
        if reg_dest_srp:
            reg_dest.next = SRP
        else:
            reg_dest.next = r_field

    @always_comb
    def spinc():
        n_sp.next = 0
        if inc_sp:
            n_sp.next = reg_bank[ SP ] + 4
        elif dec_sp:
            n_sp.next = reg_bank[ SP ] - 4

    @always(clk.posedge)
    def reg_wr():
        masked_dout = modbv(0)[32:]
        if sync_rstn == 0:
            for i in range(16):
                reg_bank[i].next = 0
        else:
            if clk_en == 1:
                if reg_ld_maskb == 1:
                    masked_dout[:] = dmem_dout[8:]
                elif reg_ld_maskw == 1:
                    masked_dout[:] = dmem_dout[16:]
                else:
                    masked_dout[:] = dmem_dout

                if reg_wr_deferred:
                    reg_bank[ reg_ld_rx ].next = masked_dout

                if wr_reg:
                    reg_bank[ reg_dest ].next = reg_wr_op

                if inc_sp == 1 or dec_sp == 1:
                    reg_bank[ SP ].next = n_sp

    if sim_print:
        @always(clk.negedge)
        def dbgprintrx():
            if wr_reg:
                print("wr R",reg_dest,"=",reg_wr_op)

    if enable_obs:
        @always_comb
        def obsreg():
            for i in range(16):
                obs_regs[i].next = reg_bank[i]
            obs_acc.next = acc
            obs_cc.next = cc

        @always(clk.posedge)
        def obsff():
            if clk_en == 1:
                obs_op.valid.next = 0
                if state == ST_NEXT_INSTR:
                    if not ( op == OPC_NEXT and r_field == OPCI_NEXT ):
                        obs_op.valid.next = 1
                        obs_op.op.next = op
                        obs_op.op_jmp.next = r_field
                        obs_op.op_inner.next = r_field
                elif state == ST_READ_PART2:
                    obs_op.valid.next = 1
                    obs_op.op.next = op
                    obs_op.op_jmp.next = r_field
                    obs_op.op_inner.next = r_field
                    obs_op.op_inner2.next = op2

    @always_comb
    def extr_instr():
        # [ o o o o  r r r r ]
        op.next = curr_ir[8:] >> 4
        r_field.next = curr_ir[4:]
        op2.next = n_ir2[8:] >> 4
        r_field2.next = n_ir2[4:]

    if sim_print:
        @always(clk.negedge)
        def prt_cpu():
            if sync_rstn == 0 or halt == 1:
                print("halt")
            else:
                print("PC:",pc)
                print("A  ",acc)
                print("R0 ",reg_bank[0 ])
                print("R1 ",reg_bank[1 ])
                print("R2 ",reg_bank[2 ])
                print("R3 ",reg_bank[3 ])
                print("R4 ",reg_bank[4 ])
                print("R5 ",reg_bank[5 ])
                print("R6 ",reg_bank[6 ])
                print("R7 ",reg_bank[7 ])
                print("R8 ",reg_bank[8 ])
                print("R9 ",reg_bank[9 ])
                print("R10",reg_bank[10])
                print("R11",reg_bank[11])
                print("R12",reg_bank[12])
                print("R13",reg_bank[13])
                print("SP ",reg_bank[14])
                print("SRP",reg_bank[15])

    return instances()

