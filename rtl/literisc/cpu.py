from myhdl import *
from myhdl import Struct, unpack_struct

from modules.common.memory import memory
from modules.common.Common import copySignal, multiflop

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
OPC_JMP       = 10 # j #nn
OPC_ADD       = 11 # A = A + Rx
OPC_SUB       = 12 # A = A - Rx
OPC_AND       = 13 # A = A & Rx
OPC_OR        = 14 # A = A | Rx
OPC_NEXT      = 15
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

# op code inner
OPCI_NOT     = 0  # A = ~A
OPCI_LSL     = 1  # c = A, A = A << 1, A<0> = 0
OPCI_LSR     = 2  # c = A, A = A >> 1, A<31> = 0
OPCI_ASR     = 3  # c = A, A = A >> 1, A<31> = A<30>
OPCI_PUSH_R  = 4  # for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
OPCI_POP_R   = 5  # for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
OPCI_ST_SRP  = 6  # M[sp].l = srp
OPCI_POP_A   = 7  # a = M[sp].l, sp = sp + 4
OPCI_NEXT    = 8 
OPCI_UNUSED1 = 9 
OPCI_MASKB   = 10 # A = A & 0xff
OPCI_MASKW   = 11 # A = A & 0xffff
OPCI_SEXB    = 12 # A<31:8>  = A<7>
OPCI_SEXW    = 13 # A<31:16> = A<15>
OPCI_J_A     = 14 # PC = A
OPCI_NOP     = 15 # NOP
# op code inner 2
OPCI2_LDB_A_OFFS = 2  # Rx = M[A+nn].b
OPCI2_LDB_A      = 3  # Rx = M[A].b
OPCI2_LDB_RX     = 4  # A = M[Rx].b
OPCI2_STB_A_OFFS = 5  # M[A+nn].b = Rx
OPCI2_STB_A      = 6  # M[A].b = Rx
OPCI2_STB_RX     = 7  # M[Rx].b = A
OPCI2_LDW_A_OFFS = 10 # Rx = M[A+nn].w
OPCI2_LDW_A      = 11 # Rx = M[A].w
OPCI2_LDW_RX     = 12 # A = M[Rx].w
OPCI2_STW_A_OFFS = 13 # M[A+nn].w = Rx
OPCI2_STW_A      = 14 # M[A].w = Rx
OPCI2_STW_RX     = 15 # M[Rx].w = A

# create symbolic names mapping from the constants
op_to_sym = {}
curr_globs = dict(globals())
for s,v in curr_globs.items():
    if s.startswith("OPC_"):
        op_to_sym[v] = s

jmp_to_sym = {}
for s,v in curr_globs.items():
    if s.startswith("OPCJ_"):
        jmp_to_sym[v] = s

inner_to_sym = {}
for s,v in curr_globs.items():
    if s.startswith("OPCI_"):
        inner_to_sym[v] = s

class InstrCov(Struct):
    def __init__(self):
        self.valid     = Signal(modbv(0)[1:])
        self.op        = Signal(modbv(0)[4:])
        self.op_jmp    = Signal(modbv(0)[4:])
        self.op_inner  = Signal(modbv(0)[4:])
        self.op_inner2 = Signal(modbv(0)[4:])

def cpu( clk, rstn,
         imem_dout,
         imem_adr,
         imem_rd,
         dmem_din,
         dmem_dout,
         dmem_adr,
         dmem_rd,
         dmem_wr,
         halt,
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

    next_ir = Signal(modbv(0)[8:])
    curr_ir = Signal(modbv(0)[8:])
    ir = Signal(modbv(0)[8:])
    imm = Signal(modbv(0)[32:])
    imm_next = Signal(modbv(0)[32:])
    op = Signal(modbv(0)[4:])
    rx = Signal(modbv(0)[32:])
    r_field  = Signal(modbv(0)[4:])
    state = Signal(modbv(0)[4:])
    next_state = Signal(modbv(0)[4:])
    alu_oper = Signal(modbv(0)[4:])

    sel_imem = Signal(modbv(0)[4:])
    load_ir = Signal(modbv(0)[1:])
    new_ir = Signal(modbv(0)[1:])
    inc_pc = Signal(modbv(0)[1:])
    inc_sp = Signal(modbv(0)[1:])
    dec_sp = Signal(modbv(0)[1:])
    load_pc = Signal(modbv(0)[1:])
    direct_load_imm = Signal(modbv(0)[1:])
    load_imm = Signal(modbv(0)[1:])
    n_load_imm = Signal(modbv(0)[1:])
    load_more = Signal(modbv(0)[1:])
    imm_more = Signal(modbv(0)[1:])
    op_sel_rx = Signal(modbv(0)[1:])
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
    n_reg_ld_rx = Signal(modbv(0)[4:])
    reg_ld_rx = Signal(modbv(0)[4:])
    wr_acc = Signal(modbv(0)[1:])
    wr_reg = Signal(modbv(0)[1:])
    reg_dest  = Signal(modbv(0)[4:])
    reg_dest_srp  = Signal(modbv(0)[1:])
    dmem_adr_sel = Signal(modbv(0)[1:])
    dmem_wr_acc = Signal(modbv(0)[1:])

    SP = 14
    SRP = 15
    # state
    RESET = 0
    NEXT_INSTR = 1
    DECODE_INSTR = 2
    READ_IMM = 3
    # sel_imem
    PC = 0
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
    # op code outer

    ist = multiflop( next_state, state, clk, rstn, reset_value=RESET )
    iir = multiflop( next_ir, ir, clk, rstn )
    irc = multiflop( load_ir, new_ir, clk, rstn )
    idf = multiflop( n_reg_wr_deferred, reg_wr_deferred, clk, rstn )
    idr = multiflop( n_reg_ld_rx, reg_ld_rx, clk, rstn )
    ili = multiflop( n_load_imm, load_imm, clk, rstn )

    @always_comb
    def ctrl():
        inc_pc.next = 0
        load_ir.next = 0
        n_load_imm.next = 0
        next_state.next = NEXT_INSTR
        sel_imem.next = 0
        load_more.next = 0

        if halt == 1:
            next_state.next = NEXT_INSTR
            load_ir.next = 1
            n_load_imm.next = 1
            inc_pc.next = 1
        else:
            if state == RESET:
                load_ir.next = 1
                n_load_imm.next = 0
                inc_pc.next = 0
                sel_imem.next = PC
                next_state.next = NEXT_INSTR
            elif state == NEXT_INSTR:
                if op == OPC_MVI or op == OPC_JMP:
                    load_ir.next = 0
                    n_load_imm.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    next_state.next = READ_IMM
                else:
                    load_ir.next = 1
                    n_load_imm.next = 0
                    inc_pc.next = 1
                    sel_imem.next = PC
                    next_state.next = NEXT_INSTR
            elif state == READ_IMM:
                if imm_more:
                    load_ir.next = 0
                    n_load_imm.next = 0
                    load_more.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    next_state.next = READ_IMM
                else:
                    load_ir.next = 1
                    n_load_imm.next = 0
                    inc_pc.next = 1
                    sel_imem.next = PC
                    next_state.next = NEXT_INSTR

    if sim_print:
        @always(clk.negedge)
        def prt_state():
            print("state:",state)
            if state == NEXT_INSTR:
                print("NEXT_INSTR")
                if op == OPC_MVI or op == OPC_JMP:
                    print("- OPC_MVI/JMP")
            elif state == READ_IMM:
                print("READ_IMM")
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
        inc_sp.next = 0
        load_pc.next = 0
        n_reg_ld_rx.next = 0
        n_reg_wr_deferred.next = 0
        op_sel_rx.next = 0
        reg_dest_srp.next = 0
        reg_wr_alu.next = 0
        reg_wr_pc.next = 0
        wr_acc.next = 0
        wr_reg.next = 0
        dmem_wr_acc.next = 0
        dmem_adr_sel.next = 0
        direct_load_imm.next = 0

        take_jump = modbv(0)[1:]
        take_jump[:] = 0
 
        if op == OPC_A_RX:
            alu_oper.next = ALU_PASS_Y
            op_sel_rx.next = 0 # D.C.
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
            wr_reg.next = 1
        elif op == OPC_RX_A:
            alu_oper.next = ALU_PASS_X
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_LD_A:
            alu_oper.next = ALU_PASS_Y
            op_sel_rx.next = 0 # D.C.
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            n_reg_wr_deferred.next = 1
            n_reg_ld_rx.next = r_field
            dmem_rd.next = 1
            dmem_adr_sel.next = 0 # ALU
        elif op == OPC_ST_A:
            alu_oper.next = ALU_PASS_Y
            op_sel_rx.next = 0 # D.C.
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            dmem_wr.next = 1
            dmem_adr_sel.next = 0 # ALU
            dmem_wr_acc.next = 0 # Rx
        elif op == OPC_MVIA:
            alu_oper.next = ALU_PASS_X
            op_sel_rx.next = 0
            alu_x_imm.next = 1
            direct_load_imm.next = 1
            alu_imm_width.next = 0 # 4 bits from first instr byte
            wr_acc.next = 1
            alu_y_pc.next = 0 # D.C.
            reg_wr_alu.next = 1
        elif op == OPC_JMP or op == OPCJ_JNZ:
            if state == NEXT_INSTR:
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0
            elif state == READ_IMM:
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0

                take_jump[:] = 1
                if imm_more == 0:
                    if r_field == OPCJ_JNZ:
                        if cc_z == 0:
                            take_jump[:] = 1
                        else:
                            take_jump[:] = 0

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
            if state == NEXT_INSTR:
                alu_oper.next = ALU_PASS_X
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
            elif state == READ_IMM:
                alu_oper.next = ALU_PASS_X
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
                wr_reg.next = 1

        elif op == OPC_RX_A:
            alu_oper.next = ALU_PASS_X
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_ADD:
            alu_oper.next = ALU_ADD
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_SUB:
            alu_oper.next = ALU_SUB
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
        elif op == OPC_NEXT:
            # the reg field is secondary opcode
            if r_field == OPCI_NOP:
                alu_oper.next = ALU_PASS_Y
                op_sel_rx.next = 1
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
            elif r_field == OPCI_J_A:
                load_pc.next = 1
                alu_oper.next = ALU_PASS_Y
                op_sel_rx.next = 0
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 0


    if sim_print:
        @always(clk.negedge)
        def dbgprint():
            if halt == 0:
                print("ir:", ir, "op:", ir[8:] >> 4, "r:", ir[4:])
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
                    "load_ir",load_ir,
                    "load_imm",load_imm,
                    "load_more",load_more)

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

                if op_sel_rx:
                    print("rx",r_field, reg_bank[r_field])
                print("imm:",imm,"imm_next",imm_next)
                print("imem_dout:",imem_dout)
                print("pc_next",pc_next)

    @always_comb
    def pcctrl():
        pc_inc.next = pc + 1
        if halt:
            pc_next.next = pc
        elif load_pc == 1:
            pc_next.next = alu_out
        elif inc_pc == 1:
            pc_next.next = pc_inc
        else:
            pc_next.next = pc

    ipc = multiflop( pc_next, pc, clk, rstn )

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
        if new_ir == 1:
            curr_ir.next = imem_dout[8:]
        else:
            curr_ir.next = ir
        next_ir.next = curr_ir

    @always_comb
    def selrx():
        #if op_sel_rx:
        if reg_wr_deferred==1 and reg_ld_rx == r_field:
            rx.next = dmem_dout
        else:
            rx.next = reg_bank[ r_field ]

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

    imm_ff = multiflop( imm_next, imm, clk, rstn )

    @always_comb
    def selalu():
        ext = modbv(0)[32:]
        ext[:] = 0
        # alu op X
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
        # alu op Y (ACC)
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
        c[:] = ~ext_res[width]
        # Overflow occurs when the sign of the operands are different and
        # the sign of the result is different from the sign of a
        v[:] = ((x[width-1] ^ y[width-1]) & (x[width-1] ^ ext_res[width-1]));
        n[:] = ext_res[width-1]
        z[:] = ext_res[width-1:] == 0

    @always_comb
    def alu():
        ln = modbv(0)[1:]
        lz = modbv(0)[1:]
        lv = modbv(0)[1:]
        lc = modbv(0)[1:]
        lc8 = modbv(0)[1:]
        lz8 = modbv(0)[1:]
        lc16 = modbv(0)[1:]
        lz16 = modbv(0)[1:]
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
        elif alu_oper == ALU_ADD:
            alu_out.next = alu_op_x + alu_op_y
        elif alu_oper == ALU_SUB:


            sub( alu_op_y, alu_op_x, tmp, extended_x, extended_y )
            flags( alu_op_y, alu_op_x, tmp, ln, lz8,  lv, lc8,  8 )
            flags( alu_op_y, alu_op_x, tmp, ln, lz16, lv, lc16, 16 )
            flags( alu_op_y, alu_op_x, tmp, ln, lz,   lv, lc,   32 )

            n_n.next = ln
            n_z.next = lz
            n_v.next = lv
            n_c.next = lc
            n_c8.next = lc8
            n_z8.next = lz8
            n_c16.next = lc16
            n_z16.next = lz16
            alu_out.next = tmp
        elif alu_oper == ALU_AND:
            alu_out.next = alu_op_x & alu_op_y
        elif alu_oper == ALU_OR:
            alu_out.next = alu_op_x | alu_op_y
        elif alu_oper == ALU_NOT:
            alu_out.next = ~alu_op_y

    @always_comb
    def ccreg():
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

    cc_ff = multiflop( n_cc, cc, clk, rstn )

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
        if dmem_adr_sel == 0:
            dmem_adr.next = alu_out
        # else: SP

    @always_comb
    def dmemd():
        if dmem_wr_acc == 1:
            dmem_din.next = acc
        else:
            dmem_din.next = rx

    @always_comb
    def to_acc():
        if wr_acc:
            acc_next.next = reg_wr_op
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


    iacc_ff = multiflop( acc_next, acc, clk, rstn )

    @always_comb
    def regd():
        if reg_dest_srp:
            reg_dest.next = SRP
        else:
            reg_dest.next = r_field

    @always(clk.posedge, rstn.negedge)
    def reg_wr():
        if rstn == 0:
            for i in range(16):
                reg_bank[i].next = 0
        else:
            if reg_wr_deferred:
                reg_bank[ reg_ld_rx ].next = dmem_dout

            if wr_reg:
                reg_bank[ reg_dest ].next = reg_wr_op
            if inc_sp:
                reg_bank[ SP ].next = reg_bank[ SP ] + 4
            elif dec_sp:
                reg_bank[ SP ].next = reg_bank[ SP ] - 4

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

        @always(clk.posedge, rstn.negedge)
        def obsff():
            obs_op.valid.next = 0
            if state == NEXT_INSTR:
                obs_op.valid.next = 1
                obs_op.op.next = op
                obs_op.op_jmp.next = r_field
                obs_op.op_inner.next = r_field

    @always_comb
    def extr_instr():
        # [ o o o o  r r r r ]
        op.next = curr_ir[8:] >> 4
        r_field.next = curr_ir[4:]

    if sim_print:
        @always(clk.negedge)
        def prt_cpu():
            if rstn == 0 or halt == 1:
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

def mem_loader( clk, rstn, waddr, wdata, wenable, done, content ):

    addr = Signal( modbv(0)[len(waddr)+1:] )

    @always(clk.posedge, rstn.negedge)
    def doinit():
        if rstn==0:
            addr.next = 0
            done.next = 0
        else:
            if addr == len(content):
                addr.next = addr
                done.next = 1
                wenable.next = 0
            else:
                addr.next = addr + 1
                wenable.next = 1
                wdata.next = content[ int(addr) ]
                waddr.next = addr
                "synthesis translate_off"
                print("mem_loader",addr)
                "synthesis translate_on"

    return instances()

class InstrCovCollector(object):

    covered_op = set()
    covered_jmp = set()
    covered_inner = set()

    def sym_op(opl):
        return [ op_to_sym[o] for o in opl ]
    def sym_jmp(opl):
        return [ jmp_to_sym[o] for o in opl ]
    def sym_inner(opl):
        return [ inner_to_sym[o] for o in opl ]

    @classmethod
    def report(cls):
        print("COV: -- op --")
        print("COV: covered:",cls.sym_op(cls.covered_op))
        print("COV: uncovered:",
            cls.sym_op(
                set(list(range(16))).difference(
                    cls.covered_op)))

        print("COV: -- jmp --")
        print("COV: covered:",cls.sym_jmp(cls.covered_jmp))
        print("COV: uncovered:",
            cls.sym_jmp(
                set(list(range(16))).difference(
                    cls.covered_jmp)))

        print("COV: -- inner --")
        print("COV: covered:",cls.sym_inner(cls.covered_inner))
        print("COV: uncovered:",
            cls.sym_inner(
                set(list(range(16))).difference(
                    cls.covered_inner)))

def cpu_tester( clk, programs ):

    rstn = Signal(modbv(0)[1:])

    imem_radr = Signal(modbv(0)[16:])
    imem_wadr = Signal(modbv(0)[16:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])

    dmem_adr = Signal(modbv(0)[16:])
    dmem_din = Signal(modbv(0)[32:])
    dmem_dout = Signal(modbv(0)[32:])
    dmem_rd = Signal(modbv(0)[1:])
    dmem_wr = Signal(modbv(0)[1:])

    load_done = Signal(modbv(0)[1:])
    halt = Signal(modbv(0)[1:])

    imem_depth = 64
    dmem_depth = 1024 

    cov_coll = InstrCovCollector

    @instance
    def progdriver():
        nr_pass = 0
        nr_fails = 0

        for prog in programs:
            print("============= program start ===============")
            any_fail = False
            imem = prog['imem'] # dict with adr -> data
            expect_reg = prog['expect'] # dict adr -> dict reg-nr -> value
            dmem = prog['dmem'] # list of dicts with { 'rd':1/0, 'adr', 'data' }
            print("expect",expect_reg)

            exp_seq = []
            if 'pc' in prog:
                exp_seq = prog['pc']
            pc_sequence = []

            rstn.next = 0
            yield clk.posedge
            yield clk.posedge
            rstn.next = 1 
            halt.next = 0 
            no_more_instr = False
            end_test = False
           
            while True:
                adr = int(imem_radr.val)
                print("imem_adr",adr)
                if adr in imem:
                    imem_dout.next = imem[ adr ]
                    print("imem_dout", imem[adr])
                else:
                    no_more_instr = True
  
                if dmem_rd.val == 1 or dmem_wr.val == 1:
                    exp_dmem = dmem[0]
                    dmem = dmem[1:]
                    dadr = int(dmem_adr.val)
                    if dmem_rd.val == 1:
                        print("dmem_adr rd",dadr)
                        if exp_dmem['rd'] == 1:
                            if dadr == exp_dmem['adr']:
                                print("check dmem rd adr ok", dadr)
                            else:
                                print("check dmem rd adr MISS act:", dadr, "exp:", exp_dmem['adr'])
                                any_fail = True
                            print("dmem_dout",exp_dmem['data'])
                            dmem_dout.next = exp_dmem['data']
                        else:
                            print("check dmem MISS act: rd exp: wr")
                            any_fail = True
                    else:
                        print("dmem_adr wr",dadr,dmem_din.val)
                        if exp_dmem['rd'] == 0:
                            if dadr == exp_dmem['adr']:
                                print("check dmem wr adr ok", dadr)
                            else:
                                print("check dmem wr adr MISS act:", dadr, "exp:", exp_dmem['adr'])
                                any_fail = True
                        else:
                            print("check dmem MISS act: wr exp: rd")
                            any_fail = True


                # check ongoing sequence
                if pc_sequence:
                    if adr == pc_sequence[0]:
                        print("check PC ok", adr)
                    else:
                        print("check PC MISS act",adr,"exp:",pc_sequence[0])
                        any_fail = True
                    pc_sequence = pc_sequence[1:]

                # start a new sequence
                if adr in exp_seq:
                    print("load sequence",exp_seq[adr])
                    pc_sequence = exp_seq[ adr ]
                if 'loop' in exp_seq and adr == exp_seq['loop']:
                    end_test = True

                if adr in expect_reg:
                    #print("expect at",adr,"exp:",expect_reg[adr])
                    for r in expect_reg[ adr ].keys():
                        print(f"expect at {adr}: {r}")
                        if r == 'A':
                            if obs_acc == expect_reg[ adr ]['A']:
                                print("check A OK", obs_acc)
                            else:
                                print(f"check A MISS act:",obs_acc,
                                       "exp:", expect_reg[ adr ][ 'A' ])
                                any_fail = True
                        elif r == 'cc':
                            if obs_cc == expect_reg[ adr ]['cc']:
                                print("check cc OK ",print_cc(obs_cc))
                            else:
                                exp = expect_reg[ adr ]['cc']
                                print("check cc MISS act:",print_cc(obs_cc),
                                      "exp:",print_cc(exp))
                                any_fail = True

                        else:
                            if obs_regs[ r ] == expect_reg[ adr ][ r ]:
                                print(f"check r{r} OK", obs_regs[r])
                            else:
                                print(f"check r{r} MISS act:",obs_regs[r],
                                       "exp:", expect_reg[ adr ][ r ])
                                any_fail = True

                if no_more_instr or end_test:
                    print("========== end of program =================")
                    if any_fail:
                        nr_fails += 1
                    else:
                        nr_pass += 1
                    break
                yield clk.negedge
                yield clk.posedge
        print("============= no more programs ===============")
        print(f"tests pass:{nr_pass} fails:{nr_fails}")
        cov_coll.report()

        raise StopSimulation


    dmem = memory(
        idata = dmem_din,
        odata = dmem_dout,
        raddr = dmem_adr,
        waddr = dmem_adr,
        renable = dmem_rd,
        wenable = dmem_wr,
        clk = clk,
        rstn = rstn,
        depth = dmem_depth,
        input_flops = 0,
        output_flops = 0,
        name = 'dmem')

    obs_regs = [ Signal(modbv(0)[32:]) for _ in range(16) ]
    obs_acc = Signal(modbv(0)[32:])
    obs_cc = Signal(modbv(0)[8:])
    obs_op = InstrCov()

    icpu = cpu(
        clk = clk,
        rstn = rstn,
        imem_dout = imem_dout,
        imem_adr = imem_radr,
        imem_rd = imem_rd,
        dmem_din = dmem_din,
        dmem_dout = dmem_dout,
        dmem_adr = dmem_adr,
        dmem_rd = dmem_rd,
        dmem_wr = dmem_wr,
        halt = halt,
        enable_obs = True,
        obs_regs = obs_regs,
        obs_acc = obs_acc,
        obs_cc = obs_cc,
        obs_op = obs_op,
        sim_print = True
    )

   

    @always(clk.negedge)
    def opcov():
        #print("OPCOV",obs_op.valid,obs_op.op)
        if obs_op.valid == 1:
            InstrCovCollector.covered_op.add( int(obs_op.op.val) )
            if obs_op.op == OPC_JMP:
                InstrCovCollector.covered_jmp.add( int(obs_op.op_jmp))
            if obs_op.op == OPC_NEXT:
                InstrCovCollector.covered_inner.add( int(obs_op.op_inner))


    return instances()

def cpu_top( clk, rstn ):

    imem_radr = Signal(modbv(0)[16:])
    imem_wadr = Signal(modbv(0)[16:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])

    dmem_adr = Signal(modbv(0)[16:])
    dmem_din = Signal(modbv(0)[32:])
    dmem_dout = Signal(modbv(0)[32:])
    dmem_rd = Signal(modbv(0)[1:])
    dmem_wr = Signal(modbv(0)[1:])

    load_done = Signal(modbv(0)[1:])
    halt = Signal(modbv(0)[1:])

    imem_depth = 64
    dmem_depth = 1024 

    program = [ 0 for i in range(imem_depth) ]
    #program[ 0  ] = 0x81 # R1 = 10
    #program[ 1  ] = 10
    #program[ 2  ] = 0x82 # R2 = 20
    #program[ 3  ] = 20
    #program[ 4  ] = 0x97 # A = 7 
    #program[ 5  ] = 0x62 # M[A].l = R2
    #program[ 6  ] = 0x31 # R1 = M[ A ].l
    #program[ 7  ] = 0x13 # A = R1 ; forward mem data to reg operand
    #program[ 8  ] = 0xff # NOP
    #program[ 9  ] = 0xff # NOP
    #program[ 10 ] = 0xaf # JSR 1 # JMP 1
    #program[ 11 ] = 1
    #program[ 12 ] = 0xff # NOP - skipped
    #program[ 13 ] = 0x1f # A = SRP
    #program[ 14 ] = 0xfe # J A
    #program[ 15 ] = 0x88 # R8 = 33
    #program[ 16 ] = 22
    #program[ 17 ] = 0xff # NOP
    #program[ 18 ] = 0xa0 # JMP 0
    #program[ 19 ] = 0
    #program[ 20 ] = 0x82 # R2 = 33
    #program[ 21 ] = 33
    #program[ 22 ] = 0x97 # A = 7 
    #program[ 23 ] = 0x9f # A = -1
    #program[ 24 ] = 0x84 # R4 = -1
    #program[ 25 ] = 0b1111111
    #program[ 26 ] = 0x85 # R5 = -2
    #program[ 27 ] = 0b1111110
    #program[ 28 ] = 0x84 # R4 = 294
    #program[ 29 ] = (294 >> 7) | 0x80
    #program[ 30 ] = (294 & 0x7f)
    #program[ 31 ] = 0xff # NOP
    #program[ 32 ] = 0xff # NOP
    #program[ 33 ] = 0xff # NOP
    #program[ 34 ] = 0x03 # R3 = A
    #program[ 35 ] = 0x92 # A = 2
    #program[ 36 ] = 0x08 # R8 = A
    #program[ 37 ] = 0x13 # A = R3
    #program[ 38 ] = 0xb8 # A = A + R8
    imem_load = tuple(program)

    imem = memory(
        idata = imem_din,
        odata = imem_dout,
        raddr = imem_radr,
        waddr = imem_wadr, # dummy
        renable = imem_rd,
        wenable = imem_wr,
        clk = clk,
        rstn = rstn,
        depth = imem_depth,
        input_flops = 0,
        output_flops = 0,
        name = 'imem')

    imem_ld = mem_loader(
        clk, rstn,
        imem_wadr,
        imem_din,
        imem_wr,
        load_done,
        imem_load )
    
    dmem = memory(
        idata = dmem_din,
        odata = dmem_dout,
        raddr = dmem_adr,
        waddr = dmem_adr,
        renable = dmem_rd,
        wenable = dmem_wr,
        clk = clk,
        rstn = rstn,
        depth = dmem_depth,
        input_flops = 0,
        output_flops = 0,
        name = 'dmem')


    icpu = cpu(
        clk = clk,
        rstn = rstn,
        imem_dout = imem_dout,
        imem_adr = imem_radr,
        imem_rd = imem_rd,
        dmem_din = dmem_din,
        dmem_dout = dmem_dout,
        dmem_adr = dmem_adr,
        dmem_rd = dmem_rd,
        dmem_wr = dmem_wr,
        halt = halt,
        sim_print = True
    )

    @always_comb
    def h():
        halt.next = not load_done

    @always(clk.posedge, rstn.negedge)
    def prt_imem():
        print("imem a:",imem_radr,"dout:",imem_dout)
        if imem_wr==1:
            print("imem wr a:",imem_wadr,"din:",imem_din)
    return instances()

def tb():

    clk = Signal(bool())
    rstn = Signal(intbv(0)[1:0])

    icpu_top = cpu_top( clk, rstn )
  
    @always(delay(10))
    def stim():
      print("=========== CLOCK ============")
      clk.next = not clk

    @instance
    def seq():
      rstn.next = 0
      yield clk.posedge
      rstn.next = 1 
      yield clk.posedge

    return instances()

def print_cc( cc ):
    v = int(cc)
    cc = modbv(v)[8:]
    return "CC n:{} c:{} z:{} v:{} c8:{} z8:{} c16:{} z16:{}".format(
            int(cc[7]), int(cc[6]), int(cc[5]), int(cc[4]), int(cc[3]), int(cc[2]), int(cc[1]), int(cc[0]) )

# check that registers that has no expected value has the reset value
def no_side_effect( expect ):
    for check in [ 'A' ] + list(range(16)) + ['cc']:
        if check not in expect:
            expect[check] = 0
    return expect


def test_1(program,expect,pc,dmem):
    # ---- test immediate ------
    program[ 0  ] = 0x81 # R1 = 10
    program[ 1  ] = 10
    expect[ 3 ] = { 1 : 10 }

    program[ 2  ] = 0x82 # R2 = 20
    program[ 3  ] = 20
    expect[ 5 ] = { 1 : 10, 2: 20 }

    program[ 4  ] = 0x97 # A = 7 
    program[ 5  ] = 0xff # NOP
    expect[ 6 ] = { 1 : 10, 2: 20, 'A': 7 }

    program[ 6  ] = 0x9f # A = -1
    expect[ 8 ] = { 1 : 10, 2: 20, 'A': 0xffffffff }

    program[ 7  ] = 0x84 # R4 = -1
    program[ 8  ] = 0b1111111
    expect[ 10 ] = { 1 : 10, 2: 20, 'A': 0xffffffff, 4: 0xffffffff }

    program[ 9  ] = 0x85 # R5 = -2
    program[ 10 ] = 0b1111110
    expect[ 12 ] = { 1 : 10, 2: 20, 'A': 0xffffffff, 4: 0xffffffff, 5: 0xfffffffe }

    program[ 11 ] = 0x84 # R4 = 294
    program[ 12 ] = (294 >> 7) | 0x80
    program[ 13 ] = (294 & 0x7f)
    expect[ 15 ] = no_side_effect( { 1 : 10, 2: 20, 'A': 0xffffffff, 4: 294, 5: 0xfffffffe } )

    program[ 14 ] = 0xff # NOP

def test_2(program,expect,pc,dmem):
    # ---- test mv and add ----------
    program[ 0 ] = 0x95 # A = 5 
    expect[ 2 ] = { 'A': 5 }

    program[ 1 ] = 0x03 # R3 = A
    expect[ 3 ] = { 'A': 5, 3: 5 }

    program[ 2 ] = 0x92 # A = 2
    expect[ 4 ] = { 'A': 2, 3: 5 }

    program[ 3 ] = 0x08 # R8 = A
    expect[ 5 ] = { 'A': 2, 3: 5, 8: 2 }

    program[ 4 ] = 0x13 # A = R3
    expect[ 6 ] = { 'A': 5, 3: 5, 8: 2 }

    program[ 5 ] = 0xb8 # A = A + R8
    expect[ 7 ] = no_side_effect({ 'A': 7, 3: 5, 8: 2 })

    program[ 6 ] = 0xff # NOP
    program[ 7 ] = 0xff # NOP

def test_3(program,expect,pc,dmem):
    # ---- test jumps ----------
    program[ 0 ] = 0xff # NOP
    program[ 1 ] = 0xaf # JSR 1
    program[ 2 ] = 1
    pc[2] = [4] # load at 2, expect next is 4
    expect[ 5 ] = { 15: 3 }

    program[ 4 ] = 0xa0 # JMP 1
    program[ 5 ] = 1
    pc[5] = [7]

    program[ 7 ] = 0xa0 # JMP 0
    program[ 8 ] = 0
    pc[8] = [9]

    # endless loop
    program[ 9  ] = 0xa0 # JMP -2
    program[ 10 ] = 0x7e # -2 
    pc[10] = [9]
    pc['loop'] = 10


def test_4(program,expect,pc,dmem):
    # ---- test long jump -----
    program[ 0 ] = 0xff # NOP
    program[ 1 ] = 0xa0 # JMP 
    program[ 2 ] = (294 >> 7) | 0x80
    program[ 3 ] = (294 & 0x7f)
    pc[3] = [ 294 + 4]

    program[ 298 ] = 0xff # NOP
    program[ 299 ] = 0xff # NOP

def test_5(program,expect,pc,dmem):
    # ---- test sub ----------
    expect = dict()
    program = dict()
    program[ 0 ] = 0x92 # A = 2 
    expect[ 2 ] = { 'A': 2 }

    program[ 1 ] = 0x03 # R3 = A = 2
    expect[ 3 ] = { 'A': 2, 3: 2 }

    program[ 2 ] = 0x97 # A = 7 
    expect[ 4 ] = { 'A': 7 }

    program[ 3 ] = 0xc3 # A = A - R3 = 7 - 2 = 5
    expect[ 5 ] = { 'A': 5,
                   #            8 16
                   #        nczvczcz
                   'cc' : 0b01001010 }

    program[ 4 ] = 0xc3 # A = A - R3
    expect[ 6 ] = { 'A': 3 }

    program[ 5 ] = 0xc3 # A = A - R3
    expect[ 7 ] = { 'A': 1 }

    program[ 6 ] = 0xc3 # A = A - R3
    expect[ 8 ] = { 'A': 0xffffffff,
                   #            8 16
                   #        nczvczcz
                   'cc' : 0b10000000 }

    r3 = 1
    program[ 7 ] = 0x83 # R3 = 1
    program[ 8 ] = r3

    r4 = 0x0f01
    program[ 9 ] = 0x84 # R4 = 0x0f01
    program[ 10 ] = (r4 >> 7) | 0x80
    program[ 11 ] = (r4 & 0x7f)
    program[ 12 ] = 0x14 # A = R4
    program[ 13 ] = 0xc3 # A = A - R3 = 0x0f01 - 1 = 0x0f00
    expect[ 15 ] = { 'A': 0x0f00, \
                     3: 1,
                     4: 0x0f01,
                     #            8 16
                     #        nczvczcz
                     'cc' : 0b01000110 }

    expect[ 15 ] = no_side_effect( expect[ 15 ] )

    program[ 14 ] = 0xff # NOP
    program[ 15 ] = 0xff # NOP

def test_6(program,expect,pc,dmem):
    # ---- test jnz ----------
    program[ 0 ] = 0x92 # A = 2 
    program[ 1 ] = 0x03 # R3 = A = 2
    program[ 2 ] = 0x97 # A = 7 
    program[ 3 ] = 0xc3 # A = A - R3 = 7 - 2 = 5
    expect[ 5 ] = { 'A': 5,
                   #            8 16
                   #        nczvczcz
                   'cc' : 0b01001010 }

    program[ 4 ] = (OPC_JMP << 4 | OPCJ_JNZ)
    program[ 5 ] = (294 >> 7) | 0x80
    program[ 6 ] = (294 & 0x7f)
    pc[6] = [ 294 + 7]

    program[ 300 ] = 0xff # NOP
    program[ 301 ] = 0xff # NOP

def test_7(program,expect,pc,dmem):
    # ---- test jnz ----------
    program[ 0 ] = 0x97 # A = 7 
    program[ 1 ] = 0x03 # R3 = A = 7
    program[ 2 ] = 0x97 # A = 7 
    program[ 3 ] = 0xc3 # A = A - R3 = 7 - 7 = 0
    expect[ 5 ] = { 'A': 0,
                   #            8 16
                   #        nczvczcz
                   'cc' : 0b01101111 }

    program[ 4 ] = (OPC_JMP << 4 | OPCJ_JNZ)
    program[ 5 ] = (294 >> 7) | 0x80
    program[ 6 ] = (294 & 0x7f)
    pc[6] = [ 7 ]

    program[ 7 ] = 0xff # NOP
    program[ 8 ] = 0xff # NOP

def test_8(program,expect,pc,dmem):
    # ---- test store --------
    program[ 0 ] = 0x93 # A = 3 
    program[ 1 ] = 0x03 # R3 = A = 3
    program[ 2 ] = 0x97 # A = 7 
    program[ 3  ] = 0x63 # M[A].l = R2 -> M[7]=3
    program[ 4 ] = 0xff # NOP
    program[ 5 ] = 0xff # NOP
    dmem.append({'rd':0, 'adr':7, 'data':3 })

def test_9(program,expect,pc,dmem):
    # ---- test load --------
    program[ 0 ] = 0x93 # A = 3 
    program[ 1 ] = 0x03 # R3 = A = 3
    program[ 2 ] = 0x97 # A = 7 
    program[ 3 ] = 0x31 # R1 = M[ A ].l
    dmem.append({'rd':1, 'adr':7, 'data':123 })
    program[ 4 ] = 0x11 # A = R1 ; forward mem data to reg operand
    expect[6] = { 'A': 123, 1: 123 }
    program[ 5 ] = 0xff # NOP
    program[ 6 ] = 0xff # NOP

def test_10(program,expect,pc,dmem):
    # ---- test j A --------
    program[ 0 ] = 0x97 # A = 7 
    program[ 1 ] = 0xfe # J A
    pc[1] = [ 7 ]
    program[ 7 ] = 0xff # NOP

def tb2():

    clk = Signal(bool())

    progs = []

    tests = [10]
    tests = list(range(1,11))

    for tid in tests:
        expect = dict()
        program = dict()
        pc = dict()
        dmem = list()

        print(f"===== add test {tid} =====")
        eval(f"test_{tid}(program,expect,pc,dmem)")

        progs.append( { 'imem': program,
                        'dmem': dmem,
                        'expect': expect,
                        'pc' : pc} )

    tests = []

    icpu_tester = cpu_tester( clk, progs )
  
    @always(delay(10))
    def stim():
      print("=========== CLOCK ============")
      clk.next = not clk

    return instances()

def main():
    if run_sim:
        #itb = tb()
        traceSignals.filename = 'trace'
        itb = traceSignals( tb2 ) 
        sim = Simulation( itb )
        sim.run( 2000 )
    else:
        clk = Signal(bool())
        rstn = Signal(intbv(0)[1:0])
        imem_radr = Signal(modbv(0)[16:])
        imem_wadr = Signal(modbv(0)[16:])
        imem_din = Signal(modbv(0)[8:])
        imem_dout = Signal(modbv(0)[8:])
        imem_rd = Signal(modbv(0)[1:])
        imem_wr = Signal(modbv(0)[1:])

        dmem_adr = Signal(modbv(0)[16:])
        dmem_din = Signal(modbv(0)[32:])
        dmem_dout = Signal(modbv(0)[32:])
        dmem_rd = Signal(modbv(0)[1:])
        dmem_wr = Signal(modbv(0)[1:])

        halt = Signal(modbv(0)[1:])

        toVerilog.standard = 'systemverilog'
        itop = toVerilog( cpu, clk, rstn,
                imem_dout,
                imem_radr,
                imem_rd,
                dmem_din,
                dmem_dout,
                dmem_adr,
                dmem_rd,
                dmem_wr,
                halt,
                False,
                None,
                None,
                None,
                None,
                False)
    
run_sim = False

if __name__ == '__main__':
    import sys
    if 'sim' in sys.argv:
        run_sim = True
    main()
