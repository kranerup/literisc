from myhdl import *

from modules.common.memory import memory
from modules.common.Common import copySignal, multiflop

def cpu( clk, rstn,
         imem_dout,
         imem_adr,
         imem_rd,
         halt):

    pc = Signal(modbv(0)[16:])
    pc_next = Signal(modbv(0)[16:])
    reg_bank = [ Signal(modbv(0)[32:]) for _ in range(16) ]
    acc = Signal(modbv(0)[32:])
    acc_next = Signal(modbv(0)[32:])

    ir = Signal(modbv(0)[8:])
    imm = Signal(modbv(0)[32:])
    imm_next = Signal(modbv(0)[32:])
    op = Signal(modbv(0)[4:])
    rx = Signal(modbv(0)[32:])
    r_field  = Signal(modbv(0)[4:])
    state = Signal(modbv(0)[4:])
    alu_oper = Signal(modbv(0)[4:])

    sel_imem = Signal(modbv(0)[4:])
    load_ir = Signal(modbv(0)[1:])
    inc_pc = Signal(modbv(0)[1:])
    inc_sp = Signal(modbv(0)[1:])
    dec_sp = Signal(modbv(0)[1:])
    load_pc = Signal(modbv(0)[1:])
    load_imm = Signal(modbv(0)[1:])
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
    wr_acc = Signal(modbv(0)[1:])
    wr_reg = Signal(modbv(0)[1:])
    reg_dest  = Signal(modbv(0)[4:])

    SP = 14
    SRP = 15
    # state
    NEXT_INSTR = 0
    DECODE_INSTR = 1
    READ_IMM = 2
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
    OPCJ_JLO      = 7  # jump <   unsigned    c8
    OPCJ_JHS      = 8  # jump >=  unsigned   !c8
    OPCJ_JZ       = 9  # jump on zero         z8
    OPCJ_JNZ      = 10 # jump on not zero    !z8
    OPCJ_JLO      = 11 # jump <   unsigned    c16
    OPCJ_JHS      = 12 # jump >=  unsigned   !c16
    OPCJ_JZ       = 13 # jump on zero         z16
    OPCJ_JNZ      = 14 # jump on not zero    !z16
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
    OPC_LDB_A_OFFS = 2  # Rx = M[A+nn].b
    OPC_LDB_A      = 3  # Rx = M[A].b
    OPC_LDB_RX     = 4  # A = M[Rx].b
    OPC_STB_A_OFFS = 5  # M[A+nn].b = Rx
    OPC_STB_A      = 6  # M[A].b = Rx
    OPC_STB_RX     = 7  # M[Rx].b = A
    OPC_LDW_A_OFFS = 10 # Rx = M[A+nn].w
    OPC_LDW_A      = 11 # Rx = M[A].w
    OPC_LDW_RX     = 12 # A = M[Rx].w
    OPC_STW_A_OFFS = 13 # M[A+nn].w = Rx
    OPC_STW_A      = 14 # M[A].w = Rx
    OPC_STW_RX     = 15 # M[Rx].w = A


    @always(clk.posedge, rstn.negedge)
    def ctrl():
        if rstn == 0 or halt == 1:
            state.next = NEXT_INSTR
            load_ir.next = 1
            load_imm.next = 1
            inc_pc.next = 1
        else:
            if state == NEXT_INSTR:
                print("NEXT_INSTR")
                if op == OPC_MVI or op == OPC_JMP:
                    print("- OPC_MVI/JMP")
                    load_ir.next = 0
                    load_imm.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    state.next = READ_IMM
                else:
                    load_ir.next = 1
                    load_imm.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    state.next = NEXT_INSTR
            elif state == READ_IMM:
                print("READ_IMM")
                if imm_more:
                    print("- imm_more")
                    load_ir.next = 0
                    load_imm.next = 0
                    load_more.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    state.next = READ_IMM
                else:
                    load_ir.next = 1
                    load_imm.next = 1
                    inc_pc.next = 1
                    sel_imem.next = PC
                    state.next = NEXT_INSTR

    @always(clk.negedge)
    def prt_state():
        print("state:",state)

    @always_comb
    def decoder():
        if op == OPC_A_RX:
            alu_oper.next = ALU_PASS_Y
            op_sel_rx.next = 0 # D.C.
            alu_x_imm.next = 0 # D.C.
            wr_acc.next = 0
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
            wr_reg.next = 1
            inc_sp.next = 0
            dec_sp.next = 0
        elif op == OPC_MVIA:
            alu_oper.next = ALU_PASS_X
            op_sel_rx.next = 0
            alu_x_imm.next = 1
            alu_imm_width.next = 0 # 4 bits from first instr byte
            wr_acc.next = 1
            alu_y_pc.next = 0 # D.C.
            reg_wr_alu.next = 1
            wr_reg.next = 0
            inc_sp.next = 0
            dec_sp.next = 0
        elif op == OPC_JMP:
            if state == NEXT_INSTR:
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0
                wr_reg.next = 0
                inc_sp.next = 0
                dec_sp.next = 0
            elif state == READ_IMM:
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 1
                reg_wr_alu.next = 0
                wr_reg.next = 1
                inc_sp.next = 0
                dec_sp.next = 0

                if imm_more:
                    alu_oper.next = ALU_PASS_X
                    load_pc.next = 0
                else:
                    alu_oper.next = ALU_ADD
                    load_pc.next = 1


            #if r_field == OPCJ_J:

        elif op == OPC_MVI: # Rx = sex(nn)
            if state == NEXT_INSTR:
                alu_oper.next = ALU_PASS_X
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
                wr_reg.next = 0
                inc_sp.next = 0
                dec_sp.next = 0
            elif state == READ_IMM:
                alu_oper.next = ALU_PASS_X
                op_sel_rx.next = 0
                alu_x_imm.next = 1
                alu_imm_width.next = 1 # 7 bits from instr byte
                wr_acc.next = 0
                alu_y_pc.next = 0 # D.C.
                reg_wr_alu.next = 1
                wr_reg.next = 1
                inc_sp.next = 0
                dec_sp.next = 0

        elif op == OPC_RX_A:
            alu_oper.next = ALU_PASS_X
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
            wr_reg.next = 0
            inc_sp.next = 0
            dec_sp.next = 0
        elif op == OPC_ADD:
            alu_oper.next = ALU_ADD
            op_sel_rx.next = 1
            alu_x_imm.next = 0
            wr_acc.next = 1
            alu_y_pc.next = 0 # acc
            reg_wr_alu.next = 1
            wr_reg.next = 0
            inc_sp.next = 0
            dec_sp.next = 0
        elif op == OPC_NEXT:
            # the reg field is secondary opcode
            if r_field == OPCI_NOP:
                alu_oper.next = ALU_PASS_Y
                op_sel_rx.next = 1
                alu_x_imm.next = 0
                wr_acc.next = 0
                alu_y_pc.next = 0 # acc
                reg_wr_alu.next = 1
                wr_reg.next = 0
                inc_sp.next = 0
                dec_sp.next = 0


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
            elif op == OPC_NEXT:
                if r_field == OPCI_NOP:
                    print("EXE NOP")
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

            if op_sel_rx:
                print("rx",r_field, reg_bank[r_field])
            print("imm:",imm,"imm_next",imm_next)
            print("imem_dout:",imem_dout)
            print("pc_next",pc_next)

    @always_comb
    def pcctrl():
        if halt:
            pc_next.next = pc
        elif load_pc == 1:
            pc_next.next = alu_out
        elif inc_pc == 1:
            pc_next.next = pc + 1
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
        if load_ir == 1:
            ir.next = imem_dout[8:]

    @always_comb
    def selrx():
        if op_sel_rx:
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
        if load_imm:
            ext = modbv(0)[32:]
            if imem_dout[6]:
                ext[:] = 0xffffffff
            ext[7:] = imem_dout[7:]
            imm_next.next = ext
        elif load_more:
            imm_next.next = imm << 7 | imem_dout[7:]
        imm_more.next = imem_dout[7]

    imm_ff = multiflop( imm_next, imm, clk, rstn )

    @always_comb
    def selalu():
        # alu op X
        if alu_x_imm:
            if alu_imm_width == 0:
                ext = modbv(0)[32:]
                if imm_next[3]:
                    ext[:] = 0xffffffff
                ext[4:] = imm_next[4:]
                alu_op_x.next = ext
            elif alu_imm_width == 1:
                alu_op_x.next = imm_next # TBD sex
        else:
            alu_op_x.next = rx
        # alu op Y (ACC)
        if alu_y_pc:
            alu_op_y.next = pc + 1
        else:
            alu_op_y.next = acc

    @always_comb
    def alu():
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
        elif alu_oper == ALU_AND:
            alu_out.next = alu_op_x & alu_op_y
        elif alu_oper == ALU_OR:
            alu_out.next = alu_op_x | alu_op_y
        elif alu_oper == ALU_NOT:
            alu_out.next = ~alu_op_y

    @always_comb
    def wrback():
        if reg_wr_alu:
            reg_wr_op.next = alu_out
        else:
            reg_wr_op.next = imem_dout

    @always_comb
    def to_acc():
        if wr_acc:
            acc_next.next = reg_wr_op
        else:
            acc_next.next = acc

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
        reg_dest.next = r_field # TBD

    @always(clk.posedge, rstn.negedge)
    def reg_wr():
        if wr_reg:
            reg_bank[ reg_dest ].next = reg_wr_op
        if inc_sp:
            reg_bank[ SP ].next = reg_bank[ SP ] + 4
        elif dec_sp:
            reg_bank[ SP ].next = reg_bank[ SP ] - 4

    @always(clk.negedge)
    def dbgprintrx():
        if wr_reg:
            print("wr R",reg_dest,"=",reg_wr_op)

    @always_comb
    def extr_instr():
        # [ o o o o  r r r r ]
        op.next = ir[8:] >> 4
        r_field.next = ir[4:]

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
                print("mem_loader",addr)

    return instances()

def cpu_top( clk, rstn ):

    imem_radr = Signal(modbv(0)[16:])
    imem_wadr = Signal(modbv(0)[16:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])
    load_done = Signal(modbv(0)[1:])
    halt = Signal(modbv(0)[1:])

    imem_depth = 32

    program = [ 0 for i in range(imem_depth) ]
    program[ 0  ] = 0xff # NOP
    program[ 1  ] = 0xff # NOP
    program[ 2  ] = 0xff # NOP
    program[ 3  ] = 0xff # NOP
    program[ 4  ] = 0xa2 # JMP 0
    program[ 5  ] = 0
    program[ 6  ] = 0x82 # R2 = 33
    program[ 7  ] = 33
    program[ 8  ] = 0x97 # A = 7 
    program[ 9  ] = 0x9f # A = -1
    program[ 10 ] = 0x84 # R4 = -1
    program[ 11 ] = 0b1111111
    program[ 12 ] = 0x85 # R5 = -2
    program[ 13 ] = 0b1111110
    program[ 14 ] = 0x84 # R4 = 294
    program[ 15 ] = (294 >> 7) | 0x80
    program[ 16 ] = (294 & 0x7f)
    program[ 17 ] = 0xff # NOP
    program[ 18 ] = 0xff # NOP
    program[ 19 ] = 0xff # NOP
    program[ 20 ] = 0x03 # R3 = A
    program[ 21 ] = 0x92 # A = 2
    program[ 22 ] = 0x08 # R8 = A
    program[ 23 ] = 0x13 # A = R3
    program[ 24 ] = 0xb8 # A = A + R8
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
    

    icpu = cpu(
        clk = clk,
        rstn = rstn,
        imem_dout = imem_dout,
        imem_adr = imem_radr,
        imem_rd = imem_rd,
        halt = halt
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

def main():
    if run_sim:
        #itb = tb()
        traceSignals.filename = 'trace'
        itb = traceSignals( tb ) 
        sim = Simulation( itb )
        sim.run( 1000 )
    else:
        clk = Signal(bool())
        rstn = Signal(intbv(0)[1:0])
        toVerilog.standard = 'systemverilog'
        itop = toVerilog(cpu_top, clk, rstn )
    
run_sim = False

if __name__ == '__main__':
    import sys
    if 'sim' in sys.argv:
        run_sim = True
    main()
