from myhdl import *
from myhdl import Struct, unpack_struct

from modules.common.memory import memory
from modules.common.Common import copySignal, multiflop

from cpu import cpu
from cpu import sym_to_op, op_to_sym, jmp_to_sym, inner_to_sym

class InstrCov(Struct):
    def __init__(self):
        self.valid     = Signal(modbv(0)[1:])
        self.op        = Signal(modbv(0)[4:])
        self.op_jmp    = Signal(modbv(0)[4:])
        self.op_inner  = Signal(modbv(0)[4:])
        self.op_inner2 = Signal(modbv(0)[4:])

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
                            if dmem_din.val == exp_dmem['data']:
                                print("check dmem wr data ok", dmem_din.val)
                            else:
                                print("check dmem wr data MISS act:",
                                      dmem_din.val, "exp:", exp_dmem['data'])

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
            if obs_op.op == sym_to_op['OPC_JMP']:
                InstrCovCollector.covered_jmp.add( int(obs_op.op_jmp))
            if obs_op.op == sym_to_op['OPC_NEXT']:
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

    program[ 4 ] = (sym_to_op['OPC_JMP'] << 4 | sym_to_op['OPCJ_JNZ'])
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

    program[ 4 ] = (sym_to_op['OPC_JMP'] << 4 | sym_to_op['OPCJ_JNZ'])
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

def test_11(program,expect,pc,dmem):
    # ---- test load to A --------
    program[ 0 ] = 0x97 # A = 7 
    program[ 1 ] = 0x03 # R3 = A = 7
    program[ 2 ] = 0x43 # A = M[ R3 ].l
    dmem.append({'rd':1, 'adr':7, 'data':123 })
    expect[5] = { 'A': 123, 3: 7 }
    program[ 3 ] = 0xff # NOP
    program[ 4 ] = 0xff # NOP
    program[ 5 ] = 0x43 # A = M[ R3 ].l
    dmem.append({'rd':1, 'adr':7, 'data':33 })
    program[ 6 ] = 0x01 # R1 = A = 7, use A directly after load
    expect[8] = { 'A': 33, 1: 33, 3: 7 }
    program[ 7 ] = 0xff # NOP
    program[ 8 ] = 0xff # NOP

def test_12(program,expect,pc,dmem):
    # ---- test pop A --------
    program[ 0 ] = 0x8e # SP = 294
    program[ 1 ] = (294 >> 7) | 0x80
    program[ 2 ] = (294 & 0x7f)
    program[ 3 ] = 0xff # NOP
    program[ 4 ] = 0xf7 # POP A
    dmem.append({'rd':1, 'adr':294, 'data':123 })
    expect[6] = { 'A': 123, 14: 298 }
    program[ 5 ] = 0x01 # R1 = A = 123, use A directly after load
    expect[7] = { 'A': 123, 1: 123, 14: 298 }
    program[ 6 ] = 0xff # NOP
    program[ 7 ] = 0xff # NOP
    program[ 8 ] = 0xf7 # POP A
    dmem.append({'rd':1, 'adr':298, 'data':32 })
    expect[10] = { 'A': 32, 14: 302 }
    program[ 9 ] = 0x1e # A = SP
    expect[11] = { 'A': 302, 14: 302 }
    program[ 10 ] = 0xff # NOP
    program[ 11 ] = 0xff # NOP

def test_13(program,expect,pc,dmem):
    # ---- test pop Rx --------
    program[ 0 ] = 0x8e # SP = 294
    program[ 1 ] = (294 >> 7) | 0x80
    program[ 2 ] = (294 & 0x7f)
    program[ 3 ] = 0xf5 # POP R3
    program[ 4 ] = 0x03 # ...
    dmem.append({'rd':1, 'adr':294, 'data':0x11 })
    dmem.append({'rd':1, 'adr':298, 'data':0x22 })
    dmem.append({'rd':1, 'adr':302, 'data':0x33 })
    dmem.append({'rd':1, 'adr':306, 'data':0x44 })
    expect[7] = { 3:0x11, 2:0x22, 1:0x33, 0:0x44, 'A':0x44 }
    program[ 5 ] = 0x10 # A = R0 ; forward mem data to reg operand
    program[ 6 ] = 0xff # NOP
    program[ 7 ] = 0xff # NOP

def test_14(program,expect,pc,dmem):
    # ---- test push Rx --------
    program[ 0 ] = 0x8e # SP = 294
    program[ 1 ] = (294 >> 7) | 0x80
    program[ 2 ] = (294 & 0x7f)
    
    program[ 3  ] = 0x83 # R3 = 0x34
    program[ 4  ] = 0x34
    program[ 5  ] = 0x82 # R2 = 0x33
    program[ 6  ] = 0x33
    program[ 7  ] = 0x81 # R1 = 0x22
    program[ 8  ] = 0x22
    program[ 9  ] = 0x97 # A = 7
    program[ 10 ] = 0x00 # R0 = A # R0 is first to be pushed

    program[ 11 ] = 0xf4 # PUSH R3
    program[ 12 ] = 0x03 # ...
    dmem.append({'rd':0, 'adr':290, 'data':7 })
    dmem.append({'rd':0, 'adr':286, 'data':0x22 })
    dmem.append({'rd':0, 'adr':282, 'data':0x33 })
    dmem.append({'rd':0, 'adr':278, 'data':0x34 })
    expect[14] = { 14: 278 }

    program[ 13 ] = 0xff # NOP
    program[ 14 ] = 0xff # NOP

def test_15(program,expect,pc,dmem):
    # ---- test push srp --------
    program[ 0 ] = 0x8e # SP = 294
    program[ 1 ] = (294 >> 7) | 0x80
    program[ 2 ] = (294 & 0x7f)
    program[ 3  ] = 0x8f # SRP = 0x34
    program[ 4  ] = 0x34
    program[ 5  ] = 0xf6 # PUSH SRP
    dmem.append({'rd':0, 'adr':290, 'data':0x34 })
    expect[7] = { 14: 290 }
    program[ 6  ] = 0xff # NOP
    program[ 7  ] = 0xff # NOP

def test_16(program,expect,pc,dmem):
    # ---- test not, shifts --------
    program[ 0 ] = 0x82 # R2 = 294
    program[ 1 ] = (294 >> 7) | 0x80
    program[ 2 ] = (294 & 0x7f)
    program[ 3  ] = 0x12 # A = R2
    program[ 4  ] = 0xf0 # A = ~A
    v = ( ~294 & 0xffffffff ) # 0xfffffed9
    expect[6] = { 'A': v , 2: 294 }
    program[ 5  ] = 0xff # NOP
    program[ 6  ] = 0xff # NOP
    program[ 7  ] = 0xf1 # A <<= 1
    expect[9] = { 'A': 0xfffffdb2, 2: 294 }
    program[ 8  ] = 0xff # NOP
    program[ 9  ] = 0xff # NOP
    program[ 10 ] = 0xf3 # A >>= 1 (arithmetic)
    expect[12] = { 'A': 0xfffffed9, 2: 294 }
    program[ 11 ] = 0xff # NOP
    program[ 12 ] = 0xff # NOP
    program[ 13 ] = 0xf2 # A >>= 1
    expect[15] = { 'A': 0x7fffff6c, 2: 294 }
    program[ 14 ] = 0xff # NOP
    program[ 15 ] = 0xff # NOP

def test_17(program,expect,pc,dmem):
    # ---- test and/or --------
    program[ 0  ] = 0x9d # A = -3
    program[ 1  ] = 0x83 # R3 = 0x3f
    program[ 2  ] = 0x3f
    program[ 3  ] = 0xd3 # A = A & R3
    expect[5] = { 'A': (-3 & 0xffffffff) & 0x3f, 3: 0x3f }
    program[ 4  ] = 0xe3 # A = A | R3
    expect[6] = { 'A': 0x3f, 3: 0x3f }
    program[ 5  ] = 0xff # NOP
    program[ 6  ] = 0xff # NOP

def tb2():

    clk = Signal(bool())

    progs = []

    tests = [17]
    tests = list(range(1,17+1))

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
        sim.run( 20000 )
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
