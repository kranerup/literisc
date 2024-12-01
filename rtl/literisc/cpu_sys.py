from myhdl import *

from modules.common.memory import memory
from modules.common.signal import signal
from modules.common.Common import copySignal

from cpu import cpu
from tb import load_a_rx, load_rx, jump_relative
from dp_mem import dp_mem
from cpu_common import flop
import re

def prog_to_tuples( program ):
    lowest = min( program.keys() )
    assert lowest == 0, f"program must start at 0, not {lowest}"
    tup = ()
    curr = lowest
    while curr in program:
        tup += (program[curr],)
        curr += 1
    assert len(tup) == len(program), f"there are holes in the program {len(program)}:{program} t:{len(tup)}:{tup}"
    return tup 

def hexdump_to_prog( dump ):
    prog = []
    for line in dump.splitlines():
        line = re.sub(r' +\|.*','',line)
        adr, hexdata = line.split(': ')
        prog += [ int(x,16) for x in hexdata.split(" ") ]
    return { idx: val for idx,val in enumerate( prog ) }

def rom(
    odata,
    raddr,
    renable,
    clk,
    clk_en,
    sync_rstn,
    depth,
    content,
    input_flops = 0,
    output_flops = 0,
    name = None ):

    n_rdata = copySignal( odata )
    @always_comb
    def read():
        if renable == 1:
            #print("rom raddr:",raddr)
            if raddr >= len(content):
                #print("raddr out of range, max",len(content),"act",raddr)
                n_rdata.next = 0
            else:
                n_rdata.next = content[int(raddr)]
        else:
            n_rdata.next = 0

    icw = flop( n_rdata, odata, clk_en, clk, sync_rstn )

    return instances()

def cpu_sys(
        clk,
        sync_rstn,
        axi
        ):

    # Memory map:
    # 0 - 8191          : IMEM (and accessible from DMEM bus)
    # 8192 - 8192+32768 : DMEM
    # 65536-100 - 65535 : IO
    perip_addr_bits = 16
    perip_data_bits = 32
    cpu_dmem_data_bits = 32
    dmem_depth = 32768 # reduced to fit FPGA BRAM ( 65536)
    imem_depth = 8192

    imem_low = 0 # if changed there needs to be added some offset calc
    imem_high = imem_depth - 1
    dmem_low = imem_depth
    dmem_high = dmem_low + dmem_depth - 1
    IO_LOW = 65536-100
    IO_HIGH = 65535

    cpu_imem_radr = signal(16)
    n_deferred_cpu_imem_radr = signal(16)
    deferred_cpu_imem_radr = signal(16)
    dmem_imem_radr = Signal(modbv(0)[16:])
    imem_radr = Signal(modbv(0)[16:])
    imem_wadr = Signal(modbv(0)[16:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    cpu_imem_rd = Signal(modbv(0)[1:])
    dmem_imem_rd = Signal(modbv(0)[1:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])
    
    dmem_adr = signal(16)
    cpu_dmem_adr = signal(16)
    dmem_din = signal(cpu_dmem_data_bits )
    dmem_dout = signal(cpu_dmem_data_bits)
    dmem_muxed_dout = signal(cpu_dmem_data_bits)
    dmem_rd = signal()
    dmem_wr = signal()
    dmem_wr_sz = signal(2)
    sel_imem_src = signal()
    
    halt = signal()
    intr = signal()

    cpu_clk = signal()
    clk_en = signal()
    rom_clk_en = signal()
  
    icpu = cpu(
            cpu_clk,
            clk_en,
            sync_rstn,
            imem_dout,
            cpu_imem_radr,
            cpu_imem_rd,
            dmem_din,
            dmem_muxed_dout,
            cpu_dmem_adr,
            dmem_rd,
            dmem_wr,
            dmem_wr_sz,
            halt,
            intr,
            False,
            None,
            None,
            None,
            None,
            False)


    # --------------- cpu clock gating ------------
    cpu_waiting = signal()
    n_cpu_waiting = signal()
    wait_type = signal(2)
    n_wait_type = signal(2)

    IO_WAIT = 1
    IMEM_WAIT = 2
     

    @always_comb
    def cg():
        if sync_rstn == 0:
            clk_en.next = 1
            rom_clk_en.next = 1
        else:
            clk_en.next = ~cpu_waiting
            if cpu_waiting == 1:
                if wait_type == IMEM_WAIT:
                    rom_clk_en.next = 1
                else:
                    rom_clk_en.next = 0
            else:
                rom_clk_en.next = 1
        cpu_clk.next = clk

    # ---------------- memory and periphery control  --------------------
    req_rd = signal()
    req_wr = signal()
    req_done = signal()
    req_addr = signal(perip_addr_bits)
    req_wdata = signal(perip_data_bits)
    req_rdata = signal(perip_data_bits)
    req_reading = signal()
    n_req_reading = signal()
    dmem_renable = signal()
    dmem_wenable = signal()
    n_imem_src = signal()
    imem_src = signal()
    n_imem_src2 = signal()
    imem_src2 = signal()
    imem_hold_data = signal(cpu_dmem_data_bits)
    n_imem_hold_data = signal(cpu_dmem_data_bits)

    icw   = flop( n_cpu_waiting, cpu_waiting, clk_en=None, clk=clk, sync_rstn=sync_rstn )
    icwio = flop( n_wait_type,   wait_type,   clk_en=None, clk=clk, sync_rstn=sync_rstn )
    ird   = flop( n_req_reading, req_reading, clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iis   = flop( n_imem_src,    imem_src,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iis2  = flop( n_imem_src2,   imem_src2,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iih   = flop( n_imem_hold_data,    imem_hold_data,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    idf   = flop( n_deferred_cpu_imem_radr, deferred_cpu_imem_radr, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    sel_axi_rd_data = signal()

    @always_comb
    def ihold():
        n_imem_hold_data.next = imem_hold_data
        if imem_src == 1:
            n_imem_hold_data.next = imem_dout
        n_imem_src2.next = imem_src


    @always_comb
    def dmux():
        if sel_axi_rd_data == 1:
            dmem_muxed_dout.next = req_rdata
        elif imem_src2 == 1:
            dmem_muxed_dout.next = imem_hold_data
        else:
            dmem_muxed_dout.next = dmem_dout

    @always_comb
    def imux():
        if sel_imem_src == 1:
            imem_radr.next = cpu_dmem_adr # no address offset 
            imem_rd.next = dmem_imem_rd
        elif imem_src == 1:
            imem_radr.next = deferred_cpu_imem_radr
        else:
            imem_radr.next = cpu_imem_radr
            imem_rd.next = cpu_imem_rd
        n_imem_src.next = sel_imem_src

    @always_comb
    def decode():
        req_rd.next = 0
        req_wr.next = 0
        req_addr.next = 0
        req_wdata.next = 0
        dmem_renable.next = 0
        dmem_wenable.next = 0
        n_cpu_waiting.next = 0
        n_wait_type.next = 0
        n_req_reading.next = 0
        dmem_imem_rd.next = 0
        n_deferred_cpu_imem_radr.next = cpu_imem_radr

        sel_axi_rd_data.next = req_reading
        sel_imem_src.next = 0

        if cpu_waiting == 1:
            if wait_type == IO_WAIT:
                n_req_reading.next = req_reading
                if req_done == 1:
                    n_cpu_waiting.next = 0
                    req_rd.next = 0
                    req_wr.next = 0
                else:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = IO_WAIT
            elif wait_type == IMEM_WAIT:
                n_cpu_waiting.next = 0
            else:
                assert False, "wait type error"
        else:
            n_req_reading.next = 0
            if dmem_rd == 1 or dmem_wr == 1:
                # --- IO access ---------------
                if cpu_dmem_adr >= IO_LOW and cpu_dmem_adr <= IO_HIGH:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = IO_WAIT
                    req_rd.next = dmem_rd
                    req_wr.next = dmem_wr
                    req_addr.next = cpu_dmem_adr - IO_LOW
                    req_wdata.next = dmem_din
                    n_req_reading.next = dmem_rd
                    dmem_renable.next = 0
                    dmem_wenable.next = 0
                # --- dmem access -------------
                elif cpu_dmem_adr >= dmem_low and cpu_dmem_adr <= dmem_high:
                    n_cpu_waiting.next = 0
                    dmem_renable.next = dmem_rd
                    dmem_wenable.next = dmem_wr
                # --- imem access -------------
                elif cpu_dmem_adr >= imem_low and cpu_dmem_adr <= imem_high:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = IMEM_WAIT
                    sel_imem_src.next = 1
                    dmem_imem_rd.next = 1
                    dmem_renable.next = 0
                    dmem_wenable.next = 0

    # ---------------- DMEM -------------------------
    dmem_wmask = signal(4)

    @always_comb
    def mask():
        if dmem_wr_sz == 0:
            dmem_wmask.next = 0b0001
        elif dmem_wr_sz == 1:
            dmem_wmask.next = 0b0011
        else: # sz=2
            dmem_wmask.next = 0b1111

    @always_comb
    def aoffs():
        dmem_adr.next = cpu_dmem_adr - dmem_low

    if False:
        dmem = memory(
            idata = dmem_din,
            odata = dmem_dout,
            raddr = dmem_adr,
            waddr = dmem_adr,
            renable = dmem_renable,
            wenable = dmem_wenable,
            wmask   = dmem_wmask,
            clk = cpu_clk,
            rstn = rstn,
            depth = dmem_depth,
            input_flops = 0,
            output_flops = 0,
            name = 'dmem')
    else:
        dmem = dp_mem(
            idata = dmem_din,
            odata = dmem_dout,
            raddr = dmem_adr,
            waddr = dmem_adr,
            renable = dmem_renable,
            wenable = dmem_wenable,
            wmask   = dmem_wmask,
            clk = cpu_clk,
            clk_en = clk_en,
            depth = dmem_depth,
            name = 'dmem')

    if False:
        # --- output counter to GPIO ------------
        program, pc = load_rx( 0, IO_LOW, pc=0 ) # R0=IO
        # R1 = counter
        program[ pc   ] = 0x91 # A = 1
        program[ pc+1 ] = 0xb1 # A = A + R1
        program[ pc+2 ] = 0x01 # R1 = A
        program[ pc+3 ] = 0xf8 # M[R0].b = A
        program[ pc+4 ] = 0x70
        p, pc = jump_relative( pc+5, 0 )
        program[ pc ]   = 0xff # nop
        program[ pc+1 ] = 0xff # nop
        program.update( p )
    elif False:
        # --- counter to GPIO and to serial port --------------
        program, pc = load_rx( 0, IO_LOW, pc=0 ) # R0=IO
        # R1 = counter
        program[ pc   ] = 0x91 # A = 1 
        program[ pc+1 ] = 0xb1 # A = A + R1
        program[ pc+2 ] = 0x01 # R1 = A
        program[ pc+3 ] = 0xf8 # M[R0].b = A
        program[ pc+4 ] = 0x70
        program[ pc+5 ] = 0x10 # A = R0
        program[ pc+6 ] = 0xf8 # M[ A + 1 ].b = R1
        program[ pc+7 ] = 0x51
        program[ pc+8 ] = 1 # single byte immediate offset
        program[ pc+9 ] = 0xf8 # R2 = M[ A + 2 ].b
        program[ pc+10] = 0x22 # -"-
        program[ pc+11] = 2 # single byte immediate offset
        p, pc = jump_relative( pc+12, 0 )
        program[ pc ]   = 0xff # nop
        program[ pc+1 ] = 0xff # nop
        program.update( p )
    else:
        program = hexdump_to_prog("""\
            00000000: 80 83 FF 1C 91 B1 01 F8 70 10 22 02 91 C2 A6 0C
            00000010: 83 80 40 85 0F 15 D1 B3 04 10 54 01 A0 66 00 00""")
        program = hexdump_to_prog("""\
00000000: 80 83 FF 1C 91 B1 01 F8 70 10 22 02 90 29 00 91
00000010: C2 A6 21 85 0F 15 D1 05 90 C5 A6 0F 91 B1 01 F8
00000020: 70 86 87 7F D6 08 87 00 C7 A6 71 15 83 80 40 B3
00000030: 04 10 54 01 A0 4E 00 00 00 00 00 00 00 00 00 00""")
        program = hexdump_to_prog("""\
00000000: 80 83 FF 1C 91 B1 01 F8 70 86 87 7F D6 08 87 00  |........p.......|
00000010: C7 A6 71 89 2D F8 49 08 90 C8 A5 0F 10 22 02 91  |..q.-.I......"..|
00000020: C2 A6 79 10 58 01 91 B9 09 A0 6A A0 57 48 65 6C  |..y.X.....j.WHel|
00000030: 6C 6F 20 57 6F 72 6C 64 21 00 00 00 00 00 00 00  |lo World!.......|""")






    boot_code = prog_to_tuples( program )

    imem = rom(
        odata        = imem_dout,
        raddr        = imem_radr,
        renable      = imem_rd,
        clk_en       = rom_clk_en,
        clk          = cpu_clk,
        sync_rstn    = sync_rstn,
        depth        = imem_depth,
        input_flops  = 0,
        output_flops = 0,
        content      = boot_code,
        name         = 'imem')


    # ---------------- AXI master -------------------------
    # single beat, non-pipelined master

    M_IDLE = 0
    M_WAIT_RVALID = 1
    M_WAIT_ARREADY = 2
    M_WAIT_AWREADY = 3
    M_WAIT_WREADY = 4

    m_state = signal(3)

    @always(clk.posedge)
    def axi_master():
        axi.arlen.next = 0

        if sync_rstn == 0:
            axi.rready.next = 0
            axi.arvalid.next = 0
            axi.araddr.next = 0
            axi.awaddr.next = 0
            axi.awvalid.next = 0
            axi.wdata.next = 0
            axi.wvalid.next = 0
            req_done.next = 0
            req_rdata.next = 0
            m_state.next = M_IDLE
        else:

            if m_state == M_IDLE:
                req_done.next = 0
                axi.rready.next = 0
                axi.arvalid.next = 0

                if req_rd == 1:
                    print("axi req_rd")
                    axi.arvalid.next = 1
                    axi.araddr.next = req_addr
                    if axi.arready == 1:
                        m_state.next = M_WAIT_RVALID
                    else:
                        m_state.next = M_WAIT_ARREADY
                elif req_wr == 1:
                    print("axi req_wr")
                    axi.awvalid.next = 1
                    axi.awaddr.next = req_addr
                    axi.wvalid.next = 1
                    axi.wdata.next = req_wdata
                    if axi.awready == 1:
                        m_state.next = M_WAIT_WREADY
                    else:
                        m_state.next = M_WAIT_AWREADY

            elif m_state == M_WAIT_ARREADY:
                if axi.arready == 1:
                    if axi.rvalid == 1:
                        axi.rready.next = 1
                        req_rdata.next = axi.rdata
                        print("axi req_done")
                        req_done.next = 1
                        m_state.next = M_IDLE
                    else:
                        m_state.next = M_WAIT_RVALID
                else:
                    m_state.next = M_WAIT_ARREADY
            elif m_state == M_WAIT_RVALID:
                axi.arvalid.next = 0
                if axi.rvalid == 1:
                    axi.rready.next = 1
                    req_rdata.next = axi.rdata
                    print("axi req_done")
                    req_done.next = 1
                    m_state.next = M_IDLE
                else:
                    m_state.next = M_WAIT_RVALID

            elif m_state == M_WAIT_AWREADY:
                if axi.awready == 1:
                    axi.awvalid.next = 0
                    if axi.wready == 1:
                        axi.wvalid.next = 0
                        req_done.next = 1
                        print("axi req_done")
                        m_state.next = M_IDLE
                    else:
                        m_state.next = M_WAIT_WREADY
                else:
                    m_state.next = M_WAIT_AWREADY
            elif m_state == M_WAIT_WREADY:
                if axi.wready == 1:
                    axi.wvalid.next = 0
                    req_done.next = 1
                    print("axi req_done")
                    m_state.next = M_IDLE
                else:
                    m_state.next = M_WAIT_WREADY

    return instances()


