from myhdl import *

from modules.common.memory import memory
from modules.common.signal import signal
from modules.common.Common import copySignal, multiflop

from cpu import cpu
from tb import load_a_rx, load_rx, jump_relative

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

def rom(
    odata,
    raddr,
    renable,
    clk,
    rstn,
    depth,
    content,
    input_flops = 0,
    output_flops = 0,
    name = None ):

    n_rdata = copySignal( odata )
    @always_comb
    def read():
        if renable == 1:
            print("raddr:",raddr)
            if raddr >= len(content):
                #print("raddr out of range, max",len(content),"act",raddr)
                n_rdata.next = 0
            else:
                n_rdata.next = content[int(raddr)]
        else:
            n_rdata.next = 0

    icw = multiflop( n_rdata, odata, clk, rstn )

    return instances()

def cpu_sys(
        clk,
        rstn,
        axi
        ):

    perip_addr_bits = 16
    perip_data_bits = 32
    cpu_dmem_data_bits = 32
    dmem_depth = 65536
    imem_depth = 65536
    IO_LOW = 65536-100
    IO_HIGH = 65535

    imem_radr = Signal(modbv(0)[16:])
    imem_wadr = Signal(modbv(0)[16:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])
    
    dmem_adr = Signal(modbv(0)[16:])
    dmem_din = signal(cpu_dmem_data_bits )
    dmem_dout = signal(cpu_dmem_data_bits)
    dmem_rd = signal()
    dmem_wr = signal()
    dmem_wr_sz = signal(2)
    
    halt = signal()
    intr = signal()

    cpu_clk = signal()
  
    icpu = cpu(
            cpu_clk,
            rstn,
            imem_dout,
            imem_radr,
            imem_rd,
            dmem_din,
            dmem_dout,
            dmem_adr,
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
    # negedge clock enable
    clk_en = signal()
    cpu_waiting = signal()
    n_cpu_waiting = signal()
     

    @always(clk.negedge, rstn.negedge)
    def cpu_hold():
        if rstn == 0:
            clk_en.next = 1
        else:
            clk_en.next = ~cpu_waiting

    # glitch free clock gate
    @always_comb
    def clk_gate():
        cpu_clk.next = clk_en & clk

    # ---------------- memory and periphery control  --------------------
    req_rd = signal()
    req_wr = signal()
    req_done = signal()
    req_addr = signal(perip_addr_bits)
    req_wdata = signal(perip_data_bits)
    req_rdata = signal(perip_data_bits)
    dmem_renable = signal()
    dmem_wenable = signal()

    icw = multiflop( n_cpu_waiting, cpu_waiting, clk, rstn )

    @always_comb
    def decode():
        req_rd.next = 0
        req_wr.next = 0
        req_addr.next = 0
        req_wdata.next = 0
        dmem_renable.next = 0
        dmem_wenable.next = 0

        if cpu_waiting == 1:
            if req_done == 1:
                n_cpu_waiting.next = 0
                req_rd.next = 0
                req_wr.next = 0
        else:
            if dmem_rd == 1 or dmem_wr == 1:
                if dmem_adr >= IO_LOW and dmem_adr <= IO_HIGH:
                    n_cpu_waiting.next = 1
                    req_rd.next = dmem_rd
                    req_wr.next = dmem_wr
                    req_addr.next = dmem_adr - IO_LOW
                    req_wdata.next = dmem_din
                else:
                    n_cpu_waiting.next = 0
                    dmem_renable.next = dmem_rd
                    dmem_wenable.next = dmem_wr

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

    boot_code = prog_to_tuples( program )

    imem = rom(
        odata        = imem_dout,
        raddr        = imem_radr,
        renable      = imem_rd,
        clk          = cpu_clk,
        rstn         = rstn,
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

    @always(clk.posedge,rstn.negedge)
    def axi_master():
        axi.arlen.next = 0

        if rstn == 0:
            axi.rready.next = 0
            axi.arvalid.next = 0
            axi.araddr.next = 0
            m_state.next = M_IDLE
        else:

            if m_state == M_IDLE:
                req_done.next = 0
                axi.rready.next = 0
                axi.arvalid.next = 0

                if req_rd == 1:
                    axi.arvalid.next = 1
                    axi.araddr.next = req_addr
                    if axi.arready == 1:
                        m_state.next = M_WAIT_RVALID
                    else:
                        m_state.next = M_WAIT_ARREADY
                elif req_wr == 1:
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
                    axi.rready.next = 1
                    m_state.next = M_WAIT_RVALID
                else:
                    m_state.next = M_WAIT_ARREADY
            elif m_state == M_WAIT_RVALID:
                axi.rready.next = 1
                axi.arvalid.next = 0
                if axi.rvalid == 1:
                    m_state.next = M_WAIT_RVALID
                    req_rdata.next = axi.rdata
                    req_done.next = 1
                    m_state.next = M_IDLE
                else:
                    m_state.next = M_WAIT_RVALID

            elif m_state == M_WAIT_AWREADY:
                if axi.awready == 1:
                    axi.awvalid.next = 0
                    m_state.next = M_WAIT_WREADY
                else:
                    m_state.next = M_WAIT_AWREADY
            elif m_state == M_WAIT_WREADY:
                if axi.wready == 1:
                    axi.wvalid.next = 0
                    req_done.next = 1
                    m_state.next = M_IDLE
                else:
                    m_state.next = M_WAIT_WREADY

    return instances()


