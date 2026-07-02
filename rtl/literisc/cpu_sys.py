from os import sync

from myhdl import *

from modules.common.memory import memory
from modules.common.signal import signal
from modules.common.Common import copySignal

from constants import *

from cpu import cpu
from tb import load_a_rx, load_rx, jump_relative
from dp_mem import dp_mem
from cpu_common import flop
import re

#TODO: Fix the problem of cpu making a master request and then being stopped by a slave_request, causing it to miss the reply. Fix by accepting slave_request when cpu is in an acceptable state.

#TODO: Fix cpu doing write or read on the same cycle that a request_we or request_re is issued. This leads to next cycle a read/write from cpu and read/write from slave is possible. dmem_port_mux makes the slave read/write take priority, making it so the dmem_wenable or dmem_renable gets ignored. Could maybe be fixed by having a mux that sets cpu_waiting to 1 in @always_comb when conf.slave_request_we or conf.slave_request_re is true.

#__verilog__("
#initial begin
#    $readmemh("program.hex", rom_mem);
#end
#")

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
        if len(line) > 0:
            line = re.sub(r' +\|.*','',line)
            adr, hexdata = line.split(': ')
            prog += [ int(x,16) for x in hexdata.split(" ") ]
    return { idx: val for idx,val in enumerate( prog ) }

def rom(
    odata,
    idata,
    raddr,
    renable,
    waddr,
    wenable,
    clk,
    clk_en,
    sync_rstn,
    depth,
    content,
    input_flops = 0,
    output_flops = 0,
    name = None ):

    n_rom_data = copySignal( odata )
    rom_data = copySignal( odata )
    ram_data = copySignal( odata )
    r_ram_addr = copySignal( raddr )
    w_ram_addr = copySignal( raddr )
    rom_addr = copySignal( raddr )
    n_sel_rom = signal()
    sel_rom = signal()
    n_sel_ram = signal()
    sel_ram = signal()
    rd_ram = signal()

    def load_rom():
        pass

    adr_bits = (depth-1).bit_length()

    @always_comb
    def select():
        n_sel_rom.next = 1
        n_sel_ram.next = 0
        rd_ram.next = 0
        if raddr >= len(content):
            n_sel_rom.next = 0
            n_sel_ram.next = 1
            rd_ram.next = 1
            rom_addr.next = 0
        else:
            rom_addr.next = raddr
        r_ram_addr.next = ( raddr - len(content) ) & (2**adr_bits - 1)
        w_ram_addr.next = ( waddr - len(content) ) & (2**adr_bits - 1)

    @always_comb
    def read():
        n_rom_data.next = 0
        if renable == 1 and n_sel_rom == 1:
            n_rom_data.next = content[int(rom_addr)]

    icw  = flop( n_rom_data, rom_data, clk_en, clk, sync_rstn )
    isro = flop( n_sel_rom, sel_rom, clk_en, clk, sync_rstn )
    isra = flop( n_sel_ram, sel_ram, clk_en, clk, sync_rstn )

    pmem = dp_mem(
        idata = idata,
        odata = ram_data,
        raddr = r_ram_addr,
        waddr = w_ram_addr,
        renable = rd_ram,
        wenable = wenable,
        wmask = wenable,
        clk   = clk,
        clk_en = clk_en,
        depth = depth - len(content),
        name = "pmem")

    @always_comb
    def oselect():
        if sel_rom == 1:
            odata.next = rom_data
        elif sel_ram == 1:
            odata.next = ram_data
        else:
            odata.next = 0


    return instances()


#rom.verilog_code = """\
#initial begin
#    $$readmemh(\"program.hex\", rom_mem);
#end
#"""

def cpu_sys(
        clk,
        sync_rstn,
        axi,
        conf,
        instr_trace,
        ):

    cpu_imem_radr = signal(32)
    n_deferred_cpu_imem_radr = signal(32)
    deferred_cpu_imem_radr = signal(32)
    dmem_imem_radr = Signal(modbv(0)[32:])
    imem_radr = Signal(modbv(0)[32:])
    imem_wadr = Signal(modbv(0)[32:])
    imem_din = Signal(modbv(0)[8:])
    imem_dout = Signal(modbv(0)[8:])
    imem_final_dout = Signal(modbv(0)[8:])
    imem_dout_cached = Signal(modbv(0)[8:])
    cpu_imem_rd = Signal(modbv(0)[1:])
    dmem_imem_rd = Signal(modbv(0)[1:])
    imem_rd = Signal(modbv(0)[1:])
    imem_wr = Signal(modbv(0)[1:])
    
    dmem_adr = signal(32)
    cpu_dmem_adr = signal(32)
    dmem_din = signal(CPU_DMEM_DATA_BITS )
    dmem_dout = signal(CPU_DMEM_DATA_BITS)
    dmem_muxed_dout = signal(CPU_DMEM_DATA_BITS)
    dmem_rd = signal()
    dmem_wr = signal()
    dmem_wr_sz = signal(2)
    sel_imem_src = signal()
    
    halt = signal()
    intr = signal()

    cpu_clk = signal()
    clk_en = signal()
    rom_clk_en = signal()
    mem_clk_en = signal()
  
    do_irq = signal()

    cpu_sync_rstn = signal()

    icpu = cpu(
            cpu_clk,
            clk_en,
            cpu_sync_rstn,
            imem_final_dout,
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
            True,
            None,
            None,
            None,
            None,
            instr_trace,
            False)


    # --------------- cpu clock gating ------------
    cpu_waiting = signal()
    prev_cpu_waiting = signal()
    n_cpu_waiting = signal()
    wait_type = signal(4)
    n_wait_type = signal(4)

    icpuwait = flop(cpu_waiting, prev_cpu_waiting, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    IO_WAIT = 1
    IMEM_WAIT = 2
    TICK_WAIT = 3
    CONF_SLAVE_WAIT = 4
    INTERRUPT_WAIT = 5

    @always_comb
    def cg():
        mem_clk_en.next = 1
        if sync_rstn == 0:
            clk_en.next = 1
            rom_clk_en.next = 1
        else:
            clk_en.next = ~cpu_waiting
            if cpu_waiting == 1:
                rom_clk_en.next = 1
                if wait_type == IMEM_WAIT:
                    rom_clk_en.next = 1
                #else:
                #    rom_clk_en.next = 0
            else:
                rom_clk_en.next = 1
        cpu_clk.next = clk

    # ---------------- memory and periphery control  --------------------
    req_rd = signal()
    req_wr = signal()
    req_done = signal()
    req_addr = signal(PERIP_ADDR_BITS)
    req_wdata = signal(PERIP_DATA_BITS)
    req_rdata = signal(PERIP_DATA_BITS)
    req_got_reply = signal()
    req_reading = signal()
    n_req_reading = signal()
    dmem_renable = signal()
    dmem_wenable = signal()
    imem_wenable = signal()
    n_imem_src = signal()
    imem_src = signal()
    n_imem_src2 = signal()
    imem_src2 = signal()
    imem_hold_data = signal(CPU_DMEM_DATA_BITS)
    n_imem_hold_data = signal(CPU_DMEM_DATA_BITS)

    # --- conf slave DMEM port signals ---
    conf_slave_dmem_radr    = signal(32)
    conf_slave_dmem_wadr    = signal(32)
    conf_slave_dmem_din     = signal(CPU_DMEM_DATA_BITS)
    conf_slave_dmem_dout    = signal(CPU_DMEM_DATA_BITS)
    conf_slave_dmem_renable = signal()
    conf_slave_dmem_wenable = signal()
    conf_slave_dmem_wmask   = signal(4)

    # --- conf slave IMEM port signals ---
    conf_slave_imem_wadr    = signal(32)
    conf_slave_imem_din     = signal(CPU_DMEM_DATA_BITS)
    conf_slave_imem_wenable = signal()

    # muxed signals going into dp_mem
    dmem_final_radr    = signal(32)
    dmem_final_wadr    = signal(32)
    dmem_final_din     = signal(CPU_DMEM_DATA_BITS)
    dmem_final_renable = signal()
    dmem_final_wenable = signal()
    dmem_final_wmask   = signal(4)
    dmem_final_dout    = signal(CPU_DMEM_DATA_BITS)

    # muxed signals going into imem write port
    imem_final_wadr    = signal(32)
    imem_final_din     = signal(CPU_DMEM_DATA_BITS)
    imem_final_wenable = signal()

    icw   = flop( n_cpu_waiting, cpu_waiting, clk_en=None, clk=clk, sync_rstn=sync_rstn )
    icwio = flop( n_wait_type,   wait_type,   clk_en=None, clk=clk, sync_rstn=sync_rstn )
    ird   = flop( n_req_reading, req_reading, clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iis   = flop( n_imem_src,    imem_src,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iis2  = flop( n_imem_src2,   imem_src2,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    iih   = flop( n_imem_hold_data,    imem_hold_data,    clk_en=None, clk=clk, sync_rstn=sync_rstn )
    idf   = flop( n_deferred_cpu_imem_radr, deferred_cpu_imem_radr, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    # interrupt-address blocking mechanism
    intr_addr_valid    = signal()
    intr_addr_data     = signal(CPU_DMEM_DATA_BITS)
    n_sel_intr_rd_data = signal()
    sel_intr_rd_data   = signal()

    iisr = flop(n_sel_intr_rd_data, sel_intr_rd_data, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    sel_axi_rd_data = signal()

    n_imem_dout_cached = Signal(modbv(0)[8:])
    imem_dout_cached = Signal(modbv(0)[8:])
    icache = flop(n_imem_dout_cached, imem_dout_cached, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    tick_sel  = signal(3)   # which bit to watch, 1-5 (0 = disabled)
    #tick_wait_valid = signal()
    #n_tick_wait_valid = signal()
    #prev_tick_bit = signal()
    #tick_bit = signal()

    #itw = flop(n_tick_wait_valid, tick_wait_valid, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    # cpu reset
    cpu_rstn = signal()
    n_cpu_rstn = signal()
    icpurstn = flop(n_cpu_rstn, cpu_rstn, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    cur = signal()

    @always_comb
    def cpu_reset_mux():
        cpu_sync_rstn.next = sync_rstn & cpu_rstn

    #@always_comb
    #def tick_edge_detect():
    #    cur.next = conf.ticks[int(tick_sel) - 1] if tick_sel != 0 else 0
    #    n_tick_wait_valid.next = (cur == 1 and prev_tick_bit == 0)  # rising edge
    #    prev_tick_bit.next = cur

    @always_comb
    def imem_cache():
        # capture on rising edge of cpu_waiting
        if prev_cpu_waiting == 0 and cpu_waiting == 1:
            n_imem_dout_cached.next = imem_dout
        else:
            n_imem_dout_cached.next = imem_dout_cached  # hold

    @always_comb
    def imem_dout_mux():
        if prev_cpu_waiting == 1 and cpu_waiting == 0:
            imem_final_dout.next = imem_dout_cached
        else:
            imem_final_dout.next = imem_dout
        #imem_final_dout.next = imem_dout

    @always_comb
    def ihold():
        n_imem_hold_data.next = imem_hold_data
        if imem_src == 1:
            #n_imem_hold_data.next = imem_dout
            n_imem_hold_data.next = imem_final_dout
        n_imem_src2.next = imem_src

    @always_comb
    def dmux():
        if sel_axi_rd_data == 1:
            dmem_muxed_dout.next = req_rdata
        elif sel_intr_rd_data == 1:
            dmem_muxed_dout.next = intr_addr_data
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
            imem_rd.next = 1 # probably needs fixing when implementing imem wr
        else:
            imem_radr.next = cpu_imem_radr
            imem_rd.next = cpu_imem_rd
        n_imem_src.next = sel_imem_src

    @always_comb
    def dmem_port_mux():
        if slave_state != SLAVE_IDLE:
            dmem_final_radr.next    = conf_slave_dmem_radr
            dmem_final_wadr.next    = conf_slave_dmem_wadr
            dmem_final_din.next     = conf_slave_dmem_din
            dmem_final_renable.next = conf_slave_dmem_renable
            dmem_final_wenable.next = conf_slave_dmem_wenable
            dmem_final_wmask.next   = conf_slave_dmem_wmask
        else:
            dmem_final_radr.next    = dmem_adr
            dmem_final_wadr.next    = dmem_adr
            dmem_final_din.next     = dmem_din
            dmem_final_renable.next = dmem_renable
            dmem_final_wenable.next = dmem_wenable
            dmem_final_wmask.next   = dmem_wmask

    @always_comb
    def imem_port_mux():
        if slave_state != SLAVE_IDLE:
            imem_final_wadr.next    = conf_slave_imem_wadr
            imem_final_din.next     = conf_slave_imem_din
            imem_final_wenable.next = conf_slave_imem_wenable
        else:
            imem_final_wadr.next    = dmem_adr
            imem_final_din.next     = dmem_din
            imem_final_wenable.next = imem_wenable

    @always_comb
    def decode():
        req_rd.next = 0
        req_wr.next = 0
        req_addr.next = 0
        req_wdata.next = 0
        dmem_renable.next = 0
        dmem_wenable.next = 0
        imem_wenable.next = 0
        n_cpu_waiting.next = 0
        n_wait_type.next = 0
        n_req_reading.next = 0
        dmem_imem_rd.next = 0
        n_deferred_cpu_imem_radr.next = cpu_imem_radr
        do_irq.next = 0

        sel_axi_rd_data.next = req_reading
        sel_imem_src.next = 0
        n_sel_intr_rd_data.next = 0

        #if slave_state != SLAVE_IDLE or conf.slave_request_we or conf.slave_request_re:
        if slave_state != SLAVE_IDLE:
            n_cpu_waiting.next = 1
        else:
            n_cpu_waiting.next = 0
            #n_wait_type.next = wait_type
            #n_req_reading.next = req_reading
            #n_sel_ticks_rd_data.next = sel_ticks_rd_data
            #n_sel_intr_rd_data.next = sel_intr_rd_data

        #if conf.slave_request_we == 1 or conf.slave_request_re == 1 or slave_state != SLAVE_IDLE:
        #    n_cpu_waiting.next = 1

        #if slave_state != SLAVE_IDLE:
        #if conf.slave_request_we == 1 or conf.slave_request_re == 1:
        #    n_cpu_waiting.next = 1

        #if slave_state_prev != SLAVE_IDLE:
        #    #print("stalling cpu: cpu_waiting=", cpu_waiting, "wait_type=", wait_type)
        #    n_cpu_waiting.next = cpu_waiting
        #    n_wait_type.next = wait_type
        #    n_req_reading.next = req_reading
        #    n_sel_ticks_rd_data.next = sel_ticks_rd_data
        #    n_sel_intr_rd_data.next = sel_intr_rd_data
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
            elif wait_type == TICK_WAIT:
                if conf.ticks[int(tick_sel) - 1] == 1:
                    n_cpu_waiting.next = 0
                else:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = TICK_WAIT

            elif wait_type == INTERRUPT_WAIT:
                if intr_addr_valid == 1:
                    n_cpu_waiting.next = 0
                    n_sel_intr_rd_data.next = 1
                else:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = INTERRUPT_WAIT
            else:
                n_cpu_waiting.next = 0
                #assert False, "wait type error"
        else:
            n_req_reading.next = 0
            if dmem_rd == 1 or dmem_wr == 1:
                # --- IO access ---------------
                #if cpu_dmem_adr >= CONF_LOW and cpu_dmem_adr <= CONF_HIGH:
                if cpu_dmem_adr >= CONF_LOW:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = IO_WAIT
                    req_rd.next = dmem_rd
                    req_wr.next = dmem_wr
                    req_addr.next = cpu_dmem_adr - CONF_LOW
                    req_wdata.next = dmem_din
                    n_req_reading.next = dmem_rd
                    dmem_renable.next = 0
                    dmem_wenable.next = 0
                elif cpu_dmem_adr == IRQ_ADDRESS and dmem_wr == 1:
                    #n_cpu_waiting.next = 1
                    do_irq.next = 1
                    #n_req_reading.next = dmem_wr
                    #n_wait_type.next = SIGNAL_WAIT
                    dmem_renable.next = 0
                    dmem_wenable.next = 0
                elif cpu_dmem_adr == TICK_ADDRESS and dmem_wr == 1:
                    tick_sel.next = dmem_din[3:]

                elif cpu_dmem_adr == TICK_ADDRESS and dmem_rd == 1:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = TICK_WAIT
                elif cpu_dmem_adr == INTERRUPT_ADDRESS and dmem_rd == 1:
                    n_cpu_waiting.next = 1
                    n_wait_type.next = INTERRUPT_WAIT
                    dmem_renable.next = 0
                    dmem_wenable.next = 0
                # --- dmem access -------------
                elif cpu_dmem_adr >= DMEM_LOW and cpu_dmem_adr <= DMEM_HIGH:
                    n_cpu_waiting.next = 0
                    dmem_renable.next = dmem_rd
                    dmem_wenable.next = dmem_wr
                # --- imem access -------------
                elif cpu_dmem_adr >= IMEM_LOW and cpu_dmem_adr <= IMEM_HIGH:
                    if dmem_wr == 0: # writes do not need delay
                        n_cpu_waiting.next = 1
                        n_wait_type.next = IMEM_WAIT
                        sel_imem_src.next = 1
                        dmem_imem_rd.next = 1
                        dmem_renable.next = 0
                        dmem_wenable.next = 0
                    else:
                        dmem_imem_rd.next = 0
                        imem_wenable.next = 1

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
        dmem_adr.next = cpu_dmem_adr - DMEM_LOW

    if False:
        dmem = memory(
            idata = dmem_final_din,
            odata = dmem_dout,
            raddr = dmem_final_radr,
            waddr = dmem_final_wadr,
            renable = dmem_final_renable,
            wenable = dmem_final_wenable,
            wmask   = dmem_final_wmask,
            clk = cpu_clk,
            rstn = rstn,
            depth = DMEM_DEPTH,
            input_flops = 0,
            output_flops = 0,
            name = 'dmem')
    else:
        dmem = dp_mem(
            idata = dmem_final_din,
            odata = dmem_dout,
            raddr = dmem_final_radr,
            waddr = dmem_final_wadr,
            renable = dmem_final_renable,
            wenable = dmem_final_wenable,
            wmask   = dmem_final_wmask,
            clk = cpu_clk,
            clk_en = mem_clk_en,
            depth = DMEM_DEPTH,
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

        # print hello world in a loop
        program = hexdump_to_prog("""\
00000000: 80 83 FF 1C 91 B1 01 F8 70 86 87 7F D6 08 87 00  |........p.......|
00000010: C7 A6 71 89 2D F8 49 08 90 C8 A5 0F 10 22 02 91  |..q.-.I......"..|
00000020: C2 A6 79 10 58 01 91 B9 09 A0 6A A0 57 48 65 6C  |..y.X.....j.WHel|
00000030: 6C 6F 20 57 6F 72 6C 64 21 00 00 00 00 00 00 00  |lo World!.......|""")

        # print hello world then copy serial rx to tx
        program = hexdump_to_prog("""\
00000000: 80 83 FF 1C 89 32 F8 49 08 90 C8 A5 0F 10 22 02  |.....2.I......".|
00000010: 91 C2 A6 79 10 58 01 91 B9 09 A0 6A 10 22 04 91  |...y.X.....j."..|
00000020: C2 A6 79 10 23 03 10 24 02 91 C4 A6 79 10 53 01  |..y.#..$....y.S.|
00000030: A0 6A 48 65 6C 6C 6F 20 57 6F 72 6C 64 21 00 00  |.jHello World!..|""")

        # test write to imem
        program = hexdump_to_prog("""\
00000000: 81 BF 7F 82 00 83 81 80 00 F8 43 F8 41 F8 42 F8  |..........C.A.B.|
00000010: 71 A0 76 48 65 6C 6C 6F 20 57 6F 72 6C 64 21 00  |q.vHello World!.|""")

        # copy rom to imem and dmem and compare after writing
        program = hexdump_to_prog("""\
00000000: 80 1F 81 83 74 82 80 C0 00 F8 40 03 F8 71 F8 72  |....t.....@..q.r|
00000010: 91 B0 00 91 B1 01 91 B2 02 90 C3 A6 6C A0 7E 48  |............l.~H|
00000020: 65 6C 6C 6F 20 57 6F 72 6C 64 21 00 00 00 00 00  |ello World!.....|""")


        program = hexdump_to_prog("""\
00000000: 80 80 48 81 83 74 82 80 C0 00 F8 40 03 F8 71 F8  |..H..t.....@..q.|
00000010: 72 91 B0 00 91 B1 01 91 B2 02 90 C3 A6 6C 80 80  |r............l..|
00000020: 48 81 83 74 82 80 C0 00 F8 40 03 F8 41 C3 A5 02  |H..t.....@..A...|
00000030: 11 04 F8 42 C3 A5 02 12 05 91 B0 00 91 B1 01 91  |...B............|
00000040: B2 02 90 C3 A6 62 A0 7E 48 65 6C 6C 6F 20 57 6F  |.....b.~Hello Wo|
00000050: 72 6C 64 21 00 00 00 00 00 00 00 00 00 00 00 00  |rld!............|""")

        # xmodem boot with timeout
        program = hexdump_to_prog("""\
00000000: 8F 81 BF 7C 80 01 81 83 74 82 83 74 87 01 8A 00  |...|....t..t....|
00000010: AF 81 7C AF 82 1E 8A 15 AF 81 5E 91 0B AF 81 27  |..|.......^....'|
00000020: 91 CB A5 6A 83 04 1A C3 A5 81 0E 83 01 1A C3 A6  |...j............|
00000030: 6A 8A 01 AF 81 59 90 0B AF 81 0C 1A 06 90 0B AF  |j....Y..........|
00000040: 81 05 1A 08 16 B8 FA 89 81 7F C9 A5 07 8A 15 AF  |................|
00000050: 81 27 A0 47 83 00 84 00 8A 01 AF 81 32 90 0B AF  |.'.G........2...|
00000060: 80 65 1A F8 71 91 B1 01 13 BA 03 8A 02 AF 81 1F  |.e..q...........|
00000070: 91 B4 04 85 81 00 C5 A6 5F 90 0B AF 80 49 13 FA  |........_....I..|
00000080: CA A5 0A 12 01 8A 15 AF 80 6F A0 FF 0E 10 C6 A5  |.........o......|
00000090: 0E 17 C6 A5 1A 12 01 8A 15 AF 80 5D A0 FE 7C 10  |...........]..|.|
000000A0: 07 91 B0 FA 00 11 02 8A 06 AF 80 4D A0 FE 6C 12  |...........M..l.|
000000B0: 01 8A 06 AF 80 43 A0 FE 62 8A 03 AF 80 51 8A 06  |.....C..b....Q..|
000000C0: AF 37 80 83 74 10 FE F4 02 80 83 FF 1C 91 CB A5  |.7..t...........|
000000D0: 06 82 A0 80 00 A0 05 82 82 80 80 00 81 01 12 C1  |................|
000000E0: A5 13 02 10 F8 21 04 91 C1 A6 71 10 F8 2A 03 90  |.....!....q..*..|
000000F0: 0B F5 02 1E FE 91 0B A0 78 F4 01 80 83 FF 1C 10  |........x.......|
00000100: F8 21 02 91 C1 A6 78 10 F8 5A 01 F5 01 1E FE F4  |.!....x..Z......|
00000110: 01 80 83 FF 1C 81 00 10 B1 00 1A F1 F1 F1 F1 F1  |................|
00000120: F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 F1 70  |...............p|
00000130: F5 01 1E FE F6 90 0B AF FF 0D 91 CB A6 76 F7 FE  |.............v..|""")
        
        # conf read coreversion loop
#        program = hexdump_to_prog("""\
#00000000: 80 83 FF 1C 40 A0 79 00 00 00 00 00 00 00 00 00  |....@.y.........|""")
        # conf read CoreVersion register, write to Scratch register
#        program = hexdump_to_prog("""\
#00000000: 80 83 FF 1C 40 80 99 0E 10 80 83 FF 28 70 A0 70  |....@.......(p.p|""")
#        program = hexdump_to_prog("""\
#00000000: 80 83 FF 1C 40 A0 79 00 00 00 00 00 00 00 00 00  |....@.y.........|""")
#        program = hexdump_to_prog("""\
#00000000: 80 99 0E 10 80 83 FF 28 70 A0 75 00 00 00 00 00  |.......(p.u.....|""")

#        program = hexdump_to_prog("""\
#00000000: 80 99 0E 10 80 83 FF 28 70 A0 75 00 00 00 00 00  |.......(p.u.....|""")
#        program = hexdump_to_prog("""\
#00000000: 80 88 80 00 F8 40 A0 78 00 00 00 00 00 00 00 00  |.....@.x........|""")
#        program = hexdump_to_prog("""\
#00000000: 80 88 80 01 F8 40 A0 78 00 00 00 00 00 00 00 00  |.....@.x........|""")
#        program = hexdump_to_prog("""\
#00000000: 80 00 F8 40 A0 7A 00 00 00 00 00 00 00 00 00 00  |...@.z..........|""")

#        # only jumps to the same label in loop
#        program = hexdump_to_prog("""\
#00000000: A0 7E 00 00 00 00 00 00 00 00 00 00 00 00 00 00  |.~..............|""")

        # boot-read-interrupt.lisp, reads INTERRUPT_ADDRESS and then jumps to PC=512
        program = hexdump_to_prog("""\
00000000: 80 83 FF 1A F8 40 80 84 00 10 FE 00 00 00 00 00  |.....@..........|""")

#        program = hexdump_to_prog("""\
#00000000: 8F 82 80 00 AF 82 07 A0 7E F4 02 82 84 80 00 1A  |........~.......|
#00000010: B2 32 12 0A F5 02 1E FE F4 03 1B 02 83 84 80 00  |.2..............|
#00000020: 1A B3 62 F5 03 1E FE F6 F4 09 1F 80 70 B0 0F 1F  |..b.........p...|
#00000030: 5A 0C 86 80 E4 57 16 0A AF 4F 1A 00 81 0F 11 D0  |Z....W...O......|
#00000040: 00 1F 50 08 94 B6 0A AF 40 1A 00 81 8F FF FF FF  |..P.....@.......|
#00000050: 70 11 D0 00 1F 50 04 87 00 80 04 17 C0 A2 81 16  |p....P..........|
#00000060: 80 03 17 C0 A5 03 90 A0 01 91 80 00 C0 A5 06 1F  |................|
#00000070: 20 04 10 A0 01 90 00 1F 50 00 88 00 80 04 18 C0  | .......P.......|
#00000080: A2 80 55 1F 20 0C 17 F1 F1 01 18 B1 B0 F8 30 10  |..U. .........0.|
#00000090: 09 1F 20 08 19 01 83 04 18 F1 F1 F1 B3 02 11 F8  |.. .............|
#000000A0: 02 E0 00 1F 50 08 80 1C 18 F1 F1 F1 01 10 02 11  |....P...........|
#000000B0: 00 12 01 10 C1 A2 17 1F 20 00 19 01 83 1C 18 F1  |........ .......|
#000000C0: F1 F1 04 13 C4 02 11 F8 12 E0 00 1F 50 00 18 00  |............P...|
#000000D0: 81 01 B1 08 10 A0 FF 24 17 B6 00 1F 22 08 12 01  |.......$...."...|
#000000E0: 10 0A 11 0B AF FE 31 1F 20 00 50 08 17 00 81 01  |......1. .P.....|
#000000F0: B1 07 10 A0 FE 63 94 B6 00 1F 22 08 12 01 10 0A  |.....c....".....|
#00000100: 11 0B AF FE 13 1F 80 10 B0 0F F5 09 F7 FE F6 F4  |................|
#00000110: 09 1F 80 58 B0 0F 86 00 80 10 16 C0 A2 14 16 00  |...X............|
#00000120: 1F 81 18 B1 01 16 B1 F8 60 16 00 81 01 B1 06 10  |........`.......|
#00000130: A0 66 1F 81 18 B1 0A AF FD 6D 88 80 E4 57 18 0A  |.f.......m...W..|
#00000140: AF FD 46 1A 00 1F 50 10 89 00 80 04 19 C0 A2 80  |..F...P.........|
#00000150: 6C 91 B8 01 19 B1 0A AF FD 2F 1A 00 1F 50 0C 20  |l......../...P. |
#00000160: 10 10 81 04 F8 11 00 1F 21 0C 11 82 1C F8 02 E0  |........!.......|
#00000170: 00 1F 50 08 20 0C 50 10 80 00 50 14 1F 20 14 10  |..P. .P...P.. ..|
#00000180: 81 04 C1 A2 2E 1F 20 08 22 14 12 F1 F1 F1 01 10  |...... .".......|
#00000190: F8 11 FA 00 1F 81 18 B1 01 19 F1 F1 02 1F 23 14  |..............#.|
#000001A0: 13 B2 B1 F8 60 1F 20 14 10 81 01 B1 01 1F 51 14  |....`. .......Q.|
#000001B0: 10 A0 49 19 00 81 01 B1 09 10 A0 FF 0D 8A 00 1F  |..I.............|
#000001C0: 80 28 B0 0F F5 09 F7 FE 00 00 00 00 00 00 00 00  |.(..............|""")

#        program = hexdump_to_prog("""\
#00000010: F4 02 82 13 12 02 1A B2 32 12 0A F5 02 1E FE F6  |........2.......|
#00000020: F4 03 1F 80 7C B0 0F 1F 5A 00 90 02 12 02 90 03  |....|...Z.......|
#00000030: 12 0A 13 0B AF 5A 1A 02 83 8F FF FF FF 7F 13 D2  |.....Z..........|
#00000040: 02 1F 23 00 13 62 1F 80 04 B0 0F F5 03 F7 FE F6  |..#..b..........|
#00000050: F4 02 1F 80 7C B0 0F 1F 82 00 B2 02 12 0A AF FF  |....|...........|
#00000060: 3E 1A 90 0A 1F 80 04 B0 0F F5 02 F7 FE 00 00 00  |>...............|""")
#


#        program = hexdump_to_prog("""
#00000000: 8F 82 80 00 AF 02 A0 7E F4 06 1F 80 48 B0 0F 82  |.......~....H...|
#00000010: 05 83 84 80 00 12 F1 F1 B3 33 13 91 B2 04 1F 54  |.........3.....T|
#00000020: 2C 84 84 80 00 1F 25 2C 15 F1 F1 B4 34 1F 54 28  |,.....%,....4.T(|
#00000030: 1F 24 28 14 90 D3 05 86 84 54 16 E5 03 90 D4 05  |.$(......T......|
#00000040: 90 E5 04 85 84 80 00 12 F1 F1 B5 63 1F 23 28 13  |...........c.#(.|
#00000050: 91 B2 02 1F 52 24 14 02 83 84 80 00 1F 24 24 14  |....R$.......$$.|
#00000060: F1 F1 B3 62 1F 22 24 1F 82 30 B2 02 1F 52 20 82  |...b."$..0...R .|
#00000070: 05 1F 52 1C 82 84 80 00 1F 23 1C 13 F1 F1 B2 32  |..R......#.....2|
#00000080: 1F 52 18 22 1C 12 91 B2 02 1F 52 10 82 84 80 00  |.R."......R.....|
#00000090: 1F 23 10 13 F1 F1 B2 32 1F 52 0C 1F 22 0C 52 14  |.#.....2.R..".R.|
#000000A0: 22 18 83 8F FF FF FF 7F 13 D2 02 83 88 80 80 80  |"...............|
#000000B0: 00 D3 83 00 C3 A5 04 83 7F A0 02 83 00 1F 23 14  |..............#.|
#000000C0: 84 8F FF FF FF 7F 14 D3 03 84 88 80 80 80 00 D4  |................|
#000000D0: 84 00 C4 A5 04 84 7F A0 02 84 00 13 90 E2 02 83  |................|
#000000E0: 88 80 80 80 00 D3 83 00 C3 A5 04 83 7F A0 02 83  |................|
#000000F0: 00 1F 24 20 14 62 53 04 1F 22 20 8A 00 1F 80 38  |..$ .bS.." ....8|
#00000100: B0 0F F5 06 1E FE 00 00 00 00 00 00 00 00 00 00  |................|""")

        # writes 596 to scratch, reads from scratch
#        program = hexdump_to_prog("""
#00000000: 8F 82 80 00 AF 02 A0 7E F4 06 1F 80 48 B0 0F 82  |.......~....H...|
#00000010: 05 83 84 80 00 12 F1 F1 B3 33 13 91 B2 04 1F 54  |.........3.....T|
#00000020: 2C 84 84 80 00 1F 25 2C 15 F1 F1 B4 34 1F 54 28  |,.....%,....4.T(|
#00000030: 1F 24 28 14 90 D3 05 86 84 54 16 E5 03 90 D4 05  |.$(......T......|
#00000040: 90 E5 04 85 84 80 00 12 F1 F1 B5 63 1F 23 28 13  |...........c.#(.|
#00000050: 91 B2 02 1F 52 24 14 02 83 84 80 00 1F 24 24 14  |....R$.......$$.|
#00000060: F1 F1 B3 62 1F 22 24 1F 82 30 B2 02 1F 52 20 82  |...b."$..0...R .|
#00000070: 05 1F 52 1C 82 84 80 00 1F 23 1C 13 F1 F1 B2 32  |..R......#.....2|
#00000080: 1F 52 18 22 1C 12 91 B2 02 1F 52 10 82 84 80 00  |.R."......R.....|
#00000090: 1F 23 10 13 F1 F1 B2 32 1F 52 0C 1F 22 0C 52 14  |.#.....2.R..".R.|
#000000A0: 22 18 83 8F FF FF FF 7F 13 D2 02 83 00 1F 23 14  |".............#.|
#000000B0: 84 8F FF FF FF 7F 14 D3 03 84 00 90 E2 02 83 88  |................|
#000000C0: 80 80 80 00 D3 83 00 C3 A5 04 83 7F A0 02 83 00  |................|
#000000D0: 1F 24 20 14 62 53 04 1F 22 20 8A 00 1F 80 38 B0  |.$ .bS.." ....8.|
#000000E0: 0F F5 06 1E FE 00 00 00 00 00 00 00 00 00 00 00  |................|""")

#        # reads core version
#        program = hexdump_to_prog("""
#00000000: 8F 82 80 00 AF 02 A0 7E F4 04 1F 80 70 B0 0F 1F  |.......~....p...|
#00000010: 82 0C B2 02 83 00 84 84 80 00 13 F1 F1 B4 33 84  |..............3.|
#00000020: 8F FF FF FF 7F 14 D3 03 12 63 8A 00 1F 80 10 B0  |.........c......|
#00000030: 0F F5 04 1E FE 00 00 00 00 00 00 00 00 00 00 00  |................|""")


    
#        program = hexdump_to_prog("""\
#00000000: 8F 82 80 00 AF 8D 7A A0 7E 00 00 00 00 00 02 00  |......z.~.......|
#00000010: F4 02 82 88 80 00 1A B2 32 12 0A F5 02 1E FE F4  |........2.......|
#00000020: 03 1B 02 83 88 80 00 1A B3 62 F5 03 1E FE F6 F4  |.........b......|
#00000030: 09 1F 80 64 B0 0F 1F 5A 14 5B 18 86 80 E4 57 20  |...d...Z.[....W |
#00000040: 14 10 F1 F1 F1 B6 06 00 81 00 0A 11 0B AF 41 1A  |..............A.|
#00000050: 07 91 B6 00 81 01 0A 11 0B AF FF 34 1A 08 92 B6  |...........4....|
#00000060: 00 81 01 0A 11 0B AF FF 27 1A 09 93 B6 00 81 01  |........'.......|
#00000070: 0A 11 0B AF FF 1A 1A 00 1F 50 10 94 B6 00 81 01  |.........P......|
#00000080: 0A 11 0B AF FF 0A 1A 00 1F 50 0C 80 00 50 08 80  |.........P...P..|
#00000090: 00 50 04 1F 20 04 81 10 10 C1 A1 03 90 A0 01 91  |.P.. ...........|
#000000A0: 80 00 C0 A5 19 80 00 1F 21 18 22 04 12 B1 F8 60  |........!."....`|
#000000B0: 1F 20 04 10 81 01 B1 01 1F 51 04 10 A0 55 17 00  |. .......Q...U..|
#000000C0: 82 04 01 12 83 00 C3 A5 09 12 83 7F B3 02 11 F2  |................|
#000000D0: A0 70 11 00 1F 50 00 80 00 50 04 1F 20 04 81 1C  |.p...P...P.. ...|
#000000E0: 10 C1 A1 03 90 A0 01 91 80 00 C0 A5 16 1F 20 08  |.............. .|
#000000F0: 81 81 00 10 C1 A1 03 90 A0 01 91 80 00 C0 A5 03  |................|
#00000100: 91 A0 01 90 80 00 C0 A5 80 6A 1F 20 18 21 08 11  |.........j. .!..|
#00000110: F2 F2 F2 B0 F8 30 1F 21 00 22 04 12 03 11 02 13  |.....0.!."......|
#00000120: 84 00 C4 A5 09 13 84 7F B4 03 12 F2 A0 70 12 01  |.............p..|
#00000130: 91 D1 FA 01 1F 22 08 97 D2 03 11 02 13 84 00 C4  |....."..........|
#00000140: A5 09 13 84 7F B4 03 12 F1 A0 70 12 E0 00 1F 21  |..........p....!|
#00000150: 18 22 08 12 F2 F2 F2 B1 F8 60 1F 20 08 10 81 01  |.".......`. ....|
#00000160: B1 01 1F 51 08 1F 20 04 10 81 01 B1 01 1F 51 04  |...Q.. .......Q.|
#00000170: 10 A0 FE 67 18 00 1F 50 00 80 00 50 04 1F 20 04  |...g...P...P.. .|
#00000180: 81 20 10 C1 A1 03 90 A0 01 91 80 00 C0 A5 16 1F  |. ..............|
#00000190: 20 08 81 81 00 10 C1 A1 03 90 A0 01 91 80 00 C0  | ...............|
#000001A0: A5 03 91 A0 01 90 80 00 C0 A5 80 6A 1F 20 18 21  |...........j. .!|
#000001B0: 08 11 F2 F2 F2 B0 F8 30 1F 21 00 22 04 12 03 11  |.......0.!."....|
#000001C0: 02 13 84 00 C4 A5 09 13 84 7F B4 03 12 F2 A0 70  |...............p|
#000001D0: 12 01 91 D1 FA 01 1F 22 08 97 D2 03 11 02 13 84  |......."........|
#000001E0: 00 C4 A5 09 13 84 7F B4 03 12 F1 A0 70 12 E0 00  |............p...|
#000001F0: 1F 21 18 22 08 12 F2 F2 F2 B1 F8 60 1F 20 08 10  |.!.".......`. ..|
#00000200: 81 01 B1 01 1F 51 08 1F 20 04 10 81 01 B1 01 1F  |.....Q.. .......|
#00000210: 51 04 10 A0 FE 67 19 00 1F 50 00 80 00 50 04 1F  |Q....g...P...P..|
#00000220: 20 04 81 20 10 C1 A1 03 90 A0 01 91 80 00 C0 A5  | .. ............|
#00000230: 16 1F 20 08 81 81 00 10 C1 A1 03 90 A0 01 91 80  |.. .............|
#00000240: 00 C0 A5 03 91 A0 01 90 80 00 C0 A5 80 6A 1F 20  |.............j. |
#00000250: 18 21 08 11 F2 F2 F2 B0 F8 30 1F 21 00 22 04 12  |.!.......0.!."..|
#00000260: 03 11 02 13 84 00 C4 A5 09 13 84 7F B4 03 12 F2  |................|
#00000270: A0 70 12 01 91 D1 FA 01 1F 22 08 97 D2 03 11 02  |.p......."......|
#00000280: 13 84 00 C4 A5 09 13 84 7F B4 03 12 F1 A0 70 12  |..............p.|
#00000290: E0 00 1F 21 18 22 08 12 F2 F2 F2 B1 F8 60 1F 20  |...!.".......`. |
#000002A0: 08 10 81 01 B1 01 1F 51 08 1F 20 04 10 81 01 B1  |.......Q.. .....|
#000002B0: 01 1F 51 04 10 A0 FE 67 1F 20 10 50 00 80 00 50  |..Q....g. .P...P|
#000002C0: 04 1F 20 04 81 20 10 C1 A1 03 90 A0 01 91 80 00  |.. .. ..........|
#000002D0: C0 A5 16 1F 20 08 81 81 00 10 C1 A1 03 90 A0 01  |.... ...........|
#000002E0: 91 80 00 C0 A5 03 91 A0 01 90 80 00 C0 A5 80 6A  |...............j|
#000002F0: 1F 20 18 21 08 11 F2 F2 F2 B0 F8 30 1F 21 00 22  |. .!.......0.!."|
#00000300: 04 12 03 11 02 13 84 00 C4 A5 09 13 84 7F B4 03  |................|
#00000310: 12 F2 A0 70 12 01 91 D1 FA 01 1F 22 08 97 D2 03  |...p......."....|
#00000320: 11 02 13 84 00 C4 A5 09 13 84 7F B4 03 12 F1 A0  |................|
#00000330: 70 12 E0 00 1F 21 18 22 08 12 F2 F2 F2 B1 F8 60  |p....!.".......`|
#00000340: 1F 20 08 10 81 01 B1 01 1F 51 08 1F 20 04 10 81  |. .......Q.. ...|
#00000350: 01 B1 01 1F 51 04 10 A0 FE 67 1F 20 0C 50 00 80  |....Q....g. .P..|
#00000360: 00 50 04 1F 20 04 10 81 04 C1 A1 03 90 A0 01 91  |.P.. ...........|
#00000370: 80 00 C0 A5 16 1F 20 08 81 81 00 10 C1 A1 03 90  |...... .........|
#00000380: A0 01 91 80 00 C0 A5 03 91 A0 01 90 80 00 C0 A5  |................|
#00000390: 80 6A 1F 20 18 21 08 11 F2 F2 F2 B0 F8 30 1F 21  |.j. .!.......0.!|
#000003A0: 00 22 04 12 03 11 02 13 84 00 C4 A5 09 13 84 7F  |."..............|
#000003B0: B4 03 12 F2 A0 70 12 01 91 D1 FA 01 1F 22 08 97  |.....p......."..|
#000003C0: D2 03 11 02 13 84 00 C4 A5 09 13 84 7F B4 03 12  |................|
#000003D0: F1 A0 70 12 E0 00 1F 21 18 22 08 12 F2 F2 F2 B1  |..p....!."......|
#000003E0: F8 60 1F 20 08 10 81 01 B1 01 1F 51 08 1F 20 04  |.`. .......Q.. .|
#000003F0: 10 81 01 B1 01 1F 51 04 10 A0 FE 67 1F 80 1C B0  |......Q....g....|
#00000400: 0F F5 09 F7 FE F6 F4 09 1F 80 6C B0 0F 1F 5A 0C  |..........l...Z.|
#00000410: 5B 10 86 80 E4 57 20 0C 10 F1 F1 F1 B6 06 00 81  |[....W .........|
#00000420: 00 0A 11 0B AF F7 69 1A 07 94 B6 00 81 01 0A 11  |......i.........|
#00000430: 0B AF F7 5C 1A 00 1F 50 04 80 0F 10 D7 07 88 00  |...\...P........|
#00000440: 89 00 80 00 1F 50 08 20 04 81 70 11 D0 00 1F 50  |.....P. ..p....P|
#00000450: 04 80 00 50 00 1F 20 00 81 1C 10 C1 A1 03 90 A0  |...P.. .........|
#00000460: 01 91 80 00 C0 A5 16 1F 20 00 81 81 00 10 C1 A1  |........ .......|
#00000470: 03 90 A0 01 91 80 00 C0 A5 03 91 A0 01 90 80 00  |................|
#00000480: C0 A5 80 51 1F 20 10 21 00 11 F2 F2 F2 B0 F8 30  |...Q. .!.......0|
#00000490: 1F 21 00 97 D1 02 10 01 12 83 00 C3 A5 09 12 83  |.!..............|
#000004A0: 7F B3 02 11 F2 A0 70 11 00 91 D0 00 1F 21 00 94  |......p......!..|
#000004B0: B1 02 10 01 12 83 00 C3 A5 09 12 83 7F B3 02 11  |................|
#000004C0: F1 A0 70 11 E7 07 1F 20 00 10 81 01 B1 01 1F 51  |..p.... .......Q|
#000004D0: 00 10 A0 FF 00 80 00 1F 50 00 1F 20 00 81 20 10  |........P.. .. .|
#000004E0: C1 A1 03 90 A0 01 91 80 00 C0 A5 80 58 1F 20 10  |............X. .|
#000004F0: 81 1C 22 00 12 B1 F2 F2 F2 B0 F8 30 81 1C 1F 22  |.."........0..."|
#00000500: 00 12 B1 01 97 D1 02 10 01 12 83 00 C3 A5 09 12  |................|
#00000510: 83 7F B3 02 11 F2 A0 70 11 00 91 D0 00 1F 21 00  |.......p......!.|
#00000520: 11 02 10 01 12 83 00 C3 A5 09 12 83 7F B3 02 11  |................|
#00000530: F1 A0 70 11 E8 08 1F 20 00 10 81 01 B1 01 1F 51  |..p.... .......Q|
#00000540: 00 10 A0 FF 15 80 00 1F 50 00 1F 20 00 81 20 10  |........P.. .. .|
#00000550: C1 A1 03 90 A0 01 91 80 00 C0 A5 80 58 1F 20 10  |............X. .|
#00000560: 81 3C 22 00 12 B1 F2 F2 F2 B0 F8 30 81 3C 1F 22  |.<"........0.<."|
#00000570: 00 12 B1 01 97 D1 02 10 01 12 83 00 C3 A5 09 12  |................|
#00000580: 83 7F B3 02 11 F2 A0 70 11 00 91 D0 00 1F 21 00  |.......p......!.|
#00000590: 11 02 10 01 12 83 00 C3 A5 09 12 83 7F B3 02 11  |................|
#000005A0: F1 A0 70 11 E9 09 1F 20 00 10 81 01 B1 01 1F 51  |..p.... .......Q|
#000005B0: 00 10 A0 FF 15 80 00 1F 50 00 1F 20 00 81 20 10  |........P.. .. .|
#000005C0: C1 A1 03 90 A0 01 91 80 00 C0 A5 80 5F 1F 20 08  |............_. .|
#000005D0: 21 10 82 80 5C 23 00 13 B2 F2 F2 F2 B1 F8 31 82  |!...\#........1.|
#000005E0: 80 5C 1F 23 00 13 B2 02 97 D2 03 11 02 13 84 00  |.\.#............|
#000005F0: C4 A5 09 13 84 7F B4 03 12 F2 A0 70 12 01 91 D1  |...........p....|
#00000600: 01 1F 22 00 12 03 11 02 13 84 00 C4 A5 09 13 84  |..".............|
#00000610: 7F B4 03 12 F1 A0 70 12 E0 00 1F 50 08 1F 20 00  |......p....P.. .|
#00000620: 10 81 01 B1 01 1F 51 00 10 A0 FF 0E 80 00 1F 50  |......Q........P|
#00000630: 00 1F 20 00 10 81 04 C1 A1 03 90 A0 01 91 80 00  |.. .............|
#00000640: C0 A5 80 5F 1F 20 04 21 10 82 80 7C 23 00 13 B2  |..._. .!...|#...|
#00000650: F2 F2 F2 B1 F8 31 82 80 7C 1F 23 00 13 B2 02 97  |.....1..|.#.....|
#00000660: D2 03 11 02 13 84 00 C4 A5 09 13 84 7F B4 03 12  |................|
#00000670: F2 A0 70 12 01 91 D1 01 1F 22 00 12 03 11 02 13  |..p......"......|
#00000680: 84 00 C4 A5 09 13 84 7F B4 03 12 F1 A0 70 12 E0  |.............p..|
#00000690: 00 1F 50 04 1F 20 00 10 81 01 B1 01 1F 51 00 10  |..P.. .......Q..|
#000006A0: A0 FF 0E 16 00 17 01 82 01 10 0A 11 0B 12 0C AF  |................|
#000006B0: F2 6D 91 B6 00 18 01 82 01 10 0A 11 0B 12 0C AF  |.m..............|
#000006C0: F2 5D 92 B6 00 19 01 82 01 10 0A 11 0B 12 0C AF  |.]..............|
#000006D0: F2 4D 93 B6 00 1F 22 08 12 01 82 01 10 0A 11 0B  |.M....".........|
#000006E0: 12 0C AF F2 3A 94 B6 00 1F 22 04 12 01 82 00 10  |....:...."......|
#000006F0: 0A 11 0B 12 0C AF F2 27 1F 80 14 B0 0F F5 09 F7  |.......'........|
#00000700: FE F6 F4 06 1F 80 60 B0 0F 86 00 80 10 16 C0 A1  |......`.........|
#00000710: 03 90 A0 01 91 80 00 C0 A5 15 91 B6 00 1F 81 10  |................|
#00000720: B1 01 16 B1 F8 60 16 00 81 01 B1 06 10 A0 5C 80  |.....`........\.|
#00000730: 00 1F 82 10 B2 01 10 0A 11 0B AF F9 48 80 00 1F  |............H...|
#00000740: 82 00 B2 01 10 0A 11 0B AF F1 63 8A 00 1F 80 20  |..........c.... |
#00000750: B0 0F F5 06 F7 FE 00 00 00 00 00 00 00 00 00 00  |................|""")


    boot_code = prog_to_tuples( program )

    imem = rom(
        odata        = imem_dout,
        idata        = imem_final_din,
        raddr        = imem_radr,
        renable      = imem_rd,
        waddr        = imem_final_wadr,
        wenable      = imem_final_wenable,
        clk          = cpu_clk,
        clk_en       = rom_clk_en,
        sync_rstn    = sync_rstn,
        depth        = IMEM_DEPTH,
        input_flops  = 0,
        output_flops = 0,
        content      = boot_code,
        name         = 'imem')


    # ---------------- AXI master -------------------------
    # single beat, non-pipelined master

    M_IDLE = 0
    M_WAIT_READ = 1
    M_WAIT_WRITE = 2

    m_state = signal(3)

    @always(clk.posedge)
    def irq_pulse():
        conf.irq.next = do_irq

    @always(clk.posedge)
    def axi_master():
        axi.arlen.next = 0
        axi.rready.next = 0
        axi.arvalid.next = 0
        axi.araddr.next = 0
        axi.awaddr.next = 0
        axi.awvalid.next = 0
        axi.wdata.next = 0
        axi.wvalid.next = 0

        #if sync_rstn == 0:
        #    axi.rready.next = 0
        #    axi.arvalid.next = 0
        #    axi.araddr.next = 0
        #    axi.awaddr.next = 0
        #    axi.awvalid.next = 0
        #    axi.wdata.next = 0
        #    axi.wvalid.next = 0
        #    req_done.next = 0
        #    req_rdata.next = 0
        #    m_state.next = M_IDLE
        #else:

        #    if m_state == M_IDLE:
        #        req_done.next = 0
        #        axi.rready.next = 0
        #        axi.arvalid.next = 0

        #        if req_rd == 1:
        #            print("axi req_rd")
        #            axi.arvalid.next = 1
        #            axi.araddr.next = req_addr
        #            if axi.arready == 1:
        #                m_state.next = M_WAIT_RVALID
        #            else:
        #                m_state.next = M_WAIT_ARREADY
        #        elif req_wr == 1:
        #            print("axi req_wr")
        #            axi.awvalid.next = 1
        #            axi.awaddr.next = req_addr
        #            axi.wvalid.next = 1
        #            axi.wdata.next = req_wdata
        #            if axi.awready == 1:
        #                m_state.next = M_WAIT_WREADY
        #            else:
        #                m_state.next = M_WAIT_AWREADY

        #    elif m_state == M_WAIT_ARREADY:
        #        if axi.arready == 1:
        #            if axi.rvalid == 1:
        #                axi.rready.next = 1
        #                req_rdata.next = axi.rdata
        #                print("axi req_done")
        #                req_done.next = 1
        #                m_state.next = M_IDLE
        #            else:
        #                m_state.next = M_WAIT_RVALID
        #        else:
        #            m_state.next = M_WAIT_ARREADY
        #    elif m_state == M_WAIT_RVALID:
        #        axi.arvalid.next = 0
        #        if axi.rvalid == 1:
        #            axi.rready.next = 1
        #            req_rdata.next = axi.rdata
        #            print("axi req_done")
        #            req_done.next = 1
        #            m_state.next = M_IDLE
        #        else:
        #            m_state.next = M_WAIT_RVALID

        #    elif m_state == M_WAIT_AWREADY:
        #        if axi.awready == 1:
        #            axi.awvalid.next = 0
        #            if axi.wready == 1:
        #                axi.wvalid.next = 0
        #                req_done.next = 1
        #                print("axi req_done")
        #                m_state.next = M_IDLE
        #            else:
        #                m_state.next = M_WAIT_WREADY
        #        else:
        #            m_state.next = M_WAIT_AWREADY
        #    elif m_state == M_WAIT_WREADY:
        #        if axi.wready == 1:
        #            axi.wvalid.next = 0
        #            req_done.next = 1
        #            print("axi req_done")
        #            m_state.next = M_IDLE
        #        else:
        #            m_state.next = M_WAIT_WREADY

    @always(clk.posedge)
    def conf_master():
        if conf.master_request_re == 1 or conf.master_request_we == 1:
            conf.master_request_address.next = 0
            conf.master_request_data.next = 0
            conf.master_request_id.next = 0
            conf.master_request_type.next = 0
            conf.master_request_we.next = 0
            conf.master_request_re.next = 0

        if sync_rstn == 0:
            conf.master_request_address.next = 0
            conf.master_request_data.next = 0
            conf.master_request_id.next = 0
            conf.master_request_type.next = 0
            conf.master_request_we.next = 0
            conf.master_request_re.next = 0
            req_done.next = 0
            req_rdata.next = 0
            req_got_reply.next = 0
            m_state.next = M_IDLE
        else:
            if m_state == M_IDLE:
                req_done.next = 0
                req_got_reply.next = 0
                if req_rd == 1:
                    # divide by 4 to translate from byte addressing to word addressing
                    conf.master_request_address.next = req_addr
                    conf.master_request_re.next = 1
                    m_state.next = M_WAIT_READ
                elif req_wr == 1:
                    conf.master_request_address.next = req_addr
                    conf.master_request_data.next = req_wdata
                    conf.master_request_we.next = 1
                    m_state.next = M_WAIT_WRITE
            elif m_state == M_WAIT_READ:
                if conf.master_reply_status != 0:
                    req_got_reply.next = 1
                    req_rdata.next = conf.master_reply_data
                    if slave_state != SLAVE_WRITE and slave_state != SLAVE_READ1:
                        req_done.next = 1
                        m_state.next = M_IDLE
                elif req_got_reply == 1 and slave_state != SLAVE_WRITE and slave_state != SLAVE_READ1:
                    req_done.next = 1
                    m_state.next = M_IDLE
            elif m_state == M_WAIT_WRITE:
                if conf.master_reply_status != 0:
                    req_got_reply.next = 1
                    if slave_state != SLAVE_WRITE and slave_state != SLAVE_READ1:
                        req_done.next = 1
                        m_state.next = M_IDLE
                elif req_got_reply == 1 and slave_state != SLAVE_WRITE and slave_state != SLAVE_READ1:
                    req_done.next = 1
                    m_state.next = M_IDLE

    SLAVE_IDLE   = 0
    SLAVE_READ1  = 1
    SLAVE_READ2  = 2
    SLAVE_WRITE  = 3

    slave_state = signal(2)  # declare as Signal outside the process
    slave_state_prev = signal(2)
    isp = flop(slave_state, slave_state_prev, clk_en=None, clk=clk, sync_rstn=sync_rstn)

    @always(clk.posedge)
    def conf_slave():
        conf.slave_reply_status.next      = 0
        conf.slave_reply_data.next        = 0
        conf.slave_reply_id.next          = 0
        conf_slave_dmem_renable.next      = 0
        conf_slave_dmem_wenable.next      = 0
        conf_slave_dmem_wmask.next        = 0b1111
        conf_slave_dmem_din.next          = 0
        conf_slave_imem_wenable.next      = 0
        conf_slave_imem_din.next          = 0
        n_cpu_rstn.next = 1
        intr_addr_valid.next              = 0

        if sync_rstn == 0:
            slave_state.next = SLAVE_IDLE
        else:
            if slave_state == SLAVE_IDLE:
                if conf.slave_request_we:
                    print("address: ", conf.slave_request_address)
                    if conf.slave_request_address == INTERRUPT_ADDRESS:
                        intr_addr_valid.next = 1
                        intr_addr_data.next  = conf.slave_request_data
                    elif conf.slave_request_address == CPU_RESET_ADDRESS:
                        n_cpu_rstn.next = 0
                    elif conf.slave_request_address <= IMEM_HIGH:
                        conf_slave_imem_wadr.next    = conf.slave_request_address
                        conf_slave_imem_din.next     = conf.slave_request_data
                        conf_slave_imem_wenable.next = 1
                    else:
                        conf_slave_dmem_wadr.next    = conf.slave_request_address
                        conf_slave_dmem_din.next     = conf.slave_request_data
                        conf_slave_dmem_wenable.next = 1
                        conf_slave_dmem_wmask.next   = 0b1111
                    conf.slave_reply_id.next     = conf.slave_request_id
                    slave_state.next             = SLAVE_WRITE

                elif conf.slave_request_re:
                    conf_slave_dmem_radr.next    = conf.slave_request_address
                    conf_slave_dmem_renable.next = 1
                    slave_state.next             = SLAVE_READ1

            elif slave_state == SLAVE_WRITE:
                conf.slave_reply_status.next = 1
                conf.slave_reply_id.next     = conf.slave_request_id
                slave_state.next             = SLAVE_IDLE
                print("slave back to IDLE, cpu_waiting=", cpu_waiting, "wait_type=", wait_type)

            elif slave_state == SLAVE_READ1:
                slave_state.next       = SLAVE_READ2

            elif slave_state == SLAVE_READ2:
                conf.slave_reply_data.next   = dmem_dout
                #print("dmem_out: ", dmem_dout)
                #print("slave_reply_data:", conf.slave_reply_data.next)
                conf.slave_reply_status.next = 1
                conf.slave_reply_id.next     = conf.slave_request_id
                slave_state.next             = SLAVE_IDLE

    #@always(clk.posedge)
    #def conf_slave():
    #    conf.slave_reply_status.next = 0
    #    conf.slave_reply_data.next   = 0
    #    conf.slave_reply_id.next     = 0
    #    conf_slave_dmem_renable.next      = 0
    #    conf_slave_dmem_wenable.next      = 0
    #    conf_slave_dmem_wmask.next        = 0b1111
    #    conf_slave_dmem_din.next     = 0
    #    conf_slave_active.next = 0
    #        #n_conf_slave_active.next          = 0

    #    if sync_rstn == 0:
    #        print("e")
    #        #n_conf_slave_active.next = 0
    #    else:
    #        if conf_slave_active == 1:
    #            print(dmem_muxed_dout)
    #            # second cycle: read data is now valid on dmem_final_dout
    #            conf.slave_reply_data.next   = dmem_muxed_dout 
    #            conf.slave_reply_status.next = 1
    #            conf.slave_reply_id.next     = conf.slave_request_id
    #            conf_slave_active.next          = 0
    #        elif conf.slave_request_we == 1:
    #            print("b")
    #            # write: single cycle, data latched by RAM on this edge
    #            conf_slave_dmem_wadr.next    = conf.slave_request_address
    #            conf_slave_dmem_din.next     = conf.slave_request_data
    #            conf_slave_dmem_wenable.next = 1
    #            conf_slave_dmem_wmask.next   = 0b1111
    #            conf_slave_active.next     = 1
    #            conf.slave_reply_id.next     = conf.slave_request_id
    #        elif conf.slave_request_re == 1:
    #            print("c")
    #            # read: issue to RAM this cycle, capture result next cycle
    #            conf_slave_dmem_radr.next    = conf.slave_request_address
    #            conf_slave_dmem_renable.next = 1
    #            conf_slave_active.next     = 1
    #            print("d")

    return instances()
