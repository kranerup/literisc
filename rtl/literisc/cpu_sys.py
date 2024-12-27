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

def cpu_sys(
        clk,
        sync_rstn,
        axi
        ):

    # Memory map:
    # 0 - X             : boot rom
    # X - 8191          : IMEM (and accessible from DMEM bus)
    # 8192 - 8192+32768 : DMEM
    # 65536-100 - 65535 : IO
    perip_addr_bits = 16
    perip_data_bits = 32
    cpu_dmem_data_bits = 32
    dmem_depth = 16384 # 32768 # reduced to fit FPGA BRAM ( 65536)
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
    imem_wenable = signal()
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
            imem_rd.next = 1 # probably needs fixing when implementing imem wr
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
        imem_wenable.next = 0
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




    boot_code = prog_to_tuples( program )

    imem = rom(
        odata        = imem_dout,
        idata        = dmem_din,
        raddr        = imem_radr,
        renable      = imem_rd,
        waddr        = dmem_adr,
        wenable      = imem_wenable,
        clk          = cpu_clk,
        clk_en       = rom_clk_en,
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


