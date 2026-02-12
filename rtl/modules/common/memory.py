import myhdl.conversion._toVerilog
from myhdl import *
from .Common import (
    pass_through,
    sliceSignal,
    signalType,
    listType,
    copySignal,
    mux2,
    flop,
    compoundWidth,
    multiflop,
    slice_stable_randrange,
    is_flopmem,
)
from random import randrange, seed
import sys
from unittesting.asic import *
import unittesting.memory_ip as memory_ip
from modules.common.Common import hwdir, rootdir
import sys
import os
from textwrap import dedent

sys.path.append(hwdir())
sys.path.append(os.path.join(hwdir(), "hdl"))
import shutil
from subprocess import call

"""
File status: The memory lacks support for inferred single-port instances, true
dual port and three-ports and above. Also lacks support for vendor-
specific features such as write-through, byte enable, different port widths 
and so on...

None of this is a problem, really. It just has to be implemented when
the need arises.

Altera does not support read_enable (as far as I can tell), this may
or may not be a problem.

The collision detection is built in for both Altera and Xilinx, but I
have not tried to make a model that infers that properly in both.
It could be worth trying out, it may be as simple as looking at their
inference patterns.

"""


def copy_modbv(sig):
    return modbv(0)[len(sig) :]


def memory_init_ff(
    idata,
    odata,
    raddr,
    waddr,
    renable,
    wenable,
    soft_reset,
    doing_init,
    clk,
    rstn,
    register_read,
    register_write,
    register_we,
    depth=16,
    write_through=0,
    reset_value=0,
    pre_load=1,
    conf_load={},
    input_flops=0,
    output_flops=0,
    hwc=None,
    name="",
):
    # A memory init in flops, with a register_read output (instead of the identical consistency_data)
    # The difference between register_read and consistency_data is that the consisteny_data is
    # not driven in the netlist, it is guarded by translate_off

    # This module exists as a quick fix to make register tables using mem_cpu_if.
    # This quick fix has the drawback that the fact that the memory is in flops
    # cannot be utilized by the conf interface. It will access like it was a real
    # memory.
    idata_flat = Signal(modbv(0)[compoundWidth(idata) :])
    odata_flat = Signal(modbv(0)[compoundWidth(odata) :])

    if register_read != None:
        data = [copySignal(idata_flat, t=modbv) for _ in range(depth)]
    else:
        data = Memory([0 for _ in range(depth)], len(idata_flat))

    lenwe = 0 if register_we == None else len(register_we)

    assert (
        lenwe == 0 or lenwe == 1 or lenwe == depth
    ), "len(register_we) = %d and is expected to be either 1 or depth = %d" % (
        lenwe,
        depth,
    )
    assert input_flops == 0
    assert output_flops == 0

    zpass = []
    zpass.append(pass_through(idata, idata_flat, name=name + ".passi"))
    zpass.append(pass_through(odata_flat, odata, name=name + ".passo"))

    # - Write
    if lenwe > 0:
        register_write_flat = [copySignal(idata_flat) for _ in range(depth)]
        for i in range(depth):
            zpass.append(
                pass_through(
                    register_write[i], register_write_flat[i], name=name + ".passrw"
                )
            )

        @always(clk.posedge, rstn.negedge)
        def memlogic():
            if rstn == 0:
                doing_init.next = 0
                for i in range(depth):
                    data[i].next = reset_value
            #        elif soft_reset==1:
            #            doing_init.next = 0
            #            for i in range(depth):
            #                data[i].next = reset_value
            else:
                doing_init.next = 0
                if lenwe == 1:
                    if register_we == 1:
                        for i in range(depth):
                            data[i].next = register_write_flat[i]
                elif lenwe > 1:
                    for i in range(depth):
                        if register_we[i] == 1:
                            data[i].next = register_write_flat[i]
                if wenable == 1:
                    data[waddr].next = idata_flat

    else:
        # No parallel writes
        @always(clk.posedge, rstn.negedge)
        def memlogic():
            if rstn == 0:
                doing_init.next = 0
                for i in range(depth):
                    data[i].next = reset_value
            else:
                doing_init.next = 0
                if wenable == 1:
                    data[waddr].next = idata_flat

    # - Read
    @always(clk.posedge, rstn.negedge)
    def outlogic():
        if rstn == 0:
            odata_flat.next = 0
        else:
            odata_flat.next = 0
            if renable == 1:
                odata_flat.next = data[raddr]
                if wenable == 1 and raddr == waddr:
                    odata_flat.next = idata_flat

    if register_read != None:
        for i in range(depth):
            zpass.append(
                pass_through(data[i], register_read[i], name=name + ".passr%d" % i)
            )

    return instances()


def memory_init(
    idata,
    odata,
    raddr,
    waddr,
    renable,
    wenable,
    soft_reset,
    doing_init,
    clk,
    rstn,
    consistency_data=None,
    depth=16,
    write_through=0,
    reset_value=0,
    pre_load=1,
    conf_load={},
    input_flops=0,
    output_flops=0,
    hwc=None,
    name="",
):
    """
    TODO: Clean up the pre_load and conf_load initialization functions to make them usable

    pre_load=1, will utilize that in an fpga the memory can be pre-loaded with values from the bitfile.
    But this also means that the memory will be retained on reset.
    So if you want the memory to re-initialized on reset you have to set pre_load=0.

    conf_load={}: Here a dictionary of addresses and data is used to initialize the memory from the bitfile.
    For some reason this is only active when pre_load=0, but the dictionary loading will only work at powerup,
    not at reset so rstn will overwrite the values written by conf_load.
    So as far as I can tell, this will not work at all.
    The exception is that in myhdlsim memories without init will actually work for conf_load.

    """

    if hwc == None:
        from settings.hwconf import create_hwconf

        create_hwconf()
        from modules.common.hwconf import get_hwconf; hwconf = get_hwconf()
    else:
        hwconf = hwc

    mem_idata = copySignal(idata)
    mem_odata = copySignal(odata)
    mem_raddr = copySignal(raddr)
    mem_waddr = copySignal(waddr)
    mem_renable = copySignal(renable)
    mem_wenable = copySignal(wenable)
    not_first_cycle = Signal(intbv(0)[1:])

    if soft_reset != 0:
        print(
            "ERROR! memory_init %s Soft reset %s is not supported" % (name, soft_reset)
        )
        assert False

    if (raddr.max).bit_length() != (depth).bit_length():
        print("WARNING!", name, "depth mismatch raddr.max", raddr.max, "depth", depth)
    if (waddr.max).bit_length() != (depth).bit_length():
        print("WARNING!", name, "depth mismatch waddr.max", waddr.max, "depth", depth)
    init_cnt = Signal(intbv(0, min=0, max=depth))
    if reset_value >= idata.max:
        print("ERROR!", name, "reset_value", reset_value, ">= max value", idata.max)
        assert False

    if listType(idata):
        print("ERROR! memory_init does not support list data... yet.")
        assert False

    width = compoundWidth(idata)
    flopmem = is_flopmem(width, depth, hwconf)

    if flopmem == 0:
        print(
            "Instantiated memory_init %s with reset value %s. w %s, d %s"
            % (name, reset_value, width, depth)
        )
    else:
        print(
            "Instantiated flopmem memory_init %s with reset value %s. w %s, d %s"
            % (name, reset_value, width, depth)
        )

    do_pre_load = pre_load
    if len(conf_load) > 0:
        do_pre_load = 0
    if hwconf.memory_mode != "inferred":
        do_pre_load = 0

    if flopmem == 0:
        if do_pre_load:

            @always(clk.posedge, rstn.negedge)
            def init():
                if rstn == 0:
                    doing_init.next = 0
                else:
                    doing_init.next = 0

        else:

            @always(clk.posedge, rstn.negedge)
            def init():
                if rstn == 0:
                    not_first_cycle.next = 0
                    doing_init.next = 1
                    init_cnt.next = 0
                elif doing_init == 1:
                    not_first_cycle.next = 1
                    if not_first_cycle == 0:
                        init_cnt.next = 0
                    elif init_cnt + 1 >= depth:
                        doing_init.next = 0
                    else:
                        init_cnt.next = init_cnt + 1

    #                    if soft_reset == 1:
    #                        first_cycle.next = 1
    #                        doing_init.next = 1
    #                        init_cnt.next = 0
    else:

        @always(clk.posedge, rstn.negedge)
        def init():
            if rstn == 0:
                doing_init.next = 0
            else:
                doing_init.next = 0

    imuxid = mux2(idata, reset_value, mem_idata, doing_init, name=name + ".mid")
    imuxwa = mux2(waddr, init_cnt, mem_waddr, doing_init, name=name + ".mwa")
    imuxwe = mux2(wenable, not_first_cycle, mem_wenable, doing_init, name=name + ".men")
    imuxra = mux2(raddr, init_cnt, mem_raddr, doing_init, name=name + ".mra")
    imuxew = mux2(renable, 0, mem_renable, doing_init, name=name + ".mre")
    imuxod = mux2(mem_odata, 0, odata, doing_init, name=name + ".mod")

    imem = memory(
        idata=mem_idata,
        odata=mem_odata,
        raddr=mem_raddr,
        waddr=mem_waddr,
        renable=mem_renable,
        wenable=mem_wenable,
        clk=clk,
        rstn=rstn,
        depth=depth,
        consistency_data=consistency_data,
        write_through=write_through,
        reset_value=reset_value,
        pre_load=do_pre_load,
        conf_load=conf_load,
        input_flops=input_flops,
        output_flops=output_flops,
        hwc=hwc,
        name=name,
    )

    return instances()


def memory(
    idata,
    odata,
    raddr,
    waddr,
    renable,
    wenable,
    clk,
    rstn,
    consistency_data=None,
    depth=16,
    write_through=0,
    reset_value=0,
    pre_load=0,
    conf_load={},
    input_flops=0,
    output_flops=0,
    hwc=None,
    single_ported=False,
    wclk=None,
    wrstn=None,
    wmask=None,
    no_io_flops=False,
    name="",
):
    if hwc == None:
        from settings.hwconf import create_hwconf

        create_hwconf()
        from modules.common.hwconf import get_hwconf; hwconf = get_hwconf()
    else:
        hwconf = hwc
    print("Instantiated memory %s with write_through %s" % (name, write_through))
    if single_ported:
        assert wclk == None, (
            "%s single_ported only supported in single clock mode." % name
        )
        assert wrstn == None, (
            "%s single_ported only supported in single clock mode." % name
        )

        # TODO: Single ported in inferred mode
        @always(clk.posedge, rstn.negedge)
        def assert_single_ported():
            if rstn == 0:
                pass
            else:
                if renable == 1 and wenable == 1:
                    "synthesis translate_off"
                    print("ERROR! read and write to single ported memory %s" % (name))
                    assert False
                    "synthesis translate_on"

        assert (
            write_through == 0
        ), "ERROR! Write through for single ported memory %s" % (name)

    assert type(reset_value).__name__ in ("int", "long") or signalType(
        reset_value
    ), "ERROR! %s reset_value %s (%s) needs to be int or signal." % (
        name,
        reset_value,
        type(reset_value).__name__,
    )

    wenablex = copySignal(wenable)
    after_reset = Signal(intbv(0)[1:])
    do_pre_load = pre_load
    if len(conf_load) > 0:
        do_pre_load = 0
    if hwconf.memory_mode != "inferred":
        do_pre_load = 0

    try:
        evaluation_timeout_cnt = hwconf.evaluation_timeout_cnt
    except:
        evaluation_timeout_cnt = None

    # evaluation_timeout_cnt filter
    # Disable timeout for mems in mac wrapper
    if "mactop" in name:
        evaluation_timeout_cnt = None

    @always(clk.posedge, rstn.negedge)
    def drive_ar():
        if rstn == 0:
            after_reset.next = 0
        else:
            after_reset.next = 1

    dualclock = 0
    if wclk == None:
        iclk = clk
        irstn = rstn
    else:
        assert (
            write_through == 0
        ), "%s write_through is not supported for dual clock memories"
        dualclock = 1
        iclk = wclk
        irstn = wrstn

    # Assert for read address range
    @always(clk.posedge, rstn.negedge)
    def assertraddr():
        if rstn == 0:
            pass
        else:
            if renable == 1 and raddr + 1 > depth:
                "synthesis translate_off"
                print(
                    name,
                    "ERROR! Read from address %s is outside the address space %s"
                    % (raddr, depth),
                )
                assert False
                "synthesis translate_on"

    # Assert for write address range
    @always(iclk.posedge, irstn.negedge)
    def assertwaddr():
        if irstn == 0:
            pass
        else:
            if wenable == 1 and waddr + 1 > depth:
                "synthesis translate_off"
                print(
                    name,
                    "ERROR! Write to address %s is outside the address space %s"
                    % (waddr, depth),
                )
                assert False
                "synthesis translate_on"

    if evaluation_timeout_cnt == None:

        @always_comb
        def drive_wen():
            if dualclock == 1:
                wenablex.next = wenable
            else:
                wenablex.next = 0
                if after_reset == 1:
                    wenablex.next = wenable

    # Evaluation timout point
    else:
        unit_bit = slice_stable_randrange(hwconf, name)(8, 16)
        carry_bit = slice_stable_randrange(hwconf, name)(8, 16)
        # Check min set
        while 2**unit_bit * 2**carry_bit > evaluation_timeout_cnt:
            if carry_bit > 0:
                carry_bit -= 1
                if slice_stable_randrange(hwconf, name)(2) == 1 and unit_bit > 0:
                    unit_bit -= 1
            else:
                unit_bit -= 1
        while 2**unit_bit * 2**carry_bit < evaluation_timeout_cnt:
            if unit_bit < 32:
                unit_bit += 1
                if slice_stable_randrange(hwconf, name)(2) == 1:
                    carry_bit += 1
            else:
                carry_bit += 1

        carry_length = 32 if carry_bit < 32 else carry_bit + 1

        zhuque = Signal(intbv(0)[unit_bit:0])
        nan = Signal(intbv(0)[carry_length:0])

        @always(iclk.posedge, irstn.negedge)
        def nun():
            if irstn == 0:
                zhuque.next = 0
            else:
                if zhuque >= 0:
                    zhuque.next = (zhuque + 1) & ((1 << unit_bit) - 1)
                else:
                    zhuque.next = 0

        max_unit = 2**unit_bit - 1

        @always(iclk.posedge, irstn.negedge)
        def xup():
            if irstn == 0:
                nan.next = 0
            else:
                if nan >= 0:
                    if zhuque == max_unit and nan[carry_bit] == 0:
                        nan.next = nan + 1
                else:
                    nan.next = 0

        @always_comb
        def wen():
            if dualclock == 1:
                wenablex.next = wenable
            else:
                wenablex.next = 0
                if after_reset == 1:
                    if nan[carry_bit] == 0:
                        wenablex.next = wenable

    if signalType(idata) and signalType(odata):
        assert signalType(odata)
        smem = memory_signal(
            idata,
            odata,
            waddr,
            raddr,
            wenablex,
            renable,
            clk,
            rstn,
            consistency_data,
            depth,
            write_through,
            reset_value,
            do_pre_load,
            conf_load,
            input_flops,
            output_flops,
            hwconf=hwconf,
            single_ported=single_ported,
            wclk=wclk,
            wrstn=wrstn,
            wmask=wmask,
            no_io_flops=no_io_flops,
            name=name + ".s",
        )
    else:
        iflat = Signal(intbv(0)[compoundWidth(idata) :])
        iconnect = pass_through(idata, iflat, name=name + ".ic")
        oflat = Signal(intbv(0)[compoundWidth(odata) :])
        oconnect = pass_through(oflat, odata, name=name + ".oc")
        lmem = memory_signal(
            iflat,
            oflat,
            waddr,
            raddr,
            wenablex,
            renable,
            clk,
            rstn,
            consistency_data,
            depth,
            write_through,
            reset_value,
            do_pre_load,
            conf_load,
            input_flops,
            output_flops,
            hwconf=hwconf,
            single_ported=single_ported,
            wclk=wclk,
            wrstn=wrstn,
            wmask=wmask,
            no_io_flops=no_io_flops,
            name=name,
        )
    return instances()


def memory_signal(
    idata,
    odata,
    waddr,
    raddr,
    wenable,
    renable,
    clk,
    rstn,
    consistency_data=None,
    depth=16,
    write_through=0,
    reset_value=0,
    pre_load=0,
    conf_load={},
    input_flops=0,
    output_flops=0,
    top_instance=1,
    hwconf=None,
    single_ported=False,
    wclk=None,
    wrstn=None,
    wmask=None,
    no_io_flops=False,
    name="",
):
    amem = asic_mem()
    width = len(idata)
    if wmask is not None:
        mwidth = len(wmask)
        assert width % mwidth == 0, f"Bad mask-width {mwidth} width={width}"
    else:
        mwidth = 0

    if consistency_data != None:
        print("Adding consistency check observation bus", name)
        zdrive = pass_through(
            data, consistency_data, translate_off=1, name=name + ".drive_cdata"
        )

    if no_io_flops:
        assert input_flops == 0, "No input flops are allowed as no_io_flops is set"
        assert output_flops == 0, "No output flops are allowed as no_io_flops is set"

    flopmem = 0
    # Populate the memory list
    width = compoundWidth(idata)
    if hwconf.memory_flop_limit != None:
        if width * depth < hwconf.memory_flop_limit:
            flopmem = 1
    if hwconf.memory_flop_dlimit != None:
        if depth < hwconf.memory_flop_dlimit:
            flopmem = 1
    if hwconf.memory_force_output_flop and output_flops == 0:
        print(
            "WARNING!",
            name,
            "hwconf.memory_force_output_flop=%s and output_flops=%s forces flopmem=1"
            % (hwconf.memory_force_output_flop, output_flops),
        )
        flopmem = 1
    if hwconf.memory_force_input_flop and input_flops == 0:
        print(
            "WARNING!",
            name,
            "hwconf.memory_force_input_flop=%s and input_flops=%s forces flopmem=1"
            % (hwconf.memory_force_input_flop, input_flops),
        )
        flopmem = 1
    if hwconf.memory_force_in_or_out_flop and input_flops + output_flops == 0:
        print(
            "WARNING!",
            name,
            "hwconf.memory_force_in_or_out_flop=%s and output_flops+input_flops=%s forces flopmem=1"
            % (hwconf.memory_force_in_or_out_flop, output_flops + input_flops),
        )
        flopmem = 1

    if hwconf.memory_mode == "verilog_memory":
        if wclk != None:
            if flopmem == 1:
                print(
                    "%s forcing the use of a memory macro for dual clock memory" % name
                )
                flopmem = 0

    if wclk == None:
        iclk = clk
        irstn = rstn
    else:
        assert (
            write_through == 0
        ), "%s write_through is not supported for dual clock memories"
        iclk = wclk
        irstn = wrstn

    if flopmem == 0:
        if mw := hwconf.memory_max_width:
            if mw < width:
                bulk_cols = width // mw
                if width % mw > 0:
                    cols = bulk_cols + 1
                else:
                    cols = bulk_cols
                bulk = width // cols
                rest = width - (bulk * bulk_cols)
                bulk_idata = [Signal(intbv(0)[bulk:]) for _ in range(bulk_cols)]
                bulk_odata = copySignal(bulk_idata)
                bulk_reset_value = [
                    (reset_value >> (bulk * i)) & ((1 << bulk) - 1)
                    for i in range(bulk_cols)
                ]
                assert mwidth == 0 or bulk_cols == 1 and rest == 0, (
                    "No support for write mask when slicing. "
                    f"width={width} mwidth={mwidth} bulk_cols={bulk_cols} rest={rest}"
                )
                if rest > 0:
                    rest_idata = Signal(intbv(0)[rest:])
                    rest_odata = Signal(intbv(0)[rest:])
                    zPassidata = pass_through(
                        idata, [bulk_idata, rest_idata], name=name + ".id"
                    )
                    zPassodata = pass_through(
                        [bulk_odata, rest_odata], odata, name=name + ".bd"
                    )
                    rest_reset_value = reset_value >> (bulk * bulk_cols)
                else:
                    zPassidata = pass_through(idata, bulk_idata, name=name + ".id2")
                    zPassodata = pass_through(bulk_odata, odata, name=name + ".bd2")

                iMemcol = []
                for i in range(bulk_cols):
                    iMemcol.append(
                        memory_signal(
                            bulk_idata[i],
                            bulk_odata[i],
                            waddr,
                            raddr,
                            wenable,
                            renable,
                            clk,
                            rstn,
                            consistency_data,
                            depth=depth,
                            write_through=write_through,
                            reset_value=bulk_reset_value[i],
                            pre_load=pre_load,
                            conf_load=conf_load,
                            input_flops=input_flops,
                            output_flops=output_flops,
                            top_instance=0,
                            hwconf=hwconf,
                            single_ported=single_ported,
                            wclk=wclk,
                            wrstn=wrstn,
                            no_io_flops=no_io_flops,
                            name=name + ".iMemcol%s" % i,
                        )
                    )
                if rest > 0:
                    iMemcol.append(
                        memory_signal(
                            rest_idata,
                            rest_odata,
                            waddr,
                            raddr,
                            wenable,
                            renable,
                            clk,
                            rstn,
                            consistency_data,
                            depth=depth,
                            write_through=write_through,
                            reset_value=rest_reset_value,
                            pre_load=pre_load,
                            conf_load=conf_load,
                            input_flops=input_flops,
                            output_flops=output_flops,
                            top_instance=0,
                            hwconf=hwconf,
                            single_ported=single_ported,
                            wclk=wclk,
                            wrstn=wrstn,
                            no_io_flops=no_io_flops,
                            name=name + ".iMemcol%s" % (cols),
                        )
                    )
                return instances()

        if hwconf.memory_mode in list(memory_ip.ips.keys()) or hwconf.memory_max_depth:
            print(
                "%s slicing memory because mode=%s and max_depth=%s != None"
                % (name, hwconf.memory_mode, hwconf.memory_max_depth)
            )
            latency = 1
            latency += input_flops
            latency += output_flops
            if hwconf.memory_mode in list(memory_ip.ips.keys()):
                ram = memory_ip.ips[hwconf.memory_mode]
            if md := hwconf.memory_max_depth:
                if hwconf.memory_mode in list(memory_ip.ips.keys()):
                    print(
                        "ERROR! You should not set a max depth when using asic "
                        "memories. Let the memory script take care of that."
                    )
                    assert False
            else:
                md = ram.max_depth
            if depth > md:
                rest = depth % md
                bulk = depth - rest
                rows = bulk // md
                if rest > 0:
                    print(name, "Splitting %s into %s + %s" % (depth, bulk, rest))
                    bulk_raddr = Signal(modbv(0)[(bulk - 1).bit_length() : 0])
                    bulk_waddr = copySignal(bulk_raddr, t=modbv)
                    rest_raddr = Signal(modbv(0)[max((rest - 1).bit_length(), 1) : 0])
                    rest_waddr = copySignal(rest_raddr)
                    bulk_renable = Signal(intbv(0)[1:])
                    rest_renable = Signal(intbv(0)[1:])
                    bulk_renable_ff = Signal(intbv(0)[1:])
                    bulk_wenable = Signal(intbv(0)[1:])
                    rest_wenable = Signal(intbv(0)[1:])
                    bulk_odata = copySignal(idata)
                    rest_odata = copySignal(odata)
                    bulk_aw = len(bulk_raddr)
                    rest_aw = len(rest_raddr)
                    tot_aw = len(raddr)
                    bulk_mem = memory_signal(
                        idata,
                        bulk_odata,
                        bulk_waddr,
                        bulk_raddr,
                        bulk_wenable,
                        bulk_renable,
                        clk,
                        rstn,
                        consistency_data,
                        depth=bulk,
                        write_through=write_through,
                        reset_value=reset_value,
                        pre_load=pre_load,
                        conf_load=conf_load,
                        input_flops=input_flops,
                        output_flops=output_flops,
                        top_instance=0,
                        hwconf=hwconf,
                        single_ported=single_ported,
                        wclk=wclk,
                        wrstn=wrstn,
                        wmask=wmask,
                        no_io_flops=no_io_flops,
                        name=name + ".bulk",
                    )
                    rest_mem = memory_signal(
                        idata,
                        rest_odata,
                        rest_waddr,
                        rest_raddr,
                        rest_wenable,
                        rest_renable,
                        clk,
                        rstn,
                        consistency_data,
                        depth=rest,
                        write_through=write_through,
                        reset_value=reset_value,
                        pre_load=pre_load,
                        conf_load=conf_load,
                        input_flops=input_flops,
                        output_flops=output_flops,
                        top_instance=0,
                        hwconf=hwconf,
                        single_ported=single_ported,
                        wclk=wclk,
                        wrstn=wrstn,
                        wmask=wmask,
                        no_io_flops=no_io_flops,
                        name=name + ".rest",
                    )

                    @always_comb
                    def split_addr():
                        wtmp = intbv(0)[tot_aw:]
                        rtmp = intbv(0)[tot_aw:]
                        bulk_raddr.next = 0
                        bulk_waddr.next = 0
                        rest_raddr.next = 0
                        rest_waddr.next = 0
                        bulk_renable.next = 0
                        rest_renable.next = 0
                        bulk_wenable.next = 0
                        rest_wenable.next = 0
                        if raddr >= bulk:
                            rest_renable.next = renable
                            "lint_waive UNEQUAL_LEN INCREMENT: Expected loss of msbs"
                            rest_raddr.next = raddr - bulk
                        else:
                            bulk_renable.next = renable
                            "lint_waive UNEQUAL_LEN INCREMENT: Expected loss of msbs"
                            bulk_raddr.next = raddr
                        if waddr >= bulk:
                            rest_wenable.next = wenable
                            "lint_waive UNEQUAL_LEN INCREMENT: Expected loss of msbs"
                            rest_waddr.next = waddr - bulk
                        else:
                            bulk_wenable.next = wenable
                            "lint_waive UNEQUAL_LEN INCREMENT: Expected loss of msbs"
                            bulk_waddr.next = waddr

                    @always_comb
                    def split_data():
                        if bulk_renable_ff == 1:
                            odata.next = bulk_odata
                        else:
                            odata.next = rest_odata

                    zLat = multiflop(
                        bulk_renable,
                        bulk_renable_ff,
                        clk,
                        rstn,
                        depth=latency,
                        name=name + ".zLat",
                    )
                    return instances()
                else:
                    mem_raddr = [
                        Signal(intbv(0)[(md - 1).bit_length() : 0]) for _ in range(rows)
                    ]
                    rsel = Signal(intbv(0)[(rows - 1).bit_length() : 0])
                    rsel_ff = Signal(intbv(0)[(rows - 1).bit_length() : 0])
                    mem_waddr = [copySignal(mem_raddr[0]) for _ in range(rows)]
                    mem_renable = [Signal(intbv(0)[1:]) for _ in range(rows)]
                    mem_wenable = [Signal(intbv(0)[1:]) for _ in range(rows)]
                    mem_odata = [copySignal(idata) for _ in range(rows)]
                    shift = (md - 1).bit_length()
                    if not 1 << shift == md:
                        raise ValueError(
                            "Only even 2^n supported as max memory depth. md %s, shift %s, 1<<shift %s"
                            % (md, shift, 1 << shift)
                        )

                    zLat = multiflop(
                        rsel, rsel_ff, clk, rstn, depth=latency, name=name + ".zLat"
                    )

                    @always_comb
                    def split_addr():
                        rsel.next = 0
                        for i in range(rows):
                            mem_renable[i].next = 0
                            mem_wenable[i].next = 0
                            mem_raddr[i].next = 0
                            mem_waddr[i].next = 0
                        if raddr >> shift < rows:
                            rsel.next = raddr >> shift
                            mem_renable[raddr >> shift].next = renable
                            mem_raddr[raddr >> shift].next = raddr[shift:]
                            mem_wenable[waddr >> shift].next = wenable
                            mem_waddr[waddr >> shift].next = waddr[shift:]

                    @always_comb
                    def or_data():
                        odata.next = mem_odata[rsel_ff]

                    iMemrow = []
                    for i in range(rows):
                        iMemrow.append(
                            memory_signal(
                                idata,
                                mem_odata[i],
                                mem_waddr[i],
                                mem_raddr[i],
                                mem_wenable[i],
                                mem_renable[i],
                                clk,
                                rstn,
                                consistency_data,
                                depth=md,
                                write_through=write_through,
                                reset_value=reset_value,
                                pre_load=pre_load,
                                conf_load=conf_load,
                                input_flops=input_flops,
                                output_flops=output_flops,
                                top_instance=0,
                                hwconf=hwconf,
                                single_ported=single_ported,
                                wclk=wclk,
                                wrstn=wrstn,
                                name=name + ".iMemrow%d" % i,
                                no_io_flops=no_io_flops,
                                wmask=wmask,
                            )
                        )
                    return instances()
        else:
            print(
                "%s No slicing because mode=%s and max_depth=%s != None"
                % (name, hwconf.memory_mode, hwconf.memory_max_depth)
            )

    print(
        name,
        "flopmem=%d, limit=%s, size=%s"
        % (flopmem, hwconf.memory_flop_limit, width * depth),
    )
    if flopmem == 0:
        ############################
        # Create verilog memory instance
        #
        def mem_inst(
            idata,
            odata,
            wenable,
            renable,
            waddr,
            raddr,
            pre_load,
            reset_value,
            write_through,
            input_flops,
            output_flops,
            clk,
            rstn,
            wclk,
            wrstn,
            wmask,
        ):
            if wclk == None:
                iclk = clk
                irstn = rstn
            else:
                assert (
                    write_through == 0
                ), "%s write_through is not supported for dual clock memories"
                iclk = wclk
                irstn = wrstn

            idatam = copySignal(idata)
            odatam = copySignal(odata)
            raddrm = copySignal(raddr)
            waddrm = copySignal(waddr)
            renablem = Signal(intbv()[1:])
            wenablem = copySignal(wenable)
            wmaskm = copySignal(wmask) if wmask is not None else None

            oflop = []
            assert signalType(waddrm), "ERROR! %s address is not signal" % name
            if input_flops > 0:
                zMfid = multiflop(
                    idata,
                    idatam,
                    depth=input_flops,
                    clk=iclk,
                    rstn=irstn,
                    name=name + ".zMfid",
                )
                zMfra = multiflop(
                    raddr,
                    raddrm,
                    depth=input_flops,
                    clk=clk,
                    rstn=rstn,
                    name=name + ".zMfra",
                )
                zMfwa = multiflop(
                    waddr,
                    waddrm,
                    depth=input_flops,
                    clk=iclk,
                    rstn=irstn,
                    name=name + ".zMfwa",
                )
                zMfre = multiflop(
                    renable,
                    renablem,
                    depth=input_flops,
                    clk=clk,
                    rstn=rstn,
                    name=name + ".zMfre",
                )
                zMfwe = multiflop(
                    wenable,
                    wenablem,
                    depth=input_flops,
                    clk=iclk,
                    rstn=irstn,
                    name=name + ".zMfwe",
                )
                if wmask is not None:
                    zMfwm = multiflop(
                        wmask,
                        wmaskm,
                        depth=input_flops,
                        clk=iclk,
                        rstn=irstn,
                        name=f"{name}.zMfwm",
                    )
            else:

                @always_comb
                def zPassi():
                    idatam.next = idata
                    raddrm.next = raddr
                    waddrm.next = waddr
                    renablem.next = renable
                    wenablem.next = wenable

                if wmask is not None:

                    @always_comb
                    def zPassi_wm():
                        wmaskm.next = wmask

            if output_flops > 0:
                zMfod = multiflop(
                    odatam,
                    odata,
                    depth=output_flops,
                    clk=clk,
                    rstn=rstn,
                    name=name + ".zMfod",
                )
            else:

                @always_comb
                def zPasso():
                    odata.next = odatam

            data = Memory([0 for _ in range(depth)], width)
            collision = Signal(intbv(0)[1:])
            collision_d1 = Signal(intbv(0)[1:])
            odata_mem = Signal(intbv(0)[width:0])
            idata_d1 = Signal(intbv(0)[width:0])

            converting = myhdl.conversion._toVerilog._converting

            @always(iclk.posedge, irstn.negedge)
            def write():
                if irstn == 0:
                    idata_d1.next = 0
                else:
                    idata_d1.next = idatam

            @always(clk.posedge)  # PALINT no_rstn
            def mem_rdef():
                if renablem == 1:
                    odata_mem.next = data[raddrm]

            if wmask is None:

                @always(iclk.posedge)  # PALINT no_rstn
                def mem_wdef():
                    if converting:  # RTL generation
                        if wenablem == 1:
                            data[waddrm].next = idatam
                    else:  # myhdlsim
                        if wenablem == 1:
                            data[waddrm] = idatam

            else:
                wgran = width // len(wmask)
                write_slices = []
                # Does not work with Memory and slice assign for some reason:
                data = [Signal(intbv(0)[width:]) for _ in range(depth)]
                for ii in range(len(wmask)):

                    def f():
                        i = ii
                        k, j = (i + 1) * wgran, i * wgran
                        dm = ((1 << wgran) - 1) << (i * wgran)

                        @always(iclk.posedge)  # PALINT no_rstn
                        def mem_wdef():
                            if converting:  # RTL generation
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm].next[k:j] = idatam[k:j]
                            else:  # myhdlsim
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm] = (idatam & dm) | (data[waddrm] & ~dm)

                        return mem_wdef

                    write_slices.append(f())

            # Note that the yield() below is only because myhdl requires a
            # @instance to have a generator function.
            if converting:  # RTL generation
                if pre_load == 1 and reset_value >= 0:

                    @instance
                    def preload_inst():
                        for i in range(depth):
                            data[i].next = reset_value
                        yield None

            else:  # myhdlsim
                if pre_load == 1 and reset_value >= 0:

                    @instance
                    def preload_inst():
                        for i in range(depth):
                            data.mem[i] = reset_value
                        yield None

            if write_through == 1:
                collision_renable = Signal(intbv(0)[1:0])

                @always_comb
                def setcoll():
                    if wenablem == 1 and renablem == 1 and waddrm == raddrm:
                        collision.next = 1
                        collision_renable.next = 0
                    else:
                        collision.next = 0
                        # TODO(yian150218) Somewhere in a module memory read data
                        # is used without issuing renable, need to find it and set
                        # renable correctly, then make collision_renable.next = renable
                        collision_renable.next = renablem

                @always_comb
                def setodata():
                    if collision_d1 == 0:
                        odatam.next = odata_mem
                    else:
                        odatam.next = idata_d1

                zFfcoll = flop(
                    collision, collision_d1, clk, rstn, name=name + ".zFfcoll"
                )
            else:  # !write_through
                zPassout = pass_through(odata_mem, odatam, name=name + ".zPassout")

                @always(clk.posedge, rstn.negedge)
                def assertproc():
                    if rstn == 0:
                        pass
                    else:
                        if wenablem == 1 and renablem == 1 and raddrm == waddrm:
                            "synthesis translate_off"
                            print(
                                name,
                                "ERROR! Read and write to same address for a "
                                "non-write-through memory at address",
                                raddrm,
                            )
                            assert False
                            "synthesis translate_on"

            ################################################################
            # verilog_memory single ported
            #
            if hwconf.memory_mode == "verilog_memory" and single_ported:
                s = os.path.join(rootdir(), "modules/common/verilog_memory_1p.v")
                d = os.path.join(hwdir(), "hdl/verilog_memory_1p.v")
                assert wmask is None
                if not os.path.isfile(d):
                    print(name, "Copying", s, "to", d)
                    shutil.copy2(s, d)

                odata.driven = "wire"
                __verilog__ = dedent(
                    f"""
                    verilog_memory_1p #(
                         .width({width}),
                         .depth({depth}),
                         .depth_w({len(waddr)}),
                         .pre_load({pre_load}),
                         .reset_value({width}'h{reset_value:x}),
                         .input_flops({input_flops}),
                         .output_flops({output_flops}))
                         {name.replace(".", "_")}_meminst(
                             .idata(%(idata)s),
                             .odata(%(odata)s),
                             .addr(%(wenable)s ? %(waddr)s : %(raddr)s),
                             .enable(%(renable)s | %(wenable)s),
                             .wenable(%(wenable)s),
                             .clk(%(clk)s),
                             .rstn(%(rstn)s));
                """
                )

            ################################################################
            # verilog_memory dual-port, single clock
            #
            elif (
                hwconf.memory_mode == "verilog_memory"
                and not single_ported
                and wclk == None
            ):
                s = os.path.join(rootdir(), "modules/common/verilog_memory.v")
                d = os.path.join(hwdir(), "hdl/verilog_memory.v")
                if wmask is None:
                    mask_width = 1
                    mask_value = "1'b1"
                else:
                    mask_width = len(wmask)
                    mask_value = "%(wmask)s"
                if not os.path.isfile(d):
                    print(name, "Copying", s, "to", d)
                    shutil.copy2(s, d)

                odata.driven = "wire"
                reset = (
                    "%(rstn)s"
                    if hwconf.memory_connect_unused_rstn
                    or (input_flops > 0 or output_flops > 0 or write_through == 1)
                    else ""
                )

                __verilog__ = dedent(
                    f"""
                    verilog_memory #(
                         .width({width}),
                         .depth({depth}),
                         .depth_w({len(waddr)}),
                         .mask_w({mask_width}),
                         .pre_load({pre_load}),
                         .reset_value({width}'h{reset_value:x}),
                         .write_through({write_through}),
                         .input_flops({input_flops}),
                         .output_flops({output_flops}))
                         {name.replace(".", "_")}_meminst(
                             .idata(%(idata)s),
                             .odata(%(odata)s),
                             .waddr(%(waddr)s),
                             .raddr(%(raddr)s),
                             .wenable(%(wenable)s),
                             .wmask({mask_value}),
                             .renable(%(renable)s),
                             .clk(%(clk)s),
                             .rstn({reset}));
                """
                )

            ################################################################
            # verilog_memory dual-port, dual clock
            #
            elif hwconf.memory_mode == "verilog_memory" and not single_ported:
                s = os.path.join(rootdir(), "modules/common/verilog_memory_2c.v")
                d = os.path.join(hwdir(), "hdl/verilog_memory_2c.v")
                assert wmask is None
                if not os.path.isfile(d):
                    print(name, "Copying", s, "to", d)
                    shutil.copy2(s, d)

                odata.driven = "wire"
                __verilog__ = dedent(
                    f"""
                    verilog_memory_2c #(
                         .width({width}),
                         .depth({depth}),
                         .depth_w({len(waddr)}),
                         .pre_load({pre_load}),
                         .reset_value({width}'h{reset_value:x}),
                         .input_flops({input_flops}),
                         .output_flops({output_flops}))
                         {name.replace(".", "_")}_meminst(
                             .idata(%(idata)s),
                             .odata(%(odata)s),
                             .waddr(%(waddr)s),
                             .raddr(%(raddr)s),
                             .wenable(%(wenable)s),
                             .renable(%(renable)s),
                             .rclk(%(clk)s),
                             .wclk(%(iclk)s),
                             .rrstn(%(rstn)s),
                             .wrstn(%(irstn)s));
                """
                )

            ################################################################
            # ASIC macros - TODO: Single ported memories, and dual ported dual clock memories
            #
            elif hwconf.memory_mode in list(memory_ip.ips.keys()) and (
                wclk == None or hwconf.memory_disable_2c_ip == False
            ):
                from modules.common.Common import loadTcConf
                import pickle

                tcconf = loadTcConf()
                pfile = os.path.join(rundir(), "generated_instances.txt")
                if not os.path.isfile(pfile):
                    written = []
                else:
                    with open(pfile, "rb") as F:
                        written = pickle.load(F)

                from glob import glob

                if hwconf.memory_mode in list(memory_ip.ips.keys()):
                    ram = memory_ip.ips[hwconf.memory_mode]
                c = ram.conf(int(depth), int(width))
                print("Memory d %s, w %s, -> instance" % (depth, width), c)
                dest_dir = os.path.join(hwdir(), "hdl")
                source_dir = "/opt/ip/instances/%s/" % (hwconf.memory_mode)
                mname = ram.name(c["md"], c["mw"], c["mux"], c["type"])
                wname = "wrap_dp_d%s_w%s" % (depth, width)
                wsource = os.path.join(source_dir, "%s.v" % (wname))
                wdest = os.path.join(dest_dir, "%s.v" % (wname))
                print("mname", mname)
                print("wname", wname)
                if not os.path.isfile(wdest):
                    print("No wrapper file found %s" % wdest)
                    if True:  # Always rewrite the wrapper, it is silly to do otherwise
                        # (unless we handcraft memories, and we don't).
                        # not os.path.isfile(wsource) or (tcconf['memory_ip_rewrite']
                        # and wsource not in written):
                        # if not tcconf['memory_ip_rewrite']:
                        #    print "No wrapper source file found %s" % wsource
                        # else:
                        #    print "Re-writing %s" % wsource
                        written.append(wsource)

                        wrap_file = os.path.join(
                            rootdir(), f"modules/common/{hwconf.memory_mode}_memory.v"
                        )
                        print("Generating %s" % (wsource))
                        print("Reading ", wrap_file)
                        with open(wrap_file, "r") as F:
                            wrap = F.readlines()
                        wrap_tmp = [w.replace("REPLACE_WNAME", wname) for w in wrap]
                        wrap = [w.replace("REPLACE_MNAME", mname) for w in wrap_tmp]
                        print("WRAP_FILE")
                        with open("%s.v" % wname, "w") as F:
                            for l in wrap:
                                F.write("%s\n" % l.rstrip())
                        print("Copying wrap source to %s" % wsource)
                        if os.path.isfile(wsource):
                            os.remove(wsource)
                        shutil.move("%s.v" % wname, wsource)
                        os.chmod(wsource, 0o666)
                    # Generate the memories
                    msource = os.path.join(source_dir, mname)
                    mdest = os.path.join(dest_dir, mname)
                    print("Checking for %s" % (mdest))
                    cmd = ram.cmd_mux(
                        depth=c["md"],
                        width=c["mw"],
                        mux=c["mux"],
                        type=c["type"],
                        target="all",
                    )
                    amem.add_cmd(cmd)
                    if not os.path.isfile("%s.v" % mdest):
                        print(
                            "No %s memory found. Copying from %s"
                            % ("%s.v" % mdest, msource)
                        )
                        cmd = ram.cmd_mux(
                            depth=c["md"],
                            width=c["mw"],
                            mux=c["mux"],
                            type=c["type"],
                            target=hwconf.memory_target,
                        )
                        if (
                            (
                                not os.path.isfile("%s.v" % msource)
                                and hwconf.memory_target in ["model", "all"]
                            )
                            or (
                                not glob("%s*.dat" % msource)
                                and hwconf.memory_target in ["txt", "all"]
                            )
                            or (
                                not glob("%s*.lib" % msource)
                                and hwconf.memory_target in ["lib", "all"]
                            )
                            or (tcconf["memory_ip_rewrite"] and msource not in written)
                        ):
                            if not tcconf["memory_ip_rewrite"]:
                                print(
                                    "No %s source memory found. Generating..."
                                    % ("%s.v" % msource)
                                )
                            else:
                                print("Re-writing %s.v" % msource)
                                written.append(msource)

                            print(name, "Generating", msource)
                            print("Running %s generator" % hwconf.memory_mode)
                            print(cmd)
                            try:
                                call(cmd, shell=True)
                            except:
                                raise ValueError("Unable to generate %s" % mname)
                            files = glob("%s*" % mname)
                            for file in files:
                                print("Moving", file)
                                os.chmod(file, 0o666)
                                d = os.path.join(source_dir, file)
                                if os.path.exists(d):
                                    os.remove(d)
                                shutil.copy2(file, d)
                                os.remove(file)
                        print(name, "Copying", msource, "to", mdest)
                        files = glob("%s*" % msource)
                        for file in files:
                            print("Copying", file)
                            shutil.copy2(file, dest_dir)
                print("Generated instances:")
                for w in written:
                    print("  %s", w)
                with open(pfile, "wb") as F:
                    pickle.dump(written, F)
                shutil.copy2(wsource, dest_dir)

                assert c["rows"] == 1, (
                    "ERROR! The asic memory wrappers should never use rows>1. "
                    "The addresses should have been split earlier in memory.py "
                    "Instance: %s, rows %s" % (wname, c["rows"])
                )
                rf = 0
                if c["type"] == "rf":
                    rf = 1
                odata.driven = "wire"
                __verilog__ = dedent(
                    f"""
                    {wname} #(
                         .width({width}),
                         .depth({depth}),
                         .depth_w({len(waddr)}),
                         .write_through({write_through}),
                         .mw({c["mw"]}),
                         .md({c["md"]}),
                         .md_w({(c["md"]-1).bit_length()}),
                         .cols({c["cols"]}),
                         .rows({c["rows"]}),
                         .input_flops({input_flops}),
                         .output_flops({output_flops}),
                         .rf({rf}))
                         meminst_{name.replace(".", "_")}(
                             .idata(%(idata)s),
                             .odata(%(odata)s),
                             .waddr(%(waddr)s),
                             .raddr(%(raddr)s),
                             .wenable(%(wenable)s),
                             .renable(%(renable)s),
                             .clk(%(clk)s),
                             .rstn(%(rstn)s));
                """
                )

            return instances()

        #
        # Create verilog memory instance
        ############################
        zmem = mem_inst(
            idata,
            odata,
            wenable,
            renable,
            waddr,
            raddr,
            pre_load,
            reset_value,
            write_through,
            input_flops,
            output_flops,
            clk,
            rstn,
            wclk,
            wrstn,
            wmask,
        )
        if single_ported:
            memtype = "sp"
        elif wclk == None:
            memtype = "dp"
        else:
            memtype = "dc"
        dic = {
            "type": memtype,
            "width": width,
            "depth": depth,
            "write_through": write_through,
            "write_mask": len(wmask) if wmask != None else None,
            "input_flops": input_flops,
            "output_flops": output_flops,
            "name": name,
        }
        print("Adding memory to macro library", dic)

        amem.add_mem(dic)

    else:  # flopmem==1
        idatam = copySignal(idata)
        odatam = copySignal(odata)
        raddrm = copySignal(raddr)
        waddrm = copySignal(waddr)
        renablem = Signal(intbv()[1:])
        wenablem = copySignal(wenable)
        wmaskm = copySignal(wmask) if wmask is not None else None
        oflop = []
        idaflop = multiflop(idata, idatam, iclk, irstn, depth=input_flops)
        iraflop = multiflop(raddr, raddrm, clk, rstn, depth=input_flops)
        iwaflop = multiflop(waddr, waddrm, iclk, irstn, depth=input_flops)
        ireflop = multiflop(renable, renablem, clk, rstn, depth=input_flops)
        iweflop = multiflop(wenable, wenablem, iclk, irstn, depth=input_flops)
        oflop.append(multiflop(odatam, odata, clk, rstn, depth=output_flops))

        data = Memory([0 for _ in range(depth)], width)
        assert depth > 0, "ERROR! %s depth %s needs to be > 0" % (name, depth)

        converting = myhdl.conversion._toVerilog._converting

        if wclk is None:
            if wmask is None:

                @always(clk.posedge, rstn.negedge)
                def mem_def():
                    if rstn == 0:
                        for i in range(depth):
                            if converting:  # RTL generation
                                data[i].next = reset_value
                            else:  # myhdlsim
                                data[i] = reset_value
                        odatam.next = 0
                    else:
                        if wenablem == 1:
                            if converting:  # RTL generation
                                data[waddrm].next = idatam
                            else:  # myhdlsim
                                data[waddrm] = idatam
                        if write_through == 1 and wenablem == 1 and raddrm == waddrm:
                            odatam.next = idatam
                        elif renablem == 1:
                            odatam.next = data[raddrm]

            else:
                assert not write_through
                # Memory and slice assignment does not work:
                data = [Signal(intbv(0)[width:]) for _ in range(depth)]

                @always(clk.posedge, rstn.negedge)
                def mem_rdef():
                    if rstn == 0:
                        odatam.next = 0
                    else:
                        if renablem == 1:
                            odatam.next = data[raddrm]

                wgran = width // len(wmask)
                write_slices = []
                for ii in range(len(wmask)):

                    def f():
                        i = ii
                        k, j = (i + 1) * wgran, i * wgran
                        dm = ((1 << wgran) - 1) << (i * wgran)

                        @always(clk.posedge)  # PALINT no_rstn
                        def mem_wdef():
                            if converting:  # RTL generation
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm].next[k:j] = idatam[k:j]
                            else:  # myhdlsim
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm] = (idatam & dm) | (data[waddrm] & ~dm)

                        return mem_wdef

                    write_slices.append(f())

        else:

            @always(clk.posedge, rstn.negedge)
            def mem_rdef():
                if rstn == 0:
                    odatam.next = 0
                else:
                    if renablem == 1:
                        odatam.next = data[raddrm]

            if wmask is None:

                @always(iclk.posedge)  # PALINT no_rstn
                def mem_wdef():
                    if wenablem == 1:
                        if converting:  # RTL generation
                            data[waddrm].next = idatam
                        else:  # myhdlsim
                            data[waddrm] = idatam

            else:
                wgran = width // len(wmask)
                write_slices = []
                for ii in range(len(wmask)):

                    def f():
                        i = ii
                        k, j = (i + 1) * wgran, i * wgran
                        dm = ((1 << wgran) - 1) << (i * wgran)

                        @always(iclk.posedge)  # PALINT no_rstn
                        def mem_wdef():
                            if converting:  # RTL generation
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm].next[k:j] = idatam
                            else:  # myhdlsim
                                if wenablem == 1 and wmaskm[i] == 1:
                                    data[waddrm] = (idatam & dm) | (data[waddrm] & ~dm)

                        return mem_wdef

                    write_slices.append(f())

    return instances()
