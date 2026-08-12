"""
test_conf_interface.py
======================
MyHDL simulation tests for the liteRISC conf_slave / conf_master interface.

Boot sequence
-------------
The ROM at IMEM[0..15] runs boot-read-interrupt.lisp, which:
  1. Reads a byte from conf_map.interrupt
  2. When non-zero, jumps to byte address PROG_BASE (512) in IMEM

All test programs are therefore loaded into IMEM at PROG_BASE via
slave writes (one byte per write), and execution is triggered by
writing any non-zero value to conf_map.interrupt.

Address arithmetic
------------------
  IMEM  : byte addresses  0 .. IMEM_HIGH  (slave writes go here when addr <= IMEM_HIGH)
  DMEM  : byte addresses  DMEM_LOW .. DMEM_HIGH

  Slave DMEM addr S (read or write)  ->  dp_mem[S - conf_map.dmem_low]
  CPU DMEM phys byte addr P          ->  dp_mem[P - DMEM_LOW]

  So slave addr S and CPU addr P access the same dp_mem word when:
      S - conf_map.dmem_low = P - DMEM_LOW
  With the default conf_map (conf_map.dmem_low == DMEM_LOW), this
  collapses to S = P -- no extra offset needed on the CPU side.

  For slave addr to be a DMEM write (not IMEM), S must be > IMEM_HIGH.
  The lowest safe value: S = IMEM_HIGH+1 (== DMEM_LOW == conf_map.dmem_low).

  CONF / master port
  ------------------
  The CONF window is CONF_LOW..CONF_HIGH (0x10000..0xFFFFFFFF).
  CPU master requests: slave_addr = (cpu_addr - CONF_LOW) // 4
  The window is large enough to reach DMEM slave addresses (> IMEM_HIGH).
  test_dual_cpu uses SLAVE_RESULT0 as the rendezvous word on B's DMEM.

Tests
-----
  1. test_slave_dmem_rw            -- pure slave DMEM write / read roundtrip
  2. test_cpu_stores_constant      -- CPU stores 42 to DMEM, slave reads
  3. test_slave_write_cpu_doubles  -- slave writes 100, CPU doubles it
  4. test_slave_write_cpu_sum      -- slave writes two inputs, CPU adds them
  5. test_master_request           -- CPU issues a master read from CONF space
  6. test_dual_cpu                 -- CPU-A writes to CPU-B's DMEM via master port
  7. test_wait_ticks               -- CPU accumulates tick counts
  8. test_master_while_slave_request -- simultaneous master reply + slave write
  9. test_cpu_reset                -- CPU reset via conf_map.cpu_reset
"""

import sys
import os
import random
from myhdl import *
from modules.common.signal import signal
from conf_map import ConfMap
from axi import Axi4
from conf import Conf
from cpu_sys import cpu_sys
from constants import (
    INTERRUPT_ADDRESS, IMEM_HIGH, DMEM_LOW, DMEM_HIGH,
    TICK_ADDRESS, CPU_RESET_ADDRESS,
    CONF_LOW, CONF_HIGH,
    compute_memory_map,
)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from asm import assemble

# ---------------------------------------------------------------------------
# Address constants
# ---------------------------------------------------------------------------

PROG_BASE = 512  # IMEM byte address where user programs are loaded

# Slave-side addresses: must be > IMEM_HIGH to land in DMEM.
# CPU-side addresses  : slave_addr + DMEM_LOW.
SLAVE_RESULT0 = IMEM_HIGH + 1        # first word of DMEM via slave
SLAVE_RESULT1 = SLAVE_RESULT0 + 4
SLAVE_INPUT0  = SLAVE_RESULT0 + 8
SLAVE_INPUT1  = SLAVE_RESULT0 + 12

CPU_RESULT0   = SLAVE_RESULT0
CPU_RESULT1   = SLAVE_RESULT1
CPU_INPUT0    = SLAVE_INPUT0
CPU_INPUT1    = SLAVE_INPUT1

# CONF / master port: CONF_LOW..CONF_HIGH, large enough to reach DMEM on B.
# CPU master requests: slave_addr = (cpu_addr - CONF_LOW) // 4
# Use SLAVE_RESULT0 as the rendezvous — it's > IMEM_HIGH so lands in B's DMEM.
DUAL_CPU_TARGET_CONF   = CONF_LOW + SLAVE_RESULT0      # no *4, master no longer divides by 4
DUAL_CPU_SLAVE_ADDR_B  = SLAVE_RESULT0                  # slave sees byte offset directly

# ---------------------------------------------------------------------------
# Assemble test programs
# ---------------------------------------------------------------------------

print("Assembling test programs via Lisp assembler ...")

_PROG_STORE_CONSTANT = assemble(
    """
    (Rx= RESULT_PHYS R1)
    (Rx= 42 R0)
    (A=Rx R0)
    (M[Rx]=A R1)
    (label done)
    (j done)
    """,
    RESULT_PHYS=CPU_RESULT0,
)

_PROG_READ_DOUBLE_WRITE = assemble(
    """
    (Rx= INPUT_PHYS R0)
    (A=M[Rx] R0)
    (lsl-a)
    (Rx= RESULT_PHYS R1)
    (M[Rx]=A R1)
    (label done)
    (j done)
    """,
    INPUT_PHYS=CPU_INPUT0,
    RESULT_PHYS=CPU_RESULT0,
)

_PROG_ADD_TWO = assemble(
    """
    (Rx= INPUT0_PHYS R0)
    (A=M[Rx] R0)
    (Rx=A R2)
    (Rx= INPUT1_PHYS R1)
    (A=M[Rx] R1)
    (A+=Rx R2)
    (Rx= RESULT_PHYS R3)
    (M[Rx]=A R3)
    (label done)
    (j done)
    """,
    INPUT0_PHYS=CPU_INPUT0,
    INPUT1_PHYS=CPU_INPUT1,
    RESULT_PHYS=CPU_RESULT0,
)

_PROG_MASTER_REQUEST_ADDRESS_0 = assemble(
    """
    (Rx= CONF_LOW R0)
    (A=M[Rx] R0)
    (label done)
    (j done)
    """,
    CONF_LOW=CONF_LOW,
)

_PROG_READ_TICKS = assemble(
    """
    (Rx= TICK_ADDRESS R0)
    (Rx= 5 R3)
    (A=Rx R3)
    (M[Rx]=A R0)
    (Rx= RESULT_PHYS R1)
    (Rx= 0 R2)
    (label main-loop)
    (A=M[Rx] R0)
    (A=Rx R2)
    (A+=Rx R3)
    (Rx=A R2)
    (M[Rx]=A R1)
    (j main-loop)
    """,
    TICK_ADDRESS=TICK_ADDRESS,
    RESULT_PHYS=CPU_RESULT0,
)

print(f"  SLAVE_RESULT0         : slave=0x{SLAVE_RESULT0:04X}  cpu=0x{CPU_RESULT0:04X}")
print(f"  SLAVE_INPUT0          : slave=0x{SLAVE_INPUT0:04X}  cpu=0x{CPU_INPUT0:04X}")
print(f"  DUAL_CPU_TARGET_CONF  : cpu_a=0x{DUAL_CPU_TARGET_CONF:05X}  slave_b=0x{DUAL_CPU_SLAVE_ADDR_B:04X}")
print(f"  PROG_STORE_CONSTANT   : {len(_PROG_STORE_CONSTANT)} bytes")
print(f"  PROG_READ_DOUBLE_WRITE: {len(_PROG_READ_DOUBLE_WRITE)} bytes")
print(f"  PROG_ADD_TWO          : {len(_PROG_ADD_TWO)} bytes")

# ---------------------------------------------------------------------------
# Simulation helpers
# ---------------------------------------------------------------------------

def _write_data(conf, clk, data, address):
    """Generator: slave single write, waits for reply."""
    conf.slave_request_data.next    = data
    conf.slave_request_address.next = address
    conf.slave_request_we.next      = 1
    yield clk.posedge
    conf.slave_request_we.next = 0
    while conf.slave_reply_status == 0:
        yield clk.posedge


def _read_data(conf, clk, address, out):
    """Generator: slave single read, writes result into out[0]."""
    conf.slave_request_address.next = address
    conf.slave_request_re.next      = 1
    yield clk.posedge
    conf.slave_request_re.next = 0
    while conf.slave_reply_status == 0:
        yield clk.posedge
    out[0] = int(conf.slave_reply_data)

def print_program_hex(name, prog_bytes):
    """Print an assembled program as a hexdump."""
    print(f"\n{name} ({len(prog_bytes)} bytes):")
    for i, byte in enumerate(prog_bytes):
        if i % 16 == 0:
            print(f"  {i:08X}: ", end="")
        print(f"{byte:02X}", end=" ")
        if i % 16 == 15:
            print()
    if len(prog_bytes) % 16 != 0:
        print()

# ---------------------------------------------------------------------------
# Individual test runners
# ---------------------------------------------------------------------------

def test_slave_dmem_rw():
    """Test 1: slave DMEM write/read roundtrip (no CPU program needed)."""
    TEST_VALUE = 0xCAFEBABE
    result = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            yield _write_data(conf, clk, TEST_VALUE, SLAVE_RESULT0)

            readback = [0]
            yield _read_data(conf, clk, SLAVE_RESULT0, readback)

            if readback[0] == TEST_VALUE:
                result[0] = "PASS"
            else:
                result[0] = f"FAIL: expected 0x{TEST_VALUE:08X}, got 0x{readback[0]:08X}"

            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_slave_dmem_rw'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(100000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_slave_dmem_rw" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_cpu_stores_constant():
    """Test 2: load program via slave, CPU stores 42 to DMEM, slave reads back."""
    EXPECTED  = 42
    MAX_POLLS = 400
    result    = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_STORE_CONSTANT:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(MAX_POLLS):
                if i % 5 != 0:
                    yield clk.posedge
                    continue
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read 0x{readback[0]:08X})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_cpu_stores_constant'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_cpu_stores_constant" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_boot_code_from_file():
    """Test: cpu_sys(boot_code_path=...) loads the boot ROM from a
    $readmemh-format hex file instead of the built-in hardcoded boot code,
    and the CPU actually executes what was loaded.

    The file replaces the *entire* boot ROM, so the program in it runs
    straight from reset (PC=0) -- no slave-triggered interrupt/jump needed,
    unlike the other tests which load a user program at PROG_BASE under the
    stock boot-read-interrupt ROM.
    """
    EXPECTED  = 42
    MAX_POLLS = 400
    result    = [None]

    boot_hex_path = "boot_code.hex"

    prog = assemble(
        """
        (Rx= RESULT_PHYS R1)
        (Rx= 42 R0)
        (A=Rx R0)
        (M[Rx]=A R1)
        (label done)
        (j done)
        """,
        RESULT_PHYS=CPU_RESULT0,
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map,
                        boot_code_path=boot_hex_path)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            readback = [0]
            for i in range(MAX_POLLS):
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read 0x{readback[0]:08X})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_boot_code_from_file'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_boot_code_from_file" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_slave_write_cpu_doubles():
    """Test 3: slave writes input, CPU reads and doubles it, slave reads result."""
    INPUT_VALUE = 100
    EXPECTED    = INPUT_VALUE << 1
    MAX_POLLS   = 400
    result      = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_READ_DOUBLE_WRITE:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, INPUT_VALUE, SLAVE_INPUT0)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_slave_write_cpu_doubles'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_slave_write_cpu_doubles" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_slave_write_cpu_sum():
    """Test 4: slave writes two inputs, CPU adds them, slave reads sum."""
    INPUT_A   = 37
    INPUT_B   = 63
    EXPECTED  = INPUT_A + INPUT_B
    MAX_POLLS = 400
    result    = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_ADD_TWO:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, INPUT_A, SLAVE_INPUT0)
            yield _write_data(conf, clk, INPUT_B, SLAVE_INPUT1)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(MAX_POLLS):
                if i % 5 != 0:
                    yield clk.posedge
                    continue
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_slave_write_cpu_sum'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_slave_write_cpu_sum" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_conf_map_variant(imem_depth, dmem_depth, imem_low, dmem_low, label):
    """Test: slave writes two inputs, CPU adds them, slave reads sum -- same
    shape as test_slave_write_cpu_sum, but with a conf_map whose imem_low/
    dmem_low are nonzero (and, for the "gapped" variant, not even adjacent
    to each other). conf_slave_imem_wadr/conf_slave_dmem_wadr are computed
    as `slave_pending_address - conf_map.imem_low/dmem_low`, entirely
    independent of cpu_sys's own internal address decode (which always
    starts IMEM at physical 0) -- so the slave-side base is a pure
    addressing convention that must be exercised at nonzero/non-contiguous
    values, not just the imem_low=0 default every other test uses.

    imem_low/dmem_low: slave-bus base addresses for IMEM/DMEM (arbitrary,
    independent of each other and of the CPU's own zero-based view).
    """
    mm = compute_memory_map(imem_depth, dmem_depth)  # CPU-side view: IMEM_LOW=0, DMEM_LOW=imem_depth

    imem_high = imem_low + imem_depth - 1
    dmem_high = dmem_low + dmem_depth - 1

    # Physical DMEM word offsets used by this test (same word, addressed
    # differently by the slave bus vs. the CPU's own program).
    phys_result0 = 0x10
    phys_input0  = 0x18
    phys_input1  = 0x1C

    slave_result0 = dmem_low + phys_result0
    slave_input0  = dmem_low + phys_input0
    slave_input1  = dmem_low + phys_input1

    cpu_result0 = mm.DMEM_LOW + phys_result0
    cpu_input0  = mm.DMEM_LOW + phys_input0
    cpu_input1  = mm.DMEM_LOW + phys_input1

    INPUT_A   = 37
    INPUT_B   = 63
    EXPECTED  = INPUT_A + INPUT_B
    MAX_POLLS = 400
    result    = [None]

    prog = assemble(
        """
        (Rx= INPUT0_PHYS R0)
        (A=M[Rx] R0)
        (Rx=A R2)
        (Rx= INPUT1_PHYS R1)
        (A=M[Rx] R1)
        (A+=Rx R2)
        (Rx= RESULT_PHYS R3)
        (M[Rx]=A R3)
        (label done)
        (j done)
        """,
        INPUT0_PHYS=cpu_input0,
        INPUT1_PHYS=cpu_input1,
        RESULT_PHYS=cpu_result0,
    )

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap(
            imem_low=imem_low, imem_high=imem_high,
            dmem_low=dmem_low, dmem_high=dmem_high,
            interrupt=dmem_high + 1, cpu_reset=dmem_high + 2,
        )
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map,
                        imem_depth=imem_depth, dmem_depth=dmem_depth)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = imem_low + PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, INPUT_A, slave_input0)
            yield _write_data(conf, clk, INPUT_B, slave_input1)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(MAX_POLLS):
                if i % 5 != 0:
                    yield clk.posedge
                    continue
                readback = [0]
                yield _read_data(conf, clk, slave_result0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = f'trace_conf_map_{label}'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_conf_map_variant[{label}]" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_conf_map_offset():
    """Test 5a: conf_map with a nonzero, contiguous slave-bus base --
    imem_low=0x1000, dmem immediately following imem (dmem_low=imem_high+1)."""
    imem_depth = 2048
    imem_low   = 0x1000
    dmem_low   = imem_low + imem_depth
    return test_conf_map_variant(imem_depth=imem_depth, dmem_depth=4096,
                                  imem_low=imem_low, dmem_low=dmem_low,
                                  label="offset")


def test_conf_map_gapped():
    """Test 5b: conf_map with nonzero, non-contiguous imem_low/dmem_low --
    a deliberate gap between the IMEM and DMEM windows on the slave bus."""
    return test_conf_map_variant(imem_depth=16384, dmem_depth=65536,
                                  imem_low=0x10000, dmem_low=0x40000,
                                  label="gapped")


def test_master_request():
    """Test 5: Master reads address CONF_LOW and puts the result into accumulator."""
    result    = [None]
    MAX_POLLS = 200

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_MASTER_REQUEST_ADDRESS_0:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            yield clk.posedge
            yield clk.posedge
            yield clk.posedge

            conf.master_reply_data.next = 42
            conf.master_reply_status.next = 1
            conf.master_reply_id.next = 0

            yield clk.posedge

            conf.master_reply_data.next = 0
            conf.master_reply_status.next = 0
            conf.master_reply_data.next = 0

            yield clk.posedge
            yield clk.posedge
            yield clk.posedge
            yield clk.posedge
            yield clk.posedge

            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_master_request'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_master_request" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_wait_ticks():
    """Test 6: CPU selects tick bit 1, waits for it, stores count to DMEM.
    TB drives conf.ticks every 8 cycles. We verify DMEM result increments."""
    result = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        cycle_count = Signal(intbv(0)[32:])

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def drive_ticks():
            cycle_count.next = cycle_count + 1
            # pulse ticks[0] every 8 cycles
            if cycle_count % 8 == 0:
                conf.ticks.next = conf.ticks + 1

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_READ_TICKS:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            # wait long enough for several ticks and loop iterations
            MAX_POLLS = 500
            prev_readback = [None]
            changes_seen  = [0]

            for i in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if prev_readback[0] is None:
                    prev_readback[0] = readback[0]
                elif readback[0] != prev_readback[0]:
                    changes_seen[0] += 1
                    prev_readback[0] = readback[0]
                    if changes_seen[0] >= 3:
                        result[0] = "PASS"
                        raise StopSimulation()
                yield clk.posedge

            result[0] = (f"FAIL: only saw {changes_seen[0]} changes, "
                         f"last value=0x{readback[0]:08X}")
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_wait_ticks'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(2000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_wait_ticks" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_dual_cpu():
    """
    Two cpu_sys instances with conf interfaces cross-connected:
      CPU-A master  -> CPU-B slave
      CPU-B master  -> CPU-A slave  (not used here, but wired for completeness)

    CPU-A writes a value to CPU-B's DMEM via the master (CONF) port,
    then the TB reads it back via CPU-B's slave port.

    Note: The CONF window is CONF_LOW..CONF_HIGH (0x10000..0xFFFFFFFF).
    The master port maps: slave_addr = cpu_addr - CONF_LOW
    SLAVE_RESULT0 > IMEM_HIGH so it lands in B's DMEM.
    DUAL_CPU_TARGET_CONF = CONF_LOW + SLAVE_RESULT0.
    """
    result = [None]

    def tb():
        clk   = Signal(bool())
        rstn  = signal()
        axi_a = Axi4(asize=16, dsize=32, idsize=1)
        axi_b = Axi4(asize=16, dsize=32, idsize=1)
        conf_a = Conf()
        conf_b = Conf()

        instr_trace_a = Signal(modbv(0)[69:])
        conf_map_a = ConfMap()
        icpu_a = cpu_sys(clk, rstn, axi_a, conf_a, instr_trace_a, conf_map=conf_map_a)
        instr_trace_b = Signal(modbv(0)[69:])
        conf_map_b = ConfMap()
        icpu_b = cpu_sys(clk, rstn, axi_b, conf_b, instr_trace_b, conf_map=conf_map_b)

        # --- cross-connect A master -> B slave ----------------------------
        @always_comb
        def a_to_b():
            # request path: A master -> B slave
            conf_b.slave_request_address.next = conf_a.master_request_address
            conf_b.slave_request_data.next    = conf_a.master_request_data
            conf_b.slave_request_id.next      = conf_a.master_request_id
            conf_b.slave_request_we.next      = conf_a.master_request_we
            conf_b.slave_request_re.next      = conf_a.master_request_re
            # reply path: B slave -> A master
            conf_a.master_reply_data.next     = conf_b.slave_reply_data
            conf_a.master_reply_status.next   = conf_b.slave_reply_status
            conf_a.master_reply_id.next       = conf_b.slave_reply_id

        @instance
        def tieoff():
            conf_b.master_reply_data.next   = 0
            conf_b.master_reply_status.next = 0
            conf_b.master_reply_id.next     = 0
            conf_a.slave_request_address.next = 0
            conf_a.slave_request_data.next    = 0
            conf_a.slave_request_id.next      = 0
            conf_a.slave_request_we.next      = 0
            conf_a.slave_request_re.next      = 0
            yield clk.posedge  # hold forever after first delta

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def inc_ticks():
            conf_a.ticks.next = conf_a.ticks + 1
            conf_b.ticks.next = conf_b.ticks + 1

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # CPU-A program: write EXPECTED to CPU-B's DMEM via CONF port,
            # then spin. DUAL_CPU_TARGET_CONF >= CONF_LOW so it
            # triggers a master write. B's slave sees slave_addr=DUAL_CPU_SLAVE_ADDR_B.
            EXPECTED = 0xABCD

            prog = assemble(
                """
                (Rx= TARGET R0)
                (Rx= EXPECTED R1)
                (A=Rx R1)
                (M[Rx]=A R0)
                (label done)
                (j done)
                """,
                TARGET=DUAL_CPU_TARGET_CONF,
                EXPECTED=EXPECTED,
            )

            print(DUAL_CPU_TARGET_CONF)

            # write program into CPU-A's IMEM starting at PROG_BASE
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf_a, clk, byte, addr)
                addr += 1

            # trigger CPU-A boot jump to PROG_BASE
            yield _write_data(conf_a, clk, 1, conf_map_a.interrupt)

            for _ in range(50):
                yield clk.posedge

            MAX_POLLS = 600
            for _ in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf_b, clk, DUAL_CPU_SLAVE_ADDR_B, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout (last read 0x{readback[0]:08X})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_dual_cpu'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(1000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_dual_cpu" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_master_while_slave_request():
    """
    Test 7: CPU-A issues a master (Conf) read request. While CPU-A is waiting
    for the reply, an external slave_request arrives on conf.  This races
    against the master reply to check whether the CPU correctly handles both
    without dropping either.
    """
    result = [None]

    def tb():
        clk   = Signal(bool())
        rstn  = signal()
        axi = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()

        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        MASTER_REPLY_VALUE = 0x1234
        SLAVE_WRITE_VALUE  = 0x5678

        prog = assemble(
            """
            (Rx= CONF_LOW R0)
            (A=M[Rx] R0)
            (Rx= RESULT_PHYS R1)
            (M[Rx]=A R1)
            (label done)
            (j done)
            """,
            CONF_LOW=CONF_LOW,
            RESULT_PHYS=CPU_RESULT0,
        )

        @instance
        def seq():
            rstn.next = 0
            conf.master_reply_data.next   = 0
            conf.master_reply_status.next = 0
            conf.master_reply_id.next     = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # load program into IMEM
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            for _ in range(20):
                yield clk.posedge

            # trigger boot jump
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            # wait for CPU to reach the IO read and stall
            for _ in range(20):
                yield clk.posedge

            # inject a slave_request while master reply is still pending
            print("TB: issuing slave write while master reply pending")
            yield _write_data(conf, clk, SLAVE_WRITE_VALUE, SLAVE_RESULT1)

            # now deliver the master reply
            print("TB: delivering master reply")
            conf.master_reply_data.next   = MASTER_REPLY_VALUE
            conf.master_reply_status.next = 1
            conf.master_reply_id.next     = 0
            yield clk.posedge
            conf.master_reply_data.next   = 0
            conf.master_reply_status.next = 0

            # poll for CPU to store the master reply value into RESULT0
            MAX_POLLS = 400
            for i in range(MAX_POLLS):
                readback_result = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback_result)
                if readback_result[0] == MASTER_REPLY_VALUE:
                    break
                yield clk.posedge
            else:
                result[0] = (f"FAIL: master reply never stored; "
                             f"RESULT0=0x{readback_result[0]:08X}")
                raise StopSimulation()

            # also verify the slave write landed correctly
            readback_slave = [0]
            yield _read_data(conf, clk, SLAVE_RESULT1, readback_slave)
            if readback_slave[0] != SLAVE_WRITE_VALUE:
                result[0] = (f"FAIL: slave write corrupted; "
                             f"RESULT1=0x{readback_slave[0]:08X} "
                             f"expected 0x{SLAVE_WRITE_VALUE:08X}")
                raise StopSimulation()

            result[0] = "PASS"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_master_while_slave'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(1000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_master_while_slave_request" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_cpu_reset():
    """Test 8: CPU runs a program that increments a DMEM value once then halts.
    After verifying the first increment, reset the CPU via conf_map.cpu_reset,
    reload the program, trigger it again, and verify a second increment."""
    result = [None]

    _PROG_INCREMENT_ONCE = assemble(
        """
        (Rx= RESULT_PHYS R0)
        (A=M[Rx] R0)
        (Rx= 1 R1)
        (A+=Rx R1)
        (M[Rx]=A R0)
        (label done)
        (j done)
        """,
        RESULT_PHYS=CPU_RESULT0,
    )

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # initialise result slot to 0
            yield _write_data(conf, clk, 0, SLAVE_RESULT0)

            # --- first run ---
            addr = PROG_BASE
            for byte in _PROG_INCREMENT_ONCE:
                yield _write_data(conf, clk, byte, addr)
                addr += 1
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            MAX_POLLS = 400
            for _ in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == 1:
                    break
                yield clk.posedge
            else:
                result[0] = f"FAIL: first increment never happened (got 0x{readback[0]:08X})"
                raise StopSimulation()

            # --- reset CPU ---
            yield _write_data(conf, clk, 1, conf_map.cpu_reset)
            # wait a few cycles for reset to propagate and boot ROM to start
            for _ in range(20):
                yield clk.posedge

            # --- second run: trigger again (program still in IMEM) ---
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for _ in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == 2:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: second increment never happened (got 0x{readback[0]:08X}, expected 2)"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_cpu_reset'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(1000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_cpu_reset" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_cpu_memory_access_slave_conflict(slave_gap_cycles=0):
    """
    Test 9: CPU writes distinct values to N different DMEM addresses while
    the TB simultaneously hammers the slave port with writes/reads to a
    separate DMEM region. Verifies that neither side's accesses are dropped
    or corrupted by the contention.

    slave_gap_cycles: idle clock cycles between consecutive slave
    transactions during the hammer phase. 0 = back-to-back (slave port
    saturated, CPU may starve for DMEM access and never make progress).
    Larger values give the CPU windows to fetch/execute between accesses.

    Memory layout (slave addresses, all > IMEM_HIGH so they land in DMEM):
      SLAVE_DONE      : done flag, CPU writes DONE_MAGIC here when finished
      CPU region      : SLAVE_CPU_BASE  + 4*i   (written by CPU program)
      TB region       : SLAVE_TB_BASE   + 4*i   (written by TB via slave port)

    Verification: after done flag is seen, TB reads back both regions via
    the slave port and compares against expected values.
    """
    N          = 16
    DONE_MAGIC = 0xD1

    SLAVE_DONE     = SLAVE_RESULT0
    SLAVE_CPU_BASE = SLAVE_RESULT0 + 0x100
    SLAVE_TB_BASE  = SLAVE_RESULT0 + 0x200

    CPU_DONE     = SLAVE_DONE
    CPU_CPU_BASE = SLAVE_CPU_BASE

    def cpu_val(i):
        return 0xA000 + i * 3 + 1

    def tb_val(i):
        return 0x5000 + i * 7 + 1

    # --- generate the CPU program: N stores, then set done flag, then spin
    lines = []
    for i in range(N):
        addr = CPU_CPU_BASE + 4 * i
        val  = cpu_val(i)
        lines.append(f"(Rx= {addr} R0)")
        lines.append(f"(Rx= {val} R1)")
        lines.append("(A=Rx R1)")
        lines.append("(M[Rx]=A R0)")
    # done flag last, so TB seeing it guarantees all stores retired
    lines.append(f"(Rx= {CPU_DONE} R0)")
    lines.append(f"(Rx= {DONE_MAGIC} R1)")
    lines.append("(A=Rx R1)")
    lines.append("(M[Rx]=A R0)")
    lines.append("(label done)")
    lines.append("(j done)")

    prog = assemble("\n".join(lines))
    result = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @instance
        def seq():
            rstn.next = 0
            conf.master_reply_data.next   = 0
            conf.master_reply_status.next = 0
            conf.master_reply_id.next     = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # clear the done flag
            yield _write_data(conf, clk, 0, SLAVE_DONE)

            # load program into IMEM
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            # trigger boot jump -- CPU starts storing to its region
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            # while the CPU runs, hammer the slave port: interleaved
            # writes and read-backs in the TB region, with a configurable
            # gap so the CPU gets memory cycles in between
            print(SLAVE_TB_BASE)
            for i in range(N):
                yield _write_data(conf, clk, tb_val(i), SLAVE_TB_BASE + 4 * i)
                for _ in range(slave_gap_cycles):
                    yield clk.posedge
                # immediate read-back of the previous slot for extra traffic
                if i > 0:
                    rb = [0]
                    yield _read_data(conf, clk, SLAVE_TB_BASE + 4 * (i - 1), rb)
                    if rb[0] != tb_val(i - 1):
                        result[0] = (f"FAIL: inline slave readback slot {i-1}: "
                                     f"got 0x{rb[0]:08X}, expected 0x{tb_val(i-1):08X}")
                        raise StopSimulation()
                    for _ in range(slave_gap_cycles):
                        yield clk.posedge

            # poll the done flag (with the same gap so the CPU can finish)
            MAX_POLLS = 800
            for i in range(MAX_POLLS):
                rb = [0]
                yield _read_data(conf, clk, SLAVE_DONE, rb)
                if rb[0] == DONE_MAGIC:
                    break
                for _ in range(slave_gap_cycles):
                    yield clk.posedge
                yield clk.posedge
            else:
                result[0] = f"FAIL: done flag never set (last read 0x{rb[0]:08X})"
                raise StopSimulation()

            # verify the CPU's N stores via the slave port
            for i in range(N):
                rb = [0]
                yield _read_data(conf, clk, SLAVE_CPU_BASE + 4 * i, rb)
                if rb[0] != cpu_val(i):
                    result[0] = (f"FAIL: CPU store slot {i} at slave addr "
                                 f"0x{SLAVE_CPU_BASE + 4*i:04X}: got 0x{rb[0]:08X}, "
                                 f"expected 0x{cpu_val(i):08X}")
                    raise StopSimulation()

            # verify the TB's slave writes weren't corrupted by CPU traffic
            for i in range(N):
                rb = [0]
                yield _read_data(conf, clk, SLAVE_TB_BASE + 4 * i, rb)
                if rb[0] != tb_val(i):
                    result[0] = (f"FAIL: slave write slot {i}: got 0x{rb[0]:08X}, "
                                 f"expected 0x{tb_val(i):08X}")
                    raise StopSimulation()

            result[0] = "PASS"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_cpu_memory_access_slave_conflict'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(4000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_cpu_memory_access_slave_conflict"
          f"(gap={slave_gap_cycles})" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_cpu_slave_race(cpu_op, max_delay=20):
    """
    Sweep every single-cycle offset (0..max_delay) between the boot trigger
    and a concurrent slave transaction, for both slave_op in {write, read},
    while the CPU performs `cpu_op` ('dmem_write' | 'dmem_read' |
    'master_write' | 'master_read'). Verifies both sides of the race at
    every offset:
      - the CPU's own operation produced the correct result
      - the slave's operation was neither dropped nor corrupted

    Exactly ONE slave transaction happens while the CPU program is
    actually running: everything else (clearing sentinels, preloading
    inputs, resetting the CPU between sweep points) happens either before
    the boot trigger or after a fixed settle window that comfortably
    exceeds how long these short programs take to finish -- there is no
    polling loop hammering the slave port while the CPU is live.

    Runs a SINGLE MyHDL simulation for the whole sweep: the program is
    loaded into IMEM once, and between sweep points the CPU core is
    restarted via conf_map.cpu_reset (same mechanism as test_cpu_reset)
    instead of tearing down and rebuilding a fresh simulation per point.
    """
    RACE_VALUE           = 0x4321   # value CPU writes, or preloads for CPU to read
    MASTER_REPLY_VALUE   = 0x2468   # value the TB's auto-responder returns for master reads
    SLAVE_RACE_WRITE_VAL = 0x1357   # value used by the concurrent slave write
    SLAVE_RACE_PRELOAD   = 0x9ABC   # value preloaded at the race site for the slave read to check
    SENTINEL             = 0x0000   # cleared into any address we're about to check, so a
                                     # dropped write reads back as SENTINEL, never as a stale
                                     # correct value left over from an earlier sweep point
    RESET_SETTLE_CYCLES  = 20       # matches test_cpu_reset's post-reset settle window
    SETTLE_CYCLES        = 120      # >> worst-case cycles for these 4-instruction programs
                                     # to retire (boot unblock + jump + a few instrs, generous
                                     # margin vs. the identical-length PROG_STORE_CONSTANT)

    RACE_ADDR_S = SLAVE_INPUT1  # untouched by any of the CPU programs below

    if cpu_op == 'dmem_write':
        prog = assemble(
            """
            (Rx= RESULT_PHYS R1)
            (Rx= VALUE R0)
            (A=Rx R0)
            (M[Rx]=A R1)
            (label done)
            (j done)
            """,
            RESULT_PHYS=CPU_RESULT0, VALUE=RACE_VALUE,
        )
    elif cpu_op == 'dmem_read':
        prog = assemble(
            """
            (Rx= INPUT_PHYS R0)
            (A=M[Rx] R0)
            (Rx= RESULT_PHYS R1)
            (M[Rx]=A R1)
            (label done)
            (j done)
            """,
            INPUT_PHYS=CPU_INPUT0, RESULT_PHYS=CPU_RESULT0,
        )
    elif cpu_op == 'master_read':
        prog = assemble(
            """
            (Rx= CONF_LOW R0)
            (A=M[Rx] R0)
            (Rx= RESULT_PHYS R1)
            (M[Rx]=A R1)
            (label done)
            (j done)
            """,
            CONF_LOW=CONF_LOW, RESULT_PHYS=CPU_RESULT0,
        )
    elif cpu_op == 'master_write':
        prog = assemble(
            """
            (Rx= CONF_LOW R0)
            (Rx= VALUE R1)
            (A=Rx R1)
            (M[Rx]=A R0)
            (label done)
            (j done)
            """,
            CONF_LOW=CONF_LOW, VALUE=RACE_VALUE,
        )
    else:
        raise ValueError(f"unknown cpu_op {cpu_op!r}")

    sweep_points = [(slave_op, d) for slave_op in ('write', 'read')
                                   for d in range(max_delay + 1)]
    failures = []
    captured_master_write = [None]  # (address, data) seen by the auto-responder

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @instance
        def master_auto_reply():
            # Always eventually replies to a master request, regardless of
            # what the slave port is doing, so master_write/master_read
            # never deadlocks the CPU during the sweep.
            conf.master_reply_status.next = 0
            conf.master_reply_data.next   = 0
            conf.master_reply_id.next     = 0
            while True:
                yield clk.posedge
                if conf.master_request_we == 1:
                    captured_master_write[0] = (int(conf.master_request_address),
                                                 int(conf.master_request_data))
                    cid = int(conf.master_request_id)
                    yield clk.posedge
                    conf.master_reply_status.next = 2
                    conf.master_reply_id.next     = cid
                    yield clk.posedge
                    conf.master_reply_status.next = 0
                elif conf.master_request_re == 1:
                    cid = int(conf.master_request_id)
                    yield clk.posedge
                    conf.master_reply_status.next = 1
                    conf.master_reply_id.next     = cid
                    conf.master_reply_data.next   = MASTER_REPLY_VALUE
                    yield clk.posedge
                    conf.master_reply_status.next = 0
                    conf.master_reply_data.next   = 0

        @instance
        def seq():
            rstn.next = 0
            conf.master_reply_data.next   = 0
            conf.master_reply_status.next = 0
            conf.master_reply_id.next     = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # Load the program into IMEM ONCE. conf_map.cpu_reset resets
            # only the CPU core (PC, registers, pipeline state) -- IMEM and
            # DMEM survive -- so every sweep point below just re-triggers
            # the same already-loaded program.
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            for idx, (slave_op, d) in enumerate(sweep_points):
                if idx > 0:
                    yield _write_data(conf, clk, 1, conf_map.cpu_reset)
                    for _ in range(RESET_SETTLE_CYCLES):
                        yield clk.posedge

                captured_master_write[0] = None

                # Sentinel-clear whatever we're about to check, so a
                # dropped write reads back as SENTINEL rather than a
                # stale correct value from an earlier sweep point.
                # Preload whatever this iteration needs. All of this
                # happens before the boot trigger, i.e. before the CPU
                # program is running.
                yield _write_data(conf, clk, SENTINEL, SLAVE_RESULT0)
                if cpu_op == 'dmem_read':
                    yield _write_data(conf, clk, RACE_VALUE, SLAVE_INPUT0)
                if slave_op == 'write':
                    yield _write_data(conf, clk, SENTINEL, RACE_ADDR_S)
                else:
                    yield _write_data(conf, clk, SLAVE_RACE_PRELOAD, RACE_ADDR_S)

                # trigger boot jump
                yield _write_data(conf, clk, 1, conf_map.interrupt)

                # wait exactly `d` cycles, then fire the ONE slave
                # transaction that actually overlaps the running CPU
                for _ in range(d):
                    yield clk.posedge

                race_readback = [0]
                if slave_op == 'write':
                    yield _write_data(conf, clk, SLAVE_RACE_WRITE_VAL, RACE_ADDR_S)
                else:
                    yield _read_data(conf, clk, RACE_ADDR_S, race_readback)

                # No polling: just wait a fixed, generous window (no slave
                # traffic at all) for the program to retire, then check.
                for _ in range(SETTLE_CYCLES):
                    yield clk.posedge

                errors = []

                # --- did the slave's own request survive? ---
                if slave_op == 'read' and race_readback[0] != SLAVE_RACE_PRELOAD:
                    errors.append(f"slave read got 0x{race_readback[0]:X}, "
                                  f"expected 0x{SLAVE_RACE_PRELOAD:X}")
                if slave_op == 'write':
                    rb2 = [0]
                    yield _read_data(conf, clk, RACE_ADDR_S, rb2)
                    if rb2[0] != SLAVE_RACE_WRITE_VAL:
                        errors.append(f"slave write lost/corrupted: read back 0x{rb2[0]:X}, "
                                      f"expected 0x{SLAVE_RACE_WRITE_VAL:X}")

                # --- did the CPU's own operation survive? ---
                if cpu_op in ('dmem_write', 'dmem_read'):
                    rb3 = [0]
                    yield _read_data(conf, clk, SLAVE_RESULT0, rb3)
                    if rb3[0] != RACE_VALUE:
                        errors.append(f"cpu {cpu_op} result wrong: 0x{rb3[0]:X}, "
                                      f"expected 0x{RACE_VALUE:X}")
                elif cpu_op == 'master_read':
                    rb3 = [0]
                    yield _read_data(conf, clk, SLAVE_RESULT0, rb3)
                    if rb3[0] != MASTER_REPLY_VALUE:
                        errors.append(f"cpu master_read result wrong: 0x{rb3[0]:X}, "
                                      f"expected 0x{MASTER_REPLY_VALUE:X}")
                elif cpu_op == 'master_write':
                    if captured_master_write[0] is None:
                        errors.append("cpu master_write: request never observed by TB")
                    else:
                        _, got_data = captured_master_write[0]
                        if got_data != RACE_VALUE:
                            errors.append(f"cpu master_write data wrong: 0x{got_data:X}, "
                                          f"expected 0x{RACE_VALUE:X}")

                if errors:
                    failures.append(f"delay={d} slave_op={slave_op}: " + "; ".join(errors))

            raise StopSimulation()

        return instances()

    traceSignals.filename = f"trace_cpu_slave_race[{cpu_op}]"
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(20000000)

    ok = not failures
    print(f"{'PASS' if ok else 'FAIL'}: test_cpu_slave_race[{cpu_op}]"
          + (f"  ({len(failures)}/{len(sweep_points)} offsets failed)" if failures else ""))
    for f in failures[:10]:
        print("   ", f)
    if len(failures) > 10:
        print(f"    ... and {len(failures) - 10} more")
    return ok


def test_cpu_slave_race_dmem_write():
    """Test 10a: slave write/read racing against a CPU dmem write, every cycle offset."""
    return test_cpu_slave_race('dmem_write')


def test_cpu_slave_race_dmem_read():
    """Test 10b: slave write/read racing against a CPU dmem read, every cycle offset."""
    return test_cpu_slave_race('dmem_read')


def test_cpu_slave_race_master_read():
    """Test 10c: slave write/read racing against a CPU master read, every cycle offset."""
    return test_cpu_slave_race('master_read')


def test_cpu_slave_race_master_write():
    """Test 10d: slave write/read racing against a CPU master write, every cycle offset."""
    return test_cpu_slave_race('master_write')


def test_imem_slave_race(cpu_op, max_delay=40):
    """
    IMEM analogue of test_cpu_slave_race: sweep every single-cycle offset
    (0..max_delay) between the boot trigger and a single concurrent slave
    IMEM write, while the CPU performs `cpu_op` ('imem_write' | 'imem_read')
    on its OWN, separate IMEM scratch byte. Verifies both sides:
      - the CPU's own imem access produced the correct result
      - the slave's concurrent imem write was neither dropped nor corrupted

    There is no slave-side "imem read" test here: conf_slave()'s read path
    always resolves through conf_slave_dmem_radr regardless of address, so
    an external slave read can never observe IMEM content in this design
    -- the only way to observe IMEM state at all is to have the CPU
    itself read it back and store the result to DMEM, which is exactly
    what this program does for both its own scratch byte and the slave's
    race-site byte.

    IMEM access through this dmem-mapped path is only reliably observable
    at byte granularity (the ROM/RAM read path is 8 bits wide even though
    the write path takes a full word), so every value used here is a
    single byte.

    To avoid a second reset/reload cycle just to read back the slave's
    race site at a TB-controlled, safely-late moment, the CPU program
    itself blocks on the existing TICK_WAIT mechanism (the same one
    test_wait_ticks uses) between its own op and the race-site read-back.
    The TB only releases that block once it knows -- by construction,
    for every d in [0, max_delay] -- that both the CPU's own op and the
    slave's race write have long since retired.

    Runs a SINGLE MyHDL simulation for the whole sweep: the program is
    loaded into IMEM once, and between sweep points the CPU core is
    restarted via conf_map.cpu_reset (as in test_cpu_reset) instead of
    rebuilding the simulation per point.
    """
    RACE_VALUE            = 0x55  # byte the CPU writes/reads at its own imem scratch site
    SLAVE_RACE_WRITE_VAL  = 0x77  # byte the slave writes at the race site
    SENTINEL              = 0x00
    RESET_SETTLE_CYCLES   = 20    # matches test_cpu_reset's post-reset settle window
    SETTLE_CYCLES         = 250   # >> worst-case cycles to reach the TICK_WAIT block point
    RELEASE_SETTLE_CYCLES = 100   # >> worst-case cycles from tick-release to program end

    TICK_SEL      = 5             # matches _PROG_READ_TICKS; selects conf.ticks bit 4
    TICK_BIT_MASK = 1 << (TICK_SEL - 1)

    IMEM_SCRATCH_CPU = PROG_BASE + 256  # CPU's own imem scratch byte -- clear of the program
    IMEM_RACE_ADDR   = PROG_BASE + 512  # slave's race target -- clear of program + scratch

    RESULT_PHYS  = CPU_RESULT0  # holds the CPU's own imem op result
    RESULT2_PHYS = CPU_RESULT1  # holds the CPU's read-back of the slave's race site

    common_tail = """
        (Rx= RESULT_PHYS R2)
        (M[Rx]=A R2)
        (Rx= TICK_ADDRESS R5)
        (Rx= TICK_SEL R6)
        (A=Rx R6)
        (M[Rx]=A R5)
        (A=M[Rx] R5)
        (Rx= RACE_ADDR_IMEM R3)
        (A=M[Rx] R3)
        (Rx= RESULT2_PHYS R4)
        (M[Rx]=A R4)
        (label done)
        (j done)
        """

    if cpu_op == 'imem_write':
        prog = assemble(
            """
            (Rx= SCRATCH_PHYS R1)
            (Rx= VALUE R0)
            (A=Rx R0)
            (M[Rx]=A R1)
            (A=M[Rx] R1)
            """ + common_tail,
            SCRATCH_PHYS=IMEM_SCRATCH_CPU, VALUE=RACE_VALUE,
            RESULT_PHYS=RESULT_PHYS, TICK_ADDRESS=TICK_ADDRESS, TICK_SEL=TICK_SEL,
            RACE_ADDR_IMEM=IMEM_RACE_ADDR, RESULT2_PHYS=RESULT2_PHYS,
        )
    elif cpu_op == 'imem_read':
        prog = assemble(
            """
            (Rx= SCRATCH_PHYS R1)
            (A=M[Rx] R1)
            """ + common_tail,
            SCRATCH_PHYS=IMEM_SCRATCH_CPU,
            RESULT_PHYS=RESULT_PHYS, TICK_ADDRESS=TICK_ADDRESS, TICK_SEL=TICK_SEL,
            RACE_ADDR_IMEM=IMEM_RACE_ADDR, RESULT2_PHYS=RESULT2_PHYS,
        )
    else:
        raise ValueError(f"unknown cpu_op {cpu_op!r}")

    failures = []

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map)

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            conf.ticks.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            # Load the program into IMEM ONCE -- conf_map.cpu_reset resets
            # only the CPU core, IMEM survives, and nothing else in this
            # test ever touches PROG_BASE.
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            for d in range(max_delay + 1):
                if d > 0:
                    yield _write_data(conf, clk, 1, conf_map.cpu_reset)
                    for _ in range(RESET_SETTLE_CYCLES):
                        yield clk.posedge

                # conf.ticks is driven exclusively by this generator (no
                # background heartbeat here) so TICK_WAIT release timing
                # stays fully TB-controlled.
                conf.ticks.next = 0
                yield clk.posedge

                # Sentinel-clear everything we're about to check, so a
                # dropped write reads back as SENTINEL rather than a
                # stale correct value from an earlier sweep point.
                yield _write_data(conf, clk, SENTINEL, SLAVE_RESULT0)
                yield _write_data(conf, clk, SENTINEL, SLAVE_RESULT1)
                yield _write_data(conf, clk, SENTINEL, IMEM_SCRATCH_CPU)
                yield _write_data(conf, clk, SENTINEL, IMEM_RACE_ADDR)
                if cpu_op == 'imem_read':
                    yield _write_data(conf, clk, RACE_VALUE, IMEM_SCRATCH_CPU)

                # trigger boot jump
                yield _write_data(conf, clk, 1, conf_map.interrupt)

                # wait exactly `d` cycles, then fire the ONE slave
                # transaction that actually overlaps the running CPU
                for _ in range(d):
                    yield clk.posedge

                yield _write_data(conf, clk, SLAVE_RACE_WRITE_VAL, IMEM_RACE_ADDR)

                # No polling: wait a fixed, generous window (no further
                # slave traffic) for the CPU to reach its TICK_WAIT block
                for _ in range(SETTLE_CYCLES):
                    yield clk.posedge

                # release the CPU to read back the now-stable race site
                conf.ticks.next = TICK_BIT_MASK
                for _ in range(RELEASE_SETTLE_CYCLES):
                    yield clk.posedge

                errors = []

                rb1 = [0]
                yield _read_data(conf, clk, SLAVE_RESULT0, rb1)
                if rb1[0] != RACE_VALUE:
                    errors.append(f"cpu {cpu_op} result wrong: 0x{rb1[0]:X}, "
                                  f"expected 0x{RACE_VALUE:X}")

                rb2 = [0]
                yield _read_data(conf, clk, SLAVE_RESULT1, rb2)
                if rb2[0] != SLAVE_RACE_WRITE_VAL:
                    errors.append(f"slave imem write lost/corrupted: read back 0x{rb2[0]:X}, "
                                  f"expected 0x{SLAVE_RACE_WRITE_VAL:X}")

                if errors:
                    failures.append(f"delay={d}: " + "; ".join(errors))

            raise StopSimulation()

        return instances()

    traceSignals.filename = f"trace_imem_slave_race[{cpu_op}]"
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(20000000)

    ok = not failures
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_slave_race[{cpu_op}]"
          + (f"  ({len(failures)}/{max_delay + 1} offsets failed)" if failures else ""))
    for f in failures[:10]:
        print("   ", f)
    if len(failures) > 10:
        print(f"    ... and {len(failures) - 10} more")
    return ok


def test_imem_slave_race_write():
    """Test 11a: slave imem write racing against a CPU imem write, every cycle offset."""
    return test_imem_slave_race('imem_write')


def test_imem_slave_race_read():
    """Test 11b: slave imem write racing against a CPU imem read, every cycle offset."""
    return test_imem_slave_race('imem_read')

def test_read_coreversion():
    """
    """
    EXPECTED  = 42
    MAX_POLLS = 400
    result    = [None]

    boot_hex_path = "boot_code.mem"

    #prog = assemble(
    #    """
    #    (Rx= RESULT_PHYS R1)
    #    (Rx= 42 R0)
    #    (A=Rx R0)
    #    (M[Rx]=A R1)
    #    (label done)
    #    (j done)
    #    """,
    #    RESULT_PHYS=CPU_RESULT0,
    #)

    #with open(boot_hex_path, "w") as f:
    #    for i, byte in enumerate(prog):
    #        f.write(f"{byte:02x} ")
    #        if i % 16 == 15:
    #            f.write("\n")
    #    f.write("\n")

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map,
                        boot_code_path=boot_hex_path)

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @instance
        def seq():
            rstn.next = 0
            yield clk.posedge
            rstn.next = 1
            yield clk.posedge

            readback = [0]

            for i in range(10000):
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read 0x{readback[0]:08X})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_read_coreversion'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_read_coreversion" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    results = []

    #test_imem_slave_race_read()
    #test_imem_slave_race_write()

    results.append(test_slave_dmem_rw())
    results.append(test_cpu_stores_constant())
    results.append(test_boot_code_from_file())
    results.append(test_slave_write_cpu_doubles())
    results.append(test_slave_write_cpu_sum())
    results.append(test_conf_map_offset())
    results.append(test_conf_map_gapped())
    results.append(test_wait_ticks())
    results.append(test_master_while_slave_request())
    results.append(test_cpu_memory_access_slave_conflict())
    results.append(test_cpu_reset())
    results.append(test_dual_cpu())
    results.append(test_cpu_slave_race_dmem_write())
    results.append(test_cpu_slave_race_dmem_read())
    results.append(test_cpu_slave_race_master_read())
    results.append(test_cpu_slave_race_master_write())
    #results.append(test_read_coreversion())

    passed = sum(results)
    total  = len(results)
    print(f"\n{passed}/{total} tests passed")
    sys.exit(0 if passed == total else 1)
