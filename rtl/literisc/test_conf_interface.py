"""
test_conf_interface.py
======================
MyHDL simulation tests for the liteRISC conf_slave / conf_master interface.

Boot sequence
-------------
The ROM at IMEM[0..15] runs boot-read-interrupt.lisp, which:
  1. Reads a byte from INTERRUPT_ADDRESS
  2. When non-zero, jumps to byte address PROG_BASE (512) in IMEM

All test programs are therefore loaded into IMEM at PROG_BASE via
slave writes (one byte per write), and execution is triggered by
writing any non-zero value to INTERRUPT_ADDRESS.

Address arithmetic
------------------
  IMEM  : byte addresses  0 .. 8191  (slave writes go here when addr <= IMEM_HIGH)
  DMEM  : byte addresses  8192..24575 (slave writes go here when addr > IMEM_HIGH)

  Slave DMEM write addr S  →  dp_mem[S]
  CPU DMEM phys byte  addr P  →  dp_mem[P - DMEM_LOW]

  So slave addr S and CPU addr P access the same dp_mem word when:
      S = P - DMEM_LOW  (i.e. P = S + DMEM_LOW)

  For slave addr to be a DMEM write (not IMEM), S must be > IMEM_HIGH = 8191.
  The lowest safe pair: S=8192, P=8192+8192=16384.

Tests
-----
  1. test_slave_dmem_rw       -- pure slave DMEM write / read roundtrip
  2. test_cpu_stores_constant -- load program, CPU stores 42 to DMEM, slave reads
  3. test_slave_write_cpu_doubles -- slave writes 100, CPU reads and shifts left 1 (=200)
  4. test_slave_write_cpu_sum -- slave writes two inputs, CPU adds them, slave reads result
"""

import sys
import os
from myhdl import *
from modules.common.signal import signal
from axi import Axi4
from conf import Conf
from cpu_sys import cpu_sys, INTERRUPT_ADDRESS, IMEM_HIGH, DMEM_LOW

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from asm import assemble

# ---------------------------------------------------------------------------
# Address constants
# ---------------------------------------------------------------------------

PROG_BASE = 512  # IMEM byte address where user programs are loaded

SLAVE_RESULT0 = 8192
SLAVE_RESULT1 = 8196
SLAVE_INPUT0  = 8200
SLAVE_INPUT1  = 8204

CPU_RESULT0   = DMEM_LOW + SLAVE_RESULT0 # 8192 + 8192
CPU_RESULT1   = DMEM_LOW + SLAVE_RESULT1
CPU_INPUT0    = DMEM_LOW + SLAVE_INPUT0
CPU_INPUT1    = DMEM_LOW + SLAVE_INPUT1

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

#
def test_slave_write_during_boot():
    """Test 5: Write during boot-read-interrupt."""
    result    = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        icpu = cpu_sys(clk, rstn, axi, conf)

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
            yield _write_data(conf, clk, 32, addr)
            for i in range(50):
                yield clk.posedge
            #for byte in _PROG_ADD_TWO:
            #    yield _write_data(conf, clk, byte, addr)
            #    addr += 1
            #    yield clk.posedge

            #result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_slave_write_during_boot'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_slave_write_during_boot" +
          (f"  ({result[0]})" if not ok else ""))
    return ok

def test_slave_dmem_rw():
    """Test 1: slave DMEM write/read roundtrip (no CPU program needed)."""
    TEST_VALUE = 0xCAFEBABE
    result = [None]

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        icpu = cpu_sys(clk, rstn, axi, conf)

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
        icpu = cpu_sys(clk, rstn, axi, conf)

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

            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            for i in range(MAX_POLLS):
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
        icpu = cpu_sys(clk, rstn, axi, conf)

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
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            for i in range(MAX_POLLS):
                if i % 15 != 0:
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
        icpu = cpu_sys(clk, rstn, axi, conf)

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

            for i in range(50):
                yield clk.posedge

            addr = PROG_BASE
            for byte in _PROG_ADD_TWO:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, INPUT_A, SLAVE_INPUT0)
            yield _write_data(conf, clk, INPUT_B, SLAVE_INPUT1)
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            for _ in range(MAX_POLLS):
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


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    results = []

    print_program_hex("PROG_STORE_CONSTANT",    _PROG_STORE_CONSTANT)

    #results.append(test_slave_write_during_boot())
    results.append(test_slave_dmem_rw())
    results.append(test_cpu_stores_constant())
    results.append(test_slave_write_cpu_doubles())
    results.append(test_slave_write_cpu_sum())

    passed = sum(results)
    total  = len(results)
    print(f"\n{passed}/{total} tests passed")
    sys.exit(0 if passed == total else 1)
