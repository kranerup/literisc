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
from cpu_sys import cpu_sys, INTERRUPT_ADDRESS, IMEM_HIGH, DMEM_LOW, IO_LOW, IO_HIGH, TICK_ADDRESS

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

_PROG_MASTER_REQUEST_ADDRESS_0 = assemble(
    """
    (Rx= IO_LOW R0)
    (A=M[Rx] R0)
    (label done)
    (j done)
    """,
    IO_LOW=IO_LOW,
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
                #if i % 15 != 0:
                #    yield clk.posedge
                #    continue
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

            addr = PROG_BASE
            for byte in _PROG_ADD_TWO:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, INPUT_A, SLAVE_INPUT0)
            yield _write_data(conf, clk, INPUT_B, SLAVE_INPUT1)
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            for i in range(MAX_POLLS):
                #if i % 15 != 0:
                #    yield clk.posedge
                #    continue
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

def test_master_request():
    """Test 5: Master reads address 0 and puts the result into accumulator"""
    result    = [None]
    MAX_POLLS = 200

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
            for byte in _PROG_MASTER_REQUEST_ADDRESS_0:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

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
            #for _ in range(MAX_POLLS):
            #    readback = [0]
            #    yield _read_data(conf, clk, SLAVE_RESULT0, readback)
            #    if readback[0] == EXPECTED:
            #        result[0] = "PASS"
            #        raise StopSimulation()
            #    yield clk.posedge

            #result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
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
        icpu = cpu_sys(clk, rstn, axi, conf)

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

            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            # wait long enough for several ticks and loop iterations
            MAX_POLLS = 500
            prev_readback = [None]
            changes_seen  = [0]

            for i in range(MAX_POLLS):
                if i % 20 != 0:
                    yield clk.posedge
                    continue
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

    CPU-A runs a program that writes a value to CPU-B's DMEM via the conf
    master port, then reads it back.  CPU-B just runs the boot ROM (idle loop).
    """
    result = [None]

    def tb():
        clk   = Signal(bool())
        rstn  = signal()
        axi_a = Axi4(asize=16, dsize=32, idsize=1)
        axi_b = Axi4(asize=16, dsize=32, idsize=1)
        conf_a = Conf()
        conf_b = Conf()

        icpu_a = cpu_sys(clk, rstn, axi_a, conf_a)
        icpu_b = cpu_sys(clk, rstn, axi_b, conf_b)

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

        # --- tie off B master (nothing drives it) -------------------------
        #@always_comb
        #def b_master_tieoff():
        #    conf_b.master_reply_data.next   = 0
        #    conf_b.master_reply_status.next = 0
        #    conf_b.master_reply_id.next     = 0
        #    # and A slave (nothing uses it)
        #    conf_a.slave_request_address.next = 0
        #    conf_a.slave_request_data.next    = 0
        #    conf_a.slave_request_id.next      = 0
        #    conf_a.slave_request_we.next      = 0
        #    conf_a.slave_request_re.next      = 0
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

            # Load a program into CPU-A's IMEM via conf_a slave.
            # Program: write 0xABCD to CPU-B's DMEM via master port,
            #          then spin forever.
            #
            # In cpu_sys the master port uses IO-space addresses:
            #   req_addr = cpu_dmem_adr - IO_LOW
            # and then conf_master does:
            #   conf.master_request_address = req_addr // 4
            #
            # On the B side, conf_slave sees that address and writes to
            # DMEM when address > IMEM_HIGH (8191).
            #
            # Pick target slave addr = 8192 (first DMEM word in B).
            # The CPU must write to IO_LOW + (8192 * 4) = 65536 + 32768 = 98304.
            # But IO space is IO_LOW..IO_HIGH = 65536..131071, so 98304 is valid.

            TARGET_IO_ADDR = IO_LOW + SLAVE_RESULT0 * 4  # 65536 + 32768

            prog = assemble(
                """
                (Rx= TARGET R0)
                (Rx= 43981 R1)
                (A=Rx R1)
                (M[Rx]=A R0)
                (label done)
                (j done)
                """,
                TARGET=TARGET_IO_ADDR,
            )

            # write program into CPU-A's IMEM starting at PROG_BASE
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf_a, clk, byte, addr)
                addr += 1

            # trigger CPU-A boot jump to PROG_BASE
            yield _write_data(conf_a, clk, 1, INTERRUPT_ADDRESS)

            # wait for CPU-A to issue the master write and for CPU-B's
            # slave to process it
            MAX_POLLS = 600
            EXPECTED  = 0xABCD
            for _ in range(MAX_POLLS):
                readback = [0]
                yield _read_data(conf_b, clk, SLAVE_RESULT0, readback)
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
    Test 7: CPU-A issues a master (IO) read request. While CPU-A is waiting
    for the reply, an external slave_request arrives on conf.  This races
    against the master reply to check whether the CPU correctly handles both
    without dropping either.

    Setup:
      - CPU-A program: read from IO_LOW (triggers master request, CPU stalls),
        then store the result to DMEM, then spin.
      - Test bench: after triggering CPU-A, waits a few cycles then issues a
        slave write to CPU-A's DMEM *while* the master reply is still pending.
      - Then sends the master reply.
      - Checks both:
          (a) the master reply data ends up in CPU-A's DMEM result slot
          (b) the slave write data is also correctly in CPU-A's DMEM
    """
    result = [None]

    def tb():
        clk   = Signal(bool())
        rstn  = signal()
        axi = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()

        icpu = cpu_sys(clk, rstn, axi, conf)

        @always(delay(10))
        def clk_gen():
            clk.next = not clk

        @always(clk.posedge)
        def inc_ticks():
            conf.ticks.next = conf.ticks + 1

        MASTER_REPLY_VALUE = 0x1234
        SLAVE_WRITE_VALUE  = 0x5678

        # CPU-A program:
        #   read from IO_LOW -> stalls waiting for master reply
        #   store result (accumulator) to DMEM result slot
        #   spin
        prog = assemble(
            """
            (Rx= IO_LOW R0)
            (A=M[Rx] R0)
            (Rx= RESULT_PHYS R1)
            (M[Rx]=A R1)
            (label done)
            (j done)
            """,
            IO_LOW=IO_LOW,
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

            # load program into CPU-A IMEM
            addr = PROG_BASE
            for byte in prog:
                yield _write_data(conf, clk, byte, addr)
                addr += 1

            # trigger boot jump
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

            # wait a few cycles for CPU-A to reach the IO read and stall
            for _ in range(20):
                yield clk.posedge

            # --- inject a slave_request while master reply is still pending ---
            # write to a different DMEM slot so we can check it independently
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

            yield clk.posedge

            # poll for CPU-A to store the master reply value into RESULT0
            MAX_POLLS = 400
            for _ in range(MAX_POLLS):
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
    After verifying the first increment, reset the CPU via CPU_RESET_ADDRESS,
    reload the program, trigger it again, and verify a second increment."""
    from cpu_sys import CPU_RESET_ADDRESS
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

            # initialise result slot to 0
            yield _write_data(conf, clk, 0, SLAVE_RESULT0)

            # --- first run ---
            addr = PROG_BASE
            for byte in _PROG_INCREMENT_ONCE:
                yield _write_data(conf, clk, byte, addr)
                addr += 1
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

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
            yield _write_data(conf, clk, 1, CPU_RESET_ADDRESS)
            # wait a few cycles for reset to propagate and boot ROM to start
            for _ in range(20):
                yield clk.posedge

            # --- second run: reload program and trigger again ---
            #addr = PROG_BASE
            #for byte in _PROG_INCREMENT_ONCE:
            #    yield _write_data(conf, clk, byte, addr)
            #    addr += 1
            yield _write_data(conf, clk, 1, INTERRUPT_ADDRESS)

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

def test_run():
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

            for i in range(100):
                yield clk.posedge
            #for byte in _PROG_ADD_TWO:
            #    yield _write_data(conf, clk, byte, addr)
            #    addr += 1
            #    yield clk.posedge

            #result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]}, expected {EXPECTED})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_run'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_run" +
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
    results.append(test_master_request())
    results.append(test_dual_cpu())
    results.append(test_wait_ticks())
    results.append(test_master_while_slave_request())
    results.append(test_cpu_reset())
    #results.append(test_run())

    print_program_hex("ticks", _PROG_READ_TICKS)

    passed = sum(results)
    total  = len(results)
    print(f"\n{passed}/{total} tests passed")
    sys.exit(0 if passed == total else 1)
