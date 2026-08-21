import sys
import os
import random
import io
import subprocess
import tempfile
from contextlib import redirect_stdout
from myhdl import *
from modules.common.signal import signal
from conf_map import ConfMap
from axi import Axi4
from conf import Conf
from cpu_sys import cpu_sys
from constants import (
    INTERRUPT_ADDRESS, IMEM_HIGH, DMEM_LOW, DMEM_HIGH,
    TICK_ADDRESS, CPU_RESET_ADDRESS,
    CONF_LOW, CONF_HIGH, CONSOLE_ADDRESS,
    compute_memory_map,
)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from asm import assemble

# ---------------------------------------------------------------------------
# lrcc C-compiler bridge (mirrors asm.py's sbcl bridge, but shells out to the
# lrcc.lisp CLI so tests can compile real C programs instead of hand-written
# assembly)
# ---------------------------------------------------------------------------

_LITERISC_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
_LRCC = os.path.join(_LITERISC_ROOT, "lrcc.lisp")
_INCLUDE_DIR = os.path.join(_LITERISC_ROOT, "include")


def _compile_c_to_mem(c_source, mem_path):
    """Compile c_source with lrcc (stdio.h enabled, -Os) straight to a
    $readmemh-format .mem file at mem_path, suitable for cpu_sys's
    boot_code_path."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".c", delete=False, dir="/tmp") as f:
        f.write(c_source)
        c_path = f.name
    try:
        result = subprocess.run(
            [_LRCC, "-I", _INCLUDE_DIR, "-Os", "-o", mem_path, c_path],
            capture_output=True, text=True, timeout=120,
        )
        if result.returncode != 0:
            raise RuntimeError(f"lrcc compile failed:\n{result.stderr}")
    finally:
        os.unlink(c_path)


def _compile_c_to_bin(c_source, base=0):
    """Compile c_source with lrcc (stdio.h enabled, -Os) to a raw byte list
    assembled as if loaded at `base`, suitable for loading via conf-slave
    writes starting at that address -- the real, on-hardware load path
    `--base` targets, as opposed to _compile_c_to_mem's boot_code_path
    (which always runs the image from address 0)."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".c", delete=False, dir="/tmp") as f:
        f.write(c_source)
        c_path = f.name
    with tempfile.NamedTemporaryFile(suffix=".bin", delete=False, dir="/tmp") as f:
        bin_path = f.name
    try:
        result = subprocess.run(
            [_LRCC, "-I", _INCLUDE_DIR, "-Os", "--base", str(base), "-o", bin_path, c_path],
            capture_output=True, text=True, timeout=120,
        )
        if result.returncode != 0:
            raise RuntimeError(f"lrcc compile failed:\n{result.stderr}")
        with open(bin_path, "rb") as bf:
            return list(bf.read())
    finally:
        os.unlink(c_path)
        os.unlink(bin_path)


def _run_c_in_emulator(c_source):
    """Compile and run c_source on the Lisp emulator, returning exactly the
    program's own stdout (lrcc sends instruction count / exit code to
    stderr, not stdout)."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".c", delete=False, dir="/tmp") as f:
        f.write(c_source)
        c_path = f.name
    try:
        result = subprocess.run(
            [_LRCC, "-I", _INCLUDE_DIR, "-Os", "-r", c_path],
            capture_output=True, text=True, timeout=120,
        )
        return result.stdout
    finally:
        os.unlink(c_path)


# ---------------------------------------------------------------------------
# Address constants
# ---------------------------------------------------------------------------

PROG_BASE = 512  # CPU-side IMEM byte address where user programs are loaded/executed
PROG_BASE_WORDS = PROG_BASE // 4  # conf.slave_request_address is word-addressed


# The addresses the tests use for storing the results of the tests or reading its output from. These are embedded into the assembly programs.
CPU_RESULT0   = IMEM_HIGH + 1        # first byte of DMEM
CPU_RESULT1   = CPU_RESULT0 + 4
CPU_INPUT0    = CPU_RESULT0 + 8
CPU_INPUT1    = CPU_RESULT0 + 12

# Same addresses except divided by 4 since the conf bus uses word addressing to access the cpus memory
SLAVE_RESULT0 = CPU_RESULT0 // 4
SLAVE_RESULT1 = CPU_RESULT1 // 4
SLAVE_INPUT0  = CPU_INPUT0 // 4
SLAVE_INPUT1  = CPU_INPUT1 // 4

DUAL_CPU_TARGET_CONF   = CONF_LOW + SLAVE_RESULT0
DUAL_CPU_SLAVE_ADDR_B  = SLAVE_RESULT0

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

def _load_program(conf, clk, prog, base_addr_words):
    """Generator: load `prog` (a list of bytes) into IMEM starting at
    base_addr_words, via conf slave WORD writes -- the conf slave
    interface is word-addressed only (no sub-word/byte transactions), so
    4 consecutive program bytes are packed little-endian into one 32-bit
    write per transaction. base_addr_words is a WORD index (e.g.
    PROG_BASE // 4), matching conf.slave_request_address's units -- NOT
    the CPU-side byte address. Padded with zero bytes if len(prog) isn't
    a multiple of 4."""
    padded = list(prog) + [0] * (-len(prog) % 4)
    addr = base_addr_words
    for i in range(0, len(padded), 4):
        word = (padded[i]
                 | (padded[i + 1] << 8)
                 | (padded[i + 2] << 16)
                 | (padded[i + 3] << 24))
        yield _write_data(conf, clk, word, addr)
        addr += 1


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
    """Test: slave DMEM write/read roundtrip (no CPU program needed)."""
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
    """Test: load program via slave, CPU stores 42 to DMEM, slave reads back."""
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

            yield _load_program(conf, clk, _PROG_STORE_CONSTANT, PROG_BASE_WORDS)

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


def test_console_output():
    """End-to-end check that a CPU store to CONSOLE_ADDRESS (the address
    include/stdio.h's putchar/_outch writes through) shows up as printed
    characters in the MyHDL simulation, exactly like the emulator's
    write-cb-write-char. Boots straight from a hand-assembled ROM that
    stores 'H', 'i', '\\n' to CONSOLE_ADDRESS then loops forever.
    """
    EXPECTED  = "Hi\n"
    result    = [None]

    boot_hex_path = "boot_code_console.hex"

    prog = assemble(
        """
        (Rx= -1 R0)
        (Rx= 72 R1)
        (A=Rx R1)
        (M[Rx].b=A R0)
        (Rx= 105 R1)
        (A=Rx R1)
        (M[Rx].b=A R0)
        (Rx= 10 R1)
        (A=Rx R1)
        (M[Rx].b=A R0)
        (label done)
        (j done)
        """
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    captured = io.StringIO()

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
            for i in range(200):
                yield clk.posedge
            result[0] = "done"
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_console_output'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(500000)

    printed = captured.getvalue()
    ok = printed == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_console_output" +
          (f"  (expected {EXPECTED!r}, got {printed!r})" if not ok else ""))
    return ok


def test_no_print_c_program():
    """Simplest tier of end-to-end C-program RTL check: a program that does
    NOT print anything at all -- no printf/puts/putchar, no IMEM string
    literal reads, so it can't hit the shared-IMEM-port stall at all. It
    just computes a value and stores it to a fixed DMEM address
    (CPU_RESULT0), read back through the CONF slave interface -- the same
    read-back mechanism test_boot_code_from_file uses for a hand-assembled
    program, here driving a program compiled through lrcc instead.

    This is the baseline: it exercises the compiled call/return/push/pop
    machinery (main() is still a real subroutine call from _START) without
    ever touching the IMEM data-read path that test_puts_c_program and
    test_console_output_c_program depend on.
    """
    EXPECTED  = 42
    MAX_POLLS = 2000
    result    = [None]

    c_source = (
        'int main() {\n'
        f'  volatile int *result = (int *){CPU_RESULT0};\n'
        '  *result = 6 * 7;\n'
        '  return 0;\n'
        '}\n'
    )

    boot_hex_path = "no_print_c_program.mem"
    _compile_c_to_mem(c_source, boot_hex_path)

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

            readback = [0]
            for i in range(MAX_POLLS):
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] == EXPECTED:
                    result[0] = "PASS"
                    raise StopSimulation()
                yield clk.posedge

            result[0] = f"FAIL: timeout after {MAX_POLLS} polls (last read {readback[0]})"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_no_print_c_program'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(2000000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_no_print_c_program" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_puts_c_program():
    """Middle tier: a program that calls puts() (print_str + putchar in a
    loop), one level simpler than printf but still exercising the IMEM
    string-literal read that steals the shared IMEM port -- same
    RTL-vs-emulator console-output diff as test_console_output_c_program.
    """
    c_source = (
        '#include <stdio.h>\n'
        '\n'
        'int main() {\n'
        '  puts("Hello");\n'
        '  return 0;\n'
        '}\n'
    )

    boot_hex_path = "puts_c_program.mem"
    _compile_c_to_mem(c_source, boot_hex_path)
    expected = _run_c_in_emulator(c_source)

    captured = io.StringIO()

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
            for i in range(20000):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_puts_c_program'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(2000000)

    printed = captured.getvalue()
    ok = printed == expected
    print(f"{'PASS' if ok else 'FAIL'}: test_puts_c_program" +
          (f"  (expected {expected!r}, got {printed!r})" if not ok else ""))
    return ok


def test_console_output_c_program():
    """Same check as test_console_output, but the program under test is a
    real C program compiled through the full lrcc pipeline (preprocessor +
    compiler + assembler), exercising printf/putchar from include/stdio.h
    instead of hand-written assembly. Confirms the RTL (MyHDL) simulation's
    console_out and the Lisp emulator's write-cb-write-char print byte-
    identical output for the same compiled program.
    """
    c_source = (
        '#include <stdio.h>\n'
        '\n'
        'int main() {\n'
        '  printf("Hi %d\\n", 42, 0, 0);\n'
        '  return 0;\n'
        '}\n'
    )

    boot_hex_path = "console_c_program.mem"
    _compile_c_to_mem(c_source, boot_hex_path)
    expected = _run_c_in_emulator(c_source)

    captured = io.StringIO()

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
            for i in range(20000):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_console_output_c_program'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(2000000)

    printed = captured.getvalue()
    ok = printed == expected
    print(f"{'PASS' if ok else 'FAIL'}: test_console_output_c_program" +
          (f"  (expected {expected!r}, got {printed!r})" if not ok else ""))
    return ok


def test_imem_read_corrupts_register():
    """Minimal repro for a real timing bug found while chasing
    test_console_output_c_program's stack corruption.

    cpu.py's curr_ir (used to decode the destination register of the *next*
    instruction) is purely combinational and unconditionally live-samples
    imem_dout, with no gating against the cycle(s) where cpu_sys.py's
    decode() steals the single shared IMEM read port to service a CPU-side
    data read from IMEM (Rx=M[A].b -- e.g. reading a string literal, which
    lives in IMEM). During that steal, curr_ir can capture the stolen data
    byte as if it were a freshly-fetched opcode+register byte, corrupting
    the decoded destination register of whatever instruction executes right
    after the IMEM read.

    This program reads one byte out of an IMEM-resident literal via
    Rx=M[A].b (R1 <- 'X'), then immediately does "Rx= 99 R2" and prints R2
    to CONSOLE_ADDRESS.
      Expected (bug fixed): prints chr(99) ('c').
      Observed  (bug present): the write to R2 gets misdirected (to SP, in
      this repro), so R2 is left at its reset value 0 and chr(0) is
      printed instead.

    Regression test for the fix in imem_dout_mux() (cpu_sys.py) that skips
    the cache replay when the wait that just ended was an IMEM_WAIT.
    """
    EXPECTED = chr(99)

    boot_hex_path = "imem_read_corrupts_register.hex"
    prog = assemble(
        """
        (Rx= -1 R6)
        (Rx= litdata R0)
        (A=Rx R0)
        (Rx=M[A].b R1)
        (Rx= 99 R2)
        (A=Rx R2)
        (M[Rx].b=A R6)
        (label done)
        (j done)
        (label litdata)
        (lstring "X")
        """
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    captured = io.StringIO()

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
            for i in range(500):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_imem_read_corrupts_register'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(20000)

    printed = captured.getvalue()
    # dp_mem_rom.py unconditionally prints "waddr <addr>\n" on every IMEM RAM
    # write; our program's own output is always whatever follows the last
    # such line (it never ends in a newline itself).
    got = printed.rsplit('\n', 1)[-1]
    ok = got == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_read_corrupts_register" +
          (f"  (expected {EXPECTED!r}, got {got!r} in full output {printed!r})"
           if not ok else ""))
    return ok


def test_imem_word_read():
    """Reads a full 32-bit word out of IMEM (Rx=M[A] R3) and
    uses the read result itself as a store address, exactly what
    putchar() does with _outch, a word-sized global pointer that lives in
    IMEM (see include/stdio.h: `volatile char *_outch = (volatile
    char*)0xffffffff;`).
    """
    EXPECTED = chr(99)

    boot_hex_path = "imem_word_read.hex"
    prog = assemble(
        """
        (Rx= litdata R0)
        (A=Rx R0)
        (Rx=M[A] R3)
        (Rx= 99 R2)
        (A=Rx R2)
        (M[Rx].b=A R3)
        (label done)
        (j done)
        (lalign-dword 0)
        (label litdata)
        (adword -1)
        """
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    captured = io.StringIO()

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
            for i in range(500):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_imem_word_read'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(20000)

    printed = captured.getvalue()
    got = printed.rsplit('\n', 1)[-1]
    ok = got == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_word_read" +
          (f"  (expected {EXPECTED!r}, got {got!r} in full output {printed!r})"
           if not ok else ""))
    return ok


def test_imem_offset_read():
    """Same idea again, but using the offset-addressed form
    Rx=M[A+n].b instead of Rx=M[A].b, to check the offset-immediate byte
    fetch (an extra cycle of instruction decode before the actual memory
    read) doesn't change the shared-IMEM-port steal behavior.
    """
    EXPECTED = chr(99)

    boot_hex_path = "imem_offset_read.hex"
    prog = assemble(
        """
        (Rx= -1 R6)
        (Rx= litdata R0)
        (A=Rx R0)
        (Rx=M[A+n].b 1 R1)
        (Rx= 99 R2)
        (A=Rx R2)
        (M[Rx].b=A R6)
        (label done)
        (j done)
        (label litdata)
        (lstring "XY")
        """
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    captured = io.StringIO()

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
            for i in range(500):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_imem_offset_read'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(20000)

    printed = captured.getvalue()
    got = printed.rsplit('\n', 1)[-1]
    ok = got == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_offset_read" +
          (f"  (expected {EXPECTED!r}, got {got!r} in full output {printed!r})"
           if not ok else ""))
    return ok


def _strip_imem_load_noise(printed):
    """Drop dp_mem_rom.py's unconditional 'waddr <addr>' print lines (one per
    conf-slave byte write used to load a program into IMEM at runtime) from
    captured stdout, leaving only the program's own console output. Safe
    here because all the load writes complete (and hence all their prints
    land) strictly before the interrupt trigger starts the CPU, so no
    'waddr' line can be interleaved with real program output."""
    lines = printed.split('\n')
    return '\n'.join(l for l in lines if not l.startswith('waddr'))


def test_imem_word_read_via_conf_load():
    """Same repro as test_imem_word_read (a full 32-bit IMEM word read used
    immediately as the address of a subsequent store -- exactly what
    putchar()'s read of the _outch global does), but with the program
    loaded through the conf slave interface at PROG_BASE (byte address
    0x200) at runtime, the same load path lrcc's --base flag targets on
    real hardware, instead of via boot_code_path (which replaces the whole
    boot ROM and runs the image from address 0 with no slave writes
    involved at all).

    Reads -1 (0xFFFFFFFF == CONSOLE_ADDRESS) out of IMEM as a word into R3,
    then stores a canary byte to the address held in R3.
      Expected (read correct): R3 == CONSOLE_ADDRESS, canary byte prints.
      Observed (bug present): the word read through the shared-IMEM-port
      steal only returns its low byte correctly (0xFF), so R3 ends up as
      some small in-range IMEM address instead and nothing is printed.
    """
    EXPECTED = chr(99)
    result   = [None]

    prog = assemble(
        """
        (Rx= litdata R0)
        (A=Rx R0)
        (Rx=M[A] R3)
        (Rx= 99 R2)
        (A=Rx R2)
        (M[Rx].b=A R3)
        (label done)
        (j done)
        (lalign-dword 0)
        (label litdata)
        (adword -1)
        """,
        base=PROG_BASE,
    )

    captured = io.StringIO()

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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(500):
                yield clk.posedge
            result[0] = "done"
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_imem_word_read_via_conf_load'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(20000)

    got = _strip_imem_load_noise(captured.getvalue()).rsplit('\n', 1)[-1]
    ok = got == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_word_read_via_conf_load" +
          (f"  (expected {EXPECTED!r}, got {got!r} in full output {captured.getvalue()!r})"
           if not ok else ""))
    return ok


def test_read_outch_via_conf_load():
    """Small hand-written assembly program, loaded through the conf slave
    interface at PROG_BASE (0x200) at runtime -- not via boot_code_path --
    that reads the word-sized OUTCH global (include/stdio.h's
    `volatile char *_outch = (volatile char*)0xffffffff;`) out of IMEM and
    stores the value it read directly to DMEM, so the test can assert on
    the read result itself via a slave read-back instead of inferring
    correctness from a side effect like console output.

      Expected (read correct): readback == 0xFFFFFFFF (CONSOLE_ADDRESS).
      Observed (bug present):  the word read through the shared-IMEM-port
      steal only returns its low byte correctly, so readback comes back as
      0x000000FF instead.

    Deliberately does NOT poll the slave interface while the CPU is
    running (unlike most other tests here) -- cpu_sys.py's
    wait_for_slave() force-stalls the CPU whenever a conf slave
    transaction is in flight, which corrupts this specific multi-cycle
    IMEM word read if a slave read/write lands mid-sequence. That's a
    separate, already-documented race (see the TODOs at the top of
    cpu_sys.py), not the bug this test targets, so it just waits a fixed
    number of idle cycles and then does a single read-back at the end.
    """
    EXPECTED  = CONSOLE_ADDRESS
    result    = [None]

    prog = assemble(
        """
        (Rx= outch R0)
        (A=Rx R0)
        (Rx=M[A] R3)
        (A=Rx R3)
        (Rx= RESULT_PHYS R1)
        (M[Rx]=A R1)
        (label done)
        (j done)
        (lalign-dword 0)
        (label outch)
        (adword -1)
        """,
        base=PROG_BASE,
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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(500):
                yield clk.posedge

            readback = [0]
            yield _read_data(conf, clk, SLAVE_RESULT0, readback)
            if readback[0] == EXPECTED:
                result[0] = "PASS"
            else:
                result[0] = f"FAIL: expected 0x{EXPECTED:08X}, got 0x{readback[0]:08X}"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_read_outch_via_conf_load'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_read_outch_via_conf_load" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_print_c_program_via_conf_load():
    """End-to-end version of test_imem_word_read_via_conf_load: compiles a
    real puts()-calling C program (via the full lrcc pipeline, exercising
    include/stdio.h's _outch/putchar), assembled for load address PROG_BASE
    with --base, and loads it into IMEM through conf-slave byte writes at
    runtime -- then triggers it via the stock boot ROM's interrupt/jump,
    exactly like `lrcc print.c -Os --base 0x200` is meant to run once
    deployed. This is the scenario test_puts_c_program/
    test_console_output_c_program don't cover: those boot straight from
    address 0 via boot_code_path, so they never observe the runtime IMEM
    word-read bug that only shows up once _outch is fetched from an IMEM
    address other than where the program happened to assemble it during a
    from-0 boot.

    Confirms RTL console output for the conf-loaded program matches the
    Lisp emulator's reference output for the same source (compiled without
    --base, since the emulator always runs a from-0 image; --base only
    changes label/jump addresses baked into the assembled bytes, not the
    program's behavior).
    """
    c_source = (
        '#include <stdio.h>\n'
        '\n'
        'int main() {\n'
        '  puts("Hello, World!");\n'
        '  return 0;\n'
        '}\n'
    )

    prog = _compile_c_to_bin(c_source, base=PROG_BASE)
    expected = _run_c_in_emulator(c_source)

    captured = io.StringIO()

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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(20000):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    with redirect_stdout(captured):
        traceSignals.filename = 'trace_print_c_program_via_conf_load'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(2000000)

    printed = _strip_imem_load_noise(captured.getvalue())
    ok = printed == expected
    print(f"{'PASS' if ok else 'FAIL'}: test_print_c_program_via_conf_load" +
          (f"  (expected {expected!r}, got {printed!r})" if not ok else ""))
    return ok


def test_imem_write_from_global():
    """Regression repro for a SEPARATE, still-unfixed bug found via
    tests/00095.c and tests/00096.c: a global variable with a constant
    initializer (e.g. `int x = 3;`) is kept resident in IMEM by the
    compiler (no copy-to-DMEM at startup), so an ordinary assignment to it
    is a genuine CPU-side WRITE to an IMEM address -- a different code path
    (imem_port_mux() in cpu_sys.py) than the read-side steal fixed in
    imem_dout_mux().

    imem_port_mux()'s non-slave branch does:
        imem_final_wadr.next = dmem_adr
    where dmem_adr = cpu_dmem_adr - mm.DMEM_LOW (computed in aoffs(), for
    real DMEM writes). For an IMEM-range address (always < DMEM_LOW), this
    underflows/wraps to a huge value, which dp_mem_rom's pmem then masks
    down to some address that can land outside its actual depth --
    reproducing the same "wenable and waddr >= depth" IndexError crash seen
    in the RTL runs of 00095.c/00096.c.

    This program does the minimal version by hand: write a byte to a
    register-held IMEM address (M[Rx].b=A, address in Rx, value in A),
    matching what `x = 0;` compiles to when x lives in IMEM. Address 0 is
    this tiny program's own first byte (self-overwrite of an
    already-fetched instruction; harmless since it's never re-fetched).
    The underflow this test targets only happens when the write address is
    *smaller* than the whole program's length (len(content)) -- an
    earlier version of this test wrote to an address exactly equal to
    len(content), which lands in-bounds and can't reproduce the bug at
    all.

    KNOWN FAILING -- documents the bug; imem_final_wadr should be
    cpu_dmem_adr directly (IMEM_LOW is always 0), not dmem_adr.
    """
    boot_hex_path = "imem_write_from_global.hex"
    prog = assemble(
        """
        (Rx= 0 R0)
        (A= 0)
        (M[Rx].b=A R0)
        (label done)
        (j done)
        """
    )

    with open(boot_hex_path, "w") as f:
        for i, byte in enumerate(prog):
            f.write(f"{byte:02x} ")
            if i % 16 == 15:
                f.write("\n")
        f.write("\n")

    result = [None]

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
            for i in range(500):
                yield clk.posedge
            result[0] = "PASS"
            raise StopSimulation()

        return instances()

    try:
        traceSignals.filename = 'trace_imem_write_from_global'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(20000)
    except IndexError as e:
        result[0] = f"FAIL: RTL simulation crashed ({e})"

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_write_from_global" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_push_pop_registers():
    """
    Sets R0/R1/R2 to distinct known values, pushes R0..R2, zeroes all
    three registers, pops R0..R2 back, then stores each register to its
    own DMEM word so the test can read them all back via the conf slave
    interface.
    """
    RESULT0 = CPU_RESULT0
    RESULT1 = CPU_RESULT0 + 4
    RESULT2 = CPU_RESULT0 + 8
    SP_INIT = DMEM_HIGH - 63  # word-aligned, comfortably inside DMEM
    EXPECTED = (111, 222, 333)
    result = [None]

    prog = assemble(
        """
        (Rx= SP_INIT SP)
        (Rx= 111 R0)
        (Rx= 222 R1)
        (Rx= 333 R2)
        (push-r R2)
        (Rx= 0 R0)
        (Rx= 0 R1)
        (Rx= 0 R2)
        (pop-r R2)
        (Rx= RESULT0 R5)
        (A=Rx R0)
        (M[Rx]=A R5)
        (Rx= RESULT1 R5)
        (A=Rx R1)
        (M[Rx]=A R5)
        (Rx= RESULT2 R5)
        (A=Rx R2)
        (M[Rx]=A R5)
        (label done)
        (j done)
        """,
        SP_INIT=SP_INIT, RESULT0=RESULT0, RESULT1=RESULT1, RESULT2=RESULT2,
    )

    boot_hex_path = "push_pop_registers.hex"
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
            for i in range(500):
                yield clk.posedge

            r0 = [0]; r1 = [0]; r2 = [0]
            yield _read_data(conf, clk, SLAVE_RESULT0, r0)
            yield _read_data(conf, clk, SLAVE_RESULT0 + 1, r1)
            yield _read_data(conf, clk, SLAVE_RESULT0 + 2, r2)
            result[0] = (r0[0], r1[0], r2[0])
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_push_pop_registers'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(20000)

    ok = result[0] == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_push_pop_registers" +
          (f"  (expected {EXPECTED}, got {result[0]})" if not ok else ""))
    return ok


def test_push_pop_registers_via_conf_load():
    """Same test as test_push_pop_registers, but the program is loaded
    through the conf slave interface at PROG_BASE (byte address 0x200) at
    runtime -- assembled with base=PROG_BASE and loaded via _load_program
    + the stock boot ROM's interrupt/jump, the same way
    test_cpu_stores_constant and the other conf-load tests do.
    """
    RESULT0 = CPU_RESULT0
    RESULT1 = CPU_RESULT0 + 4
    RESULT2 = CPU_RESULT0 + 8
    SP_INIT = DMEM_HIGH - 63  # word-aligned, comfortably inside DMEM
    EXPECTED = (111, 222, 333)
    result = [None]

    prog = assemble(
        """
        (Rx= SP_INIT SP)
        (Rx= 111 R0)
        (Rx= 222 R1)
        (Rx= 333 R2)
        (push-r R2)
        (Rx= 0 R0)
        (Rx= 0 R1)
        (Rx= 0 R2)
        (pop-r R2)
        (Rx= RESULT0 R5)
        (A=Rx R0)
        (M[Rx]=A R5)
        (Rx= RESULT1 R5)
        (A=Rx R1)
        (M[Rx]=A R5)
        (Rx= RESULT2 R5)
        (A=Rx R2)
        (M[Rx]=A R5)
        (label done)
        (j done)
        """,
        base=PROG_BASE,
        SP_INIT=SP_INIT, RESULT0=RESULT0, RESULT1=RESULT1, RESULT2=RESULT2,
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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for i in range(500):
                yield clk.posedge

            r0 = [0]; r1 = [0]; r2 = [0]
            yield _read_data(conf, clk, SLAVE_RESULT0, r0)
            yield _read_data(conf, clk, SLAVE_RESULT0 + 1, r1)
            yield _read_data(conf, clk, SLAVE_RESULT0 + 2, r2)
            result[0] = (r0[0], r1[0], r2[0])
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_push_pop_registers_via_conf_load'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(20000)

    ok = result[0] == EXPECTED
    print(f"{'PASS' if ok else 'FAIL'}: test_push_pop_registers_via_conf_load" +
          (f"  (expected {EXPECTED}, got {result[0]})" if not ok else ""))
    return ok


def test_slave_write_cpu_doubles():
    """Test: slave writes input, CPU reads and doubles it, slave reads result."""
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

            yield _load_program(conf, clk, _PROG_READ_DOUBLE_WRITE, PROG_BASE_WORDS)

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
    """Test: slave writes two inputs, CPU adds them, slave reads sum."""
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

            yield _load_program(conf, clk, _PROG_ADD_TWO, PROG_BASE_WORDS)

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
    as `(slave_pending_address - conf_map.imem_low/dmem_low) * 4` (the
    conf slave interface is word-addressed; the *4 converts to the byte
    offset the CPU's own byte-addressed memory actually uses), entirely
    independent of cpu_sys's own internal address decode (which always
    starts IMEM at physical 0) -- so the slave-side base is a pure
    addressing convention that must be exercised at nonzero/non-contiguous
    values, not just the imem_low=0 default every other test uses.

    imem_low/dmem_low: slave-bus WORD base addresses for IMEM/DMEM
    (arbitrary, independent of each other and of the CPU's own zero-based
    byte view).
    """
    mm = compute_memory_map(imem_depth, dmem_depth)  # CPU-side view: IMEM_LOW=0, DMEM_LOW=imem_depth

    # imem_low/dmem_low (like conf_map's fields generally) are word
    # addresses, but imem_depth/dmem_depth (like cpu_sys's own params)
    # are byte counts -- divide by 4 before combining the two.
    imem_high = imem_low + imem_depth // 4 - 1
    dmem_high = dmem_low + dmem_depth // 4 - 1

    # Physical DMEM byte offsets used by this test (same word, addressed
    # differently by the slave bus [word-addressed, // 4] vs. the CPU's
    # own program [byte-addressed, used as-is]).
    phys_result0 = 0x10
    phys_input0  = 0x18
    phys_input1  = 0x1C

    slave_result0 = dmem_low + phys_result0 // 4
    slave_input0  = dmem_low + phys_input0 // 4
    slave_input1  = dmem_low + phys_input1 // 4

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

            yield _load_program(conf, clk, prog, imem_low + PROG_BASE_WORDS)

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
    """Test: conf_map with a nonzero, contiguous slave-bus base --
    imem_low=0x1000, dmem immediately following imem (dmem_low=imem_high+1)."""
    imem_depth = 2048
    imem_low   = 0x1000
    dmem_low   = imem_low + imem_depth
    return test_conf_map_variant(imem_depth=imem_depth, dmem_depth=4096,
                                  imem_low=imem_low, dmem_low=dmem_low,
                                  label="offset")


def test_conf_map_gapped():
    """Test: conf_map with nonzero, non-contiguous imem_low/dmem_low --
    a deliberate gap between the IMEM and DMEM windows on the slave bus."""
    return test_conf_map_variant(imem_depth=16384, dmem_depth=65536,
                                  imem_low=0x10000, dmem_low=0x40000,
                                  label="gapped")


def test_master_request():
    """Test: Master reads address CONF_LOW and puts the result into accumulator."""
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

            yield _load_program(conf, clk, _PROG_MASTER_REQUEST_ADDRESS_0, PROG_BASE_WORDS)

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
    """Test: CPU selects tick bit 1, waits for it, stores count to DMEM.
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

            yield _load_program(conf, clk, _PROG_READ_TICKS, PROG_BASE_WORDS)

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
            yield _load_program(conf_a, clk, prog, PROG_BASE_WORDS)

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
    Test: CPU-A issues a master (Conf) read request. While CPU-A is waiting
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
            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

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
    """Test: CPU runs a program that increments a DMEM value once then halts.
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
            yield _load_program(conf, clk, _PROG_INCREMENT_ONCE, PROG_BASE_WORDS)
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
    Test: CPU writes distinct values to N different DMEM addresses while
    the TB simultaneously hammers the slave port with writes/reads to a
    separate DMEM region. Verifies that neither side's accesses are dropped
    or corrupted by the contention.

    slave_gap_cycles: idle clock cycles between consecutive slave
    transactions during the hammer phase. 0 = back-to-back (slave port
    saturated, CPU may starve for DMEM access and never make progress).
    Larger values give the CPU windows to fetch/execute between accesses.

    Memory layout, all in the DMEM region past CPU_RESULT0/SLAVE_RESULT0
    (byte addresses for the CPU's own program, word addresses -- // 4 --
    for the slave port):
      SLAVE_DONE      : done flag, CPU writes DONE_MAGIC here when finished
      CPU region      : CPU_CPU_BASE + 4*i (byte, written by CPU program),
                        SLAVE_CPU_BASE + i (word, read back via slave)
      TB region       : SLAVE_TB_BASE + i  (word, written by TB via slave
                        port only -- the CPU program never touches it)

    Verification: after done flag is seen, TB reads back both regions via
    the slave port and compares against expected values.
    """
    N          = 16
    DONE_MAGIC = 0xD1

    CPU_DONE     = CPU_RESULT0
    CPU_CPU_BASE = CPU_RESULT0 + 0x100

    SLAVE_DONE     = SLAVE_RESULT0
    SLAVE_CPU_BASE = SLAVE_RESULT0 + 0x100 // 4
    SLAVE_TB_BASE  = SLAVE_RESULT0 + 0x200 // 4

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
            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

            # trigger boot jump -- CPU starts storing to its region
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            # while the CPU runs, hammer the slave port: interleaved
            # writes and read-backs in the TB region, with a configurable
            # gap so the CPU gets memory cycles in between
            print(SLAVE_TB_BASE)
            for i in range(N):
                yield _write_data(conf, clk, tb_val(i), SLAVE_TB_BASE + i)
                for _ in range(slave_gap_cycles):
                    yield clk.posedge
                # immediate read-back of the previous slot for extra traffic
                if i > 0:
                    rb = [0]
                    yield _read_data(conf, clk, SLAVE_TB_BASE + (i - 1), rb)
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
                yield _read_data(conf, clk, SLAVE_CPU_BASE + i, rb)
                if rb[0] != cpu_val(i):
                    result[0] = (f"FAIL: CPU store slot {i} at slave addr "
                                 f"0x{SLAVE_CPU_BASE + i:04X}: got 0x{rb[0]:08X}, "
                                 f"expected 0x{cpu_val(i):08X}")
                    raise StopSimulation()

            # verify the TB's slave writes weren't corrupted by CPU traffic
            for i in range(N):
                rb = [0]
                yield _read_data(conf, clk, SLAVE_TB_BASE + i, rb)
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
            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

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
    """Test: slave write/read racing against a CPU dmem write, every cycle offset."""
    return test_cpu_slave_race('dmem_write')


def test_cpu_slave_race_dmem_read():
    """Test: slave write/read racing against a CPU dmem read, every cycle offset."""
    return test_cpu_slave_race('dmem_read')


def test_cpu_slave_race_master_read():
    """Test: slave write/read racing against a CPU master read, every cycle offset."""
    return test_cpu_slave_race('master_read')


def test_cpu_slave_race_master_write():
    """Test: slave write/read racing against a CPU master write, every cycle offset."""
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
    # conf.slave_request_address is word-addressed; these are the same
    # locations' word indices, for the _write_data calls below.
    IMEM_SCRATCH_SLAVE = IMEM_SCRATCH_CPU // 4
    IMEM_RACE_ADDR_SLAVE = IMEM_RACE_ADDR // 4

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
            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)

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
                yield _write_data(conf, clk, SENTINEL, IMEM_SCRATCH_SLAVE)
                yield _write_data(conf, clk, SENTINEL, IMEM_RACE_ADDR_SLAVE)
                if cpu_op == 'imem_read':
                    yield _write_data(conf, clk, RACE_VALUE, IMEM_SCRATCH_SLAVE)

                # trigger boot jump
                yield _write_data(conf, clk, 1, conf_map.interrupt)

                # wait exactly `d` cycles, then fire the ONE slave
                # transaction that actually overlaps the running CPU
                for _ in range(d):
                    yield clk.posedge

                yield _write_data(conf, clk, SLAVE_RACE_WRITE_VAL, IMEM_RACE_ADDR_SLAVE)

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


def test_imem_slave_read_while_running():
    """Conf-slave READ of an ordinary (RAM-backed, non-boot-ROM) IMEM word
    while the CPU spins in a separate loop loaded at PROG_BASE.

    conf_slave()'s SLAVE_WAIT state has no read path for the IMEM address
    range at all (cpu_sys.py just prints "TODO: implement reading from
    imem" and does nothing) -- the FSM still advances to SLAVE_READ2 and
    reports reply_status=1, but slave_reply_data is whatever dmem_dout
    happened to hold, not the real IMEM content.

    KNOWN FAILING -- documents that IMEM cannot be read through the conf
    slave interface at all (see also test_boot_rom_slave_read_while_running
    for the boot-ROM case, which hits this same missing read path).
    """
    KNOWN_VALUE = 0xDEADBEEF
    IMEM_TARGET       = PROG_BASE + 64  # clear of the tiny spin-loop program
    IMEM_TARGET_SLAVE = IMEM_TARGET // 4
    result = [None]

    prog = assemble(
        """
        (label done)
        (j done)
        """,
        base=PROG_BASE,
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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)
            yield _write_data(conf, clk, KNOWN_VALUE, IMEM_TARGET_SLAVE)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            # let the CPU actually be running before the slave read fires
            for _ in range(20):
                yield clk.posedge

            readback = [0]
            yield _read_data(conf, clk, IMEM_TARGET_SLAVE, readback)
            if readback[0] == KNOWN_VALUE:
                result[0] = "PASS"
            else:
                result[0] = f"FAIL: expected 0x{KNOWN_VALUE:08X}, got 0x{readback[0]:08X}"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_imem_slave_read_while_running'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_imem_slave_read_while_running" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_boot_rom_slave_write():
    """Conf-slave WRITE to word 0 of the boot ROM (the first byte of the
    hardcoded boot-read-interrupt program), while a program loaded at
    PROG_BASE reads that same address back via an ordinary CPU load and
    reports it through DMEM.

    cpu_sys.py's rom() keeps the boot code in a plain Python tuple
    (`content`) that nothing ever writes to. Its wenable/waddr inputs are
    wired straight into the RAM (`pmem`) that starts right after that
    content, via `w_ram_addr = (waddr - len(content)) & mask` -- for a
    ROM-range waddr this underflows and aliases into some RAM cell
    instead of the ROM (and can land outside pmem's own depth, crashing
    the MyHDL model the same way test_imem_write_from_global's bug does).

    KNOWN FAILING -- documents that boot ROM content cannot be modified
    through the conf slave interface: either the write silently lands
    somewhere else (readback != MARKER) or the model crashes.
    """
    MARKER = 0x11223344
    BOOT_TARGET_SLAVE = 0  # word 0 -- inside the default boot ROM
    result = [None]

    prog = assemble(
        """
        (Rx= 0 R0)
        (A=M[Rx] R0)
        (Rx= RESULT_PHYS R1)
        (M[Rx]=A R1)
        (label done)
        (j done)
        """,
        base=PROG_BASE,
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

            yield _write_data(conf, clk, 0, SLAVE_RESULT0)
            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)
            yield _write_data(conf, clk, MARKER, BOOT_TARGET_SLAVE)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            MAX_POLLS = 400
            readback = [0]
            for _ in range(MAX_POLLS):
                yield _read_data(conf, clk, SLAVE_RESULT0, readback)
                if readback[0] != 0:
                    break
                yield clk.posedge

            if readback[0] == MARKER:
                result[0] = "PASS"
            else:
                result[0] = f"FAIL: expected 0x{MARKER:08X}, got 0x{readback[0]:08X}"
            raise StopSimulation()

        return instances()

    try:
        traceSignals.filename = 'trace_boot_rom_slave_write'
        itb = traceSignals(tb)
        sim = Simulation(itb)
        sim.run(500000)
    except IndexError as e:
        result[0] = f"FAIL: RTL simulation crashed ({e})"

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_boot_rom_slave_write" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


def test_boot_rom_slave_read_while_running():
    """Conf-slave READ of boot ROM word 0, while the CPU spins in a
    separate loop loaded at PROG_BASE. Same missing read path as
    test_imem_slave_read_while_running, just targeting the boot ROM
    region instead of RAM-backed IMEM -- conf_slave() doesn't special-case
    ROM vs RAM, it just never implements IMEM reads at all.

    Expected word 0 is computed by re-assembling cpu_sys.py's own
    hardcoded default boot program (INTERRUPT_ADDRESS read + jump to
    PROG_BASE), since that's what should be sitting at address 0.

    KNOWN FAILING -- documents that boot ROM content cannot be read back
    through the conf slave interface either.
    """
    boot_word0_bytes = assemble(
        """
        (Rx= INTERRUPT_ADDRESS R0)
        (A=M[Rx].b R0)
        (Rx= PROG_BASE R0)
        (A=Rx R0)
        (j-a)
        """,
        INTERRUPT_ADDRESS=INTERRUPT_ADDRESS,
        PROG_BASE=512,
    )
    expected = (boot_word0_bytes[0]
                | (boot_word0_bytes[1] << 8)
                | (boot_word0_bytes[2] << 16)
                | (boot_word0_bytes[3] << 24))

    result = [None]

    prog = assemble(
        """
        (label done)
        (j done)
        """,
        base=PROG_BASE,
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

            yield _load_program(conf, clk, prog, PROG_BASE_WORDS)
            yield _write_data(conf, clk, 1, conf_map.interrupt)

            for _ in range(20):
                yield clk.posedge

            readback = [0]
            yield _read_data(conf, clk, 0, readback)
            if readback[0] == expected:
                result[0] = "PASS"
            else:
                result[0] = f"FAIL: expected 0x{expected:08X}, got 0x{readback[0]:08X}"
            raise StopSimulation()

        return instances()

    traceSignals.filename = 'trace_boot_rom_slave_read_while_running'
    itb = traceSignals(tb)
    sim = Simulation(itb)
    sim.run(500000)

    ok = result[0] == "PASS"
    print(f"{'PASS' if ok else 'FAIL'}: test_boot_rom_slave_read_while_running" +
          (f"  ({result[0]})" if not ok else ""))
    return ok


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

    #results.append(test_slave_dmem_rw())
    #results.append(test_cpu_stores_constant())
    #results.append(test_boot_code_from_file())
    #results.append(test_slave_write_cpu_doubles())
    #results.append(test_slave_write_cpu_sum())
    #results.append(test_conf_map_offset())
    #results.append(test_conf_map_gapped())
    #results.append(test_wait_ticks())
    #results.append(test_master_while_slave_request())
    #results.append(test_cpu_memory_access_slave_conflict())
    #results.append(test_cpu_reset())
    #results.append(test_dual_cpu())
    #results.append(test_cpu_slave_race_dmem_write())
    #results.append(test_cpu_slave_race_dmem_read())
    #results.append(test_cpu_slave_race_master_read())
    #results.append(test_cpu_slave_race_master_write())
    #results.append(test_no_print_c_program())
    #results.append(test_console_output())
    #results.append(test_puts_c_program())
    #results.append(test_console_output_c_program())
    #results.append(test_read_outch_via_conf_load())
    #results.append(test_print_c_program_via_conf_load())
    #results.append(test_read_coreversion())
    #results.append(test_imem_read_corrupts_register())
    #results.append(test_imem_word_read())
    #results.append(test_imem_offset_read())
    #results.append(test_imem_word_read_via_conf_load())
    #results.append(test_imem_write_from_global())
    #results.append(test_push_pop_registers())
    #results.append(test_push_pop_registers_via_conf_load())
    results.append(test_imem_slave_read_while_running())
    results.append(test_boot_rom_slave_write())
    results.append(test_boot_rom_slave_read_while_running())

    passed = sum(results)
    total  = len(results)
    print(f"\n{passed}/{total} tests passed")
    sys.exit(0 if passed == total else 1)
