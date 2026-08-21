"""
run_c_program.py - Run a C program against the MyHDL cpu_sys RTL model,
printing its console output (printf/putchar via include/stdio.h) exactly as
running it on the Lisp emulator would.

Also used as the RTL backend for `lrcc.lisp --rtl`: lrcc compiles the C
source itself and invokes this script with the resulting .mem file, so the
program is compiled exactly once regardless of entry point.

Accepts either:
  - a .c source file: compiled via lrcc (-I/-O/-Os apply), then run, or
  - a .mem/.vmem/.hex file: an already-assembled $readmemh image, run as-is
    (compile flags are ignored in this mode).

The program's own output goes to stdout (via cpu_sys's console_out, a store
to CONSOLE_ADDRESS); status/progress messages go to stderr, so stdout can be
diffed directly against `lrcc.lisp -r`'s stdout.

Usage:
    ./gen.sh run_c_program.py [options] source.c
    ./gen.sh run_c_program.py [options] program.mem

Options:
    -I <dir>          preprocessor include dir (repeatable, default: include/)
    -O                 optimize for speed
    -Os                optimize for size (default)
    --cycles <n>       clock cycles to run (default: 20000)
    --imem-size <n>    IMEM depth in bytes (default: matches constants.py)
    --dmem-size <n>    DMEM depth in bytes (default: matches constants.py)
    --trace            write a VCD trace (trace_run_c_program.vcd)
    --keep-mem <path>  also write the compiled $readmemh .mem file to PATH
                       (ignored when source is already a .mem/.vmem/.hex file)
"""
import argparse
import os
import subprocess
import sys
import tempfile

from myhdl import *
from modules.common.signal import signal
from conf_map import ConfMap
from axi import Axi4
from conf import Conf
from cpu_sys import cpu_sys
from constants import IMEM_DEPTH, DMEM_DEPTH

_LITERISC_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
_LRCC = os.path.join(_LITERISC_ROOT, "lrcc.lisp")
_DEFAULT_INCLUDE_DIR = os.path.join(_LITERISC_ROOT, "include")


def compile_c_to_mem(source_path, mem_path, include_dirs, optimize):
    """Compile source_path with lrcc straight to a $readmemh-format .mem
    file at mem_path, suitable for cpu_sys's boot_code_path."""
    cmd = [_LRCC]
    for inc in include_dirs:
        cmd += ["-I", inc]
    cmd.append("-O" if optimize == "speed" else "-Os")
    cmd += ["-o", mem_path, source_path]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
    if result.stderr:
        sys.stderr.write(result.stderr)
    if result.returncode != 0:
        raise SystemExit(f"lrcc compile failed (exit {result.returncode})")


def run_in_myhdl(mem_path, cycles, imem_depth, dmem_depth, trace):
    """Boot mem_path directly (PC=0, no interrupt trigger) in cpu_sys and
    run it for `cycles` clock edges."""

    def tb():
        clk  = Signal(bool())
        rstn = signal()
        axi  = Axi4(asize=16, dsize=32, idsize=1)
        conf = Conf()
        instr_trace = Signal(modbv(0)[69:])
        conf_map = ConfMap()
        icpu = cpu_sys(clk, rstn, axi, conf, instr_trace, conf_map=conf_map,
                        boot_code_path=mem_path,
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
            for i in range(cycles):
                yield clk.posedge
            raise StopSimulation()

        return instances()

    if trace:
        traceSignals.filename = 'trace_run_c_program'
        itb = traceSignals(tb)
    else:
        itb = tb()
    sim = Simulation(itb)
    sim.run(cycles * 20 + 1000)


def main():
    parser = argparse.ArgumentParser(
        description="Compile a C program with lrcc and run it against the "
                     "MyHDL cpu_sys RTL model.")
    parser.add_argument("source", help="C source file")
    parser.add_argument("-I", dest="include_dirs", action="append", default=[],
                         help="preprocessor include dir (default: include/)")
    opt = parser.add_mutually_exclusive_group()
    opt.add_argument("-O", dest="optimize", action="store_const", const="speed",
                      help="optimize for speed")
    opt.add_argument("-Os", dest="optimize", action="store_const", const="size",
                      help="optimize for size (default)")
    parser.add_argument("--cycles", type=int, default=20000,
                         help="clock cycles to run (default: 20000)")
    parser.add_argument("--imem-size", type=int, default=IMEM_DEPTH)
    parser.add_argument("--dmem-size", type=int, default=DMEM_DEPTH)
    parser.add_argument("--trace", action="store_true", help="write a VCD trace")
    parser.add_argument("--keep-mem", metavar="PATH",
                         help="also write the compiled .mem file here instead of a temp file")
    args = parser.parse_args()

    already_compiled = os.path.splitext(args.source)[1].lower() in (".mem", ".vmem", ".hex")

    if already_compiled:
        mem_path = args.source
        cleanup = False
    elif args.keep_mem:
        mem_path = args.keep_mem
        cleanup = False
    else:
        fd, mem_path = tempfile.mkstemp(suffix=".mem")
        os.close(fd)
        cleanup = True

    try:
        if not already_compiled:
            include_dirs = args.include_dirs or [_DEFAULT_INCLUDE_DIR]
            sys.stderr.write(f"Compiling {args.source} ...\n")
            compile_c_to_mem(args.source, mem_path, include_dirs, args.optimize)
        sys.stderr.write(f"Running {args.cycles} cycles in MyHDL simulation ...\n")
        run_in_myhdl(mem_path, args.cycles, args.imem_size, args.dmem_size, args.trace)
    finally:
        if cleanup:
            os.unlink(mem_path)


if __name__ == "__main__":
    main()
