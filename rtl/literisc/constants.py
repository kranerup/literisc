# Memory map, derived from IMEM_DEPTH/DMEM_DEPTH:
#   IMEM             : IMEM_LOW .. IMEM_HIGH   (boot rom + RAM), IMEM_DEPTH bytes
#   DMEM             : DMEM_LOW .. DMEM_HIGH   DMEM_DEPTH bytes
#   PERIPHERAL IO    : IO_LOW   .. IO_HIGH     (UART, GPIO etc), IO_DEPTH bytes
#   Special registers: IRQ, TICK, INTERRUPT, CPU_RESET (4 bytes)
#   CONF bus window  : CONF_LOW .. CONSOLE_ADDRESS - 1
#   Console register : CONSOLE_ADDRESS (top of the 32-bit space, carved out of
#                       the CONF window; matches the emulator's _outch/putchar
#                       convention in include/stdio.h)

PERIP_ADDR_BITS    = 16
PERIP_DATA_BITS    = 32
CPU_DMEM_DATA_BITS = 32

CONSOLE_ADDRESS = 0xFFFFFFFF

IMEM_DEPTH  = 2048
DMEM_DEPTH  = 8192
IO_DEPTH    = 100
N_SPECIAL_REGS = 4   # IRQ, TICK, INTERRUPT, CPU_RESET
CONF_WINDOW_SIZE = 2**24 - 2**16   # size of the CONF bus window (matches prior CONF_HIGH - CONF_LOW)

class MemoryMap:
    def __init__(self, imem_depth, dmem_depth):
        assert imem_depth > 0 and dmem_depth > 0, "imem_depth and dmem_depth must be positive"

        self.IMEM_LOW  = 0
        self.IMEM_HIGH = imem_depth * 4 - 1

        self.DMEM_LOW  = self.IMEM_HIGH + 1
        self.DMEM_HIGH = self.DMEM_LOW + dmem_depth * 4 - 1

        self.IO_LOW  = self.DMEM_HIGH + 1
        self.IO_HIGH = self.IO_LOW + IO_DEPTH - 1

        self.CPU_RESET_ADDRESS = self.IO_HIGH + 1
        self.INTERRUPT_ADDRESS = self.IO_HIGH + 2
        self.TICK_ADDRESS      = self.IO_HIGH + 3
        self.IRQ_ADDRESS       = self.IO_HIGH + 4

        # CONF bus window, immediately after peripheral IO
        self.CONF_LOW  = self.IO_HIGH + 5
        self.CONF_HIGH = self.CONF_LOW + CONF_WINDOW_SIZE - 1

def compute_memory_map(imem_depth=IMEM_DEPTH, dmem_depth=DMEM_DEPTH):
    return MemoryMap(imem_depth, dmem_depth)

# Default map, used by every module-level name below so that
# `from constants import *` keeps working unchanged for the default sizes.
_default_map = compute_memory_map()

IMEM_LOW    = _default_map.IMEM_LOW
IMEM_HIGH   = _default_map.IMEM_HIGH

DMEM_LOW    = _default_map.DMEM_LOW
DMEM_HIGH   = _default_map.DMEM_HIGH

IRQ_ADDRESS       = _default_map.IRQ_ADDRESS
TICK_ADDRESS      = _default_map.TICK_ADDRESS
INTERRUPT_ADDRESS = _default_map.INTERRUPT_ADDRESS
CPU_RESET_ADDRESS = _default_map.CPU_RESET_ADDRESS

IO_LOW  = _default_map.IO_LOW
IO_HIGH = _default_map.IO_HIGH

CONF_LOW  = _default_map.CONF_LOW
CONF_HIGH = _default_map.CONF_HIGH
