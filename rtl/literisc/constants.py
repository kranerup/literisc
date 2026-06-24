# Memory map:
# 0x00000 - 0x1FFF  : IMEM (boot rom + RAM), 8KB
# 0x02000 - 0x09FFF : DMEM, 32KB
# 0x0FF98 - 0x0FF9B : Special registers (IRQ, TICK, INTERRUPT, CPU_RESET)
# 0x0FF9C - 0x0FFFF : PERIPHERAL IO, 100 bytes (UART, GPIO etc)
# 0x10000 - 0xFFFFFFFFF : CONF bus window

PERIP_ADDR_BITS    = 16
PERIP_DATA_BITS    = 32
CPU_DMEM_DATA_BITS = 32

# IMEM
IMEM_DEPTH  = 8192
IMEM_LOW    = 0
IMEM_HIGH   = IMEM_DEPTH - 1             # 0x1FFF

# DMEM
DMEM_DEPTH  = 32768
DMEM_LOW    = IMEM_DEPTH                 # 0x2000
DMEM_HIGH   = DMEM_LOW + DMEM_DEPTH - 1 # 0x9FFF

# Peripheral IO (UART, GPIO etc) - matches emulator lr-soc
IO_DEPTH    = 100
IO_HIGH     = 2**16 - 1                  # 0xFFFF
IO_LOW      = IO_HIGH - IO_DEPTH + 1     # 0xFF9C

# Special registers (immediately before peripheral IO)
CPU_RESET_ADDRESS = IO_LOW - 1           # 0xFF9B
INTERRUPT_ADDRESS = IO_LOW - 2           # 0xFF9A
TICK_ADDRESS      = IO_LOW - 3           # 0xFF99
IRQ_ADDRESS       = IO_LOW - 4           # 0xFF98

# CONF bus (AXI slave interface)
CONF_LOW    = 2**16                      # 0x10000
CONF_HIGH   = 2**32 - 1                 # 0xFFFFFFFF
