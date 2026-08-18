from constants import IMEM_DEPTH, DMEM_DEPTH

# ConfMap addresses are word-addressed (conf.slave_request_address is a
# word index -- one conf slave transaction is always a full 32-bit word),
# distinct from the CPU's own byte-addressed memory map (IMEM_LOW/
# IMEM_HIGH/DMEM_LOW/DMEM_HIGH in constants.py, which are 4x these values).
# IMEM_DEPTH/DMEM_DEPTH are already word counts, used directly here.

class ConfMap:
    def __init__(self,
                 imem_low=0,
                 imem_high=IMEM_DEPTH - 1,
                 dmem_low=IMEM_DEPTH,
                 dmem_high=IMEM_DEPTH + DMEM_DEPTH - 1,
                 interrupt=IMEM_DEPTH + DMEM_DEPTH,
                 cpu_reset=IMEM_DEPTH + DMEM_DEPTH + 1):
        self.imem_low = imem_low
        self.imem_high = imem_high
        self.dmem_low = dmem_low
        self.dmem_high = dmem_high
        self.interrupt = interrupt
        self.cpu_reset = cpu_reset
