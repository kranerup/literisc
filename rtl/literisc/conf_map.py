from constants import (IMEM_LOW, IMEM_HIGH, DMEM_LOW, DMEM_HIGH,
                       INTERRUPT_ADDRESS, CPU_RESET_ADDRESS)

class ConfMap:
    def __init__(self,
                 imem_low=IMEM_LOW,
                 imem_high=IMEM_HIGH,
                 dmem_low=DMEM_LOW,
                 dmem_high=DMEM_HIGH,
                 interrupt=DMEM_HIGH+1,
                 cpu_reset=DMEM_HIGH+2):
        self.imem_low = imem_low
        self.imem_high = imem_high
        self.dmem_low = dmem_low
        self.dmem_high = dmem_high
        self.interrupt = interrupt
        self.cpu_reset = cpu_reset
