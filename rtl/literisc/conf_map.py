class ConfMap:
    def __init__(self, imem_low, imem_high, dmem_low, dmem_high, interrupt, cpu_reset):
        self.imem_low = imem_low
        self.imem_high = imem_high
        self.dmem_low = dmem_low
        self.dmem_high = dmem_high
        self.interrupt = interrupt
        self.cpu_reset = cpu_reset

