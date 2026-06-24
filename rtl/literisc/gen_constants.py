from constants import *

constants = {
    'IMEM_LOW':           IMEM_LOW,
    'IMEM_HIGH':          IMEM_HIGH,
    'DMEM_LOW':           DMEM_LOW,
    'DMEM_HIGH':          DMEM_HIGH,
    'IRQ_ADDRESS':        IRQ_ADDRESS,
    'TICK_ADDRESS':       TICK_ADDRESS,
    'INTERRUPT_ADDRESS':  INTERRUPT_ADDRESS,
    'CPU_RESET_ADDRESS':  CPU_RESET_ADDRESS,
    'IO_LOW':             IO_LOW,
    'IO_HIGH':            IO_HIGH,
    'CONF_LOW':           CONF_LOW,
    'CONF_HIGH':          CONF_HIGH,
}

with open('constants.lisp', 'w') as f:
    for name, val in constants.items():
        f.write(f'(defconstant +{name.lower()}+ {val})\n')
