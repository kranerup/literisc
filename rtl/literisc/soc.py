from myhdl import *
from axi import Axi4
from axi_slaves import axi_periphery
from cpu_sys import cpu_sys

def soc( clk, sync_rstn, gpio,
        serial_tx_data,
        serial_tx_send,
        serial_tx_ready):

    axi = Axi4(asize=16, dsize=32, idsize=1)

    icpusys = cpu_sys( clk, sync_rstn, axi )

    iperiph = axi_periphery(
        clk, sync_rstn, axi, gpio,
        serial_tx_data,
        serial_tx_send,
        serial_tx_ready)

    return instances()
