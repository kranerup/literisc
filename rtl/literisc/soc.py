from myhdl import *
from axi import Axi4
from axi_slaves import axi_periphery
from cpu_sys import cpu_sys

def soc( clk, rstn, gpio ):

    axi = Axi4(asize=16, dsize=32, idsize=1)

    icpusys = cpu_sys( clk, rstn, axi )

    iperiph = axi_periphery( clk, rstn, axi, gpio )

    return instances()