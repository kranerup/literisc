from myhdl import *

from modules.common.memory import memory
from modules.common.signal import signal
from modules.common.Common import copySignal, multiflop

def axi_periphery( clk, rstn, axi, gpio ):

    wr = signal()
    wr_data = signal( axi.dsize )
    wr_addr = signal( axi.asize )

    @always(clk.posedge,rstn.negedge)
    def gpio_slave():
        if rstn == 0:
            axi.awready.next = 0
            axi.wready.next = 0
        else:
            if wr == 1:
                wr.next = 0
            if axi.awready == 1 and axi.wready == 1:
                axi.awready.next = 0
                axi.wready.next = 0
            if axi.awvalid == 1:
                axi.awready.next = 0
                wr_addr.next = axi.awaddr
            if axi.wvalid == 1:
                axi.wready.next = 0
                wr_data.next = axi.wdata
            if axi.awvalid == 1 and axi.wvalid == 1:
                wr.next = 1
                print("gpio write",wr_addr,wr_data)
                gpio.next = wr_data
                axi.awready.next = 1
                axi.wready.next = 1

    return instances()
