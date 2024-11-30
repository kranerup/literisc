from myhdl import *

from modules.common.memory import memory
from modules.common.signal import signal
from modules.common.Common import copySignal, multiflop

def axi_periphery(clk, sync_rstn, axi, gpio,
                  serial_tx_data,
                  serial_tx_send,
                  serial_tx_ready):

    wr = signal()
    rd_addr = signal( axi.asize )
    rd = signal()

    gpio_address = 0
    serial_tx_data_address = 1
    serial_tx_status_address = 2
    serial_rx_data_address = 3
    serial_rx_status_address = 4

    # =========== write channel ================
    #    M           S
    # awvalid -> 
    # awaddr  ->
    #         <-  awready
    # wvalid  ->
    # wdata   ->
    #         <-  wready
    @always(clk.posedge)
    def gpio_wr_slave():
        if sync_rstn == 0:
            axi.awready.next = 0 # we start with channels not ready
            axi.wready.next = 0
            serial_tx_send.next = 0
            wr.next = 0
            axi.awready.next = 0
            gpio.next = 0
            serial_tx_data.next = 0
            serial_tx_send.next = 0
        else:
            serial_tx_send.next = 0

            # last cycle was a write, take down ready
            if wr == 1:
                wr.next = 0
                axi.awready.next = 0
                axi.wready.next = 0
            # wait till both channels available
            elif axi.awvalid == 1 and axi.wvalid == 1:
                wr.next = 1
                if axi.awaddr == gpio_address:
                    print("gpio write",axi.awaddr,axi.wdata)
                    gpio.next = axi.wdata
                elif axi.awaddr == serial_tx_data_address:
                    print("serial write",axi.awaddr,axi.wdata)
                    # serial port must be ready when writing to this
                    # register
                    serial_tx_data.next = axi.wdata[8:]
                    serial_tx_send.next = 1
                # ack both channels that we're ready
                axi.awready.next = 1
                axi.wready.next = 1



    # =========== read channel ================
    #    M           S
    # arvalid -> 
    # araddr  ->
    #         <-  arready
    #         <-  rvalid
    #         <-  rdata
    # rready  ->

    @always(clk.posedge)
    def gpio_rd_slave():
        if sync_rstn == 0:
            axi.arready.next = 0
            axi.rvalid.next = 0
            rd.next = 0
            rd_addr.next = 0
            axi.rdata.next = 0
        else:
            if rd == 0:
                axi.arready.next = 0
                axi.rvalid.next = 0

                if axi.arvalid == 1:
                    rd.next = 1
                    rd_addr.next = axi.araddr
                    axi.arready.next = 1
            else:
                axi.arready.next = 0
                if axi.rvalid == 0: # latch read data on first read cycle
                    if rd_addr == serial_tx_status_address:
                        axi.rdata.next = serial_tx_ready # bit 0 - ready status bit
                axi.rvalid.next = 1
                if axi.rready == 1:
                    rd.next = 0

    return instances()
