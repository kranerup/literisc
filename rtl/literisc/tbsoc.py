
from myhdl import *

from modules.common.signal import signal
from soc import soc

def tb():
    clk = Signal(bool())
    rstn = signal()
    gpio = signal(32)
    ser_txd = signal(8)
    ser_rxd = signal(8)
    ser_send = signal()
    ser_ready = signal()
    ser_rx_ready = signal()
    ser_rx_ack = signal()

    isoc = soc( clk, rstn, gpio,
               ser_txd, ser_send, ser_ready,
               ser_rxd, ser_rx_ready, ser_rx_ack )


    @always(delay(10))
    def stim():
        #if clk == 0: print("=========== CLOCK ============")
        clk.next = not clk

    @instance
    def seq():
      rstn.next = 0
      yield clk.posedge
      rstn.next = 1 
      yield clk.posedge
      print("====== reset done =======")

    @instance
    def ready():
        while True:
            ser_ready.next = 0
            for i in range(10):
                yield clk.posedge
            ser_ready.next = 1
            for i in range(10):
                yield clk.posedge

    return instances()


traceSignals.filename = 'trace'
itb = traceSignals( tb ) 
sim = Simulation( itb )
sim.run( 10000 )
