from myhdl import *

from modules.common.signal import signal
from soc import soc
from axi import Axi4
from conf import Conf
from cpu_sys import cpu_sys

def tb():
    clk = Signal(bool())
    rstn = signal()

    #axi1 = Axi4(asize=16, dsize=32, idsize=1)
    #conf1 = Conf()

    #axi2 = Axi4(asize=16, dsize=32, idsize=1)
    #conf2 = Conf()

    #conf1.slave_request_address  = conf2.master_request_address
    #conf1.slave_request_data     = conf2.master_request_data
    #conf1.slave_request_id       = conf2.master_request_id
    #conf1.slave_request_type     = conf2.master_request_type
    #conf1.slave_request_re       = conf2.master_request_re
    #conf1.slave_request_we       = conf2.master_request_we

    #conf2.master_reply_data      = conf1.slave_reply_data
    #conf2.master_reply_id        = conf1.slave_reply_id
    #conf2.master_reply_status    = conf1.slave_reply_status

    #icpusys1 = cpu_sys( clk, rstn, axi1 , conf1)
    #icpusys2 = cpu_sys( clk, rstn, axi2, conf2)


    axi = Axi4(asize=16, dsize=32, idsize=1)
    conf = Conf()
    icpusys = cpu_sys(clk, rstn, axi, conf)

    @always(clk.posedge)
    def increment_ticks():
        conf.ticks.next = conf.ticks + 1

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

      conf.slave_request_data.next = 23124
      conf.slave_request_address.next = 0
      conf.slave_request_we.next = 1
      print("====== writing master request to slave =======")
      yield clk.posedge
      conf.slave_request_we.next = 0

      while conf.slave_reply_status == 0:
            yield clk.posedge
      print("write finished")
      yield clk.posedge

      conf.slave_request_address.next = 0
      conf.slave_request_re.next = 1

      yield clk.posedge

      conf.slave_request_re.next = 0

      print("test")

      while conf.slave_reply_status == 0:
            print(conf.slave_reply_data)
            yield clk.posedge
            print(conf.slave_reply_data)

      print("read finished:")
      
      print(conf.slave_reply_data)

#    @instance
#    def ready():
#        while True:
#            ser_ready.next = 0
#            for i in range(10):
#                yield clk.posedge
#            ser_ready.next = 1
#            for i in range(10):
#                yield clk.posedge
#
    return instances()


traceSignals.filename = 'trace'
itb = traceSignals( tb ) 
sim = Simulation( itb )
sim.run( 10000 )
