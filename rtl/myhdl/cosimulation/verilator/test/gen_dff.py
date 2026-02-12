import myhdl
from myhdl import *
from myhdl.conversion import analyze

import sys

#def dff(q, d, clk):
#
#    @always(clk.posedge)
#    def logic():
#        q.next = d
#
#    return logic

def dff(q, d, clk, rst):

    @always(clk.posedge, rst.negedge)
    def logic():
        if rst == 0:
            q.next = 0
        else:
            q.next = d

    return logic


def main(BITS):
    q, d = [Signal(intbv(0,min=0,max=1<<BITS)) for i in range(2)]
    clk, reset = [Signal(bool(0)) for i in range(2)]

    toVerilog.name = 'dff'
    toVerilog(dff, q, d, clk, reset)
 
if __name__ == '__main__':
    ff_width = int(sys.argv[1])
    main(ff_width)
