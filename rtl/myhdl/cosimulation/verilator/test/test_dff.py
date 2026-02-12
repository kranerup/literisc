
import sys
import random
from random import randrange
random.seed(2)

from myhdl import Simulation, StopSimulation, Signal, \
                  delay, intbv, negedge, posedge, now, toVerilog

from dff_cosim import dff

ACTIVE_LOW, INACTIVE_HIGH = 0, 1

class TestDff(object):

    def __init__(self,ff_bits):
        self.ff_bits = ff_bits
        self.vals = [ 1<<i for i in range(ff_bits) ]
        self.vals += [randrange(1<<self.ff_bits) for i in range(1000)]

    def clkGen(self, clk):
        while 1:
            yield delay(10)
            clk.next = not clk
            
    
    def stimulus(self, d, clk, rst):
        rst.next = ACTIVE_LOW
        yield negedge(clk)
        rst.next = INACTIVE_HIGH
        for v in self.vals:
            d.next = v
            print('stim',v)
            yield negedge(clk)
        raise StopSimulation
    
    
    def check(self, q, clk, rst):
        yield posedge(rst)
        v_Z = 0
        first = 1
        for v in self.vals:
            yield posedge(clk)
            if not first:
                print('check 1')
                assert q == v_Z
            first = 0
            yield delay(3)
            print('check 2', q, v)
            assert q == v
            v_Z = v
            

    def bench(self):
        
        q, d = [Signal(intbv(0,min=0,max=1<<self.ff_bits)) for i in range(2)]
        clk, rst = [Signal(intbv(0)) for i in range(2)]
        
        DFF_1 = dff(q, d, clk, rst)
        CLK_1 = self.clkGen(clk)
        ST_1 = self.stimulus(d, clk, rst)
        CH_1 = self.check(q, clk, rst)
        
        sim = Simulation(DFF_1, CLK_1, ST_1, CH_1)
        return sim
    

    def test1(self):
        """ dff test """
        sim = self.bench()
        sim.run()

if __name__ == '__main__':
    t = TestDff( int(sys.argv[1]) )
    t.test1()
