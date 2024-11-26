from myhdl import *

from cpu import cpu


clk = Signal(bool())
rstn = Signal(intbv(0)[1:0])
imem_radr = Signal(modbv(0)[16:])
imem_wadr = Signal(modbv(0)[16:])
imem_din = Signal(modbv(0)[8:])
imem_dout = Signal(modbv(0)[8:])
imem_rd = Signal(modbv(0)[1:])
imem_wr = Signal(modbv(0)[1:])

dmem_adr = Signal(modbv(0)[16:])
dmem_din = Signal(modbv(0)[32:])
dmem_dout = Signal(modbv(0)[32:])
dmem_rd = Signal(modbv(0)[1:])
dmem_wr = Signal(modbv(0)[1:])
dmem_wr_sz = Signal(modbv(0)[2:])

halt = Signal(modbv(0)[1:])
intr = Signal(modbv(0)[1:])

toVerilog.standard = 'systemverilog'
itop = toVerilog( cpu, clk, rstn,
        imem_dout,
        imem_radr,
        imem_rd,
        dmem_din,
        dmem_dout,
        dmem_adr,
        dmem_rd,
        dmem_wr,
        dmem_wr_sz,
        halt,
        intr,
        False,
        None,
        None,
        None,
        None,
        False)
    
