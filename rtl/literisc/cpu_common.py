from myhdl import *

def flop( i, o, clk_en, clk, sync_rstn, reset_value=0 ):

    if clk_en is None:
        @always(clk.posedge)
        def ff():
            if sync_rstn == 0:
                o.next = reset_value
            else:
                o.next = i
    else:
        @always(clk.posedge)
        def ff():
            if sync_rstn == 0:
                o.next = reset_value
            else:
                if clk_en == 1:
                    o.next = i
    return instances()
