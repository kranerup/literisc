from myhdl import *
from modules.common.signal import signal

def dp_mem(
    idata,
    odata,
    raddr,
    waddr,
    renable,
    wenable,
    wmask,
    clk,
    clk_en,
    depth,
    name):

    width = len(idata)
    mask_bits = len(wmask) 
    col_width = width // mask_bits
    nr_col = mask_bits

    data = [ signal(width) for _ in range( depth ) ]

    @always(clk.posedge)
    def porta():
        if clk_en == 1:
            #print("wenable",wenable,"renable",renable,"waddr",int(waddr),"raddr",int(raddr),"depth",depth)
            if wenable == 1:
                if int(waddr) > int(depth):
                    print("wenable and waddr > depth",int(waddr),depth,len(waddr))
                for i in range(nr_col):
                    idx = i * col_width
                    if wmask[i] == 1:
                        data[ waddr ].next[idx+col_width:idx] = idata[idx+col_width:idx]
            if renable == 1:
                odata.next = data[ raddr ]

    return instances()

    # // Port-A Operation
    # always @ (posedge clkA) begin
    #   if(enaA) begin
    #       for(i=0;i<NUM_COL;i=i+1) begin
    #           if(weA[i]) begin
    #           ram_block[addrA][i*COL_WIDTH +: COL_WIDTH] <= dinA[i*COL_WIDTH +: COL_WIDTH];
    #           end
    #       end
    #       doutA <= ram_block[addrA];
    #   end
    # end
