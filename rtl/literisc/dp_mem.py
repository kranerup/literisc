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

    word_depth = depth
    data = [ signal(width) for _ in range( word_depth ) ]

    @always(clk.posedge)
    def porta():
        if clk_en == 1:
            if wenable == 1:
                widx = int(waddr) // mask_bits
                if widx >= word_depth:
                    print("wenable and waddr >= depth",int(waddr),depth,len(waddr))
                for i in range(nr_col):
                    idx = i * col_width
                    if wmask[i] == 1:
                        data[ widx ].next[idx+col_width:idx] = idata[idx+col_width:idx]
            if renable == 1:
                ridx = int(raddr) // mask_bits
                lane = int(raddr) % mask_bits
                # Always shift the requested byte down to bit 0 -- a no-op
                # for word reads, since those are always lane-0-aligned.
                odata.next = data[ ridx ] >> (col_width * lane)

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
