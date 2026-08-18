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

    # `raddr`/`waddr` are byte addresses (per the ISA: addresses are always
    # byte addresses, dword accesses must be aligned -- see README.md
    # "Load/store instructions"). Physical storage is genuinely word
    # addressed, one `width`-bit cell per `mask_bits` bytes -- `depth` (as
    # given by callers) is already a WORD count, so the cell array is
    # exactly `depth` entries; only the byte-address inputs (`waddr`/
    # `raddr`) need the //mask_bits split to get a cell index. wmask/idata
    # are expected to already be placed in the correct byte lane
    # (addr % mask_bits) by the caller -- this module only does the
    # byte-address -> (word index, lane) split, it doesn't compute the
    # lane shift for writes itself.
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
