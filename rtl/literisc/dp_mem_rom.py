from myhdl import *
from modules.common.signal import signal

def load_rom(data, i, o, boot_code_path):
    o.driven = "wire"
    __verilog__ = \
    '''
initial begin
    $readmemh("%(boot_code_path)s", imem_pmem_data); // here
end
    '''

    @always_comb
    def hello():
        o.next = i
    return instances()

def dp_mem_rom(
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
    name,
    load_from_file = False,
    boot_code_path = "boot.hex"):

    width = len(idata)
    mask_bits = len(wmask)
    col_width = width // mask_bits
    nr_col = mask_bits

    # See dp_mem.py: `raddr`/`waddr` are byte addresses, but physical
    # storage is genuinely word addressed (one `width`-bit cell per
    # `mask_bits` bytes) -- `depth` (as given by callers) is already a
    # WORD count, so the cell array is exactly `depth` entries; only the
    # byte-address inputs (`waddr`/`raddr`) need the //mask_bits split to
    # get a cell index. wmask/idata must already be placed in the correct
    # byte lane (addr % mask_bits) by the caller.
    word_depth = depth
    data = [ signal(width) for _ in range( word_depth ) ]

    if load_from_file:
        temp1 = Signal(modbv(0)[1:])
        temp2 = signal()
        load_rom_verilog = load_rom(data, temp1, temp2, boot_code_path)

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
                        print("waddr ", waddr)
                        data[ widx ].next[idx+col_width:idx] = idata[idx+col_width:idx]
            if renable == 1:
                ridx = int(raddr) // mask_bits
                lane = int(raddr) % mask_bits
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
