from myhdl import inline, ConcatSignal, always_comb, Struct
from modules.common.signal import signal
import re


class Axi4(Struct):
    """AXI4 Interface"""

    LENBITS = 8
    SIZEBITS = 3
    BURSTBITS = 2
    RESPBITS = 2
    CACHEBITS = 4

    RESP_OK = 0
    RESP_EXOK = 1
    RESP_SLVERR = 2
    RESP_DECERR = 3

    SIZE_1 = 0
    SIZE_2 = 1
    SIZE_4 = 2
    SIZE_8 = 3
    SIZE_16 = 4
    SIZE_32 = 5
    SIZE_64 = 6
    SIZE_128 = 7

    BURST_FIXED = 0
    BURST_INCR = 1
    BURST_WRAP = 2

    CACHE_B = 0b0001
    CACHE_M = 0b0010
    CACHE_RA = 0b0100
    CACHE_WA = 0b1000

    CACHE_DEV_NB = 0b0000
    CACHE_DEV_B = CACHE_B
    CACHE_NORMAL_NC_NB = CACHE_M
    CACHE_NORMAL_NC_B = CACHE_M | CACHE_B
    CACHE_NORMAL_C_B = CACHE_WA | CACHE_RA | CACHE_M | CACHE_B

    def __init__(self, asize=40, dsize=128, idsize=4):
        self.asize = asize
        self.dsize = dsize
        self.idsize = idsize
        self.bsize = dsize.bit_length() - 4

        self.awvalid = signal()
        self.awready = signal()
        self.awid = signal(idsize)
        self.awaddr = signal(asize)
        self.awlen = signal(Axi4.LENBITS)
        self.awsize = signal(Axi4.SIZEBITS)
        self.awburst = signal(Axi4.BURSTBITS)
        self.awcache = signal(Axi4.CACHEBITS)

        self.wvalid = signal()
        self.wready = signal()
        self.wdata = signal(dsize)
        self.wstrb = signal(dsize // 8)
        self.wlast = signal()

        self.bvalid = signal()
        self.bready = signal()
        self.bresp = signal(Axi4.RESPBITS)
        self.bid = signal(idsize)

        self.arvalid = signal()
        self.arready = signal()
        self.arid = signal(idsize)
        self.araddr = signal(asize)
        self.arlen = signal(Axi4.LENBITS)
        self.arsize = signal(Axi4.SIZEBITS)
        self.arburst = signal(Axi4.BURSTBITS)
        self.arcache = signal(Axi4.CACHEBITS)

        self.rvalid = signal()
        self.rready = signal()
        self.rresp = signal(Axi4.RESPBITS)
        self.rid = signal(idsize)
        self.rdata = signal(dsize)
        self.rlast = signal()


_sig_driver = dict(
    awvalid="m",
    awready="s",
    awid="m",
    awaddr="m",
    awlen="m",
    awsize="m",
    awburst="m",
    awcache="m",
    wvalid="m",
    wready="s",
    wdata="m",
    wstrb="m",
    wlast="m",
    bvalid="s",
    bready="m",
    bresp="s",
    bid="s",
    arvalid="m",
    arready="s",
    arid="m",
    araddr="m",
    arlen="m",
    arsize="m",
    arburst="m",
    arcache="m",
    rvalid="s",
    rready="m",
    rresp="s",
    rid="s",
    rdata="s",
    rlast="s",
)


@inline
def write(
    axi,
    addr,
    length,
    id,
    size,
    burst=Axi4.BURST_INCR,
    cache=Axi4.CACHE_NORMAL_C_B,
):
    axi.awvalid.next = 1
    axi.awid.next = id
    axi.awaddr.next = addr
    axi.awlen.next = length - 1
    axi.awsize.next = size
    axi.awburst.next = burst
    axi.awcache.next = cache


@inline
def read(
    axi,
    addr,
    length,
    id,
    size,
    burst=Axi4.BURST_INCR,
    cache=Axi4.CACHE_NORMAL_C_B,
):
    axi.arvalid.next = 1
    axi.arid.next = id
    axi.araddr.next = addr
    axi.arlen.next = length - 1
    axi.arsize.next = size
    axi.arburst.next = burst
    axi.arcache.next = cache


@inline
def pass_through(slv, mst):
    mst.awvalid.next = slv.awvalid
    slv.awready.next = mst.awready
    mst.awid.next = slv.awid
    mst.awaddr.next = slv.awaddr
    mst.awlen.next = slv.awlen
    mst.awsize.next = slv.awsize
    mst.awburst.next = slv.awburst
    mst.awcache.next = slv.awcache

    mst.wvalid.next = slv.wvalid
    slv.wready.next = mst.wready
    mst.wdata.next = slv.wdata
    mst.wstrb.next = slv.wstrb
    mst.wlast.next = slv.wlast

    slv.bvalid.next = mst.bvalid
    mst.bready.next = slv.bready
    slv.bresp.next = mst.bresp
    slv.bid.next = mst.bid

    mst.arvalid.next = slv.arvalid
    slv.arready.next = mst.arready
    mst.arid.next = slv.arid
    mst.araddr.next = slv.araddr
    mst.arlen.next = slv.arlen
    mst.arsize.next = slv.arsize
    mst.arburst.next = slv.arburst
    mst.arcache.next = slv.arcache

    slv.rvalid.next = mst.rvalid
    mst.rready.next = slv.rready
    slv.rresp.next = mst.rresp
    slv.rdata.next = mst.rdata
    slv.rlast.next = mst.rlast
    slv.rid.next = mst.rid


_handshake = re.compile(r"^(aw|ar|r|w|b)(valid|ready)$")


def _names(port, prefix):
    return [
        x
        for x in dir(port)
        if x.startswith(prefix) and not _handshake.match(x) and x != "bsize"
    ]


def chsize(port, prefix):
    """Return bit-size of all but handshake signals of a channel"""
    return sum(len(getattr(port, x)) for x in _names(port, prefix))


def collect(port, prefix):
    """Concat all but the handshake signals of a channel into one signal"""
    names = _names(port, prefix)
    return ConcatSignal(*[getattr(port, x) for x in reversed(names)])


def expand(port, prefix, signal):
    """
    Assign indididual channel signals from combined signal (created with collect).
    Return list of instances
    """
    names = _names(port, prefix)
    inst = []
    port_signals = []
    i = 0
    for name in names:
        port_signals.append(getattr(port, name))

        def f(psig):
            bits = len(psig)
            lsb = i
            msb = i + bits

            @always_comb
            def assign_signal():
                psig.next = signal[msb:lsb]

            return assign_signal

        inst.append(f(port_signals[-1]))
        i += len(port_signals[-1])

    return inst


def copy(slv, mst, add_id=None):
    """Copy signals between interfaces. Return list of instances."""

    assigns = []
    for sig in _sig_driver:
        d = _sig_driver[sig]

        def f(sig, d):
            if d == "m":
                tgt = getattr(mst, sig)
                src = getattr(slv, sig)
            else:
                tgt = getattr(slv, sig)
                src = getattr(mst, sig)

            if add_id is not None and sig in ("awid", "arid"):

                @always_comb
                def assign():
                    tgt.next = src + add_id

            else:

                @always_comb
                def assign():
                    tgt.next = src

            return assign

        assigns.append(f(sig, d))

    return assigns
