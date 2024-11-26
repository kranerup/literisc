from myhdl import *
from modules.common.signal import signal
from sub import sub3_w
from sub_always import sub3_w_sig
import random


def twos_comp_to_int( v, width ):
    if v & ( 1 << (width-1)) == 0:
        return v + 0
    return v - ( 1 << width )

def check_sub( op_a, op_b, width ):
    rwidth = width*4
    max_int = (2**(rwidth-1)) - 1
    min_int = -( 2**(rwidth-1) )

    do_add = modbv(0)[1:]

    op_a_s = modbv( op_a )[rwidth:]
    op_b_s = modbv( op_b )[rwidth:]
    res = modbv(0)[rwidth:]
    b_in = modbv(1)[1:]
    b_out_hi = modbv(0)[1:]
    b_out_mid = modbv(0)[1:]
    b_out_lo = modbv(0)[1:]
    v_lo = modbv(0)[1:]
    n_lo = modbv(0)[1:]
    z_lo = modbv(0)[1:]
    v_mid = modbv(0)[1:]
    n_mid = modbv(0)[1:]
    z_mid = modbv(0)[1:]
    v_hi = modbv(0)[1:]
    n_hi = modbv(0)[1:]
    z_hi = modbv(0)[1:]
    sub3_w( op_a_s, op_b_s, res, do_add, b_in,
           b_out_hi,  v_hi,  n_hi,  z_hi,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           rwidth, rwidth//2, rwidth//4 )

    a = int(op_a_s)
    b = int(op_b_s)
    r = int(res)
    #print(f"L a:{a:2d} b:{b:2d}     res:{r:2d} res-bin:{r:03b}")

    exp = op_a - op_b
    exp_bin = exp & ( 2**(rwidth)-1 )

    op_a_lo =  twos_comp_to_int( op_a & (2**width-1), width )
    op_b_lo =  twos_comp_to_int( op_b & (2**width-1), width )
    exp_lo = op_a_lo - op_b_lo

    op_a_mid =  twos_comp_to_int( op_a & (2**(2*width)-1), 2*width )
    op_b_mid =  twos_comp_to_int( op_b & (2**(2*width)-1), 2*width )
    exp_mid = op_a_mid - op_b_mid

    exp_z = 1 if exp == 0 else 0
    exp_z_lo = 1 if exp & ( 2**width - 1 ) == 0 else 0
    exp_z_mid = 1 if exp & ( 2**(2*width) - 1 ) == 0 else 0

    exp_n = 1 if exp & 1<<(rwidth-1) != 0 else 0
    exp_n_mid = 1 if exp & 1<<(2*width-1) != 0 else 0
    exp_n_lo = 1 if exp & 1<<(width-1) != 0 else 0

    #print(f"a:{op_a:2d} b:{op_b:2d}  exp:{exp:2d} exp-bin:{exp_bin:0b}  res:{r:2d} res-bin:{r:06b}")

    # compare res_lo with exp_lo
    res_conv_lo = twos_comp_to_int( res[width:], width )
    max_int_lo = (2**(width-1)) - 1
    min_int_lo = -( 2**(width-1) )
    if exp_lo > max_int_lo or exp_lo < min_int_lo:
        assert exp & (2**width-1) == res[width:]
        assert v_lo == 1
    else:
        assert exp_lo == res_conv_lo
        assert v_lo == 0

    # compare res_mid with exp_mid
    res_conv_mid = twos_comp_to_int( res[2*width:], 2*width )
    res_mid_bin = res[2*width:] + 0
    max_int_mid = (2**(2*width-1)) - 1
    min_int_mid = -( 2**(2*width-1))
    #print(f"mid a:{op_a_mid} b:{op_b_mid} r:{res_mid_bin} exp:{exp_mid}")
    #print(f"max:{max_int_mid} min:{min_int_mid}")
    if exp_mid > max_int_mid or exp_mid < min_int_mid:
        assert exp & (2**(2*width)-1) == res[2*width:]
        assert v_mid == 1
    else:
        assert exp_mid == res_conv_mid
        assert v_mid == 0

    # compare res with exp
    res_conv = twos_comp_to_int( res, rwidth )

    if exp > max_int or exp < min_int:
        assert exp & ( 2**(rwidth)-1 ) == res
        assert v_hi == 1
    else:
        assert exp == res_conv
        assert v_hi == 0

    assert exp_z == z_hi, f"z fail exp:{exp_z} act:{z_hi}"
    assert exp_z_lo == z_lo, f"z fail exp:{exp_z_lo} act:{z_lo}"
    assert exp_z_mid == z_mid, f"z fail exp:{exp_z_mid} act:{z_mid}"

    assert exp_n == n_hi, f"z fail exp:{exp_n} act:{n_hi}"
    assert exp_n_mid == n_mid, f"z fail exp:{exp_n} act:{n_mid}"
    assert exp_n_lo == n_lo, f"z fail exp:{exp_n} act:{n_lo}"

def check_add( op_a, op_b, c_in, width ):
    rwidth = width*4
    max_int = (2**(rwidth-1)) - 1
    min_int = -( 2**(rwidth-1) )

    do_add = modbv(1)[1:]

    op_a_s = modbv( op_a )[rwidth:]
    op_b_s = modbv( op_b )[rwidth:]
    res = modbv(0)[rwidth:]
    b_in = modbv( c_in )[1:]
    b_out_hi = modbv(0)[1:]
    b_out_mid = modbv(0)[1:]
    b_out_lo = modbv(0)[1:]
    v_lo = modbv(0)[1:]
    n_lo = modbv(0)[1:]
    z_lo = modbv(0)[1:]
    v_mid = modbv(0)[1:]
    n_mid = modbv(0)[1:]
    z_mid = modbv(0)[1:]
    v_hi = modbv(0)[1:]
    n_hi = modbv(0)[1:]
    z_hi = modbv(0)[1:]
    sub3_w( op_a_s, op_b_s, res, do_add, b_in,
           b_out_hi,  v_hi,  n_hi,  z_hi,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           rwidth, rwidth//2, rwidth//4 )

    #a = int(op_a_s)
    #b = int(op_b_s)
    #r = int(res)
    #print(f"L a:{a:2d} b:{b:2d} c-in:{c_in:1d}    res:{r:2d} res-bin:{r:03b}")

    exp = ( (op_a & ( 2**rwidth-1 )) +
            (op_b & ( 2**rwidth-1 )) +
            c_in )
    exp_c_out = ( exp >> rwidth ) & 1
    exp = exp & ( 2**(rwidth)-1 )

    assert exp_c_out == b_out_hi
    assert res == exp



def tb():
    clk = Signal(bool())
    rstn = signal()

    ina = Signal(modbv(0)[32:])
    inb = Signal(modbv(0)[32:])
    res = Signal(modbv(0)[32:])
    borrow_in = Signal(modbv(0)[1:])
    do_add = Signal(intbv(0)[1:])
    v_hi = Signal(intbv(0)[1:])
    n_hi = Signal(intbv(0)[1:])
    z_hi = Signal(intbv(0)[1:])
    v_mid = Signal(intbv(0)[1:])
    n_mid = Signal(intbv(0)[1:])
    z_mid = Signal(intbv(0)[1:])
    v_lo = Signal(intbv(0)[1:])
    n_lo = Signal(intbv(0)[1:])
    z_lo = Signal(intbv(0)[1:])
    b_out_hi = Signal(modbv(0)[1:])
    b_out_mid = Signal(modbv(0)[1:])
    b_out_lo = Signal(modbv(0)[1:])

    isub3 = sub3_w_sig(
            ina, inb, res, do_add, borrow_in,
            b_out_hi,  v_hi,  n_hi,  z_hi,
            b_out_mid, v_mid, n_mid, z_mid,
            b_out_lo,  v_lo,  n_lo,  z_lo,
            32, 16, 8 
            )

    @always(delay(10))
    def stim():
      clk.next = not clk

    @instance
    def seq():
      rstn.next = 0
      yield clk.posedge
      rstn.next = 1 
      yield clk.posedge
      print("====== reset done =======")

    @always(clk.negedge )
    def driver():
        if random.randrange(10) == 0:
            ina.next = 0
        elif random.randrange(10) == 1:
            r = random.getrandbits( 32 )
            r = r & 0xffffff00
            ina.next = r
        elif random.randrange(10) == 2:
            r = random.getrandbits( 32 )
            r = r & 0xffff0000
            ina.next = r
        else:
            ina.next = random.getrandbits( 32 )
        if random.randrange(10) == 0:
            inb.next = 0
        elif random.randrange(10) == 1:
            r = random.getrandbits( 32 )
            r = r & 0xffffff00
            inb.next = r
        elif random.randrange(10) == 2:
            r = random.getrandbits( 32 )
            r = r & 0xffff0000
            inb.next = r
        else:
            inb.next = random.getrandbits( 32 )
        do_add.next = random.getrandbits(1)
        borrow_in.next = random.getrandbits(1)

    @always(clk.posedge )
    def checker():

        refin_ina = (modbv(int(ina))[32:])
        refin_inb = (modbv(int(inb))[32:])
        refin_res = (modbv(0)[32:])
        refin_borrow_in = (modbv(int(borrow_in))[1:])
        refin_do_add = (intbv(int(do_add))[1:])

        ref_res = modbv(0)[32:]
        ref_b_out_hi = modbv(0)[1:]
        ref_b_out_mid = modbv(0)[1:]
        ref_b_out_lo = modbv(0)[1:]
        ref_v_lo = modbv(0)[1:]
        ref_n_lo = modbv(0)[1:]
        ref_z_lo = modbv(0)[1:]
        ref_v_mid = modbv(0)[1:]
        ref_n_mid = modbv(0)[1:]
        ref_z_mid = modbv(0)[1:]
        ref_v_hi = modbv(0)[1:]
        ref_n_hi = modbv(0)[1:]
        ref_z_hi = modbv(0)[1:]
        sub3_w( refin_ina, refin_inb, ref_res, refin_do_add, refin_borrow_in,
               ref_b_out_hi,  ref_v_hi,  ref_n_hi,  ref_z_hi,
               ref_b_out_mid, ref_v_mid, ref_n_mid, ref_z_mid,
               ref_b_out_lo,  ref_v_lo,  ref_n_lo,  ref_z_lo,
               32, 16, 8 )

        if ref_res != int(res):
            print("fail res",ref_res,res)

        assert ref_b_out_hi   == int(b_out_hi)
        assert ref_b_out_mid  == int(b_out_mid)
        assert ref_b_out_lo   == int(b_out_lo)
        assert ref_v_lo       == int(v_lo)
        assert ref_n_lo       == int(n_lo)
        assert ref_z_lo       == int(z_lo)
        assert ref_v_mid      == int(v_mid)
        assert ref_n_mid      == int(n_mid)
        assert ref_z_mid      == int(z_mid)
        assert ref_v_hi       == int(v_hi)
        assert ref_n_hi       == int(n_hi)
        assert ref_z_hi       == int(z_hi      )


    return instances()


traceSignals.filename = 'trace'
itb = traceSignals( tb ) 
sim = Simulation( itb )
sim.run( 2000000 )


