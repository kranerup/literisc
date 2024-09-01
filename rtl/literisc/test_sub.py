from myhdl import *
from sub import sub3_w, sub_w


def twos_comp_to_int( v, width ):
    if v & ( 1 << (width-1)) == 0:
        return v + 0
    return v - ( 1 << width )

def check_sub( op_a, op_b, width ):
    rwidth = width*4
    max_int = (2**(rwidth-1)) - 1
    min_int = -( 2**(rwidth-1) )

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
    sub3_w( op_a_s, op_b_s, res, b_in,
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

def check_range( outer_range, inner_range, width ):
    #print("outer",outer_range, 'inner',inner_range, flush=True)

    for op_a in range(outer_range[0],outer_range[1]):
        for op_b in inner_range:
            check_sub( op_a, op_b, width )

def check_3bit():

    print("----  all legal operand combinations for a single 3-bit subtraction ----")
    max_int = (2**(3-1)) - 1
    min_int = -( 2**(3-1) )
    print(f"loop over min-int:{min_int} to max-int:{max_int}")

    for op_a in range(-4,4):
        for op_b in range(-4,4):
            exp = op_a - op_b
            if exp > 3 or exp < -4:
                exp_ovf = True
            else:
                exp_ovf = False
            exp_bin = exp & 0b111

            # --- sbc ----
            x = modbv( op_a & 0b111 )[3:]
            y = modbv( op_b & 0b111 )[3:]
            res = modbv(0)[3:]
            borrow_in = modbv(1)[1:]
            b_out = modbv(0)[1:]

            carry_out = modbv(1)[1:]
            n = modbv(0)[1:]
            z = modbv(0)[1:]
            v = modbv(0)[1:]

            sub_w( x, y, res, borrow_in, b_out, v, n, z, 3 )

            rrr = int( res & 0b111 )
            ro = int(v)
            rb = int(b_out)

            eovf = 1 if exp_ovf else 0

            #if exp_ovf:
            #    exp_b = 1 if (exp & 0b1000) != 0 else 0
            #else:
            #    exp_b = 1 if exp >= 0 else 0
            exp_b = 1 if (exp & 0b1000) != 0 else 0

            print(f"a:{op_a:2d} b:{op_b:2d} ovf:{eovf} exp:{exp:2d} exp-bin:{exp_bin:03b} exp-b:{exp_b} rb:{rb}")

            assert ro == eovf
            assert (res & 0b111 ) == exp_bin
            #assert rb == exp_b
#
#
#  
#

def verify():
    #check_3bit()
    #return

    # chain two N-bit subtractions into a 2N-bit subtraction
    width = 3
    rwidth = width*4
    max_int = (2**(rwidth-1)) - 1
    min_int = -( 2**(rwidth-1) )
    print(f"=============== chaining {width}-bit to {rwidth}-bit ================")

    if False:
        print(f"loop over min-int:{min_int} to max-int:{max_int}")
        range_a = range( min_int, max_int+1 )
        range_b = range( min_int, max_int+1 )

        for op_a in range_a:
            for op_b in range_b:
                check_sub( op_a, op_b, width )
        print("loop done")

    if False:
        workers = 12
        loop2_range = range( -260, 260+1)
        print("loop2_range",loop2_range)
    else:
        workers = 12 
        loop2_range = range( min_int, max_int+1 )

    import concurrent.futures
    import itertools

    def divide_range(A, B, N):
        """
        Divide the range from A to B into N subranges.

        :param A: The start of the range (inclusive).
        :param B: The end of the range (exclusive).
        :param N: The number of subranges to divide into.
        :return: A list of tuples, each representing a subrange.
        """
        subrange_size = (B - A + N - 1) // N  # Adjust for rounding up
        return [(A + i * subrange_size, min(A + (i + 1) * subrange_size, B)) for i in range(N)]

    print("parallel over range",loop2_range[0],loop2_range[-1])
    with concurrent.futures.ProcessPoolExecutor(max_workers=workers) as executor:
        # Submit tasks
        futures = [ executor.submit(
            check_range,
            div_range,
            list(loop2_range),
            width )
                   for div_range in divide_range( min_int, max_int+1, workers ) ]

        completed = 0
        # Optionally, you can track the completion of futures
        for future in concurrent.futures.as_completed(futures):
            completed += 1
            res = future.result()
            #if completed % 10 == 0:
            print(f"completed {completed}",flush=True)


def top( clk, rstn, ina, inb, res, borrow ):

    @always(clk.posedge)
    def calc():
        b_in = modbv(1)[1:]
        v_lo = modbv(0)[1:]
        n_lo = modbv(0)[1:]
        z_lo = modbv(0)[1:]
        v_mid = modbv(0)[1:]
        n_mid = modbv(0)[1:]
        z_mid = modbv(0)[1:]
        v_hi = modbv(0)[1:]
        n_hi = modbv(0)[1:]
        z_hi = modbv(0)[1:]
        sub3_w( ina, inb, res, b_in, borrow,
           v_hi, n_hi, z_hi,
           v_mid, n_mid, z_mid,
           v_lo, n_lo, z_lo,
           32, 16, 8 )

    return instances()

def gen_verilog():
    clk = Signal(bool())
    rstn = Signal(intbv(0)[1:])
    ina = Signal(modbv(0)[32:])
    inb = Signal(modbv(0)[32:])
    res = Signal(modbv(0)[32:])
    borrow = Signal(modbv(0)[1:])

    toVerilog.standard = 'systemverilog'
    itop = toVerilog( top, clk, rstn, ina, inb, res, borrow )


if __name__ == '__main__':
    import sys
    if 'gen' in sys.argv:
        gen_verilog()
    else:
        verify()



