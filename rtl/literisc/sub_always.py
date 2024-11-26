from myhdl import *

def sub_w_sig( x, y, res, do_add, borrow_in, borrow_out, v, n, z, width ) :

    @always_comb
    def c():
        ext_res = modbv(0)[width+1:]
        inv_y = modbv(0)[width:]
        add = modbv(0)[1:]
        add[:] = do_add
        if add == 1: # strange myhdl error when using do_add directly
            inv_y[:] = y
        else:
            inv_y[:] = ~y[width:]
        ext_res[:] = x + inv_y + borrow_in 

        res.next = ext_res[width:0]
        borrow_out.next = ext_res[width]

        # Overflow occurs when the sign of the operands are different and
        # the sign of the result is different from the sign of operand x
        v.next = ((x[width-1] ^ y[width-1]) & (x[width-1] ^ ext_res[width-1]));
        n.next = ext_res[width-1]
        z.next = ext_res[width:] == 0

    return instances()


def sub2_w_sig( x, y, res, do_add, borrow_in,
           b_out_hi, v_hi, n_hi, z_hi,
           b_out_lo, v_lo, n_lo, z_lo,
           rwidth, width ):

    res_lo = Signal(modbv(0)[width:])
    x_lo   = Signal(modbv(0)[width:])
    y_lo   = Signal(modbv(0)[width:])
    res_hi = Signal(modbv(0)[width:])
    x_hi   = Signal(modbv(0)[width:])
    y_hi   = Signal(modbv(0)[width:])
    z_hi_tmp = Signal(modbv(0)[width:])

    isub1 = sub_w_sig( x_lo, y_lo, res_lo, do_add, borrow_in, b_out_lo, v_lo, n_lo, z_lo, width )
    isub2 = sub_w_sig( x_hi, y_hi, res_hi, do_add, b_out_lo, b_out_hi, v_hi, n_hi, z_hi_tmp, width )

    @always_comb
    def c():
        x_lo.next = x[width:0]
        y_lo.next = y[width:0]
        #sub_w( x_lo, y_lo, res_lo, do_add, borrow_in, b_out_lo, v_lo, n_lo, z_lo, width )

        x_hi.next = x[rwidth:width]
        y_hi.next = y[rwidth:width]

        #sub_w( x_hi, y_hi, res_hi, do_add, b_out_lo, b_out_hi, v_hi, n_hi, z_hi, width )

        tmp = modbv(0)[rwidth:]

        tmp[width:0] = res_lo
        tmp[rwidth:width] = res_hi
        res.next = tmp

        z_hi.next = z_hi_tmp & z_lo

    return instances()

def sub3_w_sig( x, y, res, do_add, borrow_in,
           b_out_hi,  v_hi,  n_hi,  z_hi,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           rwidth, width, lwidth ):

    res_lo    = Signal(modbv(0)[width:])
    x_lo      = Signal(modbv(0)[width:])
    y_lo      = Signal(modbv(0)[width:])
    res_hi    = Signal(modbv(0)[width:])
    x_hi      = Signal(modbv(0)[width:])
    y_hi      = Signal(modbv(0)[width:])
    b_out_hi2 = Signal(modbv(0)[1:])
    b_out_lo2 = Signal(modbv(0)[1:])
    v_hi2     = Signal(modbv(0)[1:])
    n_hi2     = Signal(modbv(0)[1:])
    z_hi2     = Signal(modbv(0)[1:])
    v_lo2     = Signal(modbv(0)[1:])
    n_lo2     = Signal(modbv(0)[1:])
    z_lo2     = Signal(modbv(0)[1:])

    isub1 = sub2_w_sig( x_lo, y_lo, res_lo, do_add, borrow_in,
               b_out_mid, v_mid, n_mid, z_mid,
               b_out_lo,  v_lo,  n_lo,  z_lo,
               width, lwidth  )
    isub2 = sub2_w_sig( x_hi, y_hi, res_hi, do_add, b_out_mid,
           b_out_hi2, v_hi2, n_hi2, z_hi2,
           b_out_lo2, v_lo2, n_lo2, z_lo2,
           width, lwidth  )

    @always_comb
    def c():
        x_lo.next = x[width:0]
        y_lo.next = y[width:0]
        #sub2_w( x_lo, y_lo, res_lo, do_add, borrow_in,
        #       b_out_mid, v_mid, n_mid, z_mid,
        #       b_out_lo,  v_lo,  n_lo,  z_lo,
        #       width, lwidth  )

        x_hi.next = x[rwidth:width]
        y_hi.next = y[rwidth:width]
        #sub2_w( x_hi, y_hi, res_hi, do_add, b_out_mid,
        #       b_out_hi2, v_hi2, n_hi2, z_hi2,
        #       b_out_lo2, v_lo2, n_lo2, z_lo2,
        #       width, lwidth  )

        tmp = modbv(0)[rwidth:]

        tmp[width:0] = res_lo
        tmp[rwidth:width] = res_hi
        res.next = tmp

        v_hi.next = v_hi2
        n_hi.next = n_hi2
        z_hi.next = z_mid & z_lo & z_lo2 & z_hi2
        b_out_hi.next = b_out_hi2

    return instances()

def gen_verilog():
    clk = Signal(bool())
    rstn = Signal(intbv(0)[1:])
    ina = Signal(modbv(0)[32:])
    inb = Signal(modbv(0)[32:])
    res = Signal(modbv(0)[32:])
    borrow_in = Signal(modbv(0)[1:])
    borrow_mid = Signal(modbv(0)[1:])
    borrow_lo = Signal(modbv(0)[1:])
    do_add = Signal(intbv(0)[1:])
    v = Signal(intbv(0)[1:])
    n = Signal(intbv(0)[1:])
    z = Signal(intbv(0)[1:])
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

    toVerilog.standard = 'systemverilog'
    #itop = toVerilog( sub_sig, ina, inb, res, do_add, borrow, borrow_mid, v, n, z, 8)
    #itop = toVerilog( sub2_w_sig,
    #    ina, inb, res, do_add, borrow_in,
    #    b_out_hi, v_hi, n_hi, z_hi,
    #    b_out_lo, v_lo, n_lo, z_lo,
    #    8, 4 )


    itop = toVerilog( sub3_w_sig,
           ina, inb, res, do_add, borrow_in,
           b_out_hi,  v_hi,  n_hi,  z_hi,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           32, 16, 8 )


if __name__ == '__main__':
    gen_verilog()

