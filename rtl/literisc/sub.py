from myhdl import *
#
# Subtracts to words and provides all flags.
# The inputs x and y, the output res all should be of size 'width'.
def sub_w( x, y, res, do_add, borrow_in, borrow_out, v, n, z, width ) :
    ext_res = modbv(0)[width+1:]
    inv_y = modbv(0)[width:]
    add = modbv(0)[1:]
    add = do_add
    if add == 1: # strange myhdl error when using do_add directly
        inv_y[:] = y
    else:
        inv_y[:] = ~y[width:]
    ext_res[:] = x + inv_y + borrow_in 
    res[:] = ext_res[width:0]
    borrow_out[:] = ext_res[width]

    # Overflow occurs when the sign of the operands are different and
    # the sign of the result is different from the sign of operand x
    v[:] = ((x[width-1] ^ y[width-1]) & (x[width-1] ^ ext_res[width-1]));
    n[:] = ext_res[width-1]
    z[:] = ext_res[width:] == 0
    #print("sub_w",bin(x),bin(y),bin(borrow_in),bin(res))

#
# Subtracts two words by performing narrow substractions
# that are combined to wider subtractions. Provides
# flags for two sizes (typically 16-bit and 8-bit).
#
def sub2_w( x, y, res, do_add, borrow_in,
           b_out_hi, v_hi, n_hi, z_hi,
           b_out_lo, v_lo, n_lo, z_lo,
           rwidth, width ):

    res_lo = modbv(0)[width:]
    x_lo = modbv(0)[width:]
    x_lo[:] = x[width:0]
    y_lo = modbv(0)[width:]
    y_lo[:] = y[width:0]
    sub_w( x_lo, y_lo, res_lo, do_add, borrow_in, b_out_lo, v_lo, n_lo, z_lo, width )
    #print("sub2w",x_lo,y_lo,res_lo)

    res_hi = modbv(0)[width:]
    x_hi = modbv(0)[width:]
    x_hi[:] = x[rwidth:width]
    y_hi = modbv(0)[width:]
    y_hi[:] = y[rwidth:width]

    sub_w( x_hi, y_hi, res_hi, do_add, b_out_lo, b_out_hi, v_hi, n_hi, z_hi, width )
    #print("sub2w",x_hi,y_hi,res_hi)

    res[width:0] = res_lo
    res[rwidth:width] = res_hi

    z_hi[:] = z_hi & z_lo

#
# Subtracts two words by performing narrow substractions
# that are combined to wider subtractions. Provides
# flags for three sizes (typically 32-bit,16-bit and 8-bit).
#
def sub3_w( x, y, res, do_add, borrow_in,
           b_out_hi,  v_hi,  n_hi,  z_hi,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           rwidth, width, lwidth ):

    res_lo = modbv(0)[width:]
    x_lo = modbv(0)[width:]
    x_lo[:] = x[width:0]
    y_lo = modbv(0)[width:]
    y_lo[:] = y[width:0]
    sub2_w( x_lo, y_lo, res_lo, do_add, borrow_in,
           b_out_mid, v_mid, n_mid, z_mid,
           b_out_lo,  v_lo,  n_lo,  z_lo,
           width, lwidth  )

    res_hi = modbv(0)[width:]
    x_hi = modbv(0)[width:]
    x_hi[:] = x[rwidth:width]
    y_hi = modbv(0)[width:]
    y_hi[:] = y[rwidth:width]
    b_out_hi2 = modbv(0)[1:]
    b_out_lo2 = modbv(0)[1:]
    v_hi2 = modbv(0)[1:]
    n_hi2 = modbv(0)[1:]
    z_hi2 = modbv(0)[1:]
    v_lo2 = modbv(0)[1:]
    n_lo2 = modbv(0)[1:]
    z_lo2 = modbv(0)[1:]
    sub2_w( x_hi, y_hi, res_hi, do_add, b_out_mid,
           b_out_hi2, v_hi2, n_hi2, z_hi2,
           b_out_lo2, v_lo2, n_lo2, z_lo2,
           width, lwidth  )

    res[width:0] = res_lo
    res[rwidth:width] = res_hi

    v_hi[:] = v_hi2
    n_hi[:] = n_hi2
    z_hi[:] = z_mid & z_lo & z_lo2 & z_hi2
    b_out_hi[:] = b_out_hi2

    #print("sub3 z",z_lo, z_mid, z_lo2, z_hi2, z_hi )

