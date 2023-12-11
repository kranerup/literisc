from myhdl import *

# res should have width, x,y should have width. res msb will be carry/borrow
def sub_w( x, y, res, borrow_in, borrow_out, v, n, z, width ) :
    ext_x = modbv(0)[width+1:]
    ext_x[:] = x
    ext_y = modbv(0)[width+1:]
    ext_y[:] = y
    ext_res = modbv(0)[width+1:]
    ext_res[:] = x + (~(y[width:])) + borrow_in 
    res[:] = ext_res[width:0]
    borrow_out[:] = ext_res[width]

    # Overflow occurs when the sign of the operands are different and
    # the sign of the result is different from the sign of operand x
    v[:] = ((x[width-1] ^ y[width-1]) & (x[width-1] ^ ext_res[width-1]));
    n[:] = ext_res[width-1]
    z[:] = ext_res[width:] == 0
    #print("sub_w",bin(x),bin(y),bin(res),z)


def sub2_w( x, y, res, borrow_in, borrow_out,
           v_hi, n_hi, z_hi,
           v_lo, n_lo, z_lo,
           rwidth, width ):

    res_lo = modbv(0)[width:]
    b_out_lo = modbv(0)[1:]
    x_lo = modbv(0)[width:]
    x_lo[:] = x[width:0]
    y_lo = modbv(0)[width:]
    y_lo[:] = y[width:0]
    sub_w( x_lo, y_lo, res_lo, borrow_in, b_out_lo, v_lo, n_lo, z_lo, width )

    res_hi = modbv(0)[width:]
    b_out_hi = modbv(0)[1:]
    x_hi = modbv(0)[width:]
    x_hi[:] = x[rwidth:width]
    y_hi = modbv(0)[width:]
    y_hi[:] = y[rwidth:width]

    sub_w( x_hi, y_hi, res_hi, b_out_lo, b_out_hi, v_hi, n_hi, z_hi, width )

    res[width:0] = res_lo
    res[rwidth:width] = res_hi
    borrow_out[:] = b_out_hi

    z_hi[:] = z_hi & z_lo
    #print("sub2_w z",z_hi,z_lo)

def sub3_w( x, y, res, borrow_in, borrow_out,
           v_hi, n_hi, z_hi,
           v_mid, n_mid, z_mid,
           v_lo, n_lo, z_lo,
           rwidth, width, lwidth ):

    res_lo = modbv(0)[width:]
    b_out_lo = modbv(0)[1:]
    x_lo = modbv(0)[width:]
    x_lo[:] = x[width:0]
    y_lo = modbv(0)[width:]
    y_lo[:] = y[width:0]
    sub2_w( x_lo, y_lo, res_lo, borrow_in, b_out_lo,
           v_mid, n_mid, z_mid,
           v_lo, n_lo, z_lo,
           width, lwidth  )

    res_hi = modbv(0)[width:]
    b_out_hi = modbv(0)[1:]
    x_hi = modbv(0)[width:]
    x_hi[:] = x[rwidth:width]
    y_hi = modbv(0)[width:]
    y_hi[:] = y[rwidth:width]
    v_hi2 = modbv(0)[1:]
    n_hi2 = modbv(0)[1:]
    z_hi2 = modbv(0)[1:]
    v_lo2 = modbv(0)[1:]
    n_lo2 = modbv(0)[1:]
    z_lo2 = modbv(0)[1:]
    sub2_w( x_hi, y_hi, res_hi, b_out_lo, b_out_hi, 
           v_hi2, n_hi2, z_hi2,
           v_lo2, n_lo2, z_lo2,
           width, lwidth  )

    res[width:0] = res_lo
    res[rwidth:width] = res_hi
    borrow_out[:] = b_out_hi

    v_hi[:] = v_hi2
    n_hi[:] = n_hi2
    z_hi[:] = z_mid & z_lo & z_lo2 & z_hi2

    #print("sub3 z",z_lo, z_mid, z_lo2, z_hi2, z_hi )

