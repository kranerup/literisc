from ._Signal import _PosedgeWaiterList, _NegedgeWaiterList
from ._module import module
from ._always import always


def checker(func_or_clock, reset=None):
    """Checker decorator.
    Can be used either the same way as the always decorator with clocks:

         @checker(clk.posedge, rstn.negede)
         def my_checker():
             ...

    or the same ways as the module decorator:

         @checker
         def my_module(clk, rstn, foo, bar, ...):
             ...

    Will in both cases make sure that the generated Verilog code is guarded by
    `ifndef SYNTHESIS ... `endif

    """

    if isinstance(func_or_clock, (_PosedgeWaiterList, _NegedgeWaiterList)):
        return _clocked_checker(func_or_clock, reset)
    elif callable(func_or_clock):
        return _module_checker(func_or_clock)
    else:
        assert False, "Argument must be clock edge or module function"


def _clocked_checker(clock, reset):
    a = always(clock, reset)

    def f(func):
        x = a(func)
        x._verilog_attr = dict(prefix="`ifndef SYNTHESIS", suffix="`endif")
        return x

    return f


def _module_checker(func):
    m = module(func)
    m._verilog_attr = dict(prefix="`ifndef SYNTHESIS", suffix="`endif")

    return m
