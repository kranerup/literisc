from myhdl import Signal, intbv, modbv

"""
Convenience functions to make declaration of signals less verbose
"""


def signal(w=1, max=None, min=None, debug_level=None, debug_descr=None):
    if max is not None:
        if min is not None:
            return Signal(
                intbv(0, max=max, min=min),
                debug_level=debug_level,
                debug_descr=debug_descr,
            )
        else:
            w = (max - 1).bit_length()
    return Signal(modbv(0)[w:0], debug_level=debug_level, debug_descr=debug_descr)


def sigarray(w, size, debug_level=None, debug_descr=None):
    return [
        signal(w, debug_level=debug_level, debug_descr=debug_descr) for _ in range(size)
    ]
