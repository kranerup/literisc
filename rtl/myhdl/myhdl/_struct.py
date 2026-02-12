"""
Base class for signal structs.
Makes it much easier to identify these during elaboration.
"""

from ._Signal import _Signal
from ._ShadowSignal import ConcatSignal
from ._always_comb import always_comb


class Struct:
    def __len__(self):
        """Return total size in bits for all signal members (recursively)"""
        return sum((len(x[1]) for x in iter_struct_signals(self)))


def iter_struct_signals(struct, prefix="", members=None):
    """
    Iterate over all signals in struct (recursively)
    Yield tuple with (name, signal) for each signal
    """
    for x, m in struct.__dict__.items():
        if members and x not in members:
            continue
        if prefix:
            name = f"{prefix}.{x}"
        else:
            name = x
        if isinstance(m, _Signal):
            yield name, m
        elif isinstance(m, Struct):
            yield from iter_struct_signals(m, name)
        elif isinstance(m, list) and m and isinstance(m[0], (_Signal, Struct)):
            for i, y in enumerate(m):
                if isinstance(y, _Signal):
                    yield f"{name}[{i}]", y
                elif isinstance(y, Struct):
                    yield from iter_struct_signals(i, f"{name}[{i}]")


def pack_struct(struct, members=None):
    """
    Create concatenated signal with all members of the struct (recursively)
    If members is given, as a list or set of names, only these members are packed.
    """
    assert isinstance(struct, Struct)
    return ConcatSignal(
        *reversed([x[1] for x in iter_struct_signals(struct, members=members)])
    )


def unpack_struct(signal, struct, members=None):
    """
    Unpack signal into struct. Return list of instances.
    If members is given, as a list or set of names, only these members are unpacked.
    """
    assert isinstance(struct, Struct)
    assert isinstance(signal, _Signal)

    def assign(target, value, index, size):
        @always_comb
        def assign_signal():
            target.next = value[index + size : index]

        return assign_signal

    inst = []
    idx = 0
    for name, member in iter_struct_signals(struct, members=members):
        size = len(member)
        inst.append(assign(member, signal, idx, size))
        idx += size
    assert idx == len(signal), f"idx={idx}, len={len(signal)}"
    return inst
