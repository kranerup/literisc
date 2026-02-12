"""
Implements efficient signal to signal connections
"""

from ._ShadowSignal import ConversionSignal
from ._instance import _SignalInstance
from ._intbv import intbv


def _flatten(s, lst):
    if isinstance(s, (list, tuple)):
        for x in s:
            _flatten(x, lst)
    else:
        lst.append(s)
    return lst


def _add_to_sigdict(sigdict, name, p, sl):
    for i, s in enumerate(sl):
        sigdict[f"{name}_{i}"] = s


def connect(dst, src, allow_mismatch=True):
    """
    Connect all signals in src to the signals in dst by flattening both recursively,
    and then assign each element.

    Both src and dst can be a signal, a list contining signals and lists (of signals
    and lists recursively).

    Returns list of signals
    """

    assert dst is not None and src is not None

    sl = _flatten(src, [])
    dl = _flatten(dst, [])

    for i, s in enumerate(sl):
        if isinstance(s, int):
            w = len(dl[i])
            sl[i] = intbv(s)[w:]

    cvs = ConversionSignal(dl, sl)
    cvs._markUsed()
    cvs._markRead()

    sigdict = dict(cvs=cvs)
    _add_to_sigdict(sigdict, "src", src, sl)
    _add_to_sigdict(sigdict, "dst", dst, dl)

    # Avoid inferring these locally:
    sl = None
    dl = None
    s = None
    src = None
    dst = None

    si = _SignalInstance("cvs", **sigdict)

    return [si]
