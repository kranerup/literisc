"""Generate Verilog modules"""

from .conversion import _toVerilog as toVerilog
from ._extractHierarchy import _userCodeMap, _memInfoMap
from textwrap import dedent
from ._Signal import _Signal
from ._intbv import intbv
from ._instance import instance
from ._struct import Struct
import sys
from hashlib import md5


_module_names = set()
_module_cache = {}
_module_prefix = None


def _uniqify_name(name, profile):
    # prefix = toVerilog.toVerilog.top_name
    prefix = "pa"
    phash = md5(profile.encode()).hexdigest()
    uname = f"{prefix}_{name}_{phash[0:6]}"
    assert uname not in _module_names
    _module_names.add(uname)
    return uname


def _flatten(name, arg):
    if name == "name":
        return []
    if isinstance(arg, _Signal):
        return [f"{name}=s{len(arg)}"]
    if isinstance(arg, (list, tuple)):
        return sum((_flatten(f"{name}[{i}]", x) for i, x in enumerate(arg)), [])
    if not hasattr(arg, "__dict__"):
        return [f"{name}={arg}"]
    args = []
    for k, v in vars(arg).items():
        if k[0] == "_":
            continue
        if isinstance(v, _Signal):
            args.append(f"{name}.{k}=s{len(v)}")
        elif isinstance(v, int):
            args.append(f"{name}.{k}={v}")
        elif isinstance(arg, (list, tuple)):
            for i, x in enumerate(args):
                args += _flatten(f"{name}.{k}[{i}]", x)
        elif hasattr(v, "__dict__"):
            a = _flatten(f"{name}.{k}", v)
            if a:
                args += a
            else:
                args.append(f"{name}.{k}={id(v)}")
    if not args:
        args.append(f"{name}={id(arg)}")
    return args


def _argument_profile(func, args, kwargs):
    profile = []
    varnames = func.__code__.co_varnames
    profile = sum((_flatten(k, v) for k, v in zip(varnames, args)), [])
    for k in sorted(kwargs):
        v = kwargs[k]
        profile += _flatten(k, v)
    return ":".join(profile)


def _cached_module(func, args, kwargs):
    global _module_prefix
    if not _module_prefix or toVerilog.toVerilog.top_name != _module_prefix:
        _module_prefix = toVerilog.toVerilog.top_name
        _module_cache.clear()
        _module_names.clear()
    func = getattr(func, "_func", func)
    m = _module_cache.get(id(func))
    profile = _argument_profile(func, args, kwargs)
    if not m or profile not in m:
        return profile, None, None, None
    return profile, *m[profile]


def _cache_module(func, profile, name, portmap, dirmap):
    fid = id(func)
    if fid not in _module_cache:
        _module_cache[fid] = {}
    _module_cache[fid][profile] = name, portmap, dirmap


def _save_signal_attributes(args, kwargs):
    """Save _driven signal attributes, and set to None"""
    attr = []
    for a in list(args) + list(kwargs.values()):
        if isinstance(a, _Signal):
            d = a._driven
            attr.append((a, d, a._name, a._debug_level))
            a._driven = None
            a._debug_level = None
        elif isinstance(a, list):
            attr += _save_signal_attributes(a, {})
        elif isinstance(a, Struct):
            attr += _save_signal_attributes((x[1] for x in _expand_interface(a)), {})
    return attr


def _restore_signal_attributes(attr):
    """Restore save _driven signal attributes"""
    for s, d, n, dbg in attr:
        s._driven = d
        s._name = n
        s._debug_level = dbg


def _convert(_c_name, _c_func, _c_converter, _vattr, *args, **kwargs):
    c = toVerilog._ToVerilogConvertor()
    c.name = _c_name
    c.directory = _c_converter.directory
    c.timescale = _c_converter.timescale
    c.standard = _c_converter.standard
    c.prefer_blocking_assignments = _c_converter.prefer_blocking_assignments
    c.radix = _c_converter.radix
    c.header = _c_converter.header
    c.no_myhdl_header = _c_converter.no_myhdl_header
    c.no_testbench = _c_converter.no_testbench
    c.initial_values = _c_converter.initial_values
    attr = _save_signal_attributes(args, kwargs)
    tv = toVerilog.toVerilog
    h = tv.header
    f = tv.footer
    tv.header = h + _vattr.get("prefix", "")
    tv.footer = f + _vattr.get("suffix", "")
    c(_c_func, *args, **kwargs)
    tv.header = h
    tv.footer = f
    _restore_signal_attributes(attr)
    return c.portmap, c.dirmap


def _copy_ucmap(ucmap):
    m = {}
    for k in ucmap:
        m[k] = {}
        v = ucmap[k]
        for j in v:
            m[k][j] = v[j]
    return m


def _expand_interface(arg, name=None):
    args = []
    if arg is None:
        return []
    if isinstance(arg, _Signal):
        return [(name, arg)]
    elif isinstance(arg, list):
        return sum((_expand_interface(x, f"{name}_{i}") for i, x in enumerate(arg)), [])
    elif isinstance(arg, Struct):
        for k, v in vars(arg).items():
            args += _expand_interface(v, f"{name}_{k}")
    return args


def _expand_interface_kw(args, kwargs, portmap):
    """Return list of signal corresponging to portmap, in portmap order"""
    sigs = dict(
        zip(
            portmap.keys(),
            (x[1] for x in sum((_expand_interface(a) for a in args), [])),
        )
    )
    kwsigs = dict(sum((_expand_interface(a, n) for n, a in kwargs.items()), []))
    kwsigs = {n: kwsigs[n] for n in portmap if n in kwsigs}
    sigs.update(kwsigs)
    return sigs


def _wrapfunc(_portmap, _dirmap, _name, _profile, _vattr, *args, **kwargs):
    def _wrap(*_a, **_kw):
        _pmap = []
        _loc = {}
        _eargs = _expand_interface_kw(_a, _kw, _portmap)
        for _portname in _portmap:
            _s = _eargs[_portname]
            _pmap.append(f"${_portname}")
            _loc[_portname] = _s
            setattr(_s, "_used", True)
            if _dirmap[_portname]:
                setattr(_s, "driven", "wire")
            else:
                setattr(_s, "read", True)

        _sep = ", "
        _vprefix = _vattr.get("prefix", "")
        _vsuffix = _vattr.get("suffix", "")
        _code = dedent(
            f"""
                {_vprefix}
                /* profile: {_profile} */
                {_name} $instance_name({_sep.join(_pmap)});
                {_vsuffix}
            """
        )

        _dummy = _Signal(intbv(0)[1:])

        @instance
        def inst():
            yield _dummy.posedge

        _wrap.verilog_code = _code
        _wrap.verilog_namespace = _loc

        globals()["_wrap"] = _wrap
        return inst

    return _wrap  # (portmap, dirmap, name, *args, **kwargs)


_wrap = None


def module(func_or_name):
    """Decorator that causes the decorated function to become
    a Verilog module.

    It can be used in two ways: With implicit module naming:

        @module
        def some_module_func(...):
            ...

    Or with explicit module naming:

        @module("my_module_name")
        def some_module_func(...):
            ...

    In the latter case, care must be taken to avoid name-clashes.
    """
    if isinstance(func_or_name, str):
        module._name = func_or_name
        return module
    else:
        func = func_or_name
        _name = module._name

    def _module(*args, **kwargs):
        if toVerilog._converting and not module.disable:
            vattr = getattr(_module, "_verilog_attr", {})
            profile, name, portmap, dirmap = _cached_module(func, args, kwargs)
            if not name:
                p = sys.getprofile()
                sys.setprofile(None)
                name = _name or _uniqify_name(func.__name__, profile)
                converter = toVerilog._converter
                toVerilog._converting = 0
                no_tb = toVerilog.toVerilog.no_testbench
                toVerilog.toVerilog.no_testbench = True
                ucmap = _copy_ucmap(_userCodeMap)
                mimap = _memInfoMap.copy()
                portmap, dirmap = _convert(
                    name, func, converter, vattr, *args, **kwargs,
                )
                _userCodeMap.clear()
                _userCodeMap.update(ucmap)
                _memInfoMap.clear()
                _memInfoMap.update(mimap)
                toVerilog._converter = converter
                toVerilog._converting = 1
                toVerilog.toVerilog.no_testbench = no_tb
                sys.setprofile(p)
                _cache_module(func, profile, name, portmap, dirmap)
            wrap = _wrapfunc(portmap, dirmap, name, profile, vattr,  *args, **kwargs)
            return wrap(*args, **kwargs)
        else:
            return func(*args, **kwargs)

    _module._func = func
    _module.__name__ = func.__name__
    module._name = None
    return _module


module.disable = False
module._name = None
