#  This file is part of the myhdl library, a Python package for using
#  Python as a Hardware Description Language.
#
#  Copyright (C) 2003-2008 Jan Decaluwe
#
#  The myhdl library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public License as
#  published by the Free Software Foundation; either version 2.1 of the
#  License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

""" myhdl _extractHierarchy module.

"""
import sys
import inspect
import string

from myhdl import ExtractHierarchyError, ToVerilogError, ToVHDLError
from myhdl._Signal import _Signal, _isListOfSigs, Memory
from myhdl._util import _flatten
from myhdl._util import _genfunc
from myhdl._misc import _isGenSeq
from myhdl._resolverefs import _resolveRefs
from myhdl._getcellvars import _getCellVars
from myhdl._Cosimulation import Cosimulation
from myhdl._struct import Struct
from myhdl._ShadowSignal import ConversionSignal
from myhdl._instance import _SignalInstance


_profileFunc = None


def _has_something(x):
    if not isinstance(x, (list, tuple, set)):
        return True
    for z in x:
        if _has_something(z):
            return True
    return False


class _error:
    pass
_error.NoInstances = "No instances found"
_error.InconsistentHierarchy = "Inconsistent hierarchy - are all instances returned ?"
_error.InconsistentToplevel = "Inconsistent top level %s for %s - should be 1"


def _add_unique(dict, key, value):
    k = key
    idx = 0
    while k in dict:
        k = f"{key}_{idx}"
        idx += 1
    dict[k] = value


def _expand_interface(name, obj, sigdict, memdict, invsigdict, mark):
    if isinstance(obj, _Signal):
        sigdict[name] = obj
        invsigdict[id(obj)] = name
        if mark:
            obj._markUsed()
    elif isinstance(obj, Struct):
        for k, v in vars(obj).items():
            _expand_interface(f"{name}_{k}", v, sigdict, memdict, invsigdict, mark)
    elif isinstance(obj, list):
        m = _makeMemInfo(obj)
        memdict[name] = m
        if mark:
            m._used = True


class _Instance(object):
    __slots__ = ['level', 'obj', 'subs', 'sigdict', 'memdict', 'name']

    def __init__(self, level, obj, subs, sigdict, memdict):
        self.level = level
        self.obj = obj
        self.subs = subs
        self.sigdict = sigdict
        self.memdict = memdict
        self.name = None

_memInfoMap = {}


class _MemInfo(object):
    __slots__ = [
        'mem', 'name', 'elObj', 'depth', '_used', '_driven', '_read',
        '_signal_mem', 'nrbits',
    ]

    def __init__(self, mem):
        self.mem = mem
        self.name = None
        self.depth = len(mem)
        self.elObj = mem[0]
        self._used = False
        self._driven = None
        self._read = None
        self._signal_mem = True
        self.nrbits = None


def _getMemInfo(mem):
    return _memInfoMap[id(mem)]


def _makeMemInfo(mem):
    key = id(mem)
    if key not in _memInfoMap:
        if isinstance(mem, Memory):
            _memInfoMap[key] = _MemInfo(mem.mem)
            _memInfoMap[key]._signal_mem = False
        else:
            _memInfoMap[key] = _MemInfo(mem)
    return _memInfoMap[key]


def _isMem(mem):
    return id(mem) in _memInfoMap

_userCodeMap = {'verilog': {},
                'vhdl': {}
                }


def _update_user_code_inst_name(subid, name, topname):
    for hdl in _userCodeMap:
        if c := _userCodeMap[hdl].get(subid):
            # Strip top module name prefix and _inst suffix:
            c.namespace["instance_name"] = name.replace(f"{topname}_", "", 1)[:-5]
            break


class _UserCode(object):
    __slots__ = ['code', 'namespace', 'funcname', 'func', 'sourcefile', 'sourceline']

    def __init__(self, code, namespace, funcname, func, sourcefile, sourceline):
        self.code = code
        self.namespace = namespace
        self.sourcefile = sourcefile
        self.func = func
        self.funcname = funcname
        self.sourceline = sourceline

    def __str__(self):
        try:
            code = self._interpolate()
        except:
            type, value, tb = sys.exc_info()
            info = "in file %s, function %s starting on line %s:\n    " % \
                   (self.sourcefile, self.funcname, self.sourceline)
            msg = "%s: %s" % (type, value)
            self.raiseError(msg, info)
        code = "\n%s\n" % code
        return code

    def _scrub_namespace(self):
        for nm, obj in self.namespace.items():
            if _isMem(obj):
                memi = _getMemInfo(obj)
                self.namespace[nm] = memi.name

    def _interpolate(self):
        self._scrub_namespace()
        return string.Template(self.code).substitute(self.namespace)


class _UserCodeDepr(_UserCode):

    def _interpolate(self):
        return self.code % self.namespace


class _UserVerilogCode(_UserCode):

    def raiseError(self, msg, info):
        raise ToVerilogError("Error in user defined Verilog code", msg, info)


class _UserVhdlCode(_UserCode):

    def raiseError(self, msg, info):
        raise ToVHDLError("Error in user defined VHDL code", msg, info)


class _UserVerilogCodeDepr(_UserVerilogCode, _UserCodeDepr):
    pass


class _UserVhdlCodeDepr(_UserVhdlCode, _UserCodeDepr):
    pass


class _UserVerilogInstance(_UserVerilogCode):

    def __str__(self):
        args = inspect.getargspec(self.func)[0]
        s = "%s %s(" % (self.funcname, self.code)
        sep = ''
        for arg in args:
            if arg in self.namespace and isinstance(self.namespace[arg], _Signal):
                signame = self.namespace[arg]._name
                s += sep
                sep = ','
                s += "\n    .%s(%s)" % (arg, signame)
        s += "\n);\n\n"
        return s


class _UserVhdlInstance(_UserVhdlCode):

    def __str__(self):
        args = inspect.getargspec(self.func)[0]
        s = "%s: entity work.%s(MyHDL)\n" % (self.code, self.funcname)
        s += "    port map ("
        sep = ''
        for arg in args:
            if arg in self.namespace and isinstance(self.namespace[arg], _Signal):
                signame = self.namespace[arg]._name
                s += sep
                sep = ','
                s += "\n        %s=>%s" % (arg, signame)
        s += "\n    );\n\n"
        return s


def _addUserCode(specs, arg, funcname, func, frame):
    classMap = {
        '__verilog__': _UserVerilogCodeDepr,
        '__vhdl__': _UserVhdlCodeDepr,
        'verilog_code': _UserVerilogCode,
        'vhdl_code': _UserVhdlCode,
        'verilog_instance': _UserVerilogInstance,
        'vhdl_instance': _UserVhdlInstance,

    }
    namespace = frame.f_globals.copy()
    namespace.update(frame.f_locals)
    if hasattr(func, "verilog_namespace"):
        namespace.update(func.verilog_namespace)
        for x in func.verilog_namespace:
            s = func.verilog_namespace[x]
    sourcefile = inspect.getsourcefile(frame)
    sourceline = inspect.getsourcelines(frame)[1]
    for hdl in _userCodeMap:
        oldspec = "__%s__" % hdl
        codespec = "%s_code" % hdl
        instancespec = "%s_instance" % hdl
        spec = None
        # XXX add warning logic
        if instancespec in specs:
            spec = instancespec
        elif codespec in specs:
            spec = codespec
        elif oldspec in specs:
            spec = oldspec
        if spec:
            assert id(arg) not in _userCodeMap[hdl]
            code = specs[spec]
            _userCodeMap[hdl][id(arg)] = classMap[spec](
                code, namespace, funcname, func, sourcefile, sourceline)


class _CallFuncVisitor(object):

    def __init__(self):
        self.linemap = {}

    def visitAssign(self, node):
        if isinstance(node.expr, ast.CallFunc):
            self.lineno = None
            self.visit(node.expr)
            self.linemap[self.lineno] = node.lineno

    def visitName(self, node):
        self.lineno = node.lineno


class _HierExtr(object):

    def __init__(self, _name, _dut, *args, **kwargs):

        name = _name
        dut = _dut
        global _profileFunc
        _memInfoMap.clear()
        for hdl in _userCodeMap:
            _userCodeMap[hdl].clear()
        self.skipNames = ('always_comb', 'instance',
                          'always_seq', '_always_seq_decorator',
                          'always', '_always_decorator',
                          'instances',
                          'processes', 'posedge', 'negedge')
        self.skip = 0
        self.hierarchy = hierarchy = []
        self.absnames = absnames = {}
        self.level = 0

        _profileFunc = self.extractor
        sys.setprofile(_profileFunc)
        _top = dut(*args, **kwargs)
        sys.setprofile(None)
        if not hierarchy:
            raise ExtractHierarchyError(_error.NoInstances)

        self.top = _top

        # streamline hierarchy
        hierarchy.reverse()
        # walk the hierarchy to define relative and absolute names
        names = {}
        top_inst = hierarchy[0]
        obj, subs = top_inst.obj, top_inst.subs
        names[id(obj)] = name
        absnames[id(obj)] = name
        if not top_inst.level == 1:
            raise ExtractHierarchyError(_error.InconsistentToplevel % (top_inst.level, name))
        for inst in hierarchy:
            obj, subs = inst.obj, inst.subs
            if id(obj) not in names:
                print(f"Looking for {obj} with id {id(obj):#x}", flush=True)
                continue
                # raise ExtractHierarchyError(_error.InconsistentHierarchy)
            inst.name = names[id(obj)]
            tn = absnames[id(obj)]
            for sn, so in subs:
                subid = id(so)
                names[subid] = sn
                aname = f"{tn}_{sn}"
                absnames[subid] = aname
                _update_user_code_inst_name(subid, aname, name)
                if isinstance(so, (tuple, list)):
                    for i, soi in enumerate(so):
                        sni = f"{sn}_{i}"
                        subid = id(soi)
                        names[subid] = sni
                        aname = f"{tn}_{sn}_{i}"
                        absnames[subid] = aname
                        _update_user_code_inst_name(subid, aname, name)

    def extractor(self, frame, event, arg):
        if event == "call":

            funcname = frame.f_code.co_name
            # skip certain functions
            if funcname in self.skipNames:
                self.skip += 1
            if not self.skip:
                self.level += 1

        elif event == "return":

            funcname = frame.f_code.co_name

            if funcname in self.skipNames:
                assert self.skip
                self.skip -= 1
                return

            if not self.skip:
                isGenSeq = _isGenSeq(arg) and _has_something(arg)

                if isGenSeq:
                    func = frame.f_globals.get(funcname)
                    func = getattr(func, "_func", func)
                    f_locals = frame.f_locals
                    if func is None:
                        # Didn't find a func in the global space, try the local "self"
                        # argument and see if it has a method called *funcname*
                        obj = f_locals.get('self')
                        if hasattr(obj, funcname):
                            func = getattr(obj, funcname)

                    specs = {}
                    for hdl in _userCodeMap:
                        spec = "__%s__" % hdl
                        if spec in f_locals and f_locals[spec]:
                            specs[spec] = f_locals[spec]
                        spec = "%s_code" % hdl
                        if func and hasattr(func, spec) and getattr(func, spec):
                            specs[spec] = getattr(func, spec)
                        spec = "%s_instance" % hdl
                        if func and hasattr(func, spec) and getattr(func, spec):
                            specs[spec] = getattr(func, spec)
                    if specs:
                        _addUserCode(specs, arg, funcname, func, frame)

                # building hierarchy only makes sense if there are generators
                if isGenSeq and arg:
                    sigdict = {}
                    memdict = {}
                    symdict = frame.f_globals.copy()
                    symdict.update(f_locals)
                    cellvars = []
                    invsigdict = {}

                    # All nested functions will be in co_consts
                    if func:
                        local_gens = []
                        consts = func.__code__.co_consts
                        for item in _flatten(arg):
                            if isinstance(item, Cosimulation):
                                continue
                            elif isinstance(item, _SignalInstance):
                                if not item.seen:
                                    cellvars.extend(item.cellvars)
                                    symdict.update(item.sigdict)
                                    item.seen = True
                                continue
                            genfunc = _genfunc(item)
                            if genfunc.__code__ in consts:
                                local_gens.append(item)
                        if local_gens:
                            cellvarlist = _getCellVars(symdict, local_gens)
                            cellvars.extend(cellvarlist)
                            objlist = _resolveRefs(symdict, local_gens)
                            cellvars.extend(objlist)
                    else:
                        cellvars = frame.f_code.co_cellvars
                    # for dict in (frame.f_globals, frame.f_locals):
                    for n, v in symdict.items():
                        # extract signals and memories
                        # also keep track of whether they are used in generators
                        # only include objects that are used in generators
                        # if not n in cellvars:
                        # continue
                        if isinstance(v, _Signal):
                            sigdict[n] = v
                            invsigdict[id(v)] = n
                            if n in cellvars or isinstance(v, ConversionSignal):
                                v._markUsed()
                        elif isinstance(v, Struct):
                            _expand_interface(
                                n, v, sigdict, memdict, invsigdict, n in cellvars
                            )
                        elif isinstance(v, Memory):
                            m = _makeMemInfo(v)
                            if v.nrbits is not None:
                                m.nrbits = v.nrbits
                            memdict[n] = m
                            if n in cellvars:
                                m._used = True
                        elif _isListOfSigs(v):
                            m = _makeMemInfo(v)
                            memdict[n] = m
                            if n in cellvars:
                                m._used = True

                    subs = []
                    infargs = _inferArgs(arg)
                    for n, sub in f_locals.items():
                        subid = id(sub)
                        if subid in infargs:
                            subs.append((n, sub))
                            for hdl in _userCodeMap:
                                if c := _userCodeMap[hdl].get(subid):
                                    for k, s in c.namespace.items():
                                        if not isinstance(s, _Signal):
                                            continue
                                        sigid = id(s)
                                        if sigid not in invsigdict:
                                            _add_unique(sigdict, k, s)
                                    c.namespace["instance_name"] = n
                                    break

                    inst = _Instance(self.level, arg, subs, sigdict, memdict)
                    self.hierarchy.append(inst)

                self.level -= 1


def _inferArgs(arg):
    c = [arg]
    if isinstance(arg, (tuple, list)):
        c += list(arg)
    return set(id(x) for x in c)
