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

#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

""" Module with the always function. """
import inspect
from types import FunctionType

from myhdl import InstanceError
from myhdl._util import _isGenFunc, _makeAST
from myhdl._Waiter import _inferWaiter
from myhdl._resolverefs import _AttrRefTransformer
from myhdl._visitors import _SigNameVisitor


class _error:
    pass
_error.NrOfArgs = "decorated generator function should not have arguments"
_error.ArgType = "decorated object should be a generator function"


class _CallInfo(object):

    def __init__(self, name, modctxt, symdict):
        self.name = name
        self.modctxt = modctxt
        self.symdict = symdict


def _cell_contents(cell):
    try:
        return cell.cell_contents
    except ValueError:
        return None


use_globals = False


def _getCallInfo(func=None):
    """Get info on the caller of an Instantiator.

    An Instantiator should be used in a block context.
    This function gets the required info about the caller.
    It uses the frame stack:
    0: this function
    1: the instantiator decorator
    2: the block function that defines instances
    3: the caller of the block function, e.g. the BlockInstance.
    """
    frame = inspect.currentframe()
    try:
        if func:
            symdict = {
                var: _cell_contents(cell) for var, cell in zip(
                    func.__code__.co_freevars or [], func.__closure__ or []
                )
            }
            symdict[func.__name__] = func
        else:
            symdict = None
        f = frame.f_back.f_back
        name = f.f_code.co_name
        if not symdict:
            symdict = dict(f.f_globals)
            symdict.update(f.f_locals)
        modctxt = False
        if use_globals:
            symdict.update(f.f_globals)
        # f_locals = f.f_back.f_locals

        # if 'self' in f_locals:
        #     from myhdl import _block
        #     modctxt = isinstance(f_locals['self'], _block._Block)
        return _CallInfo(name, modctxt, symdict)
    finally:
        del frame


def instance(genfunc):
    if not isinstance(genfunc, FunctionType):
        raise InstanceError(_error.ArgType)
    callinfo = _getCallInfo(genfunc)
    if not _isGenFunc(genfunc):
        raise InstanceError(_error.ArgType)
    if genfunc.__code__.co_argcount > 0:
        raise InstanceError(_error.NrOfArgs)
    return _Instantiator(genfunc, callinfo=callinfo)


class _Instantiator(object):

    _delete_inputs = True

    def __init__(self, genfunc, callinfo):
        self.modctxt = callinfo.modctxt
        self.genfunc = genfunc
        self.gen = genfunc()
        # infer symdict
        f = self.funcobj
        varnames = f.__code__.co_varnames
        symdict = {}
        for n, v in callinfo.symdict.items():
            if n not in varnames:
                symdict[n] = v
        self.symdict = symdict

        # /print modname, genfunc.__name__
        tree = self.ast
        # print ast.dump(tree)
        v = _AttrRefTransformer(self)
        v.visit(tree)
        v = _SigNameVisitor(self.symdict)
        v.visit(tree)
        self.inputs = v.inputs
        self.outputs = v.outputs
        self.inouts = v.inouts
        self.embedded_func = v.embedded_func
        self.sigdict = v.sigdict
        self.losdict = v.losdict

    def _cleanup(self):
        if self._delete_inputs:
            del self.inputs
        del self.outputs
        del self.inouts
        del self.symdict

    @property
    def name(self):
        return self.funcobj.__name__

    @property
    def funcobj(self):
        return self.genfunc

    @property
    def waiter(self):
        return self._waiter()(self.gen)

    def _waiter(self):
        return _inferWaiter

    @property
    def ast(self):
        return _makeAST(self.funcobj)


class _SignalInstance:
    """Empty block only containing signals"""

    def __init__(self, *cellvars, **sigdict):
        self.cellvars = list(cellvars)
        self.sigdict = dict(sigdict)
        self.seen = False
