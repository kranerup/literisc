import ast
from copy import deepcopy
from ._ast import parse_func
from ._intbv import intbv
from ._modbv import modbv


_globals = {"intbv": intbv, "modbv": modbv}


class _Unbound:
    pass


def _cell_contents(cell):
    try:
        return cell.cell_contents
    except ValueError:
        return _Unbound


def _closure_locals(func):
    """Extract function closure constants"""
    if func.__closure__ is None:
        return {}
    try:
        return {
            var: _cell_contents(cell)
            for var, cell in zip(func.__code__.co_freevars, func.__closure__)
        }
    except ValueError:
        unbound = []
        for var, cell in zip(func.__code__.co_freevars, func.__closure__):
            try:
                _ = cell.cell_contents
            except ValueError:
                unbound.append(var)
        raise ValueError(
            f"Use of undefined variables: {unbound}. " "Maybe declared after function?"
        )


def _is_const(x):
    return isinstance(x, (intbv, int, str, bool)) or x is None


def _closure_constants(clocals):
    return {x: clocals[x] for x in clocals if _is_const(clocals[x])}


def _replace_constant(node, constants):
    """Return Constant if Name node points to variable in constants"""
    if isinstance(node, ast.Name) and node.id in constants:
        return ast.Constant(value=constants[node.id])
    else:
        return node


def _iter_range(args):
    if all(isinstance(x, ast.Constant) for x in args):
        nargs = len(args)
        if nargs == 1:
            return 0, args[0].value
        elif nargs == 2:
            return args[0].value, args[1].value - args[0].value
    return None, None


class _InsertConstants(ast.NodeTransformer):
    """Insert and propagate constants"""

    def _op(self, node):
        self.generic_visit(node)
        node.left = _replace_constant(node.left, self.constants)
        node.right = _replace_constant(node.right, self.constants)
        left_const = isinstance(node.left, ast.Constant)
        right_const = isinstance(node.right, ast.Constant)
        if left_const and right_const:
            value = eval(ast.unparse(node), _globals, self.clocals)
            return ast.Constant(value=value)
        else:
            return node

    def visit_BinOp(self, node):
        return self._op(node)

    def visit_BoolOp(self, node):
        self.generic_visit(node)
        node.values = [_replace_constant(x, self.constants) for x in node.values]
        if isinstance(node.op, ast.And):
            values = []
            for x in node.values:
                if isinstance(x, ast.Constant):
                    if not x.value:
                        return ast.Constant(value=False)
                else:
                    values.append(x)
            if not values:
                return ast.Constant(value=True)
            node.values = values
        else:  # Or:
            values = []
            for x in node.values:
                if isinstance(x, ast.Constant):
                    if x.value:
                        return ast.Constant(value=True)
                else:
                    values.append(x)
            if not values:
                return ast.Constant(value=False)
            node.values = values
        if all(isinstance(x, ast.Constant) for x in node.values):
            value = eval(ast.unparse(node), _globals, self.clocals)
            return ast.Constant(value=value)
        else:
            return node

    def visit_UnaryOp(self, node):
        self.generic_visit(node)
        node.operand = _replace_constant(node.operand, self.constants)
        if isinstance(node.operand, ast.Constant):
            value = eval(ast.unparse(node), _globals, self.clocals)
            return ast.Constant(value=value)
        return node

    def visit_Compare(self, node):
        self.generic_visit(node)
        node.left = _replace_constant(node.left, self.constants)
        node.comparators = [
            _replace_constant(x, self.constants) for x in node.comparators
        ]
        if all(isinstance(x, ast.Constant) for x in ([node.left] + node.comparators)):
            try:
                value = eval(ast.unparse(node), _globals, self.clocals)
                return ast.Constant(value=value)
            except:  # noqa: E722
                return node
        else:
            return node

    def visit_Name(self, node):
        self.generic_visit(node)
        return _replace_constant(node, self.constants)

    def visit_If(self, node):
        self.generic_visit(node)
        node.test = _replace_constant(node.test, self.constants)
        return node

    def visit_For(self, node):
        """Set loop variable to constant if range length is 0 or 1"""
        self.generic_visit(node.iter)
        if not isinstance(node.target, ast.Name):
            self.generic_visit(node)
            return node
        if node.target.id in self.constants:
            del self.constants[node.target.id]
        if (
            isinstance(node.iter, ast.Call)
            and isinstance(node.iter.func, ast.Name)
            and node.iter.func.id == "range"
        ):
            for i, a in enumerate(node.iter.args):
                node.iter.args[i] = _replace_constant(a, self.constants)
            s, n = _iter_range(node.iter.args)
            if s is not None and n in (0, 1):
                self.constants[node.target.id] = s
        self.generic_visit(node)
        return node

    def visit_Assign(self, node):
        """
        Loop variables set to a constant may be reused later as plain
        variable later in the code, so remove from constants on assign
        """
        for t in node.targets:
            if isinstance(t, ast.Name) and t.id in self.constants:
                del self.constants[t.id]
        self.generic_visit(node)
        return node

    # def visit_Attribute(node):
    #     self.generic_visit(node)
    #     value = node.value
    #     if (
    #         isinstance(value, ast.Attribute)
    #         and isinstance(value.ctx, ast.Load)
    #     ):
    #         id = value.id
    #         if id in self.clocals:
    #            pass   -- dunno if there is a case for this....

    def _argmap(self, func, node):
        """
        Map parameter names to actual values.
        Handle default values too.
        """
        code = func.__code__
        argmap = {x: deepcopy(y) for x, y in zip(code.co_varnames, node.args)}
        argmap.update({k.arg: deepcopy(k.value) for k in node.keywords})
        if func.__defaults__:
            offset = code.co_argcount - len(func.__defaults__)
            names = code.co_varnames[offset:]
            for name, value in zip(names, func.__defaults__):
                if name not in argmap:
                    argmap[name] = ast.Constant(value=value)
        if func.__kwdefaults__:
            for name, value in func.__kwdefaults__.items():
                if name not in argmap:
                    argmap[name] = ast.Constant(value=value)
        return argmap

    def visit_Call(self, node):
        """
        Inline calls to local functions using simple name-mapping
        of parameters.  Works pretty much as macros.
        """
        name = node.func
        self.generic_visit(node)
        if isinstance(name, ast.Name):
            id = name.id
            if not ((func := self.clocals.get(id)) or (func := self.globals.get(id))):
                return node
            if not getattr(func, "inline", False):
                return node
            argmap = self._argmap(func, node)
            tree = reduce(func)
            body = tree.body[0].body
            tree = ast.If(
                test=ast.Constant(value=True),
                body=deepcopy(body),
                orelse=[],
            )
            tree = _rename_params(tree, argmap)
            tree._inline = True
            # Visit again to propagate more constants:
            self.generic_visit(tree)
            return tree
        return node

    def visit_Expr(self, node):
        """Remove Expr() wrapper from inlined call"""
        self.generic_visit(node)
        if hasattr(node.value, "_inline"):
            return node.value
        return node

    def __init__(self, name, clocals, globals):
        super().__init__()
        self.name = name
        self.constants = _closure_constants(clocals)
        self.clocals = clocals
        self.globals = globals


def _insert_constants(tree, name, constants, g):
    """Replace Name nodes with constants"""
    return ast.fix_missing_locations(_InsertConstants(name, constants, g).visit(tree))


class _ArgRenamer(ast.NodeTransformer):
    def __init__(self, argmap):
        self.argmap = argmap
        self._in_name = 0

    def visit_Name(self, node):
        self.generic_visit(node)
        if not self._in_name:
            if node.id in self.argmap:
                return self.argmap[node.id]
        return node


def _rename_params(tree, argmap):
    return ast.fix_missing_locations(_ArgRenamer(argmap).visit(tree))


class _ConstIfRemover(ast.NodeTransformer):
    def visit_If(self, node):
        self.generic_visit(node)
        if not node.body:
            if not node.orelse:
                return None
            node.body = [ast.Pass()]
        if isinstance(node.test, ast.Constant):
            if node.test.value:
                return node.body
            else:
                return node.orelse
        else:
            return node

    def visit_For(self, node):
        """Remove empty loops, flatten 1 iteration loops"""
        self.generic_visit(node)
        if (
            isinstance(node.iter, ast.Call)
            and isinstance(node.iter.func, ast.Name)
            and node.iter.func.id == "range"
        ):
            s, n = _iter_range(node.iter.args)
            if s is not None:
                if n == 0:
                    return node.orelse
                elif n == 1:
                    return node.body
        return node


def _remove_const_if(tree):
    return ast.fix_missing_locations(_ConstIfRemover().visit(tree))


class _Inliner(ast.NodeTransformer):
    def __init__(self, local_vars, prefix):
        super().__init__()
        self.local_vars = set(local_vars)
        self.prefix = prefix

    def visit_Name(self, node):
        self.generic_visit(node)
        if node.id in self.local_vars:
            node.id = f"{self.prefix}__{node.id}"
        return node

    def vist_Call(self, node):
        self.generic_visit(node)
        func = getattr(node.name)
        if func and getattr(func, "_inline"):
            return _inline(func, f"{self.prefix}__{node.name}")
        else:
            return node


def _inline(func, prefix):
    """Return AST for inlining of given function"""
    if not hasattr(func, "_inline"):
        return None
    tree = parse_func(func)
    local_vars = func.__code__.co_varnames
    tree = ast.fix_missing_locations(_Inliner(local_vars, prefix).visit(tree))
    body = tree.body[0].body[0].value
    return body


def reduce(func):
    """Return reduced AST of given function"""
    clocals = _closure_locals(func)
    tree = parse_func(func)
    _insert_constants(tree, func.__name__, clocals, func.__globals__)
    tree = _remove_const_if(tree)
    return tree


if __name__ == "__main__":
    t1 = ast.parse("if not (1-bar): pass")
    t2 = _insert_constants(t1, "foo", {"bar": 1})
    print(ast.dump(t2, indent="    "))
