"""Inline decorator"""


def inline(func):
    func.inline = True
    return func
