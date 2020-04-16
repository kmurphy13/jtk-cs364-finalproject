"""
SLU-C Abstract Syntax Trees
An abstract syntax tree (AST) is a data structure that represents
the concrete (text) syntax of a program
"""
from typing import Sequence, Union
import operator


# Use a class hierarchy to represent types.

class FunctionDef:
    def __init__(self, t, id: str, params, decls, stmts):
        # provide type hints for all of the parameters
        # Decls should be a dictionary
        # Key: id
        # Value: Type
        pass

    def __str__(self):
        pass


class Declaration:
    pass


class Program:

    def __init__(self, funcs: Sequence[FunctionDef]):
        self.funcs = funcs


class Expr:
    """
    Base class for expressions
    """
    pass


# TODO Don't just cut-and-paste new operations, abstract!

class BinaryExpr(Expr):
    def __init__(self, left: Expr, right: Expr, op):
        self.left = left
        self.right = right
        self.op = op

    def __str__(self):

        return "("  + str(self.left) + " " + self.op + " "+ str(self.right) + ")"
    def scheme(self) -> str:

        return "(" +self.op +  "{0} {1})".format(self.left.scheme(), self.right.scheme())

    def eval(self) -> Union[int, float]:
        l = self.left.eval()
        r = self.right.eval()
        opdict = {
            '+': operator.add(l,r),
            '-': operator.sub(l,r),
            '*': operator.mul(l, r),
            '/': operator.truediv(l,r),
            '%': operator.mod(l,r),

            '<': operator.lt(l, r),
            '<=': operator.le(l, r),
            '==': operator.eq(l, r),
            '!=': operator.ne(l, r),
            '>=': operator.ge(l, r),
            '>': operator.gt(l, r),
            '||': operator.or_(l,r),
            '&&': operator.add(l,r),
        }
        return opdict[self.op]
class UnaryMinus(Expr):
    def __init__(self, tree: Expr):
        self.tree = tree

    def __str__(self):
        return "-({0})".format(str(self.tree))

    def scheme(self):
        return "(- {0})".format(self.tree.scheme())

    def eval(self):
        return -self.tree.eval()


class IDExpr(Expr):

    def __init__(self, id: str):
        self.id = id

    def __str__(self):
        return self.id

    def scheme(self):
        return self.id

    def eval(self, env):  # a + 7
        # lookup the value of self.id. Look up where?
        # env is a dictionary
        pass


class IntLitExpr(Expr):

    def __init__(self, intlit: str):
        self.intlit = int(intlit)

    def __str__(self):
        return str(self.intlit)

    def scheme(self):
        return str(self.intlit)

    def eval(self):
        return self.intlit  # base case


if __name__ == '__main__':
    """
    Represent a + b + c * d
    ((a + b) + (c * d))
    """
    expr = BinaryExpr(BinaryExpr(IntLitExpr('5'), IntLitExpr('6'), '+'),
                   BinaryExpr(IntLitExpr('9'),IntLitExpr('8'), '*'), '+')
    print(expr)
    print(expr.eval())

