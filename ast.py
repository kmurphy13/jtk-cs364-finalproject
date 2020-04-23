from typing import Sequence, Union, Optional
import operator


# Use a class hierarchy to represent types.
class Expr:
    """
    Base class for expressions
    """
    pass


class FunctionDef:
    def __init__(self, t: str, id: IDExpr, params: Params, decls: DeclarationsExpr, stmts: StatementsStmt):
        # provide type hints for all of the parameters
        # Decls should be a dictionary
        # Key: id
        # Value: Type
        pass

    def __str__(self):
        pass

    def eval(self) -> Union[int, float, bool]:
        # an environment maps identifiers to values
        # parameters or local variables
        # to evaluate a function you evaluate all of the statements
        # within the environment
        env = {}   # TODO Fix this
        for s in self.stmts:
            s.eval(env)  # TODO define environment


class Stmt:
    pass


class IfStmt(Stmt):
    def __init__(self, cond: Expr, truepart: Stmt, falsepart : Optional[Stmt]):
        pass

    def eval(self, env):

        if self.cond.eval():
            self.truepart.eval(env)
        elif self.falsepart is not None:
            self.falsepart.eval(env)


class StatementsStmt(Stmt):
    def __init__(self):
        pass



class Declaration:
    pass


class Program:

    def __init__(self, funcs: Sequence[FunctionDef]):
        self.funcs = funcs


class BinaryExpr(Expr):
    def __init__(self, left: Expr, right: Expr, op):
        self.left = left
        self.right = right
        self.op = op

    def __str__(self):
        return "(" + str(self.left) + " " + self.op + " " + str(self.right) + ")"

    def eval(self) -> Union[int, float]:
        l = self.left.eval()
        r = self.right.eval()
        opdict = {
            '+': operator.add(l, r),
            '-': operator.sub(l, r),
            '*': operator.mul(l, r),
            '/': operator.truediv(l, r),
            '%': operator.mod(l, r),

            '<': operator.lt(l, r),
            '<=': operator.le(l, r),
            '==': operator.eq(l, r),
            '!=': operator.ne(l, r),
            '>=': operator.ge(l, r),
            '>': operator.gt(l, r),
            '||': operator.or_(l, r),
            '&&': operator.add(l, r),
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

    def typeof(self, decls) -> Type:
        # TODO type decls appropriately as a dictionary type
        # look up the variable type in the declaration dictoinary
        # from the function definition (FunctionDef)
        pass


class IntLitExpr(Expr):

    def __init__(self, intlit: str):
        self.intlit = int(intlit)

    def __str__(self):
        return str(self.intlit)

    def scheme(self):
        return str(self.intlit)

    def eval(self):
        return self.intlit   # base case

    #def typeof(self) -> Type:
    # representing SLU-C types using Python types
    def typeof(self) -> type:

        #return IntegerType
        return int

class SLUCTypeError(Exception):
    def __init__(self, message: str):
        Exception.__init__(self)
        self.message = message

    def __str__(self):
        return self.message

if __name__ == '__main__':
    """
    Represent a + b + c * d
    ((a + b) + (c * d))
    """
    expr = BinaryExpr(BinaryExpr(IntLitExpr('5'), IntLitExpr('6'), '+'),
                      BinaryExpr(IntLitExpr('9'), IntLitExpr('8'), '*'), '+')
    print(expr)
    print(expr.eval())
