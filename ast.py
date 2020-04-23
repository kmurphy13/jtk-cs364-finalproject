from typing import Sequence, Union, Optional, List, Tuple
import operator


# Use a class hierarchy to represent types.
class Expr:
    """
    Base class for expressions
    """
    pass


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


class StatementsStmt:
    def __init__(self, statement_list: List[Stmt]):
        self.statement_list = statement_list

    def __str__(self):
        for stmt in self.statement_list:
            return stmt


class PrintArg:
    def __init__(self, string_lit: str):
        self.string_lit = string_lit

    def __str__(self):
        return self.string_lit


class PrintStmt(Stmt):
    def __init__(self):
        pass


class WhileStmt(Stmt):
    def __init__(self):
        pass


class AssignStmt(Stmt):
    def __init__(self):
        pass


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


class DeclarationExpr(Expr):
    def __init__(self, dec_type, dec_id):
        self.type = dec_type
        self.id = dec_id

    def __str__(self):
        return self.type + ' ' + self.id + ';'


class DeclarationsExpr(Expr):
    def __init__(self, dec_list: List[DeclarationExpr]):
        self.dec_list = dec_list

    def __str__(self):
        for dec in self.dec_list:
            return dec


class IDExpr(Expr):

    def __init__(self, identifier: str):
        self.id = identifier

    def __str__(self):
        return self.id

    def scheme(self):
        return self.id

    def eval(self, env):  # a + 7
        # lookup the value of self.id. Look up where?
        # env is a dictionary
        pass


class FuncIDExpr(Expr):
    def __init__(self, identifier: str):
        self.id = identifier

    def __str__(self):
        return self.id


class StringLitExpr(Expr):
    def __init__(self, string_lit: str):
        self.string_lit = string_lit

    def __str__(self):
        return self.string_lit


class IntLitExpr(Expr):
    def __init__(self, intlit: str):
        self.intlit = int(intlit)

    def __str__(self):
        return str(self.intlit)


class FunctionDef:
    def __init__(self, type: str, id: IDExpr, params: List[Tuple[str, str]], decls: DeclarationsExpr, stmts: StatementsStmt):
        self.type = type
        self.id = id
        self.params = params
        self.decls = decls
        self.stmts = stmts

    def __str__(self):
        return "{0} {1}({2}){ \n {3} \n {4} \n }".format(self.type, self.id, self.params, self.decls, self.stmts)

    # def eval(self) -> Union[int, float, bool]:
    #     # an environment maps identifiers to values
    #     # parameters or local variables
    #     # to evaluate a function you evaluate all of the statements
    #     # within the environment
    #     env = {}   # TODO Fix this
    #     for s in self.stmts:
    #         s.eval(env)  # TODO define environment


class Program:
    def __init__(self, funcs: Sequence[FunctionDef]):
        self.funcs = funcs

    def __str__(self):
        for func in self.funcs:
            return func


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
