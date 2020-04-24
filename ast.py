from typing import Sequence, Union, Optional, List
import operator


# Use a class hierarchy to represent types.
class Expr:
    """
    Base class for expressions
    """
    pass


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
    def __init__(self, identifier: str):
        self.id = identifier

    def __str__(self):
        return self.id

    # def eval(self, env):  # a + 7
    #     # lookup the value of self.id. Look up where?
    #     # env is a dictionary
    #     pass


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
    def __init__(self, int_lit: str):
        self.int_lit = int(int_lit)

    def __str__(self):
        return str(self.int_lit)


class FloatLitExpr(Expr):
    def __init__(self, float_lit: str):
        self.float_lit = float(float_lit)

    def __str__(self):
        return str(self.float_lit)


class BoolExpr(Expr):
    def __init__(self, bool_val: str):
        self.bool_val = (bool_val == 'True')

    def __str__(self):
        return str(self.bool_val)


class BinaryExpr(Expr):
    def __init__(self, left: Expr, right: Expr, op):
        self.left = left
        self.right = right
        self.op = op

    def __str__(self):
        return str(self.left) + " " + str(self.op) + " " + str(self.right)

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


class Stmt:
    def __str__(self):
        return self


class Statements:
    def __init__(self, statement_list: List[Stmt]):
        self.statement_list = statement_list

    def __str__(self):
        output = ''
        if self.statement_list:
            output += str(self.statement_list[0])
            for arg in self.statement_list[1:]:
                output += ('\n' + str(arg))
        return output


class Block(Stmt):
    def __init__(self, stmts: Statements):
        self.stmts = stmts

    def __str__(self):
        return "{\n\t" + str(self.stmts) + "\n}"


class ReturnStmt(Stmt):
    def __init__(self, ret_val: Expr):
        self.ret_val = ret_val

    def __str__(self):
        return "return " + str(self.ret_val) + ";"


class IfStmt(Stmt):
    def __init__(self, cond: Expr, true_part: Stmt, false_part: Optional[Stmt]):
        self.cond = cond
        self.true_part = true_part
        self.false_part = false_part

    def __str__(self):
        if self.false_part:
            return 'if (' + str(self.cond) + ') ' + str(self.true_part) + ' else ' + str(self.false_part) + '\n'
        else:
            return 'if (' + str(self.cond) + ') ' + str(self.true_part) + '\n'


class PrintStmt(Stmt):
    def __init__(self, print_args: List[Union[Expr, StringLitExpr]]):
        self.print_args = print_args

    def __str__(self):
        output = 'print('
        if self.print_args:
            output += str(self.print_args[0])
            for arg in self.print_args[1:]:
                output += (', ' + str(arg))
        return output + ')'


class ReturnStmt(Stmt):
    def __init__(self, return_expr: Expr):
        self.return_expr = return_expr

    def __str__(self):
        return 'return ' + str(self.return_expr) + ';'


class WhileStmt(Stmt):
    def __init__(self, while_expr: Expr, while_statement: Stmt):
        self.expr = while_expr
        self.statement = while_statement

    def __str__(self):
        return 'while ' + "("+ str(self.expr) + ")" ' ' + str(self.statement)


class AssignStmt(Stmt):
    def __init__(self, assign_id: IDExpr, assign_expression: Expr):
        self.assign_id = assign_id
        self.assign_expression = assign_expression

    def __str__(self):
        return str(self.assign_id) + "=" + str(self.assign_expression) + ";"


class ParamExpr(Expr):
    def __init__(self, param_type, param_id):
        self.param_type = param_type
        self.param_id = param_id

    def __str__(self):
        return str(self.param_type) + ' ' + str(self.param_id)


class Params:
    def __init__(self, params_list: List[ParamExpr]):
        self.params = params_list

    def __str__(self):
        output = ''
        if self.params:
            output += str(self.params[0])
            for param in self.params[1:]:
                output += (', ' + str(param))
        return output


class DeclarationExpr:
    def __init__(self, dec_type, dec_id):
        self.type = dec_type
        self.id = dec_id

    def __str__(self):
        return str(self.type) + ' ' + str(self.id) + ';'


class Declarations:
    def __init__(self, dec_list: List[DeclarationExpr]):
        self.dec_list = dec_list

    def __str__(self):
        output = ''
        for dec in self.dec_list:
            output += str(dec) + '\n'
        return output


class FunctionDef:
    def __init__(self, func_type: str, func_id: IDExpr, params: Params, decls: Declarations, stmts: Statements):
        self.func_type = func_type
        self.func_id = func_id
        self.params = params
        self.decls = decls
        self.stmts = stmts

    def __str__(self):
        return self.func_type + str(self.func_id) + "(" + str(self.params) + ") {\n" + str(self.decls) + \
               "\n" + str(self.stmts) + "\n}"

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
        output = ''
        if self.funcs:
            output += str(self.funcs[0])
            for func in self.funcs[1:]:
                output += ('\n\n' + str(func))
        return output


class SLUCTypeError(Exception):
    def __init__(self, message: str):
        Exception.__init__(self)
        self.message = message

    def __str__(self):
        return self.message


if __name__ == '__main__':

    while_stmt = WhileStmt(
        BinaryExpr(IDExpr("a"), IDExpr("b"), "<"),
        Block(Statements([AssignStmt(IDExpr("a"), BinaryExpr(IDExpr("a"), IDExpr("b"), "+"))]))
    )

    print_stmt = PrintStmt([StringLitExpr("Kira"), BinaryExpr(IDExpr("a"), IDExpr("b"), "+")])

    return_stmt = ReturnStmt(BinaryExpr(IDExpr("a"), IDExpr("b"), "<"))

    declarations_test = Declarations([DeclarationExpr('int', 'b'), DeclarationExpr('bool', 'c')])

    if_stmt = IfStmt(
        BinaryExpr(IDExpr("a"), IDExpr("b"), "<"),
        Block(Statements([AssignStmt(IDExpr("a"), BinaryExpr(IDExpr("a"), IDExpr("b"), "+"))])),
        Block(Statements([AssignStmt(IDExpr("a"), BinaryExpr(IDExpr("a"), IDExpr("b"), "+"))]))
    )

    params_test = Params([ParamExpr('int', 'a'), ParamExpr('bool', 'd')])
    statements_test = Statements([if_stmt, print_stmt, return_stmt])

    expr = Program([FunctionDef(
        'bool',
        IDExpr('a_function'),
        params_test,
        declarations_test,
        statements_test
    )])

    print(expr)
