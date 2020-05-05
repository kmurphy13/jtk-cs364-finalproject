from typing import Sequence, Union, Optional, List
import operator


# Use a class hierarchy to represent types.
class Expr:
    """
    Base class for expressions
    """

    def eval(self, env: Optional[dict]):
        pass


class UnaryMinus(Expr):
    """
    Adds a unary minus to an expression (-34567)
    """
    def __init__(self, tree: Expr):
        self.tree = tree

    def __str__(self):
        return "-({0})".format(str(self.tree))

    def eval(self, env):
        return -self.tree.eval(env)


class IDExpr(Expr):
    """
    Creates an instance of an identifier expression (the x in int x;)
    """
    def __init__(self, identifier: str):
        self.id = identifier


    def __str__(self):
        return self.id

    def eval(self, env):  # a + 7
    # lookup the value of self.id. Look up where?
        return env[self.id].eval(env)


class StringLitExpr(Expr):
    """
    Creates an instance of a string literal expression ("Our team name is JTK")
    """
    def __init__(self, string_lit: str):
        self.string_lit = string_lit

    def __str__(self):
        return self.string_lit

    def eval(self, env):
        return str(self.string_lit)


class IntLitExpr(Expr):
    """
    Creates an instance of an integer literal expression (12345)
    """
    def __init__(self, int_lit: str):
        self.int_lit = int(int_lit)

    def __str__(self):
        return str(self.int_lit)

    def eval(self,env):
        return int(self.int_lit)


class FloatLitExpr(Expr):
    """
    Creates an instance of a float literal expression (3.14159/1e-9)
    """
    def __init__(self, float_lit: str):
        self.float_lit = float(float_lit)

    def __str__(self):
        return str(self.float_lit)

    def eval(self, env):
        return float(self.float_lit)


class BoolExpr(Expr):
    """
    Creates an instance of a boolean expression (true/false)
    """
    def __init__(self, bool_val: str):
        self.bool_val = bool_val.lower()

    def __str__(self):
        return str(self.bool_val)

    def eval(self,env):
        return bool(self.bool_val)

class BinaryExpr(Expr):
    """
    Base class for binary operation expressions
    The operation passed to the __init__ can be +, -, *, /, %, <, <=, ==, !=, >=, >, ||, or &&
    This operation is then used to put between the left and right expressions passed to __init__
    BinaryExpr(7, 9, '*') evaluates to 7 * 9
    """
    def __init__(self, left: Expr, right: Expr, op):
        self.left = left
        self.right = right
        self.op = op

    def __str__(self):
        return "(" + str(self.left) + " " + str(self.op) + " " + str(self.right) + ")"

    def eval(self,env) -> Union[int, float]:
        l = self.left.eval(env)
        r = self.right.eval(env)
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
    """
    Base class for statement
    """
    def __str__(self):
        return self

    def eval(self,env):
        pass


class Statements():
    """
    Base class for a list of statements
    """
    def __init__(self, statement_list: List[Stmt]):
        self.statement_list = statement_list

    def __str__(self):
        output = '\t'
        if self.statement_list:
            output += str(self.statement_list[0])
            for arg in self.statement_list[1:]:
                output += ('\n\t' + str(arg))
        return output

    def eval(self,env):
        for statement in self.statement_list:
            statement.eval(env)


class Block(Stmt):
    """
    Instance of a block
    {
     ....
    }
    """
    def __init__(self, stmts: Statements):
        self.stmts = stmts

    def __str__(self):
        return "{\n" + str(self.stmts) + "\n\t}"

    def eval(self,env):
        return self.stmts.eval()


class IfStmt(Stmt):
    """
    Instance of an if statement
    :param cond: the condition checked by the if statement
    :param true_part: the executable code if the condition is true
    :param false_part: the executable code if the condition is false
    if (cond) {
        true_part
    } else {
        false_part
    }
    """
    def __init__(self, cond: Expr, true_part: Stmt, false_part: Optional[Stmt]):
        self.cond = cond
        self.true_part = true_part
        self.false_part = false_part

    def __str__(self):
        if self.false_part:
            return 'if (' + str(self.cond) + ')\n\t\t' + str(self.true_part) + '\n\telse\n\t\t' + str(self.false_part) + '\n'
        else:
            return 'if (' + str(self.cond) + ') \n\t\t' + str(self.true_part) + '\n'

    def eval(self,env):
        if self.cond.eval(env):
            return self.true_part.eval(env)
        elif self.false_part is not None:
            return self.false_part.eval(env)




class PrintStmt(Stmt):
    """
    Creates an instance of a print statement from a list of print arguments
    """
    def __init__(self, print_args: List[Union[Expr, StringLitExpr]]):
        self.print_args = print_args

    def __str__(self):
        output = 'print('
        if self.print_args:
            output += str(self.print_args[0])
            for arg in self.print_args[1:]:
                output += (', ' + str(arg))
        return output + ');'

    def eval(self):
        for arg in self.print_args:
            print(arg.eval(), end = '')




class ReturnStmt(Stmt):
    """
    Creates an instance of a return statement
    :param return_expr: What you are returning
    return return_expr
    """
    def __init__(self, return_expr: Expr):
        self.return_expr = return_expr

    def __str__(self):
        return 'return ' + str(self.return_expr) + ';'

    def eval(self,env):
        return self.return_expr.eval()


class WhileStmt(Stmt):
    """
    Creates an instance of a while statement
    :param while_expr: the condition to check in order to keep evaluating the contents of the statement
    :param while_statement: the contents of the while statement
    while (while_expr) {
        while_statement
    }
    """
    def __init__(self, while_expr: Expr, while_statement: Stmt):
        self.expr = while_expr
        self.statement = while_statement

    def __str__(self):
        return 'while ' + "("+ str(self.expr) + ")" + str(self.statement)

    def eval(self,env):
        while(self.expr.eval(env)):
            self.statement.eval(env)




class AssignStmt(Stmt):
    """
    Creates an instance of an assignment statement
    :param assign_id: the identifier that is being assigned
    :param assign_expression: the value that the identifier is being assigned
    assign_id = assign_expression
    """
    def __init__(self, assign_id: IDExpr, assign_expression: Expr):
        self.assign_id = assign_id
        self.assign_expression = assign_expression

    def __str__(self):
        return str(self.assign_id) + " = " + str(self.assign_expression) + ";"


class ParamExpr(Expr):
    """
    Creates an instance of a parameter expression
    :param param_type: the data type of the parameter
    :param param_id: the identifier of the parameter
    param_type param_id
    int x
    """
    def __init__(self, param_type, param_id):
        self.param_type = param_type
        self.param_id = param_id

    def __str__(self):
        return str(self.param_type) + ' ' + str(self.param_id)


class Params:
    """
    Creates a list of ParamExprs that could be used for the parameters of a function definition
    int print_birthday (int day, int month, int year) {
    int day, int month, int year is the Params list
    """
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
    """
    Creates an instance of a declaration expression
    :param dec_type: the data type of the variable declaration
    :param dec_id: the identifier of the variable
    dec_type dec_id;
    int x;
    """
    def __init__(self, dec_type, dec_id):
        self.type = dec_type
        self.id = dec_id

    def __str__(self):
        return "\t" + str(self.type) + ' ' + str(self.id) + ';'


class Declarations:
    """
    Creates a list of DeclarationExprs that could be defined at the begging of a function
    int print_birthday (int day, int month, int year) {
        string name;
        int age;

    string name;
    int age; is the Params list
    """
    def __init__(self, dec_list: List[DeclarationExpr]):
        self.dec_list = dec_list

    def __str__(self):
        output = ''
        for dec in self.dec_list:
            output += str(dec) + '\n'
        return output


class FunctionDef:
    """
    Creates an instance of a function
    :param func_type: the data type of the return value of the function
    :param func_id: the identifier of the function
    :param params: the list of parameters passed to the function
    :param decls: the list of variable delarations at the beginning of the function
    :param stmts: the list of statements that get evaluated after the declarations
    func_type func_id (params) {
        decls
        stmts
    }
    int sum_3_or_5(int n) {
        // decls
        int sum;
        int i;
        // stmts
        sum = 0;
        i = 0;
        while (i < 4) {
            if (i % 3 == 0 || i % 5 == 0)
                sum = sum + i;
            else sum = 0;

            i = i + 1;
        }
        return sum;
    }
    """
    def __init__(self, func_type: str, func_id: IDExpr, params: Params, decls: Declarations, stmts: Statements):
        self.func_type = func_type
        self.func_id = func_id
        self.params = params
        self.decls = decls
        self.stmts = stmts

    def __str__(self):
        return self.func_type + ' ' + str(self.func_id) + "(" + str(self.params) + ") {\n" + str(self.decls) + \
               "\n" + str(self.stmts) + "\n}"

    def eval(self, env):
        # an environment maps identifiers to values
        # parameters or local variables
        # to evaluate a function you evaluate all of the statements
        # within the environment
        return self.stmts.eval(env)


class Program:
    """
    Creates an instance of a program that is made up of a list of function definitions
    """
    def __init__(self, funcs: Sequence[FunctionDef]):
        self.funcs = funcs

    def __str__(self):
        output = ''
        if self.funcs:
            output += str(self.funcs[0])
            for func in self.funcs[1:]:
                output += ('\n\n' + str(func))
        return output

    def eval(self, env):
        for func in self.funcs:
            func.eval(env)


class SLUCTypeError(Exception):
    """
    Creates an instance of a SLUC Type Error
    """
    def __init__(self, message: str):
        Exception.__init__(self)
        self.message = message

    def __str__(self):
        return self.message


if __name__ == '__main__':
    #binexpr = BinaryExpr(IntLitExpr('5'), IntLitExpr('3'),'+')
    #print(binexpr)

    pass
    #printstmt = PrintStmt([StringLitExpr("The Answer to 5+3 is "), binexpr]).eval()

