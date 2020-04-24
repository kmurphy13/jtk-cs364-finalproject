from lexer import Lexer
from ast import *


class Parser:

    def __init__(self, fn: str):

        self.lex = Lexer(fn)
        self.tg = self.lex.token_generator()
        self.curr_token = next(self.tg)

    def next_token(self):
        self.curr_token = next(self.tg)

    # top-level function that will be called
    def program(self):
        functions = []
        curr_func = self.function_def()
        while curr_func:
            functions.append(curr_func)
            curr_func = self.function_def()
        return Program(functions)

    def function_def(self):
        var_dict = {}

        func_type = self.type()
        if self.curr_token[0][0] == Lexer.ID.id or self.curr_token[0][0] == Lexer.MAIN.id:
            tmp = self.curr_token
            func_id = tmp[1]
            if func_id in var_dict:
                raise SLUCSyntaxError("Variable declared line {0} already exists".format(self.curr_token[2]))
            func_id = IDExpr(tmp[1])
            self.next_token()
            if self.curr_token[1] == Lexer.LPAREN.value:
                self.next_token()
                func_params = self.params(var_dict)
                if self.curr_token[1] == Lexer.LCBRAC.value:
                    self.next_token()
                    func_decls = self.declarations(var_dict)
                    func_stmts = self.statements(var_dict)
                    if self.curr_token[1] == Lexer.RCBRAC.value:
                        self.next_token()
                        return FunctionDef(func_type, func_id, func_params, func_decls, func_stmts)
        else:
            return False

    def params(self, var_dict):
        params_list = []

        while self.curr_token[0][0] != Lexer.RPAREN.id:
            param_type = self.type()
            if self.curr_token[0][0] == Lexer.ID.id: # using ID in expression
                tmp = self.curr_token
                func_id = tmp[1]
                if func_id in var_dict:
                    raise SLUCSyntaxError("Variable '{1}' declared line {0} already exists".format(
                        func_id, self.curr_token[2]))
                else:
                    var_dict[func_id] = None
                    param_id = IDExpr(tmp[1])
                    params_list.append(ParamExpr(param_type, param_id))
            self.next_token()
        self.next_token()
        return Params(params_list)

    def declarations(self, var_dict):
        declaration_list = []
        curr_dec = self.declaration(var_dict)
        while curr_dec:
            declaration_list.append(curr_dec)
            curr_dec = self.declaration(var_dict)
        return Declarations(declaration_list)

    def declaration(self, var_dict):
        dec_type = self.type()
        if dec_type:  # using ID in expression
            tmp = self.curr_token
            func_id = tmp[1]
            if func_id in var_dict:
                raise SLUCSyntaxError("ERROR: Variable '{0}' declared on line {1} has already been declared".format(
                    func_id, self.curr_token[2]))
            else:
                var_dict[func_id] = None
                self.next_token()
                id = IDExpr(tmp[1])
                if self.curr_token[1] == Lexer.SEMICOLON.value:
                    self.next_token()
                    return DeclarationExpr(dec_type, id)

        return False

    def type(self):
        # parse int declaration
        if self.curr_token[0][0] == Lexer.INTK.id:
            tmp = self.curr_token
            self.next_token()
            return tmp[1]
        # parse bool declaration
        if self.curr_token[0][0] == Lexer.BOOL.id:
            tmp = self.curr_token
            self.next_token()
            return tmp[1]

            # parse float declaration
        if self.curr_token[0][0] == Lexer.FLOAT.id:
            tmp = self.curr_token
            self.next_token()
            return tmp[1]
        else:
            return False

    def statements(self, var_dict):
        statement_list = []
        curr_statement = self.statement(var_dict)
        while curr_statement:
            if self.curr_token[1] == Lexer.SEMICOLON.value:
                self.next_token()
            statement_list.append(curr_statement)
            curr_statement = self.statement(var_dict)
        return Statements(statement_list)

    def statement(self, var_dict):
        # get the id of the token
        current_token = self.curr_token[0][0]
        value = self.curr_token[1]
        # Check through the possible values for statement
        # Statement → ; | Block | Assignment | IfStatement | WhileStatement | PrintStmt | ReturnStmt

        if current_token == Lexer.SEMICOLON.id:
            print('semicolon called')
            self.next_token()
            return ';'
        if current_token == Lexer.LCBRAC.id:
            return self.block(var_dict)
        elif current_token == Lexer.ID.id:
            return self.assignment(var_dict)
        elif current_token == Lexer.IF.id:
            return self.if_stmt(var_dict)
        elif current_token == Lexer.WHILE.id:
            return self.while_statement(var_dict)
        elif current_token == Lexer.PRINT.id:
            return self.print_statement(var_dict)
        elif current_token == Lexer.RET.id:
            return self.return_stmt(var_dict)
        else:
            return False

    def return_stmt(self, var_dict):
        self.next_token()
        curr_expr = self.expression(var_dict)
        if self.curr_token[0][0] == Lexer.SEMICOLON.id:
            self.next_token()
            return ReturnStmt(curr_expr)
        raise SLUCSyntaxError("ERROR: Invalid return statement on line {0}".format(self.curr_token[2]))

    def block(self, var_dict):
        self.next_token()
        block_content = self.statements(var_dict)
        if self.curr_token[0][0] == Lexer.RCBRAC.id:
            self.next_token()
            return Block(block_content)
        else:
            raise SLUCSyntaxError("ERROR: Missing closing curly brace on line {0}".format(self.curr_token[2]))

    def assignment(self, var_dict):
        curr_id = self.curr_token[1]
        if curr_id not in var_dict:
            raise SLUCSyntaxError("ERROR: Variable '{0}' used on line {1} is not defined".format(
                curr_id, self.curr_token[2]))
        else:
            self.next_token()
            if self.curr_token[0][0] == Lexer.EQ.id:
                self.next_token()
                curr_expr = self.expression(var_dict)
                var_dict[curr_id] = curr_expr
                return AssignStmt(IDExpr(curr_id), curr_expr)

        raise SLUCSyntaxError("Invalid assignment statement on line {0}".format(self.curr_token[2]))

    def if_stmt(self, var_dict):
        # if ( Expression ) Statement [ else Statement ]
        if self.curr_token[0][0] == Lexer.IF.id:
            self.next_token()
            if self.curr_token[0][0] == Lexer.LPAREN.id:
                self.next_token()
                condition = self.expression(var_dict)
                if self.curr_token[0][0] == Lexer.RPAREN.id:
                    self.next_token()
                    true_part = self.statement(var_dict)
                    false_part = None
                    self.next_token()
                    while self.curr_token[0][0] == Lexer.ELSE.id:
                        self.next_token()
                        false_part = self.statement(var_dict)
                    return IfStmt(condition, true_part, (false_part if false_part else None))
                else:
                    raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        raise SLUCSyntaxError("ERROR: Invalid if statement on line {0}".format(self.curr_token[2]))

    def while_statement(self, var_dict):
        if self.curr_token[0][0] == Lexer.WHILE.id:
            self.next_token()
        if self.curr_token[0][0] == Lexer.LPAREN.id:
            self.next_token()
            while_expr = self.expression(var_dict)
            if self.curr_token[0][0] == Lexer.RPAREN.id:
                self.next_token()
                while_statement = self.statement(var_dict)
                return WhileStmt(while_expr, while_statement)
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.curr_token[2]))

    def print_statement(self, var_dict):
        if self.curr_token[0][0] == Lexer.PRINT.id:
            self.next_token()
        if self.curr_token[0][0] == Lexer.LPAREN.id:
            self.next_token()
            print_arguments = []
            print_arguments.append(self.print_arg(var_dict))
            while self.curr_token[0][0] in {Lexer.COMMA.id}:
                self.next_token()
                print_arguments.append(self.print_arg(var_dict))
            if self.curr_token[0][0] == Lexer.RPAREN.id:
                self.next_token()
                return PrintStmt(print_arguments)
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.curr_token[2]))

    def print_arg(self, var_dict):
        if self.curr_token[0][0] == Lexer.STRING.id:
            tmp = self.curr_token[1]
            self.next_token()
            return StringLitExpr(tmp)
            # return string_lit_expr(self.curr_token[1]) ????
        else:
            return self.expression(var_dict)

    def expression(self, var_dict):
        left = self.conjunction(var_dict)
        while self.curr_token[0][0] in {Lexer.LOR.id}:
            self.next_token()
            right = self.conjunction(var_dict)
            left = BinaryExpr(left, right, "||")
        return left

    def conjunction(self, var_dict):
        left = self.equality(var_dict)
        while self.curr_token[0][0] in {Lexer.LAND.id}:
            self.next_token()
            right = self.equality(var_dict)
            left = BinaryExpr(left, right, "&&")
        return left

    def equality(self, var_dict):  # a == b      3*z != 99
        left = self.relation(var_dict)
        while self.curr_token[0][0] in {Lexer.EQUAL.id, Lexer.NEQUAL.id}:
            op = self.curr_token[1]
            self.next_token()
            right = self.relation(var_dict)
            left = BinaryExpr(left, right, op)
        return left

    def relation(self, var_dict):  # a < b
        left = self.addition(var_dict)
        while self.curr_token[0][0] in {Lexer.LT.id, Lexer.LEQ.id, Lexer.GT.id, Lexer.GEQ}:
            op = self.curr_token[1]
            self.next_token()
            right = self.addition(var_dict)
            left = BinaryExpr(left, right, op)
        return left

    def addition(self, var_dict) -> Expr:
        """
        Term  →  Term { + Term }
        """

        left = self.term(var_dict)

        while self.curr_token[0][0] in { Lexer.PLUS.id, Lexer.MINUS.id }:
            operation = self.curr_token[1]
            self.next_token()  # advance to the next token
            right = self.term(var_dict)
            left = BinaryExpr(left, right, operation)

        return left

    def term(self, var_dict) -> Expr:
        left = self.fact(var_dict)

        while self.curr_token[0][0] in {Lexer.MULT.id, Lexer.DIV.id, Lexer.MOD.id}:
            operation = self.curr_token[1]
            self.next_token()
            right = self.fact(var_dict)
            left = BinaryExpr(left, right, operation)

        return left

    def fact(self, var_dict) -> Expr:
        # only advance to the next token on a successful match.
        if self.curr_token[0][0] == Lexer.MINUS.id:
            self.next_token()
            tree = self.primary(var_dict)
            return UnaryMinus(tree)

        return self.primary(var_dict)

    def primary(self, var_dict) -> Expr:
        # parse an ID
        if self.curr_token[0][0] == Lexer.ID.id:
            tmp = self.curr_token
            self.next_token()
            if tmp[1] in var_dict:
                return IDExpr(tmp[1])
            else:
                raise SLUCSyntaxError("ERROR: Variable '{0}' used on line {1} is not defined".format(
                    tmp[1], self.curr_token[2]))

        # parse an integer literal
        if self.curr_token[0][0] == Lexer.INT.id:
            tmp = self.curr_token
            self.next_token()
            return IntLitExpr(tmp[1])

        # parse an real
        if self.curr_token[0][0] == Lexer.REAL.id:
            tmp = self.curr_token
            self.next_token()
            return FloatLitExpr(tmp[1])

        # parse booleans
        if self.curr_token[0][0] in {Lexer.TRUE.id, Lexer.FALSE.id}:
            tmp = self.curr_token
            self.next_token()
            return BoolExpr(tmp[1])

        # parse a parenthesized expression
        if self.curr_token[0][0] == Lexer.LPAREN.id:
            self.next_token()
            tree = self.expression()
            if self.curr_token[0][0] == Lexer.RPAREN.id:
                self.next_token()
                return tree
            else:
                # use the line number from your token object
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(-1))

        # what if we get here we have a problem
        raise SLUCSyntaxError("ERROR: Unexpected token {0} on line {1}".format(self.curr_token[1], self.curr_token[2]))


# from Python's exception
class SLUCSyntaxError(Exception):
    def __init__(self, message: str):
        Exception.__init__(self)
        self.message = message

    def __str__(self):
        return self.message


if __name__ == '__main__':
    p = Parser('simple.c')
    t = p.program()
    print(t)
