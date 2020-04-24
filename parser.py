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
            functions.append(FunctionDef(curr_func))
            curr_func = self.function_def()
        return Program(functions)

    def function_def(self):
        func_type = self.type()
        if self.curr_token[0][0] == Lexer.ID.id:
            tmp = self.curr_token
            # TODO check to make sure ID is declared (in the dictionary)
            func_id = IDExpr(tmp[1])
            self.next_token()
            if self.curr_token[1] == Lexer.LPAREN.value:
                self.next_token()
                func_params = self.params()
                print("Params " + str(func_params))
                if self.curr_token[1] == Lexer.LCBRAC.value:
                    self.next_token()
                    func_decls = self.declarations()
                    print("decls " + str(func_decls))
                    func_stmts = self.statements()
                    print("stmts " + str(func_stmts))
                    if self.curr_token[1] == Lexer.RCBRAC.value:
                        self.next_token()
                        return FunctionDef(func_type, func_id, func_params, func_decls, func_stmts)
        else:
            return False

    def params(self):
        params_list = []

        while self.curr_token[0][0] != Lexer.RPAREN.id:
            param_type = self.type()
            if self.curr_token[0][0] == Lexer.ID.id: # using ID in expression
                tmp = self.curr_token
                # TODO check to make sure ID is declared (in the dictionary)
                param_id = IDExpr(tmp[1])
                params_list.append(ParamExpr(param_type, param_id))
            self.next_token()
        self.next_token()
        return Params(params_list)

    def declarations(self):
        declaration_list = []
        curr_dec = self.declaration()
        while curr_dec:
            declaration_list.append(curr_dec)
            curr_dec = self.declaration()
        return Declarations(declaration_list)

    def declaration(self):
        dec_type = self.type()
        if dec_type:  # using ID in expression
            tmp = self.curr_token
            # TODO check to make sure ID is declared (in the dictionary)
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

    def statements(self):
        statement_list = []
        curr_statement = self.statement()
        while curr_statement:
            statement_list.append(curr_statement)
            curr_statement = self.statement()
        return Statements(statement_list)

    def statement(self):
        # get the id of the token
        current_token = self.curr_token[0][0]
        # Check through the possible values for statement
        # Statement → ; | Block | Assignment | IfStatement | WhileStatement | PrintStmt | ReturnStmt
        if current_token == Lexer.SEMICOLON.id:
            self.next_token()
            return ';'
        elif current_token == Lexer.LCBRAC.id:
            return self.block()
        elif current_token == Lexer.ID.id:
            return self.assignment()
        elif current_token == Lexer.IF.id:
            return self.if_stmt()
        elif current_token == Lexer.WHILE.id:
            return self.while_statement()
        elif current_token == Lexer.PRINT.id:
            return self.print_statement()
        elif self.curr_token[1] == 'return':
            return self.return_stmt()
        else:
            self.next_token()
            return False

    def return_stmt(self):
        self.next_token()
        curr_expr = self.expression()
        if self.curr_token[0][0] == Lexer.SEMICOLON.id:
            self.next_token()
            return curr_expr
        raise SLUCSyntaxError("Invalid return statement on line {0}".format(self.curr_token[2]))

    def block(self):
        self.next_token()
        block_content = self.statements()
        if self.curr_token[0][0] == Lexer.RCBRAC.id:
            self.next_token()
            return Block(block_content)
        else:
            raise SLUCSyntaxError("ERROR: Missing closing curly brace on line {0}".format(self.curr_token[2]))

    def assignment(self):
        curr_id = self.curr_token[1]
        self.next_token()
        if self.curr_token[0][0] == Lexer.EQ.id:
            self.next_token()
            return AssignStmt(IDExpr(curr_id), self.expression())

        raise SLUCSyntaxError("Invalid assignment statement on line {0}".format(self.curr_token[2]))

    def if_stmt(self):
        # if ( Expression ) Statement [ else Statement ]
        if self.curr_token[0][0] == Lexer.IF.id:
            self.next_token()
            if self.curr_token[0][0] == Lexer.LPAREN.id:
                self.next_token()
                condition = self.expression()
                if self.curr_token[0][0] == Lexer.RPAREN.id:
                    self.next_token()
                    true_part = self.statement()
                    false_part = None
                    while self.curr_token[0][0] == Lexer.ELSE.id:
                        self.next_token()
                        false_part = self.statement()
                    return IfStmt(condition, true_part, (false_part if false_part else None))
                else:
                    raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        raise SLUCSyntaxError("ERROR: Invalid if statement on line {0}".format(self.curr_token[2]))

    def while_statement(self):
        if self.curr_token[0][0] == Lexer.WHILE.id:
            self.next_token()
        if self.curr_token[0][0] == Lexer.LPAREN.id:
            self.next_token()
            self.expression()
            if self.curr_token[0][0] == Lexer.RPAREN.id:
                self.next_token()
                self.statements()
                # self.next_token() TODO: does this go here?
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.curr_token[2]))

    def print_statement(self):
        if self.curr_token[0][0] == Lexer.PRINT.id:
            self.next_token()
        if self.curr_token[0][0] == Lexer.LPAREN.id:
            self.next_token()
            self.print_arg()
            self.next_token()
            while self.curr_token[0][0] in {Lexer.COMMA.id}:
                self.next_token()
                self.print_arg()
            if self.curr_token[0][0] == Lexer.RPAREN.id:
                self.next_token()
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.curr_token[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.curr_token[2]))

    def print_arg(self):
        if self.curr_token[0][0] == Lexer.STRING.id:
            pass
            # return string_lit_expr(self.curr_token[1]) ????
        else:
            self.expression()
            self.next_token()

    def expression(self):
        left = self.conjunction()
        while self.curr_token[0][0] in {Lexer.LOR.id}:
            self.next_token()
            right = self.conjunction()
            left = BinaryExpr(left, right, "||")
        return left

    def conjunction(self):
        left = self.equality()
        while self.curr_token[0][0] in {Lexer.LAND.id}:
            self.next_token()
            right = self.equality()
            left = BinaryExpr(left, right, "&&")
        return left

    def equality(self):  # a == b      3*z != 99
        left = self.relation()
        while self.curr_token[0][0] in {Lexer.EQUAL.id, Lexer.NEQUAL.id}:
            op = self.curr_token[0][0]
            self.next_token()
            right = self.relation()
            left = BinaryExpr(left, right, op)
        return left

    def relation(self):  # a < b
        left = self.addition()
        while self.curr_token[0][0] in {Lexer.LT.id, Lexer.LEQ.id, Lexer.GT.id, Lexer.GEQ}:
            op = self.curr_token[0][0]
            self.next_token()
            right = self.addition()
            left = BinaryExpr(left, right, op)
        return left

    def addition(self) -> Expr:
        """
        Term  →  Term { + Term }
        """

        left = self.term()

        while self.curr_token[0][0] in { Lexer.PLUS.id, Lexer.MINUS.id }:
            self.next_token()  # advance to the next token
            right = self.term()
            left = BinaryExpr(left, right, '+')

        return left

    def term(self) -> Expr:
        left = self.fact()

        while self.curr_token[0][0] in {Lexer.MULT.id, Lexer.DIV.id, Lexer.MOD.id}:
            operation = self.curr_token[1]
            self.next_token()
            right = self.fact()
            left = BinaryExpr(left, right, operation)

        return left

    def fact(self) -> Expr:
        # only advance to the next token on a successful match.
        if self.curr_token[0][0] == Lexer.MINUS.id:
            self.next_token()
            tree = self.primary()
            return UnaryMinus(tree)

        return self.primary()

    def primary(self) -> Expr:
        # parse an ID
        if self.curr_token[0][0] == Lexer.ID.id:
            tmp = self.curr_token
            self.next_token()
            return IDExpr(tmp[1])

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
