import sys

from lexer import Lexer
from ast import *


class Parser:

    def __init__(self, fn: str):

        self.lex = Lexer(fn)
        self.tg = self.lex.token_generator()
        self.curr_token = next(self.tg)
        self.func_ids = {}

    def next_token(self):
        self.curr_token = next(self.tg)

    # top-level function that will be called
    def program(self):
        """
        Program → { FunctionDef }
        """
        functions = {}
        curr_func = self.function_def()
        while curr_func:
            functions[curr_func.func_id] = curr_func
            curr_func = self.function_def()
        return Program(functions).eval()

    def function_def(self):
        """
        FunctionDef → Type id ( Params ) { Declarations Statements }
        """
        func_type = self.type()
        if self.curr_token[0][0] == Lexer.ID.id or self.curr_token[0][0] == Lexer.MAIN.id:
            tmp = self.curr_token
            func_id_val = tmp[1]
            var_dict = {x: self.func_ids[x] for x in self.func_ids}
            if func_id_val in var_dict:
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
                        func = FunctionDef(func_type, func_id, func_params, func_decls, func_stmts, var_dict)
                        self.func_ids[func_id_val] = func
                        return func
                    else:
                        raise SLUCSyntaxError("Missing closing curly bracket on line {0}".format(self.curr_token[2]))
                else:
                    raise SLUCSyntaxError("Missing opening curly bracket on line {0}".format(self.curr_token[2]))
        else:
            return False

    def function_call(self):
        if self.curr_token[1] in self.func_ids:
            func_id = self.curr_token[1]
            self.next_token()
            if self.curr_token[0][0] == Lexer.LPAREN.id:
                arguments = []
                type_dict = {
                    Lexer.BOOL.id: BoolExpr,
                    Lexer.INT.id: IntLitExpr,
                    Lexer.FLOAT.id: FloatLitExpr,
                    Lexer.ID.id: IDExpr
                }
                self.next_token()
                while self.curr_token[0][0] != Lexer.RPAREN.id:
                    if self.curr_token[0][0] == Lexer.COMMA.id:
                        self.next_token()
                    else:
                        if self.curr_token[1] in self.func_ids:
                            func_call = self.function_call()
                            arguments.append(func_call)
                        else:
                            tmp_tok = self.curr_token
                            self.next_token()

                            # check for BinaryExpr
                            if self.curr_token[1] in op_dict():
                                op = self.curr_token
                                self.next_token()
                                right = self.curr_token
                                arguments.append(
                                    BinaryExpr(
                                        type_dict[tmp_tok[0][0]](tmp_tok[1]),
                                        type_dict[right[0][0]](right[1]),
                                        op[1]
                                    )
                                )
                            # argument is not a binary expr
                            else:
                                arguments.append(type_dict[tmp_tok[0][0]](tmp_tok[1]))
                self.next_token()
                val = FunctionCallExpr(IDExpr(func_id), arguments, self.func_ids)
                return val
        else:
            raise SLUCSyntaxError("The function {0} called on line {1} does not exist".format(
                self.curr_token[1], self.curr_token[2]))

    def params(self, var_dict):
        """
        Params → Type id { , Type id } | ε
        """
        params_list = []

        while self.curr_token[0][0] != Lexer.RPAREN.id:
            param_type = self.type()
            if self.curr_token[0][0] == Lexer.ID.id:  # using ID in expression
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
        """
        Declarations → { Declaration }
        """
        declaration_list = []
        curr_dec = self.declaration(var_dict)
        while curr_dec:
            declaration_list.append(curr_dec)
            curr_dec = self.declaration(var_dict)
        return Declarations(declaration_list)

    def declaration(self, var_dict):
        """
        Declaration → Type Identifier ;
        """
        dec_type = self.type()
        if dec_type:  # using ID in expression
            tmp = self.curr_token
            func_id = tmp[1]
            if func_id in var_dict:
                raise SLUCSyntaxError("ERROR: Variable '{0}' declared on line {1} has already been declared".format(
                    func_id, self.curr_token[2]))
            else:
                if tmp[0][0] == Lexer.ID.id:
                    var_dict[func_id] = None
                    self.next_token()
                    id = IDExpr(tmp[1])
                    if self.curr_token[1] == Lexer.SEMICOLON.value:
                        self.next_token()
                        return DeclarationExpr(dec_type, id)
                else:
                    raise SLUCSyntaxError("ERROR: Variable '{0}' declared on line {1} is not a valid identifier.".format(func_id, self.curr_token[2]))
        return False

    def type(self):
        """
        Type → int | bool | float
        """
        # parse declaration
        if self.curr_token[0][0] in {Lexer.INTK.id, Lexer.BOOL.id, Lexer.FLOAT.id}:
            tmp = self.curr_token
            self.next_token()
            return tmp[1]
        else:
            return False

    def statements(self, var_dict):
        """
        Statements → { Statement }
        """
        statement_list = []
        curr_statement = self.statement(var_dict)
        while curr_statement:
            if self.curr_token[1] == Lexer.SEMICOLON.value:
                self.next_token()
            statement_list.append(curr_statement)
            curr_statement = self.statement(var_dict)
        return Statements(statement_list)

    def statement(self, var_dict):
        """
        Statement → ; | Block | Assignment | IfStatement | WhileStatement | PrintStmt | ReturnStmt
        """
        # get the id of the token
        current_token = self.curr_token[0][0]
        value = self.curr_token[1]
        # Check through the possible values for statement
        state_dict = {
            Lexer.LCBRAC.id: self.block,
            Lexer.ID.id: self.assignment,
            Lexer.IF.id: self.if_stmt,
            Lexer.WHILE.id: self.while_statement,
            Lexer.PRINT.id: self.print_statement,
            Lexer.RET.id: self.return_stmt
        }

        if current_token == Lexer.SEMICOLON.id:
            self.next_token()
            return ';'
        if current_token in state_dict.keys():
            return state_dict[current_token](var_dict)
        else:
            return False
        
    def return_stmt(self, var_dict):
        """
        ReturnStmt → return Expression ;
        """
        self.next_token()
        curr_expr = self.expression(var_dict)
        if self.curr_token[0][0] == Lexer.SEMICOLON.id:
            self.next_token()
            return ReturnStmt(curr_expr)
        raise SLUCSyntaxError("ERROR: Invalid return statement on line {0}".format(self.curr_token[2]))

    def block(self, var_dict):
        """
        Block → { Statements }
        """
        self.next_token()
        block_content = self.statements(var_dict)
        if self.curr_token[0][0] == Lexer.RCBRAC.id:
            self.next_token()
            return Block(block_content)
        else:
            raise SLUCSyntaxError("ERROR: Missing closing curly bracket on line {0}".format(self.curr_token[2]))

    def assignment(self, var_dict):
        """
        Assignment → id = Expression ;
        """
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
        """
        IfStatement → if ( Expression ) Statement [ else Statement ]
        """
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
                    raise SLUCSyntaxError("ERROR: Missing right paren in if statement on line {0}".format(self.curr_token[2]))
        raise SLUCSyntaxError("ERROR: Missing left paren in if statement on line {0}".format(self.curr_token[2]))

    def while_statement(self, var_dict):
        """
        WhileStatement → while ( Expression ) Statement
        """
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
        """
        PrintStmt → print( PrintArg { , PrintArg })
        """
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
        """
        PrintArg → Expression | stringlit | FunctionCall
        """
        if self.curr_token[0][0] == Lexer.STRING.id:
            tmp = self.curr_token[1]
            self.next_token()
            return StringLitExpr(tmp)
            # return string_lit_expr(self.curr_token[1]) ????
        else:
            return self.expression(var_dict)

    def expression(self, var_dict):
        """
        Expression → Conjunction { || Conjunction }
        """
        left = self.conjunction(var_dict)
        while self.curr_token[0][0] in {Lexer.LOR.id}:
            self.next_token()
            right = self.conjunction(var_dict)
            left = BinaryExpr(left, right, "||")
        return left

    def conjunction(self, var_dict):
        """
        Conjunction → Equality { && Equality }
        """
        left = self.equality(var_dict)
        while self.curr_token[0][0] in {Lexer.LAND.id}:
            self.next_token()
            right = self.equality(var_dict)
            left = BinaryExpr(left, right, "&&")
        return left

    def equality(self, var_dict):
        """
        Equality → Relation [ EquOp Relation ]
        """
        left = self.relation(var_dict)
        while self.curr_token[0][0] in {Lexer.EQUAL.id, Lexer.NEQUAL.id}:
            op = self.curr_token[1]
            self.next_token()
            right = self.relation(var_dict)
            left = BinaryExpr(left, right, op)
        return left

    def relation(self, var_dict):
        """
        Relation → Addition [ RelOp Addition ]
        """
        left = self.addition(var_dict)
        if self.curr_token[0][0] in {Lexer.LT.id, Lexer.LEQ.id, Lexer.GT.id, Lexer.GEQ}:
            op = self.curr_token[1]
            self.next_token()
            right = self.addition(var_dict)
            left = BinaryExpr(left, right, op)
        return left

    def addition(self, var_dict) -> Expr:
        """
        Addition  →  Term { + Term }
        """
        left = self.term(var_dict)

        while self.curr_token[0][0] in { Lexer.PLUS.id, Lexer.MINUS.id }:
            operation = self.curr_token[1]
            self.next_token()  # advance to the next token
            right = self.term(var_dict)
            left = BinaryExpr(left, right, operation)

        return left

    def term(self, var_dict) -> Expr:
        """
        Term → Factor { MulOp Factor }
        """
        left = self.fact(var_dict)

        while self.curr_token[0][0] in {Lexer.MULT.id, Lexer.DIV.id, Lexer.MOD.id}:
            operation = self.curr_token[1]
            self.next_token()
            right = self.fact(var_dict)
            left = BinaryExpr(left, right, operation)

        return left

    def fact(self, var_dict) -> Expr:
        """
        Factor → [ UnaryOp ] Primary
        """
        # only advance to the next token on a successful match.
        if self.curr_token[0][0] == Lexer.MINUS.id:
            self.next_token()
            tree = self.primary(var_dict)
            return UnaryMinus(tree)

        return self.primary(var_dict)

    def primary(self, var_dict) -> Expr:
        """
        Primary → id | intlit | floatlit | true | false | ( Expression ) | FunctionCall
        """
        # parse an ID
        if self.curr_token[0][0] == Lexer.ID.id:
            tmp = self.curr_token
            if tmp[1] in var_dict and tmp[1] not in self.func_ids:
                self.next_token()
                return IDExpr(tmp[1])
            elif tmp[1] in self.func_ids:
                return self.function_call()
            else:
                raise SLUCSyntaxError("ERROR: Variable '{0}' used on line {1} is not defined".format(
                    tmp[1], tmp[2]))

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
            tree = self.expression(var_dict)
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
    try:
        p = Parser("newtest.c")
        t = p.program()

    except SLUCSyntaxError as e:
        print(e)



