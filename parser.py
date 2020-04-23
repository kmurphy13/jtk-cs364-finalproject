from lexer import Lexer

from ast import Expr, UnaryMinus, IDExpr, IntLitExpr, BinaryExpr


"""
  Program         →  { FunctionDef }

  FunctionDef     →  Type id ( Params ) { Declarations Statements }

  Params          →  Type id { , Type id } | ε

  Declarations    →  { Declaration }

  Declaration     →  Type  id  ;

  Type            →  int | bool | float

  Statements      →  { Statement }

  Statement       →  ; | Block | Assignment | IfStatement |     
                     WhileStatement |  PrintStmt | ReturnStmt

  ReturnStmt      →  return Expression ;
  Block           →  { Statements }

  Assignment      →  id = Expression ;

  IfStatement     →  if ( Expression ) Statement [ else Statement ]
 
  WhileStatement  →  while ( Expression ) Statement  

  PrintStmt       →  print(PrintArg { , PrintArg })

  PrintArg        →  Expression | stringlit

  Expression      →  Conjunction { || Conjunction }

  Conjunction     →  Equality { && Equality }
 
  Equality        →  Relation [ EquOp Relation ]

  Relation        →  Addition [ RelOp Addition ]

  Addition        →  Term { AddOp Term }
  Term            →  Factor { MulOp Factor }
  Factor          →  [ UnaryOp ] Primary
  UnaryOp         →  - | !
  Primary         →  id | intlit | floatlit | ( Expression )
  RelOp           →  < | <= | > | >=   
  AddOp           →  + | -  
  MulOp           →  * | / | %  
  EquOp           →  == | != 
"""

class Parser:

    def __init__(self, fn: str):

        self.lex = Lexer(fn)
        self.tg = self.lex.token_generator()
        self.next_token()

    """
        Expr  →  Term { (+ | -) Term }
        Term  → Fact { (* | / | %) Fact }
        Fact  → [ - ] Primary
        Primary  → ID | INTLIT | ( Expr )
        
        Recursive descent parser. Each non-terminal corresponds 
        to a function.
        
        -7  -(7 * 5)  -b   unary minus
    """

    def next_token(self):
        self.currtok = next(self.tg)

    # top-level function that will be called
    def program(self):
        curr_func = self.function_def()
        while curr_func:
            self.function_def()

    def function_def(self):
        self.type()
        if self.currtok[1] == Lexer.ID.value:
            tmp = self.currtok
            # TODO check to make sure ID is declared (in the dictionary)
            self.next_token()
            IDExpr(tmp[1])
            if self.currtok[1] == Lexer.LPAREN.value:
                self.params()
                self.next_token()
                if self.currtok[1] == Lexer.RPAREN.value:
                    self.next_token()
                    if self.currtok[1] == Lexer.RCBRAC.value:
                        self.declarations()
                        self.statements()
                        self.next_token()
                        if self.currtok[1] == Lexer.LCBRAC.value:
                            self.next_token()
        else:
            return False
    def params(self):
        self.type()
        if self.currtok[1] == Lexer.value:  # using ID in expression
            tmp = self.currtok
            # TODO check to make sure ID is declared (in the dictionary)
            self.next_token()
            id = IDExpr(tmp[1])

    def declarations(self):
        curr_type = self.type()
        while curr_type:
            self.statement()
    def declaration(self):
        self.type()
        if self.currtok[1] == Lexer.value:  # using ID in expression
            tmp = self.currtok
            # TODO check to make sure ID is declared (in the dictionary)
            self.next_token()
            id = IDExpr(tmp[1])
            if self.currtok[1] == Lexer.SEMICOLON.value:
                self.next_token()
    def type(self):
        # parse int declaration
        if self.currtok[0][0] == Lexer.INTK.id:  # using ID in expression
            tmp = self.currtok
            self.next_token()
            return tmp[1]
        # parse bool declaration
        if self.currtok[0][0] == Lexer.BOOL.id:
            tmp = self.currtok
            self.next_token()
            return tmp[1]

            # parse float declaration
        if self.currtok[0][0] == Lexer.FLOAT.id:
            tmp = self.currtok
            self.next_token()
            return tmp[1]
        else:
            return False

    def statements(self):
        while self.currtok[0][0] == Lexer.RCBRAC.id:
            self.statement()

    def statement(self):
        # get the id of the token
        current_token = self.currtok[0][0]
        # Check through the possible values for statement
        # Statement → ; | Block | Assignment | IfStatement | WhileStatement | PrintStmt | ReturnStmt
        if current_token == Lexer.SEMICOLON.id:
            return Lexer.SEMICOLON.value
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
        elif self.currtok[1] == 'return':
            return self.return_stmt()
        else:
            raise SLUCSyntaxError("Invalid statement on line {0}".format(self.currtok[2]))

    def return_stmt(self):
        self.next_token()
        curr_expr = self.expression()
        if self.currtok[1] == Lexer.SEMICOLON.value:
            self.next_token()
            return 'return ' + curr_expr + ';'
        raise SLUCSyntaxError("Invalid return statement on line {0}".format(self.currtok[2]))

    def block(self):
        self.next_token()
        curr_stmts = self.statements()
        if self.currtok[1] == Lexer.RCBRAC.value:
            self.next_token()
            return curr_stmts

        raise SLUCSyntaxError("ERROR: Missing closing curly brace on line {0}".format(self.currtok[2]))

    def assignment(self):
        self.next_token()
        if self.currtok[0][0] == Lexer.EQ.id:
            self.next_token()
            return self.expression()

        raise SLUCSyntaxError("Invalid assignment statement on line {0}".format(self.currtok[2]))

    def if_stmt(self):
        # if ( Expression ) Statement [ else Statement ]
        if self.currtok[0][0] == Lexer.IF.id:
            self.next_token()
            if self.currtok[0][0] == Lexer.LPAREN.id:
                self.next_token()
                self.expression()
                if self.currtok[0][0] == Lexer.RPAREN.id:
                    self.next_token()
                    self.statement()
                    while self.currtok[0][0] == Lexer.ELSE.id:
                        self.next_token()
                        self.statement()
                else:
                    raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.currtok[2]))
        raise SLUCSyntaxError("ERROR: Invalid if statement on line {0}".format(self.currtok[2]))


    def while_statement(self):
        if self.currtok[0][0] == Lexer.WHILE.id:
            self.next_token()
        if self.currtok[0][0] == Lexer.LPAREN.id:
            self.next_token()
            self.expression()
            if self.currtok[0][0] == Lexer.RPAREN.id:
                self.next_token()
                self.statements()
                # self.next_token() TODO: does this go here?
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.currtok[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.currtok[2]))

    def print_statement(self):
        if self.currtok[0][0] == Lexer.PRINT.id:
            self.next_token()
        if self.currtok[0][0] == Lexer.LPAREN.id:
            self.next_token()
            self.print_arg()
            self.next_token()
            while self.currtok[0][0] in { Lexer.COMMA.id}:
                self.next_token()
                self.print_arg()
            if self.currtok[0][0] == Lexer.RPAREN.id:
                self.next_token()
            else:
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(self.currtok[2]))
        else:
            raise SLUCSyntaxError("ERROR: Missing left paren on line {0}".format(self.currtok[2]))

    def print_arg(self):
        if self.currtok[0][0] == Lexer.STRING.id:
            pass
            # return string_lit_expr(self.currtok[1]) ????
        else:
            self.expression()
            self.next_token()

    def expression(self):
        left = self.conjunction()
        while self.currtok[0][0] in {Lexer.LOR.id}:
            self.next_token()
            right = self.conjunction()
            left = BinaryExpr(left, right, "||")
        return left

    def conjunction(self):
        left = self.equality()
        while self.currtok[0][0] in {Lexer.LAND.id}:
            self.next_token()
            right = self.equality()
            left = BinaryExpr(left, right, "&&")
        return left

    def equality(self):  # a == b      3*z != 99
        left = self.relation()
        while self.currtok[0][0] in {Lexer.EQUAL.id, Lexer.NEQUAL.id}:
            op = self.currtok[0][0]
            self.next_token()
            right = self.relation()
            left = BinaryExpr(left, right, op)
        return left

    def relation(self):  # a < b
        left = self.addition()
        while self.currtok[0][0] in {Lexer.LT.id, Lexer.LEQ.id, Lexer.GT.id, Lexer.GEQ}:
            op = self.currtok[0][0]
            self.next_token()
            right = self.addition()
            left = BinaryExpr(left, right, op)
        return left

    def addition(self) -> Expr:
        """
        Term  →  Term { + Term }
        """

        left = self.term()

        while self.currtok[0][0] in { Lexer.PLUS.id, Lexer.MINUS.id }:
            self.next_token()  # advance to the next token
                                          # because we matched a +
            right = self.term()
            left = BinaryExpr(left,right, '+')

        return left

    def term(self) -> Expr:
        """
        Term  → Fact { * Fact }
        """
        left = self.fact()

        while self.currtok[0][0] in { Lexer.MULT.id, Lexer.DIVIDE.id }:
            self.next_token()
            right = self.fact()
            left = BinaryExpr(left, right, '*')

        return left

    def fact(self) -> Expr:
        """
        Fact  → [ - ] Primary
            e.g., -a  -(b+c)  -6    (b+c) a 6
        """

        # only advance to the next token on a successful match.
        if self.currtok[0][0] == Lexer.MINUS.id:
            self.next_token()
            tree = self.primary()
            return UnaryMinus(tree)

        return self.primary()

    def primary(self) -> Expr:
        """
        Primary  → ID | INTLIT | ( Expr )
        """

        # TODO Add real literals

        # parse an ID

        # parse an integer literal
        if self.currtok[0][0] == Lexer.INT.id:
            tmp = self.currtok
            self.next_token()
            return IntLitExpr(tmp[1])

        # parse a parenthesized expression
        if self.currtok[0][0] == Lexer.LPAREN.id:
            self.next_token()
            tree = self.addition() # TODO Keeps changing!
            if self.currtok[0][0] == Lexer.RPAREN.id:
                self.next_token()
                return tree
            else:
                # use the line number from your token object
                raise SLUCSyntaxError("ERROR: Missing right paren on line {0}".format(-1))

        # what if we get here we have a problem
        raise SLUCSyntaxError("ERROR: Unexpected token {0} on line {1}".format(self.currtok[1], -1))



# create our own exception by inheriting
# from Python's exception
class SLUCSyntaxError(Exception):
    def __init__(self, message: str):
        Exception.__init__(self)
        self.message = message

    def __str__(self):
        return self.message

if __name__ == '__main__':

    p = Parser('simple.c')
    t = p.addition()
    print(t)
