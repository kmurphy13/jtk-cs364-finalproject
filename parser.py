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
        self.currtok = next(self.tg)

    """
        Expr  →  Term { (+ | -) Term }
        Term  → Fact { (* | / | %) Fact }
        Fact  → [ - ] Primary
        Primary  → ID | INTLIT | ( Expr )
        
        Recursive descent parser. Each non-terminal corresponds 
        to a function.
        
        -7  -(7 * 5)  -b   unary minus
    """

    # top-level function that will be called
    def program(self):
        """
            Program         →  { FunctionDef }
        """
        self.functionDef()

    def functionDef(self):
        self.type()
        if self.currtok[1] == Lexer.ID:  # using ID in expression
            tmp = self.currtok
            # TODO check to make sure ID is declared (in the dictionary)
            self.currtok = next(self.tg)
            IDExpr(tmp[1])
            if self.currtok[1] == Lexer.LPAREN:
                self.params()
                self.currtok = next(self.tg)
                if self.currtok[1] == Lexer.RPAREN:
                    self.currtok = next(self.tg)
                    if self.currtok[1] == Lexer.RCBRAC:
                        self.declarations()
                        self.statements()
                        self.currtok = next(self.tg)
                        if self.currtok[1] == Lexer.LCBRAC:
                            self.currtok = next(self.tg)


    def params(self):
        self.type()
        if self.currtok[1] == Lexer.ID:  # using ID in expression
            tmp = self.currtok
            # TODO check to make sure ID is declared (in the dictionary)
            self.currtok = next(self.tg)
            id = IDExpr(tmp[1])
            if self.currtok[1] == Lexer.COMMA:



    def declarations(self):
        pass
    def declaration(self):
        pass
    def type(self):
        # parse int declaration
        if self.currtok[1] == Lexer.INTK:  # using ID in expression
            tmp = self.currtok
            self.currtok = next(self.tg)
            return tmp[1]

        # parse bool declaration
        if self.currtok[1] == Lexer.BOOL:
            tmp = self.currtok
            self.currtok = next(self.tg)
            return tmp[1]

            # parse float declaration
        if self.currtok[1] == Lexer.FLOAT:
            tmp = self.currtok
            self.currtok = next(self.tg)
            return tmp[1]

    def statements(self):
        
    def statment(self):
        pass

    def return_stmt(self):
        pass

    def block(self):
        pass

    def assignment(self):
        pass

    def if_stmt(self):
        pass
    def while_statement(self):
        pass

    def print_statement(self):
        pass

    def print_arg(self):
        pass

    def expression(self):
        pass

    def conjunction(self):
        pass

    def conjuction(self):
        pass

    def equality(self):  # a == b      3*z != 99
        pass

    def relation(self):  # a < b
        pass

    def addition(self) -> Expr:
        """
        Term  →  Term { + Term }
        """

        left = self.term()

        while self.currtok[1] in { Lexer.PLUS, Lexer.MINUS }:
            self.currtok = next(self.tg)  # advance to the next token
                                          # because we matched a +
            right = self.term()
            left = BinaryExpr(left,right, '+')

        return left

    def term(self) -> Expr:
        """
        Term  → Fact { * Fact }
        """
        left = self.fact()

        while self.currtok[1] in { Lexer.MULT, Lexer.DIVIDE }:
            self.currtok = next(self.tg)
            right = self.fact()
            left = BinaryExpr(left, right, '*')

        return left

    def fact(self) -> Expr:
        """
        Fact  → [ - ] Primary
            e.g., -a  -(b+c)  -6    (b+c) a 6
        """

        # only advance to the next token on a successful match.
        if self.currtok[1] == Lexer.MINUS:
            self.currtok = next(self.tg)
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
        if self.currtok[1] == Lexer.INT:
            tmp = self.currtok
            self.currtok = next(self.tg)
            return IntLitExpr(tmp[1])

        # parse a parenthesized expression
        if self.currtok[1] == Lexer.LPAREN:
            self.currtok = next(self.tg)
            tree = self.addition() # TODO Keeps changing!
            if self.currtok[1] == Lexer.RPAREN:
                self.currtok = next(self.tg)
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
