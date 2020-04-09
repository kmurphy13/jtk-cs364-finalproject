import sys
from collections import namedtuple
from typing import Generator, Tuple
import re
class Lexer:

    # class variables that represent a code for a "kind" of token.
    # TODO Clean this up so it is much shorter

    # namedtuple is the id and then the regex/value
    Token = namedtuple('Token', ['id', 'value'])

    # INT = Token(1, "[1-9]+")
    # ID = Token(0, "[_a-zA-Z]\w*")
    # REAL = Token(1, '')
    # COMMENT = Token(3, '')
    # STRING = Token(4, '')
    PLUS = Token(5, '+')
    LPAREN = Token(6, '(')
    RPAREN = Token(7, ')')
    MULT = Token(8, '*')
    EOF = Token(9, '') #TODO what is the end-of-file token
    PRINT = Token(10, 'print')
    BOOL = Token(11, 'bool')
    ELSE = Token(12, 'else')
    FALSE = Token(13, 'false')
    IF = Token(14, 'if')
    TRUE = Token(15, 'true')
    WHILE = Token(16, 'while')
    DIV = Token(17, '/')
    MOD = Token(18, '%')
    NOT = Token(19, '!')
    LOR = Token(20, '||')
    LAND = Token(21, '&&')
    EQUAL = Token(22, '==')
    NEQUAL = Token(23, '!=')
    LT = Token(24, '<')
    LEQ = Token(25, '<=')
    GT = Token(26, '>')
    GEQ = Token(27, '>=')
    MINUS = Token(28, '-')

    singleton_dict = {
        PLUS[1]: PLUS[0], #
        LPAREN[1]: LPAREN[0], #
        RPAREN[1]: RPAREN[0], #
        MULT[1]: MULT[0],  #
        EOF[1]: EOF[0],
        PRINT[1]: PRINT[0],#
        BOOL[1]: BOOL[0], #
        ELSE[1]: ELSE[0],#
        FALSE[1]: FALSE[0],#
        IF[1]: IF[0],#
        TRUE[1]: TRUE[0],#
        WHILE[1]: WHILE[0],#
        DIV[1]: DIV[0],
        MOD[1]: MOD[0],
        NOT[1]: NOT[0],
        LOR[1]: LOR[0],
        LAND[1]: LAND[0],
        EQUAL[1]: EQUAL[0],
        LT[1]: LT[0],
        LEQ[1]: LEQ[0],
        GT[1]: GT[0],
        GEQ[1]: GEQ[0],
        MINUS[1]: MINUS[0] #
    }
    split_patt = re.compile(
        r"""             # Split on 
           (\+) |        #  plus and capture
           (\*) |        #  times and capture
           \s   |        #  whitespace
           (\() |        #  left paren and capture
           (\)) |        #  right paren and capture
           (\/) |       # divide and capture
           (\|\|)|  
           (&&) |   
           (==) | 
           (!=) | 
           (<=) | 
           (>=) | 
           (>)  |   
           (<)  | 
           (-)  | 
           (%)  | 
           (!)  | 
           (\b(print)\b) |
           (\b(bool)\b) |
           (\b(else)\b) |
           (\b(false)\b) |
           (\b(if)\b) |
           (\b(true)\b) |
           (\b(while)\b) 
    
        """,
        re.VERBOSE
    )

    def __init__(self, fn: str):
        try:
            self.f = open(fn)
        except IOError:
            print("File {} not found".format(fn))
            print("Exiting")
            sys.exit(0)  # can't go on

    def token_generator(self) -> Generator[Tuple[int, str], None, None]:

        # TODO Can we make this more readable by putting this elsewhere?
        # check out the documentation on |
        # Don't forget about ^ and $
        # TEST TEST TEST try and break your code
        # SOLID


        # regular expression for an ID
        # regular expression for an integer literal
        # def is_token_type(token_type, token):
        #     if re.findall(token_type[0], token):
        #         yield (token_type[1], token)

        # add all singletons
        # key = id
        # value = string value of the token


        for line in self.f:

            # save recognizing string literals and comments
            # until the end (do these last). Try and recognize
            # these *before* you split the line

            tokens = (t for t in self.split_patt.split(line) if t)
            for t in tokens:
                # TODO replace with a dictionary

                # if singleton_dict[t]:
                yield(self.singleton_dict[t], t)
                # else:
                #     is_token_type(Lexer.ID, t)
                #     is_token_type(Lexer.STRING, t)
                #     is_token_type(Lexer.INT, t)
                #     is_token_type(Lexer.REAL, t)
                #     is_token_type(Lexer.COMMENT, t)


if __name__ == "__main__":

    lex = Lexer("test.sluc")  # use command line arguments

    g = lex.token_generator()
    # print(lex.PLUS[1], lex.PLUS[0])
    while True:

        try:
            print(next(g))
        except StopIteration:
            print("Done")
            break
