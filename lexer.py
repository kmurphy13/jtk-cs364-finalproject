import sys
from collections import namedtuple
from typing import Generator, Tuple
import re
class Lexer:

    # class variables that represent a code for a "kind" of token.
    # TODO Clean this up so it is much shorter

    # namedtuple is the id and then the regex/value
    Token = namedtuple('Token', ['id', 'value'])

    INT = Token(0, '')
    ID = Token(1, '')
    STRING = Token(2, '')
    REAL = Token(3, '')
    COMMENT = Token(4, '')

    # All of these could just be defined in the singleton_dict

    # PLUS = Token(5, '+')
    # LPAREN = Token(6, '(')
    # RPAREN = Token(7, ')')
    # MULT = Token(8, '*')
    # EOF = Token(9, '')  # TODO return special end-of-file token


    def __init__(self, fn: str):
        try:
            self.f = open(fn)
        except IOError:
            print("File {} not found".format(fn))
            print("Exiting")
            sys.exit(1)  # can't go on

    def token_generator(self) -> Generator[Tuple[int, str], None, None]:

        # TODO Can we make this more readable by putting this elsewhere?
        # check out the documentation on |
        # Don't forget about ^ and $
        # TEST TEST TEST try and break your code
        # SOLID
        split_patt = re.compile(
            r"""             # Split on 
               (\+) |        #  plus and capture
               (\*) |        #  times and capture
               (-)  |        #  minus and capture, minus not special unless in []
               \s   |        #  whitespace
               (\() |        #  left paren and capture
               (\))          #  right paren and capture
            """,
            re.VERBOSE
        )

        # regular expression for an ID
        # regular expression for an integer literal
        def is_token_type(token_type, token):
            if re.findall(token_type[1], token):
                yield (token_type[0], token)

        # add all singletons
        # key = id
        # value = string value of the token
        singleton_dict = dict[(
            (5, '+'),
            (6, '('),
            (7, ')'),
            (8, '*'),
            # end of file
            (9, '')
        )]

        for line in self.f:

            # save recognizing string literals and comments
            # until the end (do these last). Try and recognize
            # these *before* you split the line

            tokens = (t for t in split_patt.split(line) if t)

            for t in tokens:
                # TODO replace with a dictionary

                if singleton_dict[t]:
                    yield(singleton_dict[t], t)
                else:
                    is_token_type(Lexer.ID, t)
                    is_token_type(Lexer.STRING, t)
                    is_token_type(Lexer.INT, t)
                    is_token_type(Lexer.REAL, t)
                    is_token_type(Lexer.COMMENT, t)


if __name__ == "__main__":

    lex = Lexer("test.sluc")  # use command line arguments

    g = lex.token_generator()

    while True:
        try:
            print(next(g))
        except StopIteration:
            print("Done")
            break
