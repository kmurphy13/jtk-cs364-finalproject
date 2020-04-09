from collections import namedtuple
from typing import Generator, Tuple
import re
import sys


class Lexer:

    # class variables that represent a code for a "kind" of token.

    # namedtuple is the id and then the regex/value
    Token = namedtuple('Token', ['id', 'value'])

    INT = Token(0, "(?:^\d[_\d]*\d$)|^\d*$")
    ID = Token(1, "^[_a-zA-Z]\w*$")
    REAL = Token(2, '^\d+(?:[_\d]*\d)?(?:\.\d|e[+-]?\d+(?:[_\d]*\d)?)(?:[_\d]*\d)*(?:e[+-]?\d+(?:[_\d]*\d)?)?$')
    COMMENT = Token(3, '\/\/.*')
    STRING = Token(4, '".*"')

    PLUS = Token(5, '+')
    LPAREN = Token(6, '(')
    RPAREN = Token(7, ')')
    MULT = Token(8, '*')
    EOF = Token(9, '')
    PRINT = Token(10, 'print')
    BOOL = Token(11, 'bool')
    ELSE = Token(12, 'else')
    FALSE = Token(13, 'false')
    IF = Token(14, 'if')
    TRUE = Token(15, 'true')
    WHILE = Token(16, 'while')
    INTK = Token(17, 'int')
    CHAR = Token(18, 'char')
    MAIN = Token(19, 'main')
    FLOAT = Token(20, 'float')

    LAND = Token(21, '&&')
    EQUAL = Token(22, '==')
    NEQUAL = Token(23, '!=')
    LT = Token(24, '<')
    LEQ = Token(25, '<=')
    GT = Token(26, '>')
    GEQ = Token(27, '>=')
    MINUS = Token(28, '-')
    BITSHIFTL = Token(29, '<<')
    BITSHIFTR = Token(30, '>>')
    COMMA = Token(31, ',')
    SEMICOLON = Token(32, ';')
    EQ = Token(33, '=')
    LSBRAC = Token(34, '[')
    RSBRAC = Token(35, ']')
    LCBRAC = Token(36, '{')
    RCBRAC = Token(37, '}')
    DIV = Token(38, '/')
    MOD = Token(39, '%')
    NOT = Token(40, '!')
    LOR = Token(41, '||')

    singleton_dict = {
        PLUS[1]: PLUS[0],
        LPAREN[1]: LPAREN[0],
        RPAREN[1]: RPAREN[0],
        MULT[1]: MULT[0],
        EOF[1]: EOF[0],
        PRINT[1]: PRINT[0],
        BOOL[1]: BOOL[0],
        ELSE[1]: ELSE[0],
        FALSE[1]: FALSE[0],
        IF[1]: IF[0],
        TRUE[1]: TRUE[0],
        WHILE[1]: WHILE[0],
        DIV[1]: DIV[0],
        MOD[1]: MOD[0],
        NOT[1]: NOT[0],
        LOR[1]: LOR[0],
        LAND[1]: LAND[0],
        EQUAL[1]: EQUAL[0],
        NEQUAL[1]: NEQUAL[0],
        LT[1]: LT[0],
        LEQ[1]: LEQ[0],
        GT[1]: GT[0],
        GEQ[1]: GEQ[0],
        MINUS[1]: MINUS[0],
        BITSHIFTL[1]: BITSHIFTL[0],
        BITSHIFTR[1]: BITSHIFTR[0],
        COMMA[1]: COMMA[0],
        SEMICOLON[1]: SEMICOLON[0],
        EQ[1]: EQ[0],
        LSBRAC[1]: LSBRAC[0],
        RSBRAC[1]: RSBRAC[0],
        LCBRAC[1]: LCBRAC[0],
        RCBRAC[1]: RCBRAC[0],
        INTK[1]: INTK[0],
        CHAR[1]: CHAR[0],
        MAIN[1]: MAIN[0],
        FLOAT[1]: FLOAT[0]

    }

    split_patt = re.compile(
        r"""               #  Split on:
           \s     |        #  whitespace
           
           (\+)   |        #  operator: plus
           (\*)   |        #  operator: times
           (^/$)  |        #  operator: divide
           (-)    |        #  operator: subtract 
           (<<)   |        #  operator: bitshift left
           (>>)   |        #  operator: bitshift right
           (\|\|) |        #  operator: or
           (&&)   |        #  operator: and
           (==)   |        #  operator: equal
           (!=)   |        #  operator: not equal
           (<=)   |        #  operator: less than or equal
           (>=)   |        #  operator: greater than or equal
           (>)    |        #  operator: greater than
           (<)    |        #  operator: less than
           (%)    |        #  operator: mod
           (!)    |        #  operator: not 
           
           (;)    |        #  punctuation: semicolon
           (,)    |        #  punctuation: comma
           ({)    |        #  punctuation: left bracket
           (})    |        #  punctuation: right bracket
           (\()   |        #  punctuation: left parenthesis
           (\))   |        #  punctuation: right parenthesis
           (\[)   |        #  punctuation: Left bracket
           (\])   |        #  punctuation: right bracket
           
           (\bprint\b)  |    # keyword: print
           (\bbool\b)   |    # keyword: bool
           (\belse\b)   |    # keyword: else
           (\bfalse\b)  |    # keyword: false
           (\bif\b)     |    # keyword: if
           (\btrue\b)   |    # keyword: true
           (\bwhile\b)  |    # keyword: while
           (\bfloat\b)  |    # keyword: float
           (\bint\b)    |    # keyword: int
           (\bmain\b)    |    # keyword: main
           (\bchar\b)    |    # keyword: int
           
           (^\d[_\d]*$)                            |   # integer     TODO: Do plus/minus need to be included? Also not sure about the ^ and $ 
           (\d[._\d]*(?:e[+-]?)*[._\d]*)           |   # real number 
           ([_a-zA-Z]\w*)                          |   # identifier
           (".*")                                  |   # string      TODO: This seems too simple
           (\/\/.*)                                |   # comment
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

    def check_non_singletons(self, token: str, line_num: int):
        if re.match(Lexer.ID[1], token):
            return Lexer.ID[0], token, line_num
        elif re.match(Lexer.STRING[1], token):
            return Lexer.STRING[0], token, line_num
        elif re.match(Lexer.INT[1], token):
            return Lexer.INT[0], token, line_num
        elif re.match(Lexer.REAL[1], token):
            return Lexer.REAL[0], token, line_num
        elif re.match(Lexer.COMMENT[1], token):
            return "COMMENT"
            # return 'Comment: ' + token + ', ' + str(line_num)
        else:
            return ("ILLEGAL", 'Illegal token: ' + token + ', Line number: ' + str(line_num))
    def token_generator(self) -> Generator[Tuple[int, str], None, None]:
        line_count = 1
        for line in self.f:
            # save recognizing string literals and comments
            # until the end (do these last). Try and recognize
            # these *before* you split the line

            tokens = (t for t in self.split_patt.split(line) if t)
            for t in tokens:
                if t in self.singleton_dict:
                    yield (self.singleton_dict[t], t, line_count)
                elif "COMMENT" == self.check_non_singletons(t, line_count):
                    pass
                elif "ILLEGAL" == self.check_non_singletons(t, line_count)[0]:
                    print(self.check_non_singletons(t, line_count)[1])
                else:
                    yield self.check_non_singletons(t, line_count)  # testing regex

            line_count += 1
        line_count += 1
        yield Lexer.EOF.id, Lexer.EOF.value, line_count


if __name__ == "__main__":
    # use command line args
    lex = Lexer(sys.argv[1])

    # generate tokens
    g = lex.token_generator()

    print("Token            Name                      Line Number")
    print("------------------------------------------------------")

    while True:
        try:
            print(next(g))
        except StopIteration:
            print("Done")
            break
