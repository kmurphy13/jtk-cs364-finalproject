from collections import namedtuple
from typing import Generator, Tuple
import re
import sys


class Lexer:

    # class variables that represent a code for a "kind" of token.

    # namedtuple is the id and then the regex/value
    Token = namedtuple('Token', ['id', 'value', 'description'])

    # non-singleton tokens
    INT = Token(0, "(?:^\d[_\d]*\d$)|^\d*$", 'integer')
    ID = Token(1, "^[_a-zA-Z]\w*$", 'identifier')
    REAL = Token(2, '^\d+(?:[_\d]*\d)?(?:\.\d|e[+-]?\d+(?:[_\d]*\d)?)(?:[_\d]*\d)*(?:e[+-]?\d+(?:[_\d]*\d)?)?$', 'real-number')
    COMMENT = Token(3, '//.*', 'comment')
    STRING = Token(4, '".*"', 'string-literal')

    # singleton tokens
    PLUS = Token(5, '+', 'plus sign')
    LPAREN = Token(6, '(', 'left paren')
    RPAREN = Token(7, ')', 'right paren')
    MULT = Token(8, '*', 'multiplication sign')
    EOF = Token(9, '', 'end-of-file')
    PRINT = Token(10, 'print', 'keyword')
    BOOL = Token(11, 'bool', 'keyword')
    ELSE = Token(12, 'else', 'keyword')
    FALSE = Token(13, 'false', 'keyword')
    IF = Token(14, 'if', 'keyword')
    TRUE = Token(15, 'true', 'keyword')
    WHILE = Token(16, 'while', 'keyword')
    INTK = Token(17, 'int', 'keyword')
    CHAR = Token(18, 'char', 'keyword')
    MAIN = Token(19, 'main', 'keyword')
    FLOAT = Token(20, 'float', 'keyword')
    LAND = Token(21, '&&', 'logical and')
    EQUAL = Token(22, '==', 'equal-equal')
    NEQUAL = Token(23, '!=', 'not equal')
    LT = Token(24, '<', 'less than')
    LEQ = Token(25, '<=', 'less than equal')
    GT = Token(26, '>', 'greater than')
    GEQ = Token(27, '>=', 'greater than equal')
    MINUS = Token(28, '-', 'minus')
    BITSHIFTL = Token(29, '<<', 'bit shift left')
    BITSHIFTR = Token(30, '>>', 'bit shift right')
    COMMA = Token(31, ',', 'comma')
    SEMICOLON = Token(32, ';', 'semicolon')
    EQ = Token(33, '=', 'assignment')
    LSBRAC = Token(34, '[', 'square bracket left')
    RSBRAC = Token(35, ']', 'square bracket right')
    LCBRAC = Token(36, '{', 'curly bracket left')
    RCBRAC = Token(37, '}', 'curly bracket right')
    DIV = Token(38, '/', 'divide')
    MOD = Token(39, '%', 'modulus')
    NOT = Token(40, '!', 'not')
    LOR = Token(41, '||', 'logical or')
    RET = Token(42, 'return','keyword')

    # dictionary of singleton tokens
    singleton_dict = {
        PLUS[1]: (PLUS[0], PLUS[2]),
        LPAREN[1]: (LPAREN[0], LPAREN[2]),
        RPAREN[1]: (RPAREN[0],RPAREN[2]),
        MULT[1]: (MULT[0], MULT[2]),
        EOF[1]: (EOF[0], EOF[2]),
        PRINT[1]: (PRINT[0], PRINT[2]),
        BOOL[1]: (BOOL[0], BOOL[2]),
        ELSE[1]: (ELSE[0], ELSE[2]),
        FALSE[1]: (FALSE[0], FALSE[2]),
        IF[1]: (IF[0], IF[2]),
        TRUE[1]: (TRUE[0], TRUE[2]),
        WHILE[1]: (WHILE[0], WHILE[2]),
        DIV[1]: (DIV[0], DIV[2]),
        MOD[1]: (MOD[0], MOD[2]),
        NOT[1]: (NOT[0], NOT[2]),
        LOR[1]: (LOR[0], LOR[2]),
        LAND[1]: (LAND[0], LAND[2]),
        EQUAL[1]: (EQUAL[0], EQUAL[2]),
        NEQUAL[1]: (NEQUAL[0], NEQUAL[2]),
        LT[1]: (LT[0], LT[2]),
        LEQ[1]: (LEQ[0], LEQ[2]),
        GT[1]: (GT[0], GT[2]),
        GEQ[1]: (GEQ[0], GEQ[2]),
        MINUS[1]: (MINUS[0], MINUS[2]),
        BITSHIFTL[1]: (BITSHIFTL[0], BITSHIFTL[2]),
        BITSHIFTR[1]: (BITSHIFTR[0],BITSHIFTR[2]),
        COMMA[1]: (COMMA[0], COMMA[2]),
        SEMICOLON[1]: (SEMICOLON[0], SEMICOLON[2]),
        EQ[1]: (EQ[0], EQ[2]),
        LSBRAC[1]: (LSBRAC[0], LSBRAC[2]),
        RSBRAC[1]: (RSBRAC[0], RSBRAC[2]),
        LCBRAC[1]: (LCBRAC[0], LCBRAC[2]),
        RCBRAC[1]: (RCBRAC[0], RCBRAC[2]),
        INTK[1]: (INTK[0], INTK[2]),
        CHAR[1]: (CHAR[0], CHAR[2]),
        MAIN[1]: (MAIN[0], MAIN[2]),
        FLOAT[1]: (FLOAT[0], FLOAT[2]),
        EOF[1]: (EOF[0], EOF[2]),
        RET[1]: (RET[0], RET[2])
    }

    split_patt = re.compile(
        r"""               #  Split on:
           \s     |        #  whitespace
           ((?<!\de)\+)   |        #  operator: plus
           (\*)   |        #  operator: times
           (/(?!/))  |        #  operator: divide
           ((?<!\de)-)    |        #  operator: subtract 
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
           (\bmain\b)   |    # keyword: main
           (\bchar\b)   |   # keyword: int
           (\breturn\b) |   # keyword: return 
           (//.*)       |   # comment
           ("(?:\\.|[^"\\])*")    
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
            return (Lexer.ID[0], Lexer.ID[2]), token, line_num
        elif re.match(Lexer.STRING[1], token):
            return (Lexer.STRING[0], Lexer.STRING[2]), token, line_num
        elif re.match(Lexer.INT[1], token):
            return (Lexer.INT[0], Lexer.INT[2]), token, line_num
        elif re.match(Lexer.REAL[1], token):
            return (Lexer.REAL[0],Lexer.REAL[2]), token, line_num
        elif re.match(Lexer.COMMENT[1], token):
            return "COMMENT"
        else:
            return "ILLEGAL", 'Invalid character sequence "' + token + '" on line: ' + str(line_num)

    def token_generator(self) -> Generator[Tuple[int, str], None, None]:

        # keep line number variable
        line_count = 1

        # iterate over all off the lines in the file
        for line in self.f:

            # get the tokens on the line
            tokens = (t for t in self.split_patt.split(line) if t)
            for t in tokens:
                if t in self.singleton_dict:
                    yield (self.singleton_dict[t], t, line_count)
                elif "COMMENT" == self.check_non_singletons(t, line_count):
                    pass
                elif "ILLEGAL" == self.check_non_singletons(t, line_count)[0]:
                    # invalid token
                    print()
                    print(self.check_non_singletons(t, line_count)[1])
                    print()
                else:
                    yield self.check_non_singletons(t, line_count)  # testing regex
            # increment line count
            line_count += 1

        # yield end-of-file token
        yield (Lexer.EOF.id, Lexer.EOF.description), Lexer.EOF.value, line_count


if __name__ == "__main__":
    # use command line args
    lex = Lexer(sys.argv[1])

    # generate tokens
    g = lex.token_generator()

    # print headers
    print("%-20s %-65s %-20s" %("Token", "Name", "Line Number"))
    print("-----------------------------------------------------------------------------------------------------")

    while True:
        try:
            p = next(g)
            print("%-20s %-65s %-20i" %(p[0][1], p[1], p[2]))
            print(p)
        except StopIteration:
            print("Done")
            break
