# jtk-cs364-finalproject
Final Project for SLU CS364: Programming Languages.

CS 364 - Lexical Analyzer for SLU-C
SLU-C is our simplified programming language that looks a little like C. In this assignment you are going to complete the lexical analyzer (lexer) that we began in class. You need to get the lexer working in order to do Parts II and III of the project.

Requirements
Here are the requirements. Your lexer should ...

1) Recognize all of the tokens described below.
  a. Literals: integers, true, false, real numbers, string literals, comments, and identifiers.
  b. An integer is a sequence of one or more digits (e.g.,12, 0, 1432).
  c. A real number is a sequence of one or more digits, followed by a decimal point, followed by one or more digits (e.g., 3.14159, 0.0). Errors include 3. and .4
  d. A real number can also be expressed in scientific notation. 2.07e-9 and 2.7e+9 and 2.7e9. The decimal part is not needed 23e9 23e-9 23e+9 are still floating-point literals.
  e. Integers and floating-point numbers can contain underscores. 123_456_789 and 1.23_4e-1_2 but an underscore cannot start or finish a number.
  f. An identifier consists of letters, digits, and underscores, and cannot start with a digit.
  g. A string literal starts with a double quote, contains any number of characters, and ends with a double quote. String literals do not cross a line boundary. Starting off, do not worry about double quotes in a string literal. For full credit include escaped double quotes in a string literal. "He said \"go\""
  h. A comment starts with a // and goes to the end of the line. Comments do not cross a line boundary.
  i. Keywords: print, bool, else, false, if, true, float, int, while More may be added later.
  j. operators: || && == != < <= > >= + - * / % !
  k. punctuation: ; , { } ( )
2) Learn how to use command line arguments in Python. Read the name of the file being processed from the command line so I can invoke your lexer as:
  path-to-python3 lexer.py lexertest.c
Learn how to use command line arguments when your program is run from Pycharm. What should happen if we forget to put a file name or the file name is wrong? Yourprogram should not crash and it should have a nice error message.
3) Exit gracefully with an appropriate error message if the file being processed does not exist.
4) Have nicely formatted output listing the token recognized, the name of the token, and the line number the token was found on. For example, something like the following wold do.

Token            Name                      Line Number
------------------------------------------------------
Keyword          if                        1
Left paren       (                         1
Right paren      )                         2
identifer        count                     2
greater-equal    >=                        4
equal-equal      ==                        4
assignment       =                         8
This table is produced by your main program, not your Lexer class/function/generator.
5) Recognize // comments properly but the lexer should not return them as a token. It should skip over or toss them. We do not want to see comments in the output.
6) Ignore (toss) all whitespace.
7) Correctly identify lexical errors. Produce a reasonable error message and a line number. What are possible lexical errors? You lexer should continue after it finds an error and not crash or quit.
8) Your Lexer class should provide one function named next that returns a token (as we implemented in class).
9) Your program should use data abstraction (classes, functions, generators, and use python features we have studied.). Adhere to SOLID. Functions should not get too big.
10) Do not pollute the global namespace. You should only introduce the one Lexer class or function into the name space and maybe some stuff inside of your main if __name__ == "__main__:"
11) Name your lexer file lexer.py and make it its own module. Your main program can be in this file but make sure you check that it is running as main.
12) Every function/class should be commented appropriately with purpose, preconditions. Use doc strings judiciously.
13) Your program should produce the correct output.
14) Your program should not crash or hang. Try your best to break your code.
15) All error messages should be user friendly and not pertain to the implementation. For example Unrecognized character sequence "123abc" on line 5 is a decent message whereas next() returned an error IOException at token 123abc is unfriendly.
16) Here is a sample input test file.
