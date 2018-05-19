from uncertainty import *

# BNF grammars for mathematical expressions: 


# <expression> ::= <signed-term> | <expression> "+" <term> | <expression> "-" <term>
# <signed-term> ::= "-" <term> | <term>
# <term> ::= <factor> | <term> * <factor> 
# <factor> ::= <element> | <element> ** <numeral>
# <element> ::= <measurement> | <variable> | <numeral> | "(" <expression> ")"
# <measure> ::= <variable> | <decimal> "+/-" <decimal> | <variable> "+/-" <decimal>
# <variable> ::= [a-z]
# <decimal> ::= [0-9]+ "." [0-9]+ | <numeral> ::= <numeral> | <numeral> "." <numeral>
# <numeral> ::= [0-9]+


# recursive descent parser for mathematical expressions

# first define the internal representation of mathematical expressions
# using classes

class Number:
    def __init__(self, value):
        self.value = value

class Decimal:
    def __init__(self, integ, frac):
        self.integ = integ
        self.frac = frac
        
class Variable:
    def __init__(self, char):
        self.char = char

class BinaryME:
    def __init__(self, left, right):
        self.left = left
        self.right = right

# expr is a mathematical expression
# could be Number, Variable, GroupME, NegME, etc. 
class GroupME:
    def __init__(self, expr):
        self.expr = expr

class NegME:
    def __init__(self, expr):
        self.expr = expr


class AddME:
    def __init__(self, left, right):
        self.left = left
        self.right = right

class SubME:
    def __init__(self, left, right):
        self.left = left
        self.right = right

class MulME:
    def __init__(self, left, right):
        self.left = left
        self.right = right

class PowerME:
    def __init__(self, base, exp):
        self.base = base
        self.exp = exp

class ErrorME:
    def __init__(self, msg):
        self.msg = msg


        
def show_me(me):
    if type(me) == Number:
        return ('(' + 'Num ' + str(me.value) + ')')

    elif type(me) == Decimal:
        return ('(' + 'Dec ' + str(me.integ) + '.' + str(me.frac) + ')')
    
    elif type(me) == Variable:
        return ('(' + 'Var ' + str(me.char) + ')')

    elif type(me) == GroupME:
        return ('(' + 'Group ' + show_me(me.expr) + ')')

    elif type(me) == NegME:
        return ('(' + 'Neg ' + show_me(me.expr) + ')')

    elif type(me) == AddME:
        return ('(' + 'Add ' + show_me(me.left) + ' ' + show_me(me.right) + ')')

    elif type(me) == SubME:
        return ('(' + 'Sub ' + show_me(me.left) + ' ' + show_me(me.right) + ')')

    elif type(me) == MulME:
        return ('(' + 'Mul ' + show_me(me.left) + ' ' + show_me(me.right) + ')')

    elif type(me) == PowerME:
        return ('(' + 'Power ' + show_me(me.base) + ' ' + str(me.exp) + ')')
    
    elif type(me) == ErrorME:
        return me.msg


# Parser
# Input: a string, output:a list containing a ME and more characters
def parse_elem(string):
    first = string[0]
    rest = string[1:]
    if first == '(':
        # (ME, String)
        expr_and_str = parse_expr(rest)
        str_first = expr_and_str[1][0]
        if str_first == ')':
            return [ GroupME(expr_and_str[0]), expr_and_str[1][1:] ]
        else:
            return ErrorME('Elem parse failure at group')
    elif first.isalpha:
        return [ Variable(first), rest]
    else:
        numeral_and_str = parse_numeral(string)
        if type(numeral_and_str) == ErrorME:
            return ErrorME('Elem parse failure at numeral')
        else:
            return [ Number( numeral_and_str[0]), numeral_and_str[1]]

# Integer parsing
def parse_numeral(string):
    if string == "":
        return ErrorME('Empty string to parse numeral')
    else:
        first = string[0]
        rest = string[1:]
        if first.isdigit():
            return extend_num(int(first), rest)
        else:
            return ErrorME('Numeral parse failure')

def extend_num(accum, string):
    if string == "":
        return [ accum, ""]
    else:
        first = string[0]
        rest = string[1:]
        if first.isdigit():
            return extend_num( 10*accum + int(first), rest)
        else:
            return [accum, string]

def parse_factor(string):
    elem_and_more = parse_elem(string)
    if type(elem_and_more) == ErrorME:
        return ErrorME('Factor parse failure')
    elif (elem_and_more[1] == ""):
        return elem_and_more
    elif (elem_and_more[1][0] == '*' and elem_and_more[1][1] == '*'):
        numeral_and_more = parse_numeral(elem_and_more[1][2:])
        if type(numeral_and_more) == ErrorME:
            return ErrorME('Factor parse failure at power')
        else:
            return [PowerME( elem_and_more[0], numeral_and_more[0]), numeral_and_more[1]]
    else:
        return elem_and_more

def parse_term(string):
    factor_and_more = parse_factor(string)
    if type(factor_and_more) == ErrorME:
        return ErrorME('Term parse failure in parsing factor')
    else:
        return extend_term(factor_and_more[0], factor_and_more[1])

def extend_term(me, string):
    if string == "":
        return [me, ""]
    elif (string[0] == '*'):
        factor_and_more = parse_factor(string[1:])
        if type(factor_and_more) == ErrorME:
            return ErrorME('Extend failure factor after star')
        else:
            return extend_term(MulME(me, factor_and_more[0]), factor_and_more[1])
    else:
        return [me, string]

def parse_expr(string):
    if (string[0] == '-'):
        term_and_more = parse_term(string[1:])
        if type(term_and_more) == ErrorME:
            return ErrorME('Expr parse failure minus')
        else:
            return [NegME(term_and_more[0], term_and_more[1])]
    else:
        term_and_more = parse_term(string)
        if type(term_and_more) == ErrorME:
            return ErrorME('Expr parse failure')
        else:
            return extend_expr(term_and_more[0], term_and_more[1])

def extend_expr(me, string):
    if string == "":
        return [me, ""]
    elif (string[0] == '+'):
        term_and_more = parse_term(string[1:])
        if type(term_and_more) == ErrorME:
            return ErrorME('Expr extend fail at add')
        else:
            return extend_expr(AddME(me, term_and_more[0]), term_and_more[1])
    elif (string[0] == '-'):
        term_and_more = parse_term(string[1:])
        if type(term_and_more) == ErrorME:
            return ErrorME('Expr extend fail at add')
        else:
            return extend_expr(SubME(me, term_and_more[0]), term_and_more[1])
    else:
        return [me, string]


def parse_measure

def parse_decimal(string):
    numeral_and_more = parse_numeral(string)
    if type(numeral_and_more) == ErrorME:
        return ErrorME('Decimal parse failure at numeral parse')
    elif (numeral_and_more[1] == ""):
        return numeral_and_more
    elif (numeral_and_more[1][0] == '.'):
        after_dot = parse_numeral(numeral_and_more[1][1:])
        if type(after_dot) == ErrorME:
            return ErrorME('Decimal parse failure after dot')
        else:
            return [Decimal(numeral_and_more[0], after_dot[0]), after_dot[1]]
    else:
        return numeral_and_more

    
def parse_me(string):
    if string == "":
        return ErrorME('Empty string')
    expr_and_more = parse_expr(string)
    print( type(expr_and_more))
    print( show_me(expr_and_more[0]))
    print( expr_and_more[1])
    if expr_and_more[1] == "":
        return expr_and_more[0]
    else:
        return ErrorME('me parse failure')
        



test_decimal = "1111.3232332"
decimal_parse = parse_decimal(test_decimal)
print( show_me(decimal_parse[0]))

print( show_me(Decimal(1,2)))
