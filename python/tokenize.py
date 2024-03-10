from stream import *

class Token():
    def _membersStr(self):
        return ", ".join((f'{k}={repr(v)}' for k, v in self.__dict__.items()))
    def __repr__(self):
        return f'{type(self).__name__}({self._membersStr()})'

class Integer(Token):
    def __init__(self, value):
        self.value = value
class Float(Token):
    def __init__(self, value):
        self.value = value
class Identifier(Token):
    def __init__(self, value):
        self.value = value
class Plus(Token):
    pass
class Minus(Token):
    pass
class Times(Token):
    pass
class Divide(Token):
    pass
class Mod(Token):
    pass
class Pipe(Token):
    pass
class LParen(Token):
    pass
class RParen(Token):
    pass
class LBrace(Token):
    pass
class RBrace(Token):
    pass
class LBracket(Token):
    pass
class RBracket(Token):
    pass
class Comma(Token):
    pass
class Dot(Token):
    pass
class Elipsis          (Token):
    pass
class Epsilon          (Token):
    pass
class Indent           (Token):
    pass
class UnIndent         (Token):
    pass
class EOL              (Token):
    pass
class Equals           (Token):
    pass
class Arrow            (Token):
    pass
class SingleUnderscore (Token):
    pass
class Is               (Token):
    pass
class Not              (Token):
    pass
class In               (Token):
    pass
class Update           (Token):
    pass
class For              (Token):
    pass
class To               (Token):
    pass
class And              (Token):
    pass
class Or               (Token):
    pass
class NotEquals        (Token):
    pass
class Question         (Token):
    pass
class Maybe            (Token):
    pass
class Struct           (Token):
    pass
class Union            (Token):
    pass
class Type             (Token):
    pass
class Interface        (Token):
    pass
class Set              (Token):
    pass
class Module           (Token):
    pass
class Def              (Token):
    pass
class Const            (Token):
    pass
class Assign           (Token):
    pass
class If               (Token):
    pass
class Match            (Token):
    pass
class Case             (Token):
    pass
class Return           (Token):
    pass
class Pound            (Token):
    pass

def isWhitespace(el):
    return el == '\n' or el == ' '

def span(string, func):
    for i, el in enumerate(string):
        if not func(el):
            return string[:i], string[i:]
    return string, '' 
def findRightmost(string: str, el):
    i = string.rfind(el)
    return i if i >= 0 else None
def find(string: str, el):
    i = string.find(el)
    return i if i >= 0 else None

def isDigit(string):
    return ord('0') <= ord(string) <= ord('9')

def isAlpha(string):
    return ord('a') <= ord(string) <= ord('z') or ord('A') <= ord(string) <= ord('Z')
def isIdentifierBegining(string):
    return isAlpha(string) or string == '_'
def isIdentifier(string):
    return isDigit(string) or isAlpha(string) or string == '_'

def tokenizeIdentifier(string):
    match string:
        case 'is':
            return Is()
        case 'not':
            return Not()
        case 'in':
            return In()
        case 'update':
            return Update()
        case 'for':
            return For()
        case 'to':
            return To()
        case 'and':
            return And()
        case 'or':
            return Or()
        case 'struct':
            return Struct()
        case 'union':
            return Union()
        case 'type':
            return Type()
        case 'interface':
            return Interface()
        case 'set':
            return Set()
        case 'module':
            return Module()
        case 'def':
            return Def()
        case 'const':
            return Const()
        case 'if':
            return If()
        case 'match':
            return Match()
        case 'case':
            return Case()
        case 'maybe':
            return Maybe()
        case 'return':
            return Return()
        case default:
            return Identifier(default)

@streamFunc
def Tokenize(input: str, tabs: int) -> Stream:
    if input == '':
        return empty
    match input[0]:
        case '+':
            return Plus() * Tokenize(input[1:], tabs)
        case '-':
            return Minus() * Tokenize(input[1:], tabs)
        case '*':
            return Times() * Tokenize(input[1:], tabs)
        case '/':
            return Divide() * Tokenize(input[1:], tabs)
        case '%':
            return Mod() * Tokenize(input[1:], tabs)
        case '|':
            return Pipe() * Tokenize(input[1:], tabs)
        case '(':
            return LParen() * Tokenize(input[1:], tabs)
        case ')':
            return RParen() * Tokenize(input[1:], tabs)
        case '{':
            return LBrace() * Tokenize(input[1:], tabs)
        case '}':
            return RBrace() * Tokenize(input[1:], tabs)
        case '[':
            return LBracket() * Tokenize(input[1:], tabs)
        case ']':
            return RBracket() * Tokenize(input[1:], tabs)
        case ',':
            return Comma() * Tokenize(input[1:], tabs)
        case '.':
            if input.startswith('...'):
                return Elipsis() * Tokenize(input[3:], tabs)
            return Dot() * Tokenize(input[1:], tabs)
        case el if isWhitespace(el):
            wspace, rest = span(input, isWhitespace)
            index = findRightmost(input, '\n')
            if index is None:
                return Tokenize(input[len(wspace):], tabs)
            spaces = wspace[index+1:]
            numSpaces = len(spaces)
            if numSpaces % 4 != 0:
                assert False and 'indentation must be multiple of 4'
            numTabs = numSpaces // 4
            if numTabs > tabs:
                return chain(
                    [EOL()], 
                    (Indent() for _ in range(numTabs - tabs)), 
                    Tokenize(rest, numSpaces),
                )
            if numTabs < tabs:
                return chain(
                    [EOL()],
                    (UnIndent() for _ in range(tabs - numTabs)),
                    Tokenize(rest, numSpaces)
                )
            return EOL() * Tokenize(rest, numSpaces)
        case '=':
            if input.startswith('=='):
                return Equals() * Tokenize(input[2:], tabs)
            if input.startswith('=>'):
                return Arrow() * Tokenize(input[2:], tabs)
            return Assign() * Tokenize(input[1:], tabs)
        case '!':
            if not input.startswith('!='):
                assert False and 'unexpected token !, not followed by `=`'
            return NotEquals() * Tokenize(input[2:], tabs)
        case '?':
            if input.startswith('??'):
                return Maybe() * Tokenize(input[2:], tabs)
            return Question() * Tokenize(input[1:], tabs)
        case '#':
            index = find(input, '\n')
            if index is None:
                return empty 
            return Tokenize(input[index+1:], tabs)
        case el if isDigit(el):
            digits, rest = span(input, isDigit)
            if not rest.startswith('.'):
                return Integer(digits) * Tokenize(rest, tabs)
            moreDigits, rest = span(rest[1:], isDigit)
            return Float(digits + '.' + moreDigits) * Tokenize(rest, tabs)
        case el if isIdentifierBegining(el):
            ident, rest = span(input, isIdentifier)
            return tokenizeIdentifier(ident) * Tokenize(rest, tabs)

print(*Tokenize('if i == 23 (wow) => cool + 27', 0))
