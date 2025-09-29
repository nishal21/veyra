import re

class Token:
    def __init__(self, type_, value, line=1):
        self.type = type_
        self.value = value
        self.line = line

    def __repr__(self):
        return 'Token({}, {}, line {})'.format(self.type, self.value, self.line)

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[0] if self.text else None
        self.line = 1

    def advance(self):
        if self.current_char == '\n':
            self.line += 1
        self.pos += 1
        if self.pos >= len(self.text):
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]

    def skip_whitespace(self):
        while self.current_char and self.current_char.isspace():
            self.advance()

    def read_number(self):
        result = ''
        while self.current_char and (self.current_char.isdigit() or self.current_char == '.'):
            result += self.current_char
            self.advance()
        if '.' in result:
            return float(result)
        else:
            return int(result)

    def read_identifier(self):
        result = ''
        while self.current_char and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return result

    def read_string(self):
        self.advance()  # skip opening quote
        result = ''
        while self.current_char and self.current_char != '"':
            if self.current_char == '\\':
                result += self.current_char
                self.advance()
                if self.current_char:
                    result += self.current_char
                    self.advance()
            else:
                result += self.current_char
                self.advance()
        self.advance()  # skip closing quote
        return result

    def read_string_single(self):
        self.advance()  # skip opening quote
        result = ''
        while self.current_char and self.current_char != "'":
            if self.current_char == '\\':
                result += self.current_char
                self.advance()
                if self.current_char:
                    result += self.current_char
                    self.advance()
            else:
                result += self.current_char
                self.advance()
        self.advance()  # skip closing quote
        return result

    def tokenize(self):
        tokens = []
        while self.current_char:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            elif self.current_char == '/':
                self.advance()
                if self.current_char == '/':
                    # Skip comment
                    while self.current_char and self.current_char != '\n':
                        self.advance()
                    if self.current_char == '\n':
                        self.advance()
                    continue
                else:
                    # Not a comment, put back
                    self.pos -= 1
                    self.current_char = '/'
                    tokens.append(Token('OPERATOR', '/', self.line))
                    self.advance()
            elif self.current_char.isdigit():
                tokens.append(Token('NUMBER', self.read_number(), self.line))
            elif self.current_char.isalpha() or self.current_char == '_':
                ident = self.read_identifier()
                if ident in ['fn', 'let', 'if', 'else', 'return', 'println', 'while', 'true', 'false', 'for', 'in', 'match', 'import', 'class', 'try', 'catch', 'new']:
                    tokens.append(Token('KEYWORD', ident, self.line))
                else:
                    tokens.append(Token('IDENTIFIER', ident, self.line))
            elif self.current_char == '"':
                tokens.append(Token('STRING', self.read_string(), self.line))
            elif self.current_char == "'":
                tokens.append(Token('STRING', self.read_string_single(), self.line))
            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    tokens.append(Token('OPERATOR', '==', self.line))
                elif self.current_char == '>':
                    self.advance()
                    tokens.append(Token('OPERATOR', '=>', self.line))
                else:
                    tokens.append(Token('OPERATOR', '=', self.line))
            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    tokens.append(Token('OPERATOR', '<=', self.line))
                else:
                    tokens.append(Token('OPERATOR', '<', self.line))
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    tokens.append(Token('OPERATOR', '>=', self.line))
                else:
                    tokens.append(Token('OPERATOR', '>', self.line))
            elif self.current_char == '&':
                self.advance()
                if self.current_char == '&':
                    self.advance()
                    tokens.append(Token('OPERATOR', '&&', self.line))
                else:
                    raise ValueError('Invalid character: {}'.format(self.current_char))
            elif self.current_char == '|':
                self.advance()
                if self.current_char == '|':
                    self.advance()
                    tokens.append(Token('OPERATOR', '||', self.line))
                else:
                    raise ValueError('Invalid character: {}'.format(self.current_char))
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    tokens.append(Token('OPERATOR', '!=', self.line))
                else:
                    tokens.append(Token('OPERATOR', '!', self.line))
            elif self.current_char in '+-*/%':
                op = self.current_char
                self.advance()
                tokens.append(Token('OPERATOR', op, self.line))
            elif self.current_char == '.':
                self.advance()
                if self.current_char == '.':
                    self.advance()
                    tokens.append(Token('OPERATOR', '..', self.line))
                else:
                    tokens.append(Token('PUNCTUATION', '.', self.line))
            elif self.current_char in '{}();,[]:':
                tokens.append(Token('PUNCTUATION', self.current_char, self.line))
                self.advance()
            else:
                raise ValueError('Invalid character: {} at line {}'.format(self.current_char, self.line))
        tokens.append(Token('EOF', None))
        return tokens