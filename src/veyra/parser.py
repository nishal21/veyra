from .lexer import Token

class ASTNode:
    pass

class Program(ASTNode):
    def __init__(self, statements, functions, classes):
        self.statements = statements
        self.functions = functions
        self.classes = classes

class Function(ASTNode):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

class ClassDefinition(ASTNode):
    def __init__(self, name, methods):
        self.name = name
        self.methods = methods

class Method(ASTNode):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

class Block(ASTNode):
    def __init__(self, statements):
        self.statements = statements

class IfStatement(ASTNode):
    def __init__(self, condition, then_block, else_block):
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block

class WhileStatement(ASTNode):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

class ForStatement(ASTNode):
    def __init__(self, var, start, end, body):
        self.var = var
        self.start = start
        self.end = end
        self.body = body

class LetStatement(ASTNode):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

class AssignmentStatement(ASTNode):
    def __init__(self, target, expr):
        self.target = target
        self.expr = expr

class FunctionCallStatement(ASTNode):
    def __init__(self, name, args):
        self.name = name
        self.args = args

class ReturnStatement(ASTNode):
    def __init__(self, expr):
        self.expr = expr

class ImportStatement(ASTNode):
    def __init__(self, module):
        self.module = module

class PrintStatement(ASTNode):
    def __init__(self, expr):
        self.expr = expr

class ExpressionStatement(ASTNode):
    def __init__(self, expr):
        self.expr = expr

class TryStatement(ASTNode):
    def __init__(self, try_block, catch_var, catch_block):
        self.try_block = try_block
        self.catch_var = catch_var
        self.catch_block = catch_block

class Expression(ASTNode):
    pass

class NewExpression(Expression):
    def __init__(self, class_name, args):
        self.class_name = class_name
        self.args = args

class AttributeAccess(Expression):
    def __init__(self, object_expr, attr):
        self.object_expr = object_expr
        self.attr = attr

class MethodCall(Expression):
    def __init__(self, object_expr, method, args):
        self.object_expr = object_expr
        self.method = method
        self.args = args

class BinaryOp(Expression):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class UnaryOp(Expression):
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

class Number(Expression):
    def __init__(self, value):
        self.value = value

class Float(Expression):
    def __init__(self, value):
        self.value = value

class Boolean(Expression):
    def __init__(self, value):
        self.value = value

class String(Expression):
    def __init__(self, value):
        self.value = value

class Variable(Expression):
    def __init__(self, name):
        self.name = name

class FunctionCall(Expression):
    def __init__(self, name, args):
        self.name = name
        self.args = args

class Array(Expression):
    def __init__(self, elements):
        self.elements = elements

class Index(Expression):
    def __init__(self, array, index):
        self.array = array
        self.index = index

class Dict(Expression):
    def __init__(self, items):
        self.items = items  # list of (key_expr, value_expr)

class MatchExpression(Expression):
    def __init__(self, expr, cases):
        self.expr = expr
        self.cases = cases

class Case:
    def __init__(self, pattern, expr):
        self.pattern = pattern
        self.expr = expr

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current_token = self.tokens[0] if self.tokens else None

    def advance(self):
        self.pos += 1
        if self.pos >= len(self.tokens):
            self.current_token = None
        else:
            self.current_token = self.tokens[self.pos]

    def eat(self, token_type):
        if self.current_token and self.current_token.type == token_type:
            self.advance()
        else:
            raise ValueError('Expected {}, got {} at line {}'.format(token_type, self.current_token, self.current_token.line if self.current_token else 'EOF'))

    def parse(self):
        statements = []
        functions = []
        classes = []
        while self.current_token and self.current_token.type != 'EOF':
            if self.current_token.value == 'fn':
                functions.append(self.parse_function())
            elif self.current_token.value == 'class':
                classes.append(self.parse_class())
            else:
                statements.append(self.parse_statement())
        return Program(statements, functions, classes)

    def parse_function(self):
        self.eat('KEYWORD')  # fn
        name = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('PUNCTUATION')  # (
        params = []
        if self.current_token.type == 'IDENTIFIER':
            params.append(self.current_token.value)
            self.eat('IDENTIFIER')
            while self.current_token.value == ',':
                self.eat('PUNCTUATION')
                params.append(self.current_token.value)
                self.eat('IDENTIFIER')
        self.eat('PUNCTUATION')  # )
        self.eat('PUNCTUATION')  # {
        body = self.parse_block()
        self.eat('PUNCTUATION')  # }
        return Function(name, params, body)

    def parse_class(self):
        self.eat('KEYWORD')  # class
        name = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('PUNCTUATION')  # {
        methods = []
        while self.current_token and self.current_token.value != '}':
            if self.current_token.value == 'fn':
                methods.append(self.parse_method())
            else:
                raise ValueError('Invalid class member')
        self.eat('PUNCTUATION')  # }
        return ClassDefinition(name, methods)

    def parse_method(self):
        self.eat('KEYWORD')  # fn
        name = self.current_token.value
        self.eat('IDENTIFIER')
        self.eat('PUNCTUATION')  # (
        params = []
        if self.current_token.type == 'IDENTIFIER':
            params.append(self.current_token.value)
            self.eat('IDENTIFIER')
            while self.current_token.value == ',':
                self.eat('PUNCTUATION')
                params.append(self.current_token.value)
                self.eat('IDENTIFIER')
        self.eat('PUNCTUATION')  # )
        self.eat('PUNCTUATION')  # {
        body = self.parse_block()
        self.eat('PUNCTUATION')  # }
        return Method(name, params, body)

    def parse_block(self):
        statements = []
        while self.current_token and self.current_token.value != '}':
            statements.append(self.parse_statement())
        return Block(statements)

    def parse_statement(self):
        if self.current_token.type == 'KEYWORD':
            if self.current_token.value == 'let':
                self.advance()
                name = self.current_token.value
                self.eat('IDENTIFIER')
                self.eat('OPERATOR')  # =
                expr = self.parse_expression()
                self.eat('PUNCTUATION')  # ;
                return LetStatement(name, expr)
            elif self.current_token.value == 'import':
                self.advance()
                module = self.current_token.value
                self.eat('STRING')
                self.eat('PUNCTUATION')  # ;
                return ImportStatement(module)
            elif self.current_token.value == 'if':
                self.advance()
                condition = self.parse_expression()
                self.eat('PUNCTUATION')  # {
                then_block = self.parse_block()
                self.eat('PUNCTUATION')  # }
                else_block = None
                if self.current_token and self.current_token.value == 'else':
                    self.advance()
                    self.eat('PUNCTUATION')  # {
                    else_block = self.parse_block()
                    self.eat('PUNCTUATION')  # }
                return IfStatement(condition, then_block, else_block)
            elif self.current_token.value == 'while':
                self.advance()
                condition = self.parse_expression()
                self.eat('PUNCTUATION')  # {
                body = self.parse_block()
                self.eat('PUNCTUATION')  # }
                return WhileStatement(condition, body)
            elif self.current_token.value == 'for':
                self.advance()
                var = self.current_token.value
                self.eat('IDENTIFIER')
                self.eat('KEYWORD')  # in
                start = self.parse_expression()
                self.eat('OPERATOR')  # ..
                end = self.parse_expression()
                self.eat('PUNCTUATION')  # {
                body = self.parse_block()
                self.eat('PUNCTUATION')  # }
                return ForStatement(var, start, end, body)
            elif self.current_token.value == 'return':
                self.advance()
                expr = self.parse_expression()
                self.eat('PUNCTUATION')  # ;
                return ReturnStatement(expr)
            elif self.current_token.value == 'println':
                self.advance()
                self.eat('PUNCTUATION')  # (
                expr = self.parse_expression()
                self.eat('PUNCTUATION')  # )
                self.eat('PUNCTUATION')  # ;
                return PrintStatement(expr)
            elif self.current_token.value == 'try':
                self.advance()
                self.eat('PUNCTUATION')  # {
                try_block = self.parse_block()
                self.eat('PUNCTUATION')  # }
                self.eat('KEYWORD')  # catch
                self.eat('PUNCTUATION')  # (
                catch_var = self.current_token.value
                self.eat('IDENTIFIER')
                self.eat('PUNCTUATION')  # )
                self.eat('PUNCTUATION')  # {
                catch_block = self.parse_block()
                self.eat('PUNCTUATION')  # }
                return TryStatement(try_block, catch_var, catch_block)
        elif self.current_token.type == 'IDENTIFIER':
            name = self.current_token.value
            self.advance()
            target = Variable(name)
            while self.current_token and (self.current_token.value == '[' or self.current_token.value == '.'):
                if self.current_token.value == '[':
                    self.eat('PUNCTUATION')  # [
                    index = self.parse_expression()
                    self.eat('PUNCTUATION')  # ]
                    target = Index(target, index)
                elif self.current_token.value == '.':
                    self.eat('PUNCTUATION')  # .
                    attr = self.current_token.value
                    self.eat('IDENTIFIER')
                    target = AttributeAccess(target, attr)
            if self.current_token.value == '=':
                self.eat('OPERATOR')  # =
                expr = self.parse_expression()
                self.eat('PUNCTUATION')  # ;
                return AssignmentStatement(target, expr)
            elif self.current_token.value == '(':
                self.eat('PUNCTUATION')  # (
                args = []
                if self.current_token.value != ')':
                    args.append(self.parse_expression())
                    while self.current_token.value == ',':
                        self.eat('PUNCTUATION')
                        args.append(self.parse_expression())
                self.eat('PUNCTUATION')  # )
                self.eat('PUNCTUATION')  # ;
                return FunctionCallStatement(name, args)
            else:
                self.eat('PUNCTUATION')  # ;
                return ExpressionStatement(target)
        raise ValueError('Invalid statement: {} at line {}'.format(self.current_token, self.current_token.line))

    def parse_expression(self):
        return self.parse_or()

    def parse_or(self):
        left = self.parse_and()
        while self.current_token and self.current_token.type == 'OPERATOR' and self.current_token.value == '||':
            op = self.current_token.value
            self.advance()
            right = self.parse_and()
            left = BinaryOp(left, op, right)
        return left

    def parse_and(self):
        left = self.parse_comparison()
        while self.current_token and self.current_token.type == 'OPERATOR' and self.current_token.value == '&&':
            op = self.current_token.value
            self.advance()
            right = self.parse_comparison()
            left = BinaryOp(left, op, right)
        return left

    def parse_comparison(self):
        left = self.parse_term()
        while self.current_token and self.current_token.type == 'OPERATOR' and self.current_token.value in ['>', '<', '==', '<=', '>=', '!=']:
            op = self.current_token.value
            self.advance()
            right = self.parse_term()
            left = BinaryOp(left, op, right)
        return left

    def parse_term(self):
        left = self.parse_factor()
        while self.current_token and self.current_token.type == 'OPERATOR' and self.current_token.value in ['+', '-']:
            op = self.current_token.value
            self.advance()
            right = self.parse_factor()
            left = BinaryOp(left, op, right)
        return left

    def parse_factor(self):
        left = self.parse_unary()
        while self.current_token and self.current_token.type == 'OPERATOR' and self.current_token.value in ['*', '/', '%']:
            op = self.current_token.value
            self.advance()
            right = self.parse_unary()
            left = BinaryOp(left, op, right)
        return left

    def parse_unary(self):
        if self.current_token.type == 'OPERATOR' and self.current_token.value == '-':
            self.advance()
            return UnaryOp('-', self.parse_unary())
        elif self.current_token.type == 'OPERATOR' and self.current_token.value == '!':
            self.advance()
            return UnaryOp('!', self.parse_unary())
        return self.parse_primary()

    def parse_primary(self):
        if self.current_token.type == 'KEYWORD' and self.current_token.value == 'match':
            return self.parse_match()
        elif self.current_token.type == 'KEYWORD' and self.current_token.value == 'new':
            self.advance()
            class_name = self.current_token.value
            self.eat('IDENTIFIER')
            self.eat('PUNCTUATION')  # (
            args = []
            if self.current_token.value != ')':
                args.append(self.parse_expression())
                while self.current_token.value == ',':
                    self.eat('PUNCTUATION')
                    args.append(self.parse_expression())
            self.eat('PUNCTUATION')  # )
            return NewExpression(class_name, args)
        elif self.current_token.type == 'NUMBER':
            value = self.current_token.value
            self.advance()
            if isinstance(value, float):
                return Float(value)
            else:
                return Number(value)
        elif self.current_token.type == 'KEYWORD' and self.current_token.value == 'true':
            self.advance()
            return Boolean(True)
        elif self.current_token.type == 'KEYWORD' and self.current_token.value == 'false':
            self.advance()
            return Boolean(False)
        elif self.current_token.type == 'STRING':
            value = self.current_token.value
            self.advance()
            return String(value)
        elif self.current_token.type == 'IDENTIFIER':
            name = self.current_token.value
            self.advance()
            expr = Variable(name)
            while self.current_token and (self.current_token.value == '[' or self.current_token.value == '.' or self.current_token.value == '('):
                if self.current_token.value == '[':
                    self.eat('PUNCTUATION')  # [
                    index = self.parse_expression()
                    self.eat('PUNCTUATION')  # ]
                    expr = Index(expr, index)
                elif self.current_token.value == '.':
                    self.eat('PUNCTUATION')  # .
                    attr = self.current_token.value
                    self.eat('IDENTIFIER')
                    if self.current_token.value == '(':
                        # Method call
                        self.eat('PUNCTUATION')  # (
                        args = []
                        if self.current_token.value != ')':
                            args.append(self.parse_expression())
                            while self.current_token.value == ',':
                                self.eat('PUNCTUATION')
                                args.append(self.parse_expression())
                        self.eat('PUNCTUATION')  # )
                        expr = MethodCall(expr, attr, args)
                    else:
                        expr = AttributeAccess(expr, attr)
                elif self.current_token.value == '(':
                    # Function call
                    self.eat('PUNCTUATION')  # (
                    args = []
                    if self.current_token.value != ')':
                        args.append(self.parse_expression())
                        while self.current_token.value == ',':
                            self.eat('PUNCTUATION')
                            args.append(self.parse_expression())
                    self.eat('PUNCTUATION')  # )
                    expr = FunctionCall(name, args)
                    break  # No more chaining after function call
            return expr
        elif self.current_token.value == '[':
            self.eat('PUNCTUATION')  # [
            elements = []
            if self.current_token.value != ']':
                elements.append(self.parse_expression())
                while self.current_token.value == ',':
                    self.eat('PUNCTUATION')
                    elements.append(self.parse_expression())
            self.eat('PUNCTUATION')  # ]
            return Array(elements)
        elif self.current_token.value == '{':
            return self.parse_dict()
        elif self.current_token.value == '(':
            self.eat('PUNCTUATION')
            expr = self.parse_expression()
            self.eat('PUNCTUATION')  # )
            return expr
        raise ValueError('Invalid primary: {} at line {}'.format(self.current_token, self.current_token.line))

    def parse_match(self):
        self.eat('KEYWORD')  # match
        expr = self.parse_expression()
        self.eat('PUNCTUATION')  # {
        cases = []
        while self.current_token.value != '}':
            pattern = self.parse_pattern()
            self.eat('OPERATOR')  # =>
            case_expr = self.parse_expression()
            cases.append(Case(pattern, case_expr))
            if self.current_token.value == ',':
                self.eat('PUNCTUATION')
        self.eat('PUNCTUATION')  # }
        return MatchExpression(expr, cases)

    def parse_dict(self):
        self.eat('PUNCTUATION')  # {
        items = []
        if self.current_token.value != '}':
            key_expr = self.parse_expression()
            self.eat('PUNCTUATION')  # :
            value_expr = self.parse_expression()
            items.append((key_expr, value_expr))
            while self.current_token.value == ',':
                self.eat('PUNCTUATION')
                key_expr = self.parse_expression()
                self.eat('PUNCTUATION')  # :
                value_expr = self.parse_expression()
                items.append((key_expr, value_expr))
        self.eat('PUNCTUATION')  # }
        return Dict(items)

    def parse_pattern(self):
        # Simple patterns: literals or _
        if self.current_token.type == 'NUMBER':
            value = self.current_token.value
            self.advance()
            if isinstance(value, float):
                return Float(value)
            else:
                return Number(value)
        elif self.current_token.type == 'KEYWORD' and self.current_token.value == 'true':
            self.advance()
            return Boolean(True)
        elif self.current_token.type == 'KEYWORD' and self.current_token.value == 'false':
            self.advance()
            return Boolean(False)
        elif self.current_token.type == 'IDENTIFIER' and self.current_token.value == '_':
            self.advance()
            return '_'
        else:
            raise ValueError('Invalid pattern at line {}'.format(self.current_token.line))
