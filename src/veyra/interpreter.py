from .parser import Program, Function, Block, LetStatement, AssignmentStatement, FunctionCallStatement, IfStatement, WhileStatement, ForStatement, ReturnStatement, PrintStatement, ImportStatement, BinaryOp, UnaryOp, Number, Float, Boolean, String, Variable, FunctionCall, Array, Index, Dict, MatchExpression, Case

class Interpreter:
    def __init__(self):
        self.global_env = {}
        self.functions = {}
        self.channels = {}  # id to queue

    def interpret(self, program):
        print("Starting interpretation")
        for func in program.functions:
            self.functions[func.name] = func
        # Execute top-level statements (including imports)
        for stmt in program.statements:
            self.execute_statement(stmt, self.global_env)
        if 'main' in self.functions:
            print("Calling main")
            self.call_function('main', [])
        else:
            print("No main, executed top-level statements")

    def call_function(self, name, args):
        if name == 'channel':
            chan_id = len(self.channels)
            self.channels[chan_id] = []
            return chan_id
        elif name == 'send':
            chan_id, value = args
            self.channels[chan_id].append(value)
            return None
        elif name == 'receive':
            chan_id = args[0]
            if self.channels[chan_id]:
                return self.channels[chan_id].pop(0)
            else:
                return None  # blocking, but for now return None
        elif name == 'http_get':
            import urllib.request
            url = args[0]
            try:
                response = urllib.request.urlopen(url)
                return response.read().decode('utf-8')
            except:
                return "Error fetching URL"
        elif name == 'web_serve':
            html_content = args[0]
            port = args[1] if len(args) > 1 else 8080
            print("Starting Veyra web server on port {}...".format(port))
            print("Visit http://localhost:{} in your browser".format(port))
            # Simple HTTP server
            import http.server
            import socketserver
            class Handler(http.server.SimpleHTTPRequestHandler):
                def do_GET(self):
                    self.send_response(200)
                    self.send_header('Content-type', 'text/html')
                    self.end_headers()
                    self.wfile.write(html_content.encode('utf-8'))
            with socketserver.TCPServer(("", port), Handler) as httpd:
                httpd.serve_forever()
            return "Saved"
        elif name == 'html_element':
            tag, content = args[0], args[1] if len(args) > 1 else ""
            attrs = args[2] if len(args) > 2 else {}
            attr_str = " ".join('{}="{}"'.format(k, v) for k, v in attrs.items())
            if attr_str:
                return '<{} {}>{}</{}>'.format(tag, attr_str, content, tag)
            else:
                return '<{}>{}</{}>'.format(tag, content, tag)
        elif name == 'css_style':
            selector, rules_str = args
            return '{} {{ {} }}'.format(selector, rules_str)
        elif name == 'create_page':
            title, body_html, css = args
            return '''<!DOCTYPE html>
<html>
<head>
    <title>{}</title>
    <style>{}</style>
</head>
<body>
{}
</body>
</html>'''.format(title, css, body_html)
        elif name == 'route':
            path, handler = args
            print('Route {} -> {}'.format(path, handler))
            return 'Route added'
        elif name == 'responsive_css':
            return '''
@media (max-width: 600px) {
    body { font-size: 14px; }
    .container { width: 100%; }
}
'''
        elif name == 'len':
            return len(args[0])
        elif name == 'abs':
            return abs(args[0])
        elif name == 'max':
            return max(args)
        elif name == 'min':
            return min(args)
        elif name == 'pow':
            return pow(args[0], args[1])
        elif name == 'sqrt':
            import math
            return math.sqrt(args[0])
        elif name == 'upper':
            return str(args[0]).upper()
        elif name == 'lower':
            return str(args[0]).lower()
        elif name == 'split':
            return str(args[0]).split(args[1] if len(args) > 1 else None)
        elif name == 'join':
            return args[1].join(args[0])
        elif name == 'replace_str':
            return str(args[0]).replace(args[1], args[2])
        elif name == 'startswith':
            return str(args[0]).startswith(args[1])
        elif name == 'endswith':
            return str(args[0]).endswith(args[1])
        elif name == 'ai_linear_regression':
            # Simple implementation
            X, y = args
            # Mock: return slope and intercept
            return {"slope": 1.0, "intercept": 0.0}
        elif name == 'ui_button':
            text, callback = args
            button_html = '<button onclick="{}">{}</button>'.format(callback, text)
            return button_html
        elif name == 'ui_layout':
            direction, children = args
            layout_html = '<div style="display: flex; flex-direction: {};">{}</div>'.format(direction, ''.join(children))
            return layout_html
        elif name == 'ui_form':
            type_, placeholder = args
            form_html = '<form><input type="{}" placeholder="{}" /><button type="submit">Submit</button></form>'.format(type_, placeholder)
            return form_html
        elif name == 'ui_animation':
            element, anim_type = args
            css = '{} {{ animation: {} 2s ease-in-out; }}'.format(element, anim_type)
            return css
        elif name == 'ui_accessible':
            element, label = args
            return '<{} aria-label="{}">{}</{}>'.format(element, label, label, element)
        elif name == 'replace':
            string, old, new = args
            return string.replace(old, new)
        elif name == 'rand':
            import random
            return random.random()
        else:
            func = self.functions[name]
        if len(args) != len(func.params):
            raise ValueError("Function {} expects {} arguments, got {}".format(name, len(func.params), len(args)))
        local_env = dict(zip(func.params, args))
        result = self.execute_block(func.body, local_env)
        if isinstance(result, ReturnValue):
            return result.value
        return None

    def execute_block(self, block, env):
        for stmt in block.statements:
            result = self.execute_statement(stmt, env)
            if isinstance(result, ReturnValue):
                return result
        return None

    def execute_statement(self, stmt, env):
        if isinstance(stmt, LetStatement):
            value = self.evaluate_expression(stmt.expr, env)
            env[stmt.name] = value
        elif isinstance(stmt, AssignmentStatement):
            if stmt.name in env:
                value = self.evaluate_expression(stmt.expr, env)
                env[stmt.name] = value
            else:
                raise ValueError("Undefined variable: {}".format(stmt.name))
        elif isinstance(stmt, FunctionCallStatement):
            args = [self.evaluate_expression(arg, env) for arg in stmt.args]
            self.call_function(stmt.name, args)
        elif isinstance(stmt, IfStatement):
            condition = self.evaluate_expression(stmt.condition, env)
            if condition:
                result = self.execute_block(stmt.then_block, env)
                if isinstance(result, ReturnValue):
                    return result
            elif stmt.else_block:
                result = self.execute_block(stmt.else_block, env)
                if isinstance(result, ReturnValue):
                    return result
        elif isinstance(stmt, WhileStatement):
            while self.evaluate_expression(stmt.condition, env):
                result = self.execute_block(stmt.body, env)
                if isinstance(result, ReturnValue):
                    return result
        elif isinstance(stmt, ForStatement):
            start_val = self.evaluate_expression(stmt.start, env)
            end_val = self.evaluate_expression(stmt.end, env)
            for i in range(start_val, end_val):
                env[stmt.var] = i
                result = self.execute_block(stmt.body, env)
                if isinstance(result, ReturnValue):
                    return result
        elif isinstance(stmt, ReturnStatement):
            value = self.evaluate_expression(stmt.expr, env)
            return ReturnValue(value)
        elif isinstance(stmt, ImportStatement):
            self.import_module(stmt.module)
        elif isinstance(stmt, PrintStatement):
            value = self.evaluate_expression(stmt.expr, env)
            print(value)
        return None

    def evaluate_expression(self, expr, env):
        if isinstance(expr, Number):
            return expr.value
        elif isinstance(expr, Float):
            return expr.value
        elif isinstance(expr, Boolean):
            return expr.value
        elif isinstance(expr, String):
            return expr.value
        elif isinstance(expr, Variable):
            if expr.name in env:
                return env[expr.name]
            elif expr.name in self.global_env:
                return self.global_env[expr.name]
            else:
                raise ValueError("Undefined variable: {}".format(expr.name))
        elif isinstance(expr, Index):
            array = self.evaluate_expression(expr.array, env)
            index = self.evaluate_expression(expr.index, env)
            if isinstance(array, (list, dict)):
                return array[index]
            else:
                raise ValueError("Cannot index non-array or non-dict")
        elif isinstance(expr, BinaryOp):
            left = self.evaluate_expression(expr.left, env)
            right = self.evaluate_expression(expr.right, env)
            if expr.op == '+':
                if isinstance(left, str) or isinstance(right, str):
                    return str(left) + str(right)
                else:
                    return left + right
            elif expr.op == '-':
                return left - right
            elif expr.op == '*':
                return left * right
            elif expr.op == '/':
                return left / right
            elif expr.op == '%':
                return left % right
            elif expr.op == '>':
                return left > right
            elif expr.op == '<':
                return left < right
            elif expr.op == '==':
                return left == right
            elif expr.op == '<=':
                return left <= right
            elif expr.op == '>=':
                return left >= right
            elif expr.op == '!=':
                return left != right
            elif expr.op == '&&':
                return left and right
            elif expr.op == '||':
                return left or right
        elif isinstance(expr, UnaryOp):
            operand = self.evaluate_expression(expr.expr, env)
            if expr.op == '-':
                return -operand
            elif expr.op == '!':
                return not operand
        elif isinstance(expr, FunctionCall):
            args = [self.evaluate_expression(arg, env) for arg in expr.args]
            return self.call_function(expr.name, args)
        elif isinstance(expr, Array):
            elements = [self.evaluate_expression(e, env) for e in expr.elements]
            return elements  # Return as list
        elif isinstance(expr, Dict):
            return {self.evaluate_expression(k, env): self.evaluate_expression(v, env) for k, v in expr.items}
        elif isinstance(expr, MatchExpression):
            value = self.evaluate_expression(expr.expr, env)
            for case in expr.cases:
                if case.pattern == '_':
                    return self.evaluate_expression(case.expr, env)
                elif isinstance(case.pattern, (Number, Float, Boolean, String)):
                    if value == self.evaluate_expression(case.pattern, env):
                        return self.evaluate_expression(case.expr, env)
                # For now, only handle simple patterns
            raise ValueError("No matching case for value: {}".format(value))
        raise ValueError("Unknown expression type: {}".format(type(expr)))

    def import_module(self, module_name):
        # Load and execute the module
        try:
            with open(module_name, 'r') as f:
                code = f.read()
            from .lexer import Lexer
            from .parser import Parser
            lexer = Lexer(code)
            tokens = lexer.tokenize()
            parser = Parser(tokens)
            module_ast = parser.parse()
            # Execute in a new environment, but merge into global
            module_env = {}
            for func in module_ast.functions:
                self.functions[func.name] = func
            for stmt in module_ast.statements:
                if isinstance(stmt, LetStatement):
                    value = self.evaluate_expression(stmt.expr, module_env)
                    self.global_env[stmt.name] = value
                # Skip other statements for now
        except Exception as e:
            raise ValueError('Error importing {}: {}'.format(module_name, e))

class ReturnValue:
    def __init__(self, value):
        self.value = value

if __name__ == '__main__':
    import sys
    from .lexer import Lexer
    from .parser import Parser
    if len(sys.argv) > 1:
        try:
            with open(sys.argv[1], 'r') as f:
                text = f.read()
            lexer = Lexer(text)
            tokens = lexer.tokenize()
            parser = Parser(tokens)
            program = parser.parse()
            interpreter = Interpreter()
            interpreter.interpret(program)
        except IOError as e:
            print("Error: Could not read file '{}': {}".format(sys.argv[1], e))
            sys.exit(1)
        except ValueError as e:
            print("Veyra Error: {}".format(e))
            sys.exit(1)
        except Exception as e:
            import traceback
            print("Unexpected error: {}".format(e))
            traceback.print_exc()
            sys.exit(1)
