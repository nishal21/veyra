from .parser import Program, Function, Block, LetStatement, AssignmentStatement, FunctionCallStatement, IfStatement, WhileStatement, ForStatement, ReturnStatement, PrintStatement, ImportStatement, BinaryOp, UnaryOp, Number, Float, Boolean, String, Variable, FunctionCall, Array, Index, Dict, MatchExpression, Case, TryStatement, ClassDefinition, Method, NewExpression, AttributeAccess, MethodCall, ExpressionStatement, TryStatement, ClassDefinition, Method, NewExpression, AttributeAccess, MethodCall, ExpressionStatement

class Interpreter:
    def __init__(self):
        self.global_env = {}
        self.functions = {}
        self.classes = {}
        self.channels = {}  # id to queue

    def interpret(self, program):
        print("Starting interpretation")
        for func in program.functions:
            self.functions[func.name] = func
        for cls in program.classes:
            self.classes[cls.name] = cls
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
            import threading
            class Handler(http.server.SimpleHTTPRequestHandler):
                def do_GET(self):
                    self.send_response(200)
                    self.send_header('Content-type', 'text/html')
                    self.end_headers()
                    self.wfile.write(html_content.encode('utf-8'))
            def run_server():
                with socketserver.TCPServer(("", port), Handler) as httpd:
                    httpd.serve_forever()
            threading.Thread(target=run_server).start()
            return "Server started on port {}".format(port)
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
        elif name == 'substring':
            s = str(args[0])
            start = args[1]
            end = args[2] if len(args) > 2 else len(s)
            return s[start:end]
        elif name == 'index_of':
            return str(args[0]).find(args[1])
        elif name == 'last_index_of':
            return str(args[0]).rfind(args[1])
        elif name == 'trim':
            return str(args[0]).strip()
        elif name == 'to_string':
            return str(args[0])
        elif name == 'to_int':
            return int(args[0])
        elif name == 'to_float':
            return float(args[0])
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
        elif name == 'read_file':
            filename = args[0]
            try:
                with open(filename, 'r') as f:
                    return f.read()
            except:
                return "Error reading file"
        elif name == 'write_file':
            filename, content = args
            try:
                with open(filename, 'w') as f:
                    f.write(content)
                return "File written"
            except:
                return "Error writing file"
        elif name == 'list_dir':
            import os
            dirname = args[0] if args else '.'
            try:
                return os.listdir(dirname)
            except:
                return "Error listing directory"
        elif name == 'json_parse':
            import json
            return json.loads(args[0])
        elif name == 'json_stringify':
            import json
            return json.dumps(args[0])
        elif name == 'time_now':
            import time
            return time.time()
        elif name == 'sleep':
            import time
            time.sleep(args[0])
            return None
        elif name == 'rand':
            import random
            return random.random()
        elif name == 'matrix_multiply':
            # Simple matrix multiplication
            a, b = args
            if not a or not b or len(a[0]) != len(b):
                raise ValueError("Invalid matrix dimensions")
            result = []
            for i in range(len(a)):
                row = []
                for j in range(len(b[0])):
                    val = 0
                    for k in range(len(b)):
                        val += a[i][k] * b[k][j]
                    row.append(val)
                result.append(row)
            return result
        elif name == 'relu':
            x = args[0]
            return max(0, x)
        elif name == 'sigmoid':
            import math
            x = args[0]
            return 1 / (1 + math.exp(-x))
        elif name == 'tanh':
            import math
            x = args[0]
            return math.tanh(x)
        elif name == 'softmax':
            import math
            arr = args[0]
            exp_arr = [math.exp(x) for x in arr]
            sum_exp = sum(exp_arr)
            return [x / sum_exp for x in exp_arr]
        else:
            func = self.functions[name]
        if len(args) != len(func.params):
            raise ValueError("Function {} expects {} arguments, got {}".format(name, len(func.params), len(args)))
        local_env = dict(zip(func.params, args))
        result = self.execute_block(func.body, local_env)
        if isinstance(result, ReturnValue):
            return result.value
        return None

    def call_method(self, instance, method, args):
        if len(args) != len(method.params):
            raise ValueError("Method {} expects {} arguments, got {}".format(method.name, len(method.params), len(args)))
        local_env = dict(zip(method.params, args))
        local_env['self'] = instance
        result = self.execute_block(method.body, local_env)
        if isinstance(result, ReturnValue):
            return result.value
        return None

    def assign_expression(self, target, value, env):
        if isinstance(target, Variable):
            if target.name in env:
                env[target.name] = value
            elif target.name in self.global_env:
                self.global_env[target.name] = value
            else:
                raise ValueError("Undefined variable: {}".format(target.name))
        elif isinstance(target, Index):
            array = self.evaluate_expression(target.array, env)
            index = self.evaluate_expression(target.index, env)
            array[index] = value
        elif isinstance(target, AttributeAccess):
            obj = self.evaluate_expression(target.object_expr, env)
            obj.fields[target.attr] = value
        else:
            raise ValueError("Invalid assignment target")

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
            value = self.evaluate_expression(stmt.expr, env)
            self.assign_expression(stmt.target, value, env)
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
        elif isinstance(stmt, TryStatement):
            try:
                result = self.execute_block(stmt.try_block, env)
                if isinstance(result, ReturnValue):
                    return result
            except Exception as e:
                catch_env = dict(env)
                catch_env[stmt.catch_var] = str(e)
                result = self.execute_block(stmt.catch_block, catch_env)
                if isinstance(result, ReturnValue):
                    return result
        elif isinstance(stmt, ExpressionStatement):
            self.evaluate_expression(stmt.expr, env)
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
        elif isinstance(expr, AttributeAccess):
            obj = self.evaluate_expression(expr.object_expr, env)
            return obj.fields[expr.attr]
        elif isinstance(expr, MethodCall):
            obj = self.evaluate_expression(expr.object_expr, env)
            args = [self.evaluate_expression(arg, env) for arg in expr.args]
            method = next(m for m in obj.cls.methods if m.name == expr.method)
            return self.call_method(obj, method, args)
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
        elif isinstance(expr, NewExpression):
            cls = self.classes[expr.class_name]
            instance = VeyraInstance(cls)
            # Call constructor if exists
            if 'init' in [m.name for m in cls.methods]:
                init_method = next(m for m in cls.methods if m.name == 'init')
                args = [self.evaluate_expression(arg, env) for arg in expr.args]
                self.call_method(instance, init_method, args)
            return instance
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
        import os
        paths = [module_name, os.path.join('lib', module_name), os.path.join(os.path.dirname(__file__), 'lib', module_name), os.path.join(os.path.dirname(os.path.dirname(__file__)), 'src', 'veyra', 'lib', module_name)]
        for path in paths:
            if os.path.exists(path):
                try:
                    with open(path, 'r') as f:
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
                    for cls in module_ast.classes:
                        self.classes[cls.name] = cls
                    for stmt in module_ast.statements:
                        if isinstance(stmt, LetStatement):
                            value = self.evaluate_expression(stmt.expr, module_env)
                            self.global_env[stmt.name] = value
                        # Skip other statements for now
                    return
                except Exception as e:
                    raise ValueError('Error importing {}: {}'.format(module_name, e))
        raise ValueError('Module {} not found'.format(module_name))

class VeyraInstance:
    def __init__(self, cls):
        self.cls = cls
        self.fields = {}

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
