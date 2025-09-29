"""
Tests for Veyra interpreter
"""

import pytest
from src.veyra.lexer import Lexer
from src.veyra.parser import Parser
from src.veyra.interpreter import Interpreter


def test_arithmetic():
    """Test basic arithmetic operations"""
    code = """
fn main() {
    let result = 2 + 3 * 4;
    println(result);
}
"""
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()
    interpreter = Interpreter()

    # Capture print output
    import io
    import sys
    captured_output = io.StringIO()
    sys.stdout = captured_output

    interpreter.interpret(program)

    sys.stdout = sys.__stdout__
    output = captured_output.getvalue()

    assert "14" in output  # 2 + (3 * 4) = 14


def test_function_call():
    """Test function calls"""
    code = """
fn add(a, b) {
    return a + b;
}

fn main() {
    let result = add(5, 3);
    println(result);
}
"""
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()
    interpreter = Interpreter()

    import io
    import sys
    captured_output = io.StringIO()
    sys.stdout = captured_output

    interpreter.interpret(program)

    sys.stdout = sys.__stdout__
    output = captured_output.getvalue()

    assert "8" in output


def test_builtin_functions():
    """Test built-in functions"""
    code = """
fn main() {
    let result = abs(-5);
    println(result);
}
"""
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()
    interpreter = Interpreter()

    import io
    import sys
    captured_output = io.StringIO()
    sys.stdout = captured_output

    interpreter.interpret(program)

    sys.stdout = sys.__stdout__
    output = captured_output.getvalue()

    assert "5" in output