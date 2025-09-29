"""
Tests for Veyra parser
"""

import pytest
from src.veyra.lexer import Lexer
from src.veyra.parser import Parser


def test_simple_function():
    """Test parsing a simple function"""
    code = """
fn add(a, b) {
    return a + b;
}
"""
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()

    assert len(program.functions) == 1
    func = program.functions[0]
    assert func.name == "add"
    assert len(func.params) == 2
    assert func.params == ["a", "b"]


def test_variable_declaration():
    """Test parsing variable declarations"""
    code = "let x = 42;"
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()

    assert len(program.statements) == 1
    stmt = program.statements[0]
    assert stmt.__class__.__name__ == "LetStatement"
    assert stmt.name == "x"


def test_if_statement():
    """Test parsing if statements"""
    code = """
if x > 0 {
    return true;
} else {
    return false;
}
"""
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    program = parser.parse()

    assert len(program.statements) == 1
    stmt = program.statements[0]
    assert stmt.__class__.__name__ == "IfStatement"