"""
Tests for Veyra lexer
"""

import pytest
from src.veyra.lexer import Lexer, Token


def test_basic_tokens():
    """Test basic token recognition"""
    lexer = Lexer("let x = 42;")
    tokens = lexer.tokenize()

    expected = [
        Token("KEYWORD", "let"),
        Token("IDENTIFIER", "x"),
        Token("OPERATOR", "="),
        Token("NUMBER", 42),
        Token("PUNCTUATION", ";"),
        Token("EOF", None)
    ]

    assert len(tokens) == len(expected)
    for actual, exp in zip(tokens, expected):
        assert actual.type == exp.type
        assert actual.value == exp.value


def test_string_tokens():
    """Test string tokenization"""
    lexer = Lexer('"hello world"')
    tokens = lexer.tokenize()

    assert len(tokens) == 2
    assert tokens[0].type == "STRING"
    assert tokens[0].value == "hello world"


def test_operators():
    """Test operator tokenization"""
    lexer = Lexer("x <= y && z >= 5")
    tokens = lexer.tokenize()

    expected_values = ["x", "<=", "y", "&&", "z", ">=", "5"]
    actual_values = [t.value for t in tokens[:-1]]  # exclude EOF

    assert actual_values == expected_values


def test_comments():
    """Test comment handling"""
    lexer = Lexer("// this is a comment\nlet x = 1;")
    tokens = lexer.tokenize()

    # Should skip comment and tokenize the rest
    assert len(tokens) == 5  # let x = 1 ; EOF
    assert tokens[0].value == "let"