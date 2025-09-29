"""
Veyra Programming Language

A modern programming language designed for web development and rapid prototyping,
featuring built-in web capabilities and a clean, readable syntax.
"""

__version__ = "0.1.1"
__author__ = "Nishal"
__email__ = "nishalamv@gmail.com"

from .interpreter import Interpreter
from .lexer import Lexer
from .parser import Parser
from .pm import VeyraPM

__all__ = ['Interpreter', 'Lexer', 'Parser', 'VeyraPM']