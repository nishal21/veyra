#!/usr/bin/env python3
"""
Veyra Command Line Interface
"""

import sys
import os
from .interpreter import Interpreter
from .lexer import Lexer
from .parser import Parser
from .pm import VeyraPM

def main():
    """Main CLI entry point"""
    if len(sys.argv) < 2:
        print("Veyra Programming Language v0.1.0")
        print("Usage:")
        print("  veyra <file.veyra>    - Run a Veyra program")
        print("  veyra pm <command>    - Package manager commands")
        return

    command = sys.argv[1]

    if command.endswith('.veyra'):
        # Run a Veyra file
        run_file(command)
    elif command == 'pm':
        # Package manager
        if len(sys.argv) < 3:
            print("Package manager commands:")
            print("  veyra pm install <package>")
            print("  veyra pm list")
            return
        pm_command = sys.argv[2]
        pm = VeyraPM()
        if pm_command == 'install' and len(sys.argv) > 3:
            pm.install(sys.argv[3])
        elif pm_command == 'list':
            pm.list_packages()
        else:
            print("Unknown package manager command")
    else:
        print("Unknown command: {}".format(command))

def run_file(filename):
    """Run a Veyra file"""
    try:
        with open(filename, 'r') as f:
            text = f.read()

        lexer = Lexer(text)
        tokens = lexer.tokenize()

        parser = Parser(tokens)
        program = parser.parse()

        interpreter = Interpreter()
        interpreter.interpret(program)

    except FileNotFoundError:
        print("Error: File '{}' not found".format(filename))
        sys.exit(1)
    except Exception as e:
        print("Veyra Error: {}".format(e))
        sys.exit(1)

if __name__ == '__main__':
    main()