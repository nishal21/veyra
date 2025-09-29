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

def run_file(filename):
    """Run a Veyra file"""
    if not os.path.exists(filename):
        print("File not found: {}".format(filename))
        return
    with open(filename, 'r') as f:
        code = f.read()
    try:
        lexer = Lexer(code)
        tokens = lexer.tokenize()
        parser = Parser(tokens)
        program = parser.parse()
        interpreter = Interpreter()
        interpreter.interpret(program)
    except Exception as e:
        print("Error: {}".format(e))

def main():
    """Main CLI entry point"""
    if len(sys.argv) < 2:
        print("Veyra Programming Language v0.1.0")
        print("Usage:")
        print("  veyra <file.veyra>    - Run a Veyra program")
        print("  veyra repl            - Start interactive REPL")
        print("  veyra pm <command>    - Package manager commands")
        return

    command = sys.argv[1]

    if command.endswith('.veyra'):
        # Run a Veyra file
        run_file(command)
    elif command == 'repl':
        # Start REPL
        start_repl()
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

def start_repl():
    """Start the interactive REPL"""
    print("Veyra REPL v0.1.0")
    print("Type 'exit' to quit, or enter multi-line code ending with empty line")
    interpreter = Interpreter()
    while True:
        try:
            code = ""
            while True:
                line = input("veyra> " if not code else "...> ")
                if line.strip() == 'exit':
                    return
                code += line + "\n"
                if not line.strip():
                    break
            if code.strip():
                lexer = Lexer(code)
                tokens = lexer.tokenize()
                parser = Parser(tokens)
                program = parser.parse()
                interpreter.interpret(program)
        except KeyboardInterrupt:
            print("\nExiting REPL")
            break
        except Exception as e:
            print("Error: {}".format(e))

if __name__ == '__main__':
    main()