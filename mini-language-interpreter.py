"""
Project Title: Mini Interpreter
Authors: Daniel Shkoratov , ID : 207167032 .
         Tal Yosef , ID : 316507516 .
         Ohad Maymon , ID : 319067310 .
         Tom Ben Baruch ID : 207339037 .

Description:
This script implements a mini interpreter for a simple programming language. It includes a lexer, parser, and interpreter
to evaluate basic expressions, function definitions, conditional statements, loops, and function calls.

The script consists of:
- Lexer: Tokenizes the input code.
- Parser: Converts tokens into an Abstract Syntax Tree (AST).
- Interpreter: Evaluates the AST and executes the code.

Usage:
Run the script and observe how it interprets different code snippets. You can modify the `main()` function to test different
inputs or extend the interpreter with new features.

"""


import re
from typing import List, Tuple

# Lexer
def lexer(code: str) -> List[Tuple[str, str]]:
    tokens = []
    token_specification = [
        ('KEYWORD', r'\b(if|else|while|for|func|return)\b'),
        ('IDENTIFIER', r'\b[a-zA-Z_][a-zA-Z0-9_]*\b'),
        ('NUMBER', r'\b\d+\b'),
        ('OPERATOR', r'(==|!=|&&|\|\||[+\-*/=%<>!])'),
        ('DELIMITER', r'[{}()\[\],]'),
        ('STRING', r'"[^"]*"'),
        ('WHITESPACE', r'\s+'),
        ('UNKNOWN', r'.')
    ]
    regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    for match in re.finditer(regex, code):
        kind = match.lastgroup
        value = match.group(kind)
        if kind == 'WHITESPACE':
            continue
        tokens.append((kind, value))
    return tokens

# AST Nodes
class ASTNode: pass

class ExpressionNode(ASTNode):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

class NumberNode(ASTNode):
    def __init__(self, value):
        self.value = value

class IdentifierNode(ASTNode):
    def __init__(self, name):
        self.name = name

class AssignmentNode(ASTNode):
    def __init__(self, identifier, expression):
        self.identifier = identifier
        self.expression = expression

class IfNode(ASTNode):
    def __init__(self, condition, true_block, false_block=None):
        self.condition = condition
        self.true_block = true_block
        self.false_block = false_block

class WhileNode(ASTNode):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

class BlockNode(ASTNode):
    def __init__(self, statements):
        self.statements = statements

class FunctionNode(ASTNode):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

class ReturnNode(ASTNode):
    def __init__(self, value):
        self.value = value

class FunctionCallNode(ASTNode):
    def __init__(self, identifier, arguments):
        self.identifier = identifier
        self.arguments = arguments


# Parser
class Parser:
    def __init__(self, tokens: List[Tuple[str, str]]):
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> ASTNode:
        return self.parse_block()

    def parse_block(self) -> BlockNode:
        statements = []
        while self.pos < len(self.tokens) and self.tokens[self.pos][1] != '}':
            if self.tokens[self.pos][1] == ';':
                self.pos += 1
            else:
                statement = self.parse_statement()
                statements.append(statement)
        return BlockNode(statements)

    def parse_statement(self) -> ASTNode:
        token_type, value = self.tokens[self.pos]

        if token_type == 'KEYWORD':
            if value == 'if':
                return self.parse_if()
            elif value == 'while':
                return self.parse_while()
            elif value == 'return':
                return self.parse_return()
            elif value == 'func':
                return self.parse_function()
        elif token_type == 'IDENTIFIER':
            if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1][1] == '(':
                return self.parse_function_call()  # Handle function calls
            else:
                return self.parse_assignment()
        else:
            self.pos += 1  # Advance position to avoid infinite loop
            return None

    def parse_if(self) -> IfNode:
        self.pos += 1
        condition = self.parse_expression()
        self.expect('{')
        true_block = self.parse_block()
        self.expect('}')
        false_block = None
        if self.pos < len(self.tokens) and self.tokens[self.pos][1] == 'else':
            self.pos += 1
            self.expect('{')
            false_block = self.parse_block()
            self.expect('}')
        return IfNode(condition, true_block, false_block)

    def parse_while(self) -> WhileNode:
        self.pos += 1
        condition = self.parse_expression()
        self.expect('{')
        body = self.parse_block()
        self.expect('}')
        return WhileNode(condition, body)

    def parse_function(self) -> FunctionNode:
        self.pos += 1
        name = self.tokens[self.pos][1]
        self.pos += 1
        self.expect('(')
        params = []
        while self.tokens[self.pos][1] != ')':
            params.append(self.tokens[self.pos][1])
            self.pos += 1
            if self.tokens[self.pos][1] == ',':
                self.pos += 1
        self.pos += 1
        self.expect('{')
        body = self.parse_block()
        self.expect('}')
        return FunctionNode(name, params, body)

    def parse_return(self) -> ReturnNode:
        self.pos += 1
        value = self.parse_expression()
        return ReturnNode(value)

    def parse_assignment(self) -> AssignmentNode:
        identifier = self.tokens[self.pos][1]
        self.pos += 1
        self.expect('=')
        expression = self.parse_expression()
        return AssignmentNode(IdentifierNode(identifier), expression)

    def parse_function_call(self) -> FunctionCallNode:
        identifier = self.tokens[self.pos][1]  # Get the function name
        self.pos += 1  # Move past the function name
        self.expect('(')  # Expect and move past '('

        arguments = []
        while self.tokens[self.pos][1] != ')':  # Loop until you reach ')'
            arguments.append(self.parse_expression())  # Parse each argument
            if self.tokens[self.pos][1] == ',':
                self.pos += 1  # Skip over the ',' between arguments

        self.pos += 1  # Move past ')'
        return FunctionCallNode(IdentifierNode(identifier), arguments)

    def parse_expression(self) -> ASTNode:
        left = self.parse_term()
        while self.pos < len(self.tokens) and self.tokens[self.pos][0] == 'OPERATOR':
            operator = self.tokens[self.pos][1]
            self.pos += 1
            right = self.parse_term()
            left = ExpressionNode(left, operator, right)
        return left

    def parse_term(self) -> ASTNode:
        token_type, value = self.tokens[self.pos]
        if token_type == 'NUMBER':
            self.pos += 1
            return NumberNode(value)
        elif token_type == 'IDENTIFIER':
            if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1][1] == '(':
                return self.parse_function_call()
            self.pos += 1
            return IdentifierNode(value)
        elif value == '(':
            self.pos += 1
            expression = self.parse_expression()
            self.expect(')')
            return expression

    def expect(self, expected_value):
        if self.tokens[self.pos][1] != expected_value:
            raise ValueError(f"Expected '{expected_value}' but got: {self.tokens[self.pos][1]}")
        self.pos += 1

# Interpreter
# Interpreter
class Interpreter:
    def __init__(self):
        self.env = {}

    def interpret(self, node: ASTNode):
        if isinstance(node, BlockNode):
            return self.eval_block(node)
        elif isinstance(node, IfNode):
            return self.eval_if(node)
        elif isinstance(node, WhileNode):
            return self.eval_while(node)
        elif isinstance(node, AssignmentNode):
            return self.eval_assignment(node)
        elif isinstance(node, ExpressionNode):
            return self.eval_expression(node)
        elif isinstance(node, NumberNode):
            return int(node.value)
        elif isinstance(node, IdentifierNode):
            return self.env.get(node.name, 0)
        elif isinstance(node, FunctionNode):
            return self.eval_function(node)
        elif isinstance(node, FunctionCallNode):
            return self.call_function(node.identifier.name, node.arguments)
        elif isinstance(node, ReturnNode):
            return self.eval_return(node)

    def eval_block(self, node: BlockNode):
        for statement in node.statements:
            result = self.interpret(statement)
            if isinstance(statement, ReturnNode):
                return result

    def eval_if(self, node: IfNode):
        condition_value = self.interpret(node.condition)
        if condition_value:
            return self.interpret(node.true_block)
        elif node.false_block:
            return self.interpret(node.false_block)

    def eval_while(self, node: WhileNode):
        while self.interpret(node.condition):
            self.interpret(node.body)

    def eval_assignment(self, node: AssignmentNode):
        value = self.interpret(node.expression)
        self.env[node.identifier.name] = value

    def eval_expression(self, node: ExpressionNode):
        left = self.interpret(node.left)
        right = self.interpret(node.right)
        if node.operator == '+':
            return left + right
        elif node.operator == '-':
            return left - right
        elif node.operator == '*':
            return left * right
        elif node.operator == '/':
            if right == 0:
                raise ValueError("Division by zero")
            return left // right
        elif node.operator == '%':
            return left % right
        elif node.operator == '>':
            return left > right
        elif node.operator == '<':
            return left < right
        elif node.operator == '==':
            return left == right
        elif node.operator == '!=':
            return left != right

    def eval_function(self, node: FunctionNode):
        # Store the function definition in the environment
        self.env[node.name] = node

    def eval_return(self, node: ReturnNode):
        return self.interpret(node.value)

    def call_function(self, name, args):
        function = self.env.get(name)
        if not function:
            raise ValueError(f"Function '{name}' not defined")

        # Save the current environment
        previous_env = self.env.copy()

        # Set up the function's local environment
        for param, arg in zip(function.params, args):
            self.env[param] = self.interpret(arg)

        # Execute the function and capture the result
        result = self.interpret(function.body)

        # Restore the previous environment
        self.env = previous_env

        return result


def main():
    def test_interpreter(code: str, description: str):
        print(f"\n{description}")
        print("Code being tested:")
        print(code.strip())
        tokens = lexer(code)
        parser = Parser(tokens)
        program = parser.parse()
        interpreter = Interpreter()
        interpreter.interpret(program)
        print("Final Environment:", interpreter.env)

    # Test basic arithmetic and variable assignments
    code1 = '''
    m = 20 + 5
    n = m * 2
    o = n / 3
    p = o - 2
    q = 15 % 4
    '''
    test_interpreter(code1, "Testing Code 1: Basic arithmetic and variable assignments")

    # Test variable assignments and arithmetic
    code2 = '''
    x = 10
    y = x + 5
    z = y * 2
    '''
    test_interpreter(code2, "Testing Code 2: Variable assignments and arithmetic")

    # Test conditional statements (if-else)
    code3 = '''
    a = 10
    b = 5
    if a > b {
        c = a + b
    } else {
        c = a - b
    }
    d = 7
    if d == 7 {
        e = d * 2
    } else {
        e = d + 2
    }
    '''
    test_interpreter(code3, "Testing Code 3: Conditional statements (if-else)")

    # Test loop constructs (while)
    code4 = '''
    i = 0
    sum = 0
    while i < 5 {
        sum = sum + i
        i = i + 1
    }
    '''
    test_interpreter(code4, "Testing Code 4: Loop constructs (while)")

    # Test function definition and calls
    code5 = '''
    func add(a, b) {
        return a + b
    }
    result = add(3, 4)
    '''
    test_interpreter(code5, "Testing Code 5: Function definition and function calls")

    # Test more complex function with loop and return
    code6 = '''
    func gcd(a, b) {
        while b != 0 {
            temp = b
            b = a % b
            a = temp
        }
        return a
    }
    m = 20
    n = 8
    result_gcd = gcd(m, n)
    '''
    test_interpreter(code6, "Testing Code 6: Function with loop and return value (GCD calculation)")

if __name__ == "__main__":
    main()


