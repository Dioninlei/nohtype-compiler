#!/usr/bin/env python3
"""
Reverse Python Compiler (RPC)
A compiler for the "Reverse Python" language - a simple esoteric language where Python keywords are reversed.
File extension: .rpc

Author: [Your Name]
Date: May 6, 2025
"""

import re
import sys
import os

class Token:
    """Token class representing the smallest unit of the language"""
    def __init__(self, token_type, value, line_num=None, col_num=None):
        self.type = token_type
        self.value = value
        self.line_num = line_num
        self.col_num = col_num
    
    def __str__(self):
        return f"Token({self.type}, {self.value}, line:{self.line_num}, col:{self.col_num})"
    
    def __repr__(self):
        return self.__str__()

class Lexer:
    """Lexical analyzer for Reverse Python language"""
    
    # Define token types
    TOKEN_TYPES = {
        'KEYWORD': ['tnirp', 'rof', 'ni', 'elihw', 'fi', 'esle', 'tupni', 'fed', 'nruter', 'kaerb', 'eunitnoc', 'ssap'],
        'OPERATOR': ['+', '-', '*', '/', '%', '=', '==', '!=', '<', '>', '<=', '>=', '+=', '-=', '*=', '/='],
        'DELIMITER': ['(', ')', '[', ']', '{', '}', ':', ',', '.', ';'],
        'LITERAL': ['STRING', 'INTEGER', 'FLOAT', 'BOOLEAN'],
        'IDENTIFIER': 'IDENTIFIER',
        'INDENT': 'INDENT',
        'DEDENT': 'DEDENT',
        'NEWLINE': 'NEWLINE',
        'EOF': 'EOF'
    }
    
    def __init__(self, source_code):
        self.source_code = source_code
        self.position = 0
        self.line_num = 1
        self.col_num = 1
        self.current_char = self.source_code[0] if len(self.source_code) > 0 else None
        self.indent_stack = [0]  # Stack to keep track of indentation levels
        self.tokens = []
    
    def advance(self):
        """Move to the next character in the source code"""
        self.position += 1
        self.col_num += 1
        
        if self.position >= len(self.source_code):
            self.current_char = None
        else:
            self.current_char = self.source_code[self.position]
            
            if self.current_char == '\n':
                self.line_num += 1
                self.col_num = 0
    
    def peek(self, n=1):
        """Look ahead n characters without advancing"""
        peek_pos = self.position + n
        if peek_pos >= len(self.source_code):
            return None
        return self.source_code[peek_pos]
    
    def skip_whitespace(self):
        """Skip whitespace characters except for newlines and indentation at start of line"""
        # Only skip spaces and tabs if not at the beginning of a line
        if self.col_num > 1:
            while self.current_char is not None and self.current_char in ' \t':
                self.advance()
    
    def handle_indentation(self):
        """Handle Python-style indentation to generate INDENT and DEDENT tokens"""
        # Count spaces at the beginning of the line
        indent_level = 0
        while self.current_char is not None and self.current_char in ' \t':
            if self.current_char == ' ':
                indent_level += 1
            else:  # Tab character
                indent_level += 4  # Assuming tabs are 4 spaces
            self.advance()
        
        # Compare with the current indentation level
        current_indent = self.indent_stack[-1]
        
        if indent_level > current_indent:
            # Indentation increased
            self.indent_stack.append(indent_level)
            self.tokens.append(Token('INDENT', indent_level, self.line_num, self.col_num))
        
        elif indent_level < current_indent:
            # Indentation decreased, maybe more than one level
            while indent_level < self.indent_stack[-1]:
                self.indent_stack.pop()
                self.tokens.append(Token('DEDENT', indent_level, self.line_num, self.col_num))
                
            # Check if we have a valid indentation level
            if indent_level != self.indent_stack[-1]:
                raise SyntaxError(f"Invalid indentation at line {self.line_num}")
    
    def skip_comment(self):
        """Skip comments (# in regular Python, # in Reverse Python too)"""
        while self.current_char is not None and self.current_char != '\n':
            self.advance()
    
    def get_identifier_or_keyword(self):
        """Get an identifier or keyword"""
        start_col = self.col_num
        result = ''
        
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        
        # Check if it's a keyword
        if result in self.TOKEN_TYPES['KEYWORD']:
            return Token('KEYWORD', result, self.line_num, start_col)
        # Otherwise, it's an identifier
        else:
            return Token('IDENTIFIER', result, self.line_num, start_col)
    
    def get_number(self):
        """Get a number (integer or float)"""
        start_col = self.col_num
        result = ''
        is_float = False
        
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if is_float:  # Second decimal point encountered
                    break
                is_float = True
            result += self.current_char
            self.advance()
        
        if is_float:
            return Token('LITERAL', float(result), self.line_num, start_col)
        else:
            return Token('LITERAL', int(result), self.line_num, start_col)
    
    def get_string(self):
        """Get a string literal"""
        start_col = self.col_num
        quote_char = self.current_char  # Either ' or "
        self.advance()  # Skip the opening quote
        result = ''
        
        while self.current_char is not None and self.current_char != quote_char:
            # Handle escape sequences
            if self.current_char == '\\' and self.peek() == quote_char:
                self.advance()  # Skip the backslash
            
            result += self.current_char
            self.advance()
        
        if self.current_char is None:
            raise SyntaxError(f"Unterminated string at line {self.line_num}")
        
        self.advance()  # Skip the closing quote
        return Token('LITERAL', result, self.line_num, start_col)
    
    def tokenize(self):
        """Convert source code into tokens"""
        while self.current_char is not None:
            # Handle indentation at the beginning of lines
            if self.col_num == 1:
                self.handle_indentation()
            
            # Skip whitespace
            if self.current_char in ' \t':
                self.skip_whitespace()
                continue
            
            # Handle newlines
            if self.current_char == '\n':
                self.tokens.append(Token('NEWLINE', '\\n', self.line_num, self.col_num))
                self.advance()
                continue
            
            # Handle comments
            if self.current_char == '#':
                self.skip_comment()
                continue
            
            # Handle identifiers and keywords
            if self.current_char.isalpha() or self.current_char == '_':
                self.tokens.append(self.get_identifier_or_keyword())
                continue
            
            # Handle numbers
            if self.current_char.isdigit():
                self.tokens.append(self.get_number())
                continue
            
            # Handle strings
            if self.current_char in ["'", '"']:
                self.tokens.append(self.get_string())
                continue
            
            # Handle operators and delimiters
            if self.current_char in '+-*/%=!<>()[]{}:,.;':
                # Check for two-character operators
                if self.current_char in ['=', '!', '<', '>'] and self.peek() == '=':
                    operator = self.current_char + self.peek()
                    self.tokens.append(Token('OPERATOR', operator, self.line_num, self.col_num))
                    self.advance()
                    self.advance()
                # Check for assignment operators (+=, -=, etc.)
                elif self.current_char in ['+', '-', '*', '/'] and self.peek() == '=':
                    operator = self.current_char + self.peek()
                    self.tokens.append(Token('OPERATOR', operator, self.line_num, self.col_num))
                    self.advance()
                    self.advance()
                else:
                    # Single character operator or delimiter
                    char_type = 'OPERATOR' if self.current_char in '+-*/%=!<>' else 'DELIMITER'
                    self.tokens.append(Token(char_type, self.current_char, self.line_num, self.col_num))
                    self.advance()
                continue
            
            # If we get here, we encountered an unexpected character
            raise SyntaxError(f"Unexpected character '{self.current_char}' at line {self.line_num}, column {self.col_num}")
        
        # Add EOF token
        self.tokens.append(Token('EOF', None, self.line_num, self.col_num))
        
        # Add any remaining DEDENTs if needed
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            self.tokens.append(Token('DEDENT', 0, self.line_num, self.col_num))
        
        return self.tokens

class ASTNode:
    """Base class for AST nodes"""
    pass

class Program(ASTNode):
    """Root node of the AST"""
    def __init__(self, statements):
        self.statements = statements

class Statement(ASTNode):
    """Base class for statement nodes"""
    pass

class PrintStatement(Statement):
    """tnirp statement node"""
    def __init__(self, expression):
        self.expression = expression

class InputStatement(Statement):
    """tupni statement node"""
    def __init__(self, variable):
        self.variable = variable

class AssignmentStatement(Statement):
    """Assignment statement node"""
    def __init__(self, variable, expression):
        self.variable = variable
        self.expression = expression

class IfStatement(Statement):
    """fi statement node"""
    def __init__(self, condition, if_block, else_block=None):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

class WhileStatement(Statement):
    """elihw statement node"""
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

class ForStatement(Statement):
    """rof statement node"""
    def __init__(self, variable, iterable, block):
        self.variable = variable
        self.iterable = iterable
        self.block = block

class FunctionDefinition(Statement):
    """fed statement node"""
    def __init__(self, name, parameters, body):
        self.name = name
        self.parameters = parameters
        self.body = body

class ReturnStatement(Statement):
    """nruter statement node"""
    def __init__(self, expression=None):
        self.expression = expression

class Expression(ASTNode):
    """Base class for expression nodes"""
    pass

class BinaryOperation(Expression):
    """Binary operation node"""
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

class UnaryOperation(Expression):
    """Unary operation node"""
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

class Literal(Expression):
    """Literal value node"""
    def __init__(self, value):
        self.value = value

class Identifier(Expression):
    """Variable or function name node"""
    def __init__(self, name):
        self.name = name

class FunctionCall(Expression):
    """Function call node"""
    def __init__(self, function, arguments):
        self.function = function
        self.arguments = arguments

class Parser:
    """Syntax analyzer for Reverse Python language"""
    
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_token_idx = 0
        self.current_token = self.tokens[0]
    
    def advance(self):
        """Move to the next token"""
        self.current_token_idx += 1
        if self.current_token_idx < len(self.tokens):
            self.current_token = self.tokens[self.current_token_idx]
    
    def eat(self, token_type):
        """Consume a token of a specific type"""
        if self.current_token.type == token_type:
            token = self.current_token
            self.advance()
            return token
        else:
            raise SyntaxError(f"Expected {token_type} but got {self.current_token.type} at line {self.current_token.line_num}")
    
    def peek(self, n=1):
        """Look ahead n tokens without advancing"""
        peek_idx = self.current_token_idx + n
        if peek_idx >= len(self.tokens):
            return None
        return self.tokens[peek_idx]
    
    def parse(self):
        """Parse the tokens into an AST"""
        program = self.program()
        if self.current_token.type != 'EOF':
            raise SyntaxError(f"Expected EOF but got {self.current_token.type} at line {self.current_token.line_num}")
        return program
    
    def program(self):
        """Parse the whole program"""
        statements = []
        
        while self.current_token.type != 'EOF':
            if self.current_token.type == 'NEWLINE':
                self.advance()
                continue
            
            statements.append(self.statement())
        
        return Program(statements)
    
    def statement(self):
        """Parse a statement"""
        if self.current_token.type == 'KEYWORD':
            if self.current_token.value == 'tnirp':
                return self.print_statement()
            elif self.current_token.value == 'tupni':
                return self.input_statement()
            elif self.current_token.value == 'fi':
                return self.if_statement()
            elif self.current_token.value == 'elihw':
                return self.while_statement()
            elif self.current_token.value == 'rof':
                return self.for_statement()
            elif self.current_token.value == 'fed':
                return self.function_definition()
            elif self.current_token.value == 'nruter':
                return self.return_statement()
            elif self.current_token.value in ['kaerb', 'eunitnoc', 'ssap']:
                keyword = self.eat('KEYWORD').value
                self.eat_newline_or_eof()
                if keyword == 'kaerb':
                    return 'break'
                elif keyword == 'eunitnoc':
                    return 'continue'
                else:  # ssap
                    return 'pass'
        
        # If not a keyword, must be an assignment or expression statement
        expr = self.expression()
        
        # Check if it's an assignment
        if self.current_token.type == 'OPERATOR' and self.current_token.value == '=':
            self.eat('OPERATOR')  # Consume '='
            value = self.expression()
            self.eat_newline_or_eof()
            return AssignmentStatement(expr, value)
        
        # Otherwise, it's an expression statement (like a function call)
        self.eat_newline_or_eof()
        return expr
    
    def eat_newline_or_eof(self):
        """Consume a newline or EOF token"""
        if self.current_token.type == 'NEWLINE':
            self.eat('NEWLINE')
        elif self.current_token.type == 'EOF':
            pass  # Don't consume EOF
        else:
            raise SyntaxError(f"Expected newline or EOF but got {self.current_token.type} at line {self.current_token.line_num}")
    
    def print_statement(self):
        """Parse a tnirp statement"""
        self.eat('KEYWORD')  # Consume 'tnirp'
        self.eat('DELIMITER')  # Consume '('
        expr = self.expression()
        self.eat('DELIMITER')  # Consume ')'
        self.eat_newline_or_eof()
        return PrintStatement(expr)
    
    def input_statement(self):
        """Parse a tupni statement"""
        self.eat('KEYWORD')  # Consume 'tupni'
        self.eat('DELIMITER')  # Consume '('
        expr = self.expression()
        self.eat('DELIMITER')  # Consume ')'
        self.eat_newline_or_eof()
        return InputStatement(expr)
    
    def if_statement(self):
        """Parse an fi statement"""
        self.eat('KEYWORD')  # Consume 'fi'
        condition = self.expression()
        self.eat('DELIMITER')  # Consume ':'
        
        # Parse the if block (indented)
        self.eat('NEWLINE')
        self.eat('INDENT')
        if_block = []
        
        while self.current_token.type != 'DEDENT' and self.current_token.type != 'EOF':
            if self.current_token.type == 'NEWLINE':
                self.advance()
                continue
            
            if_block.append(self.statement())
        
        # Consume DEDENT
        if self.current_token.type == 'DEDENT':
            self.eat('DEDENT')
        
        # Check for esle (else)
        else_block = None
        if self.current_token.type == 'KEYWORD' and self.current_token.value == 'esle':
            self.eat('KEYWORD')  # Consume 'esle'
            self.eat('DELIMITER')  # Consume ':'
            
            # Parse the else block (indented)
            self.eat('NEWLINE')
            self.eat('INDENT')
            else_block = []
            
            while self.current_token.type != 'DEDENT' and self.current_token.type != 'EOF':
                if self.current_token.type == 'NEWLINE':
                    self.advance()
                    continue
                
                else_block.append(self.statement())
            
            # Consume DEDENT
            if self.current_token.type == 'DEDENT':
                self.eat('DEDENT')
        
        return IfStatement(condition, if_block, else_block)
    
    def while_statement(self):
        """Parse an elihw statement"""
        self.eat('KEYWORD')  # Consume 'elihw'
        condition = self.expression()
        self.eat('DELIMITER')  # Consume ':'
        
        # Parse the while block (indented)
        self.eat('NEWLINE')
        self.eat('INDENT')
        block = []
        
        while self.current_token.type != 'DEDENT' and self.current_token.type != 'EOF':
            if self.current_token.type == 'NEWLINE':
                self.advance()
                continue
            
            block.append(self.statement())
        
        # Consume DEDENT
        if self.current_token.type == 'DEDENT':
            self.eat('DEDENT')
        
        return WhileStatement(condition, block)
    
    def for_statement(self):
        """Parse a rof statement"""
        self.eat('KEYWORD')  # Consume 'rof'
        variable = self.eat('IDENTIFIER').value
        
        # Consume 'ni' (in)
        if self.current_token.type != 'KEYWORD' or self.current_token.value != 'ni':
            raise SyntaxError(f"Expected 'ni' but got {self.current_token.value} at line {self.current_token.line_num}")
        self.eat('KEYWORD')
        
        iterable = self.expression()
        self.eat('DELIMITER')  # Consume ':'
        
        # Parse the for block (indented)
        self.eat('NEWLINE')
        self.eat('INDENT')
        block = []
        
        while self.current_token.type != 'DEDENT' and self.current_token.type != 'EOF':
            if self.current_token.type == 'NEWLINE':
                self.advance()
                continue
            
            block.append(self.statement())
        
        # Consume DEDENT
        if self.current_token.type == 'DEDENT':
            self.eat('DEDENT')
        
        return ForStatement(variable, iterable, block)
    
    def function_definition(self):
        """Parse a fed statement"""
        self.eat('KEYWORD')  # Consume 'fed'
        name = self.eat('IDENTIFIER').value
        
        # Parse parameters
        self.eat('DELIMITER')  # Consume '('
        parameters = []
        
        if self.current_token.type != 'DELIMITER' or self.current_token.value != ')':
            # There are parameters
            parameters.append(self.eat('IDENTIFIER').value)
            
            while self.current_token.type == 'DELIMITER' and self.current_token.value == ',':
                self.eat('DELIMITER')  # Consume ','
                parameters.append(self.eat('IDENTIFIER').value)
        
        self.eat('DELIMITER')  # Consume ')'
        self.eat('DELIMITER')  # Consume ':'
        
        # Parse the function body (indented)
        self.eat('NEWLINE')
        self.eat('INDENT')
        body = []
        
        while self.current_token.type != 'DEDENT' and self.current_token.type != 'EOF':
            if self.current_token.type == 'NEWLINE':
                self.advance()
                continue
            
            body.append(self.statement())
        
        # Consume DEDENT
        if self.current_token.type == 'DEDENT':
            self.eat('DEDENT')
        
        return FunctionDefinition(name, parameters, body)
    
    def return_statement(self):
        """Parse a nruter statement"""
        self.eat('KEYWORD')  # Consume 'nruter'
        
        # Check if there's an expression after 'nruter'
        expression = None
        if self.current_token.type != 'NEWLINE' and self.current_token.type != 'EOF':
            expression = self.expression()
        
        self.eat_newline_or_eof()
        return ReturnStatement(expression)
    
    def expression(self):
        """Parse an expression"""
        return self.logical_or()
    
    def logical_or(self):
        """Parse logical OR expressions"""
        expr = self.logical_and()
        
        while self.current_token.type == 'KEYWORD' and self.current_token.value == 'ro':
            self.eat('KEYWORD')  # Consume 'ro'
            right = self.logical_and()
            expr = BinaryOperation(expr, 'or', right)
        
        return expr
    
    def logical_and(self):
        """Parse logical AND expressions"""
        expr = self.equality()
        
        while self.current_token.type == 'KEYWORD' and self.current_token.value == 'dna':
            self.eat('KEYWORD')  # Consume 'dna'
            right = self.equality()
            expr = BinaryOperation(expr, 'and', right)
        
        return expr
    
    def equality(self):
        """Parse equality expressions"""
        expr = self.comparison()
        
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['==', '!=']:
            operator = self.eat('OPERATOR').value
            right = self.comparison()
            expr = BinaryOperation(expr, operator, right)
        
        return expr
    
    def comparison(self):
        """Parse comparison expressions"""
        expr = self.term()
        
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['<', '>', '<=', '>=']:
            operator = self.eat('OPERATOR').value
            right = self.term()
            expr = BinaryOperation(expr, operator, right)
        
        return expr
    
    def term(self):
        """Parse addition and subtraction"""
        expr = self.factor()
        
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['+', '-']:
            operator = self.eat('OPERATOR').value
            right = self.factor()
            expr = BinaryOperation(expr, operator, right)
        
        return expr
    
    def factor(self):
        """Parse multiplication and division"""
        expr = self.unary()
        
        while self.current_token.type == 'OPERATOR' and self.current_token.value in ['*', '/', '%']:
            operator = self.eat('OPERATOR').value
            right = self.unary()
            expr = BinaryOperation(expr, operator, right)
        
        return expr
    
    def unary(self):
        """Parse unary operations"""
        if self.current_token.type == 'OPERATOR' and self.current_token.value in ['+', '-', '!']:
            operator = self.eat('OPERATOR').value
            operand = self.unary()
            return UnaryOperation(operator, operand)
        else:
            return self.primary()
    
    def primary(self):
        """Parse primary expressions"""
        if self.current_token.type == 'LITERAL':
            value = self.eat('LITERAL').value
            return Literal(value)
        
        elif self.current_token.type == 'IDENTIFIER':
            identifier = self.eat('IDENTIFIER').value
            
            # Check if it's a function call
            if self.current_token.type == 'DELIMITER' and self.current_token.value == '(':
                self.eat('DELIMITER')  # Consume '('
                arguments = []
                
                if self.current_token.type != 'DELIMITER' or self.current_token.value != ')':
                    # There are arguments
                    arguments.append(self.expression())
                    
                    while self.current_token.type == 'DELIMITER' and self.current_token.value == ',':
                        self.eat('DELIMITER')  # Consume ','
                        arguments.append(self.expression())
                
                self.eat('DELIMITER')  # Consume ')'
                return FunctionCall(identifier, arguments)
            
            # Otherwise, it's a variable reference
            return Identifier(identifier)
        
        elif self.current_token.type == 'DELIMITER' and self.current_token.value == '(':
            self.eat('DELIMITER')  # Consume '('
            expr = self.expression()
            self.eat('DELIMITER')  # Consume ')'
            return expr
        
        else:
            raise SyntaxError(f"Unexpected token {self.current_token.type} at line {self.current_token.line_num}")

class SemanticAnalyzer:
    """Performs semantic analysis on the AST"""
    
    def __init__(self):
        self.symbol_table = {}  # Symbol table for variables and functions
        self.errors = []
    
    def analyze(self, ast):
        """Analyze the AST for semantic errors"""
        if isinstance(ast, Program):
            for statement in ast.statements:
                self.analyze_statement(statement)
        
        return self.errors
    
    def analyze_statement(self, statement):
        """Analyze a statement node"""
        if isinstance(statement, PrintStatement):
            self.analyze_expression(statement.expression)
        
        elif isinstance(statement, InputStatement):
            if not isinstance(statement.variable, Identifier):
                self.errors.append(f"Input target must be a variable")
            else:
                # Add variable to symbol table
                self.symbol_table[statement.variable.name] = 'variable'
        
        elif isinstance(statement, AssignmentStatement):
            if not isinstance(statement.variable, Identifier):
                self.errors.append(f"Assignment target must be a variable")
            else:
                # Add variable to symbol table
                self.symbol_table[statement.variable.name] = 'variable'
            
            self.analyze_expression(statement.expression)
        
        elif isinstance(statement, IfStatement):
            self.analyze_expression(statement.condition)
            
            for stmt in statement.if_block:
                self.analyze_statement(stmt)
            
            if statement.else_block:
                for stmt in statement.else_block:
                    self.analyze_statement(stmt)
        
        elif isinstance(statement, WhileStatement):
            self.analyze_expression(statement.condition)
            
            for stmt in statement.block:
                self.analyze_statement(stmt)
        
        elif isinstance(statement, ForStatement):
            # Add loop variable to symbol table
            self.symbol_table[statement.variable] = 'variable'
            
            self.analyze_expression(statement.iterable)
            
            for stmt in statement.block:
                self.analyze_statement(stmt)
        
        elif isinstance(statement, FunctionDefinition):
            # Add function to symbol table
            self.symbol_table[statement.name] = 'function'
            
            # Create a new scope for parameters
            old_symbol_table = self.symbol_table.copy()
            
            # Add parameters to symbol table
            for param in statement.parameters:
                self.symbol_table[param] = 'variable'
            
            # Analyze function body
            for stmt in statement.body:
                self.analyze_statement(stmt)
            
            # Restore old scope
            self.symbol_table = old_symbol_table
        
        elif isinstance(statement, ReturnStatement):
            if statement.expression:
                self.analyze_expression(statement.expression)
    
    def analyze_expression(self, expression):
        """Analyze an expression node"""
        if isinstance(expression, BinaryOperation):
            self.analyze_expression(expression.left)
            self.analyze_expression(expression.right)
        
        elif isinstance(expression, UnaryOperation):
            self.analyze_expression(expression.operand)
        
        elif isinstance(expression, Identifier):
            # Check if variable exists
            if expression.name not in self.symbol_table:
                self.errors.append(f"Undefined variable '{expression.name}'")
        
        elif isinstance(expression, FunctionCall):
            # Check if function exists
            if expression.function not in self.symbol_table:
                self.errors.append(f"Undefined function '{expression.function}'")
            elif self.symbol_table[expression.function] != 'function':
                self.errors.append(f"'{expression.function}' is not a function")
            
            # Analyze arguments
            for arg in expression.arguments:
                self.analyze_expression(arg)

class CodeGenerator:
    """Generates Python code from the AST"""
    
    def __init__(self):
        self.code = []
        self.indent_level = 0
    
    def indent(self):
        """Increase indentation level"""
        self.indent_level += 1
    
    def dedent(self):
        """Decrease indentation level"""
        if self.indent_level > 0:
            self.indent_level -= 1
    
    def add_line(self, line):
        """Add a line of code with proper indentation"""
        self.code.append(' ' * 4 * self.indent_level + line)
    
    def generate(self, ast):
        """Generate Python code from the AST"""
        if isinstance(ast, Program):
            # Add standard imports
            self.add_line("# Generated Python code")
            self.add_line("")
            
            # Generate code for statements
            for statement in ast.statements:
                self.generate_statement(statement)
        
        return '\n'.join(self.code)
    
    def generate_statement(self, statement):
        """Generate code for a statement"""
        if isinstance(statement, PrintStatement):
            self.add_line(f"print({self.generate_expression(statement.expression)})")
        
        elif isinstance(statement, InputStatement):
            if isinstance(statement.variable, Identifier):
                self.add_line(f"{statement.variable.name} = input()")
            else:
                # Handle error case
                self.add_line(f"# Error: Input target must be a variable")
        
        elif isinstance(statement, AssignmentStatement):
            if isinstance(statement.variable, Identifier):
                self.add_line(f"{statement.variable.name} = {self.generate_expression(statement.expression)}")
            else:
                # Handle error case
                self.add_line(f"# Error: Assignment target must be a variable")
        
        elif isinstance(statement, IfStatement):
            self.add_line(f"if {self.generate_expression(statement.condition)}:")
            self.indent()
            
            if statement.if_block:
                for stmt in statement.if_block:
                    self.generate_statement(stmt)
            else:
                self.add_line("pass")
            
            self.dedent()
            
            if statement.else_block:
                self.add_line("else:")
                self.indent()
                
                for stmt in statement.else_block:
                    self.generate_statement(stmt)
                
                self.dedent()
        
        elif isinstance(statement, WhileStatement):
            self.add_line(f"while {self.generate_expression(statement.condition)}:")
            self.indent()
            
            if statement.block:
                for stmt in statement.block:
                    self.generate_statement(stmt)
            else:
                self.add_line("pass")
            
            self.dedent()
        
        elif isinstance(statement, ForStatement):
            self.add_line(f"for {statement.variable} in {self.generate_expression(statement.iterable)}:")
            self.indent()
            
            if statement.block:
                for stmt in statement.block:
                    self.generate_statement(stmt)
            else:
                self.add_line("pass")
            
            self.dedent()
        
        elif isinstance(statement, FunctionDefinition):
            params = ", ".join(statement.parameters)
            self.add_line(f"def {statement.name}({params}):")
            self.indent()
            
            if statement.body:
                for stmt in statement.body:
                    self.generate_statement(stmt)
            else:
                self.add_line("pass")
            
            self.dedent()
            self.add_line("")  # Add a blank line after function definition
        
        elif isinstance(statement, ReturnStatement):
            if statement.expression:
                self.add_line(f"return {self.generate_expression(statement.expression)}")
            else:
                self.add_line("return")
        
        elif statement == 'break':
            self.add_line("break")
        
        elif statement == 'continue':
            self.add_line("continue")
        
        elif statement == 'pass':
            self.add_line("pass")
        
        elif isinstance(statement, Expression):
            # Handle expression statements (like function calls)
            self.add_line(f"{self.generate_expression(statement)}")
    
    def generate_expression(self, expression):
        """Generate code for an expression"""
        if isinstance(expression, BinaryOperation):
            # Map reversed keywords back to Python operators
            op_map = {
                'ro': 'or',
                'dna': 'and'
            }
            
            op = op_map.get(expression.operator, expression.operator)
            return f"({self.generate_expression(expression.left)} {op} {self.generate_expression(expression.right)})"
        
        elif isinstance(expression, UnaryOperation):
            return f"({expression.operator}{self.generate_expression(expression.operand)})"
        
        elif isinstance(expression, Literal):
            if isinstance(expression.value, str):
                # Escape special characters in the string
                escaped = expression.value.replace('\\', '\\\\').replace('"', '\\"')
                return f'"{escaped}"'
            else:
                return str(expression.value)
        
        elif isinstance(expression, Identifier):
            return expression.name
        
        elif isinstance(expression, FunctionCall):
            args = ", ".join(self.generate_expression(arg) for arg in expression.arguments)
            return f"{expression.function}({args})"

def translate_keywords(code):
    """Translate reversed Python keywords back to normal Python keywords"""
    keyword_map = {
        'tnirp': 'print',
        'rof': 'for',
        'ni': 'in',
        'elihw': 'while',
        'fi': 'if',
        'esle': 'else',
        'tupni': 'input',
        'fed': 'def',
        'nruter': 'return',
        'kaerb': 'break',
        'eunitnoc': 'continue',
        'ssap': 'pass',
        'ro': 'or', 
        'dna': 'and'
    }
    
    # Create a regex pattern for matching whole words only
    pattern = r'\b(' + '|'.join(keyword_map.keys()) + r')\b'
    
    # Replace each keyword
    return re.sub(pattern, lambda m: keyword_map[m.group(0)], code)

def main():
    """Main function to run the compiler"""
    if len(sys.argv) < 2:
        print("Usage: python nohtype_compiler.py <input_file.rpc> [output_file.py]")
        return
    
    input_file = sys.argv[1]
    
    # Determine output file name
    if len(sys.argv) >= 3:
        output_file = sys.argv[2]
    else:
        output_file = os.path.splitext(input_file)[0] + '.py'
    
    try:
        # Read the input file
        with open(input_file, 'r') as f:
            source_code = f.read()
        
        # Lexical analysis
        lexer = Lexer(source_code)
        tokens = lexer.tokenize()
        
        # Syntax analysis
        parser = Parser(tokens)
        ast = parser.parse()
        
        # Semantic analysis
        analyzer = SemanticAnalyzer()
        errors = analyzer.analyze(ast)
        
        if errors:
            print("Semantic errors:")
            for error in errors:
                print(f"  - {error}")
            return
        
        # Code generation
        generator = CodeGenerator()
        python_code = generator.generate(ast)
        
        # Write the output file
        with open(output_file, 'w') as f:
            f.write(python_code)
        
        print(f"Successfully compiled {input_file} to {output_file}")
    
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found")
    except SyntaxError as e:
        print(f"Syntax error: {e}")
    except Exception as e:
        print(f"Error: {e}")

class Interpreter:
    """Interprets the AST directly without generating Python code"""
    
    def __init__(self):
        self.variables = {}
        self.functions = {}
        self.return_value = None
        self.should_return = False
        self.should_break = False
        self.should_continue = False
    
    def interpret(self, ast):
        """Interpret the AST"""
        if isinstance(ast, Program):
            for statement in ast.statements:
                self.interpret_statement(statement)
                
                # Check for control flow signals
                if self.should_return or self.should_break or self.should_continue:
                    break
    
    def interpret_statement(self, statement):
        """Interpret a statement"""
        # Check for control flow signals
        if self.should_return or self.should_break or self.should_continue:
            return
        
        if isinstance(statement, PrintStatement):
            value = self.evaluate_expression(statement.expression)
            print(value)
        
        elif isinstance(statement, InputStatement):
            if isinstance(statement.variable, Identifier):
                self.variables[statement.variable.name] = input()
            else:
                raise RuntimeError("Input target must be a variable")
        
        elif isinstance(statement, AssignmentStatement):
            if isinstance(statement.variable, Identifier):
                value = self.evaluate_expression(statement.expression)
                self.variables[statement.variable.name] = value
            else:
                raise RuntimeError("Assignment target must be a variable")
        
        elif isinstance(statement, IfStatement):
            condition = self.evaluate_expression(statement.condition)
            
            if condition:
                for stmt in statement.if_block:
                    self.interpret_statement(stmt)
                    if self.should_return or self.should_break or self.should_continue:
                        break
            elif statement.else_block:
                for stmt in statement.else_block:
                    self.interpret_statement(stmt)
                    if self.should_return or self.should_break or self.should_continue:
                        break
        
        elif isinstance(statement, WhileStatement):
            while self.evaluate_expression(statement.condition):
                for stmt in statement.block:
                    self.interpret_statement(stmt)
                    if self.should_return or self.should_break or self.should_continue:
                        break
                
                # Handle break and continue
                if self.should_break:
                    self.should_break = False
                    break
                
                if self.should_continue:
                    self.should_continue = False
                    continue
        
        elif isinstance(statement, ForStatement):
            iterable = self.evaluate_expression(statement.iterable)
            
            for item in iterable:
                # Assign the loop variable
                self.variables[statement.variable] = item
                
                for stmt in statement.block:
                    self.interpret_statement(stmt)
                    if self.should_return or self.should_break or self.should_continue:
                        break
                
                # Handle break and continue
                if self.should_break:
                    self.should_break = False
                    break
                
                if self.should_continue:
                    self.should_continue = False
                    continue
        
        elif isinstance(statement, FunctionDefinition):
            # Store the function definition
            self.functions[statement.name] = statement
        
        elif isinstance(statement, ReturnStatement):
            if statement.expression:
                self.return_value = self.evaluate_expression(statement.expression)
            else:
                self.return_value = None
            
            self.should_return = True
        
        elif statement == 'break':
            self.should_break = True
        
        elif statement == 'continue':
            self.should_continue = True
        
        elif statement == 'pass':
            pass  # Do nothing
        
        elif isinstance(statement, Expression):
            # Evaluate expression statements (like function calls)
            self.evaluate_expression(statement)
    
    def evaluate_expression(self, expression):
        """Evaluate an expression"""
        if isinstance(expression, BinaryOperation):
            left = self.evaluate_expression(expression.left)
            right = self.evaluate_expression(expression.right)
            
            # Handle logical operators with short-circuit evaluation
            if expression.operator == 'or':
                return left or right
            elif expression.operator == 'and':
                return left and right
            
            # Handle other operators
            elif expression.operator == '+':
                return left + right
            elif expression.operator == '-':
                return left - right
            elif expression.operator == '*':
                return left * right
            elif expression.operator == '/':
                return left / right
            elif expression.operator == '%':
                return left % right
            elif expression.operator == '==':
                return left == right
            elif expression.operator == '!=':
                return left != right
            elif expression.operator == '<':
                return left < right
            elif expression.operator == '>':
                return left > right
            elif expression.operator == '<=':
                return left <= right
            elif expression.operator == '>=':
                return left >= right
        
        elif isinstance(expression, UnaryOperation):
            operand = self.evaluate_expression(expression.operand)
            
            if expression.operator == '+':
                return +operand
            elif expression.operator == '-':
                return -operand
            elif expression.operator == '!':
                return not operand
        
        elif isinstance(expression, Literal):
            return expression.value
        
        elif isinstance(expression, Identifier):
            if expression.name in self.variables:
                return self.variables[expression.name]
            else:
                raise RuntimeError(f"Undefined variable '{expression.name}'")
        
        elif isinstance(expression, FunctionCall):
            if expression.function in self.functions:
                func_def = self.functions[expression.function]
                
                # Evaluate arguments
                args = [self.evaluate_expression(arg) for arg in expression.arguments]
                
                # Check argument count
                if len(args) != len(func_def.parameters):
                    raise RuntimeError(f"Function '{expression.function}' expects {len(func_def.parameters)} arguments, got {len(args)}")
                
                # Save current variables
                old_variables = self.variables.copy()
                
                # Set up parameters
                for i, param in enumerate(func_def.parameters):
                    self.variables[param] = args[i]
                
                # Reset control flow flags
                old_should_return = self.should_return
                old_return_value = self.return_value
                self.should_return = False
                
                # Execute function body
                for stmt in func_def.body:
                    self.interpret_statement(stmt)
                    if self.should_return:
                        break
                
                # Get return value
                result = self.return_value
                
                # Restore variables and control flow state
                self.variables = old_variables
                self.should_return = old_should_return
                self.return_value = old_return_value
                
                return result
            else:
                raise RuntimeError(f"Undefined function '{expression.function}'")

def run_interpreter(source_code):
    """Run the interpreter directly on the source code"""
    try:
        # Lexical analysis
        lexer = Lexer(source_code)
        tokens = lexer.tokenize()
        
        # Syntax analysis
        parser = Parser(tokens)
        ast = parser.parse()
        
        # Semantic analysis
        analyzer = SemanticAnalyzer()
        errors = analyzer.analyze(ast)
        
        if errors:
            print("Semantic errors:")
            for error in errors:
                print(f"  - {error}")
            return
        
        # Interpretation
        interpreter = Interpreter()
        interpreter.interpret(ast)
        
        return True
    
    except SyntaxError as e:
        print(f"Syntax error: {e}")
        return False
    except RuntimeError as e:
        print(f"Runtime error: {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    main()
