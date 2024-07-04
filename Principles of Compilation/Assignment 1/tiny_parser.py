from tiny_scanner import TinyScanner, TinyToken
from pt_node import PTNode  # Import PTNode from the pt_node.py module



class TinyParser:
    """ Parser for the Tiny language, building a parse tree from tokens. """
    def __init__(self, fpath):
        """ Initialize the parser with the file path of the Tiny source code. """
        self.scanner = TinyScanner(fpath)

    def parse_program(self):
        """ Parse the program production rule. """
        stmtseq_node = self.parse_stmtseq()
        if self.scanner.current_token.kind != "EOS":
            raise ValueError(f"Unexpected token after program end: {self.scanner.current_token}")
        return PTNode("program", [stmtseq_node])  # Return the root node of the parse tree.

    def parse_stmtseq(self):
        """ Parse a sequence of statements, handling multiple statements separated by semicolons. """
        statements = []
        # Continue parsing statements until we reach a token that signals the end of a block or program.
        while self.scanner.current_token and self.scanner.current_token.kind not in ["END", "UNTIL", "ELSE", "EOS"]:
            statement_node = self.parse_statement()  # Parse an individual statement.
            statements.append(statement_node)
            # If a semicolon is found, consume it and continue parsing the next statement.
            if self.scanner.current_token and self.scanner.current_token.kind == "SEMI":
                self.scanner.match(["SEMI"])
        return PTNode("stmtseq", statements)  # Return a parse tree node with the parsed statements as children.

    def parse_statement(self):
        """ Parse an individual statement, dispatching to the correct statement parser based on the current token. """
        token_kind = self.scanner.current_token.kind
        # Depending on the kind of the current token, parse the corresponding statement type.
        if token_kind == "IF":
            return self.parse_ifstmt()
        elif token_kind == "REPEAT":
            return self.parse_repeatstmt()
        elif token_kind == "IDENTIFIER":
            return self.parse_assignstmt()
        elif token_kind == "READ":
            return self.parse_readstmt()
        elif token_kind == "WRITE":
            return self.parse_writestmt()
        else:
            # If the current token does not start any known statement type, raise an error.
            raise ValueError(f"Unexpected token: {self.scanner.current_token}")


    def parse_ifstmt(self):
        """ Parse an if-statement, which includes an expression, a then-statement sequence, 
        and an optional else-statement sequence. """
        self.scanner.match(["IF"])
        condition = self.parse_exp()  # Parse the condition expression.
        self.scanner.match(["THEN"]) 
        then_stmtseq = self.parse_stmtseq()  # Parse the 'then' statement sequence.
        else_stmtseq = None  # Initialize the optional else-statement sequence.
        # Check if there is an else part in the if-statement.
        if self.scanner.current_token and self.scanner.current_token.kind == "ELSE":
            self.scanner.match(["ELSE"])
            else_stmtseq = self.parse_stmtseq()  # Parse the 'else' statement sequence.
        self.scanner.match(["END"]) 
        # Create a parse tree node for the if-statement, including all parts parsed.
        if else_stmtseq:
            return PTNode("ifstmt", [condition, then_stmtseq, else_stmtseq])
        else:
            return PTNode("ifstmt", [condition, then_stmtseq])

    def parse_repeatstmt(self):
        """ Parse a repeat-statement, which includes a statement sequence and a terminating expression. """
        self.scanner.match(["REPEAT"])
        stmtseq = self.parse_stmtseq()  # Parse the statement sequence inside the repeat.
        self.scanner.match(["UNTIL"]) 
        condition = self.parse_exp()  # Parse the terminating expression.
        return PTNode("repeatstmt", [stmtseq, condition])  # Return a parse tree node for the repeat-statement.


    def parse_assignstmt(self):
        """ Parse an assignment statement, which consists of an identifier, an assignment operator, and an expression. """
        identifier = self.scanner.match(["IDENTIFIER"])  # Capture the identifier.
        self.scanner.match(["ASSIGN"])
        expression = self.parse_exp()  # Parse the expression to be assigned.
        return PTNode("assignstmt", [expression], value=identifier)  # Return a parse tree node for the assignment.

    def parse_readstmt(self):
        """ Parse a read statement, which involves reading a value into an identifier. """
        self.scanner.match(["READ"])
        identifier = self.scanner.match(["IDENTIFIER"])  # Capture the identifier to read into.
        return PTNode("readstmt", [], value=identifier)  # Return a parse tree node for the read statement.

    def parse_writestmt(self):
        """ Parse a write statement, which prints the value of an expression. """
        self.scanner.match(["WRITE"])
        expression = self.parse_exp()  # Parse the expression whose value is to be written.
        return PTNode("writestmt", [expression])  # Return a parse tree node for the write statement.

    def parse_exp(self):
        """ Parse an expression, which may be a simple expression or a comparison of two simple expressions. """
        expr_node = self.parse_simple_expr()  # Parse the first simple expression.
        # If the current token is a comparison operator, parse the full comparison expression.
        if self.scanner.current_token.kind in ["LT", "EQ"]:
            comp_op_node = self.parse_comp_op()  # Parse the comparison operator.
            right_expr_node = self.parse_simple_expr()  # Parse the second simple expression.
            expr_node = PTNode("exp", [expr_node, comp_op_node, right_expr_node])  # Combine into a full expression node.
        return expr_node  # Return the parse tree node for the expression.


    def parse_comp_op(self):
        """ Parse a comparison operator, which could be less-than '<' or equals '='. """
        if self.scanner.current_token.kind == "LT":
            self.scanner.match(["LT"])
            return PTNode("comp_op", [], value="<")  # Return a parse tree node for the less-than operator.
        elif self.scanner.current_token.kind == "EQ":
            self.scanner.match(["EQ"])
            return PTNode("comp_op", [], value="=")  # Return a parse tree node for the equals operator.

    def parse_simple_expr(self):
        """ Parse a simple expression, which may be an addition or subtraction of terms. """
        simple_expr_node = self.parse_term()  # Parse the first term.
        # Continue parsing additional terms if an addition or subtraction operator is present.
        while self.scanner.current_token.kind in ["PLUS", "MINUS"]:
            addop_node = self.parse_addop()  # Parse the addition or subtraction operator.
            term_node = self.parse_term()  # Parse the next term.
            simple_expr_node = PTNode("simple_expr", [simple_expr_node, addop_node, term_node])  # Combine into a simple expression node.
        return simple_expr_node  # Return the parse tree node for the simple expression.

    def parse_addop(self):
        """ Parse an addition or subtraction operator. """
        if self.scanner.current_token.kind == "PLUS":
            self.scanner.match(["PLUS"])
            return PTNode("addop", [], value="+")  # Return a parse tree node for the plus operator.
        elif self.scanner.current_token.kind == "MINUS":
            self.scanner.match(["MINUS"])
            return PTNode("addop", [], value="-")  # Return a parse tree node for the minus operator.

    def parse_term(self):
        """ Parse a term, which may be a multiplication or division of factors. """
        term_node = self.parse_factor()  # Parse the first factor.
        # Continue parsing additional factors if a multiplication or division operator is present.
        while self.scanner.current_token.kind in ["TIMES", "DIVIDE"]:
            mulop_node = self.parse_mulop()  # Parse the multiplication or division operator.
            factor_node = self.parse_factor()  # Parse the next factor.
            term_node = PTNode("term", [term_node, mulop_node, factor_node])  # Combine into a term node.
        return term_node  # Return the parse tree node for the term.

    def parse_mulop(self):
        """ Parse a multiplication or division operator. """
        if self.scanner.current_token.kind == "TIMES":
            self.scanner.match(["TIMES"])
            return PTNode("mulop", [], value="*")  # Return a parse tree node for the times operator.
        elif self.scanner.current_token.kind == "DIVIDE":
            self.scanner.match(["DIVIDE"])
            return PTNode("mulop", [], value="/")  # Return a parse tree node for the divide operator.

    def parse_factor(self):
        """ Parse a factor, which could be an expression in parentheses, a number, or an identifier. """
        if self.scanner.current_token.kind == "NUMBER":
            num_node = self.scanner.match(["NUMBER"])  # Capture the number.
            return PTNode("factor", [], value=num_node)  # Return a parse tree node for the number.
        elif self.scanner.current_token.kind == "IDENTIFIER":
            identifier_node = self.scanner.match(["IDENTIFIER"])  # Capture the identifier.
            return PTNode("factor", [], value=identifier_node)  # Return a parse tree node for the identifier.
        elif self.scanner.current_token.kind == "OPEN_PAREN":
            self.scanner.match(["OPEN_PAREN"])
            exp_node = self.parse_exp()  # Parse the expression inside the parentheses.
            self.scanner.match(["CLOSE_PAREN"])
            return PTNode("factor", [exp_node])  # Return a parse tree node for the factor with the nested expression.




    # The main execution
if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python tiny_parser.py <path to source file>")
        sys.exit(1)

    source_path = sys.argv[1]
    parser = TinyParser(source_path)
    parse_tree_root = parser.parse_program()
    print("Parse Tree:")
    print("-" * 25)
    parse_tree_root.dump()
    print("=" * 25)
    print()