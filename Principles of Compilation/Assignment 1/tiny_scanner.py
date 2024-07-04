import re

# Define regular expressions to capture comments and Tiny language tokens.
COMMENT_RE = re.compile(r"\{.*?\}", re.DOTALL)
TOKENS_RE = re.compile(r"[a-zA-Z]+"
                       r"|:=|;"
                       r"|\d+"
                       r"|[()=+-/*<]"
                       r"|if|then|else|end|repeat|until|read|write")

# Define the reserved words and symbols of the Tiny language.
RESERVED_WORDS = {
    "if": "IF", "then": "THEN", "else": "ELSE", "end": "END",
    "repeat": "REPEAT", "until": "UNTIL", "read": "READ", "write": "WRITE"
}

SYMBOLS = {
    ":=": "ASSIGN", ";": "SEMI",
    "=": "EQ", "<": "LT",
    "+": "PLUS", "-": "MINUS", "*": "TIMES", "/": "DIVIDE",
    "(": "OPEN_PAREN", ")": "CLOSE_PAREN"
}

class TinyToken:
    """ Represents a token in the Tiny language. """
    def __init__(self, spelling):
        """ Initialize the token with its spelling and determine its kind and value. """
        self.spelling = spelling
        self.kind = None
        self.value = None
        self.determine_kind()

    def determine_kind(self):
        """ Determine the kind of token (e.g., identifier, number, reserved word). """
        if self.spelling == "EOS":
            self.kind = "EOS"  # End-of-source token kind.
        elif self.spelling.isalpha():
            if self.spelling in RESERVED_WORDS:
                self.kind = RESERVED_WORDS[self.spelling]
            else:
                self.kind = "IDENTIFIER"
        elif self.spelling.isdigit():
            self.kind = "NUMBER"
            self.value = int(self.spelling)
        elif self.spelling in SYMBOLS:
            self.kind = SYMBOLS[self.spelling]
        else:
            raise ValueError(f"Illegal symbol '{self.spelling}'.")

    def __str__(self):
        """ String representation of the token. """
        return f"[Token: '{self.spelling}' ({self.kind})]"

class TinyScanner:
    """ Scanner for the Tiny language, producing a sequence of tokens from source code. """
    def __init__(self, fpath):
        """ Initialize the scanner with the file path of the Tiny source code. """
        with open(fpath, "r") as source_file:
            self.source = source_file.read()
        self.source = COMMENT_RE.sub("", self.source)  # Remove comments from source code.
        self.tokens = TOKENS_RE.findall(self.source)  # Find all tokens in the source code.
        self.tokens.append("EOS")  # Add End-of-Source token to signal the end.
        self.current_token = None
        self.advance()  # Advance to the first token.

    def advance(self):
        """ Advance to the next token in the source code. """
        if self.tokens:
            tkn = self.tokens.pop(0)
            if tkn == "EOS":
                self.current_token = TinyToken("EOS")  # Create a special token for EOS.
            else:
                self.current_token = TinyToken(tkn)
        else:
            self.current_token = None  # No more tokens available.

    def match(self, expected_kinds):
        """ Match the current token against expected kinds and advance to the next token. """
        if self.current_token.kind not in expected_kinds:
            raise ValueError(f"Expected one of {expected_kinds}, but found {self.current_token.kind}.")
        else:
            value = self.current_token.spelling
            self.advance()  # Consume the token and advance to the next.
            return value
