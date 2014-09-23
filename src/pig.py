# -*- coding: utf-8 -*-
import sys

# for pig
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound

# for GDB
from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

class TOMLLexer(RegexLexer):
    """
    Lexer for TOML, a simple language for config files
    """

    name = 'TOML'
    aliases = ['toml']
    filenames = ['*.toml']

    tokens = {
        'root': [

            # Basics, comments, strings
            (r'\s+', Text),
            (r'#.*?$', Comment.Single),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'(true|false)$', Keyword.Constant),
            ('[a-zA-Z_][a-zA-Z0-9_\-]*', Name),

            # Datetime
            (r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z', Number.Integer),

            # Numbers
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?j?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+j?', Number.Float),
            (r'\-?\d+', Number.Integer),

            # Punctuation
            (r'[]{}:(),;[]', Punctuation),
            (r'\.', Punctuation),

            # Operators
            (r'=', Operator)

        ]
    }

# courtesy of https://github.com/snarez/gdb_lexer
class GDBLexer(RegexLexer):
    name = 'GDB'
    aliases = ['gdb']
    filenames = ['*.gdb']

    string = r'"[^"]*"'
    char = r'[a-zA-Z$._0-9@]'
    identifier = r'(?:[a-zA-Z$_]' + char + '*|\.' + char + '+)'
    number = r'(?:0[xX][a-zA-Z0-9]+|\d+)'

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'(\(?gdb[\)\$]|>)( )('+identifier+')(/?)(\d*)(\w*)',
                bygroups(Keyword.Type, Text, Name.Builtin, Text, Literal.Number.Integer, Keyword.Constant)),
            (number, Number.Hex),
            (string, String),
            (r'=', Operator),
            (r'(\$\d+)( = {)', bygroups(Name.Variable, Text), 'struct'),
            (r'\$'+identifier+'+', Name.Variable),
            (r'\$'+number+'+', Name.Variable),
            (r'#.*', Comment),
            (r'<snip>', Comment.Special),
            (r'<'+identifier+'+\+?\d*>', Name.Function),
            (r'->'+identifier+'+', Name.Attribute),
            (r'(\()(\s*struct\s*'+identifier+'+\s*\*)(\))', bygroups(Text, Keyword.Type, Text)),
            (r'\((int|long|short|char)\s*\*?', Keyword.Type),
            (r'\b(if)\b', Name.Builtin),
            (r'.', Text),
        ],
        'struct': [
            (r'(\s*)([^\s]*)( = {)', bygroups(Text, Name.Variable, Text), '#push'),
            (r'(\s*)([^\s]*)( = )', bygroups(Text, Name.Variable, Text)),
            (r'\s*},?', Text, '#pop'),
            (number, Number.Hex),
            (string, String),
            (r'.', Text)
        ],
   }

# ugh windows line endings
if sys.platform == "win32":
    import os, msvcrt
    msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
    msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)

html = HtmlFormatter(encoding='utf-8', nowrap=True)

while True:
    lang = sys.stdin.readline().rstrip("\n")
    amt = int(sys.stdin.readline().rstrip("\n"))
    code = sys.stdin.read(amt)

    rv = ""
    try:
        try:
            if lang == "gdb":
                lex = GDBLexer(encoding="utf-8")
            elif lang == "toml":
                lex = TOMLLexer(encoding="utf-8")
            else:
                lex = get_lexer_by_name(lang, encoding="utf-8")
        except ClassNotFound as err:
            lex = get_lexer_by_name("text", encoding="utf-8")

        rv = highlight(code, lex, html)
    except ValueError as err:
        rv = "Pygments Error: {}".format(err)

    sys.stdout.write(str(len(rv)))
    sys.stdout.write("\n")
    sys.stdout.flush()

    if not hasattr(sys.stdout, 'buffer'):
        sys.stdout.write(rv)
        sys.stdout.flush()
    else:
        sys.stdout.buffer.write(rv)
        sys.stdout.buffer.flush()

