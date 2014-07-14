# coding: utf-8
import sys

# for pig
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound

# for GDB
from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

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
    lang = sys.stdin.readline().strip()
    amt = int(sys.stdin.readline())
    code = sys.stdin.read(amt)

    rv = ""
    try:
        try:
            lex = GDBLexer() if lang == "gdb" else get_lexer_by_name(lang)
        except ClassNotFound as err:
            lex = get_lexer_by_name("text")

        lex.encoding = 'utf-8'
        rv = highlight(code, lex, html)
    except ValueError as err:
        rv = "Pygments Error: %s" % (err)

    sys.stdout.write(str(len(rv)))
    sys.stdout.write("\n")
    sys.stdout.write(rv)
    sys.stdout.flush()

