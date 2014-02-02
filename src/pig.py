# coding: utf-8
import sys
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name

# ugh windows line endings
if sys.platform == "win32":
    import os, msvcrt
    msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
    msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)

html = HtmlFormatter(encoding='utf-8', nowrap=True)

while True:
    lang = sys.stdin.readline()
    amt = int(sys.stdin.readline())
    code = sys.stdin.read(amt)

    rv = ""
    try:
        lex = get_lexer_by_name(lang.strip())
        lex.encoding = 'utf-8'
        rv = highlight(code, lex, html)
    except ValueError as err:
        rv = "Pygments Error: %s" % (err)

    sys.stdout.write(str(len(rv)))
    sys.stdout.write("\n")
    sys.stdout.write(rv)
    sys.stdout.flush()

