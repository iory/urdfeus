"""A reader for the EusLisp source of generated model files.

This turns ``.l`` text into nested Python data: lists for lists, :class:`Symbol`
for symbols, ``str``/``int``/``float`` for the corresponding literals and numpy
arrays for the ``#f(...)`` / ``#i(...)`` / ``#2f((...))`` vector and matrix
literals.

It is a *reader*, not an evaluator: no macro expansion, no package handling, no
``#.`` evaluation. That is enough for the machine-generated models
:mod:`urdfeus.eus_parse` walks, and anything it does not recognise raises
:class:`EusReadError` rather than being skipped.
"""

import re

import numpy as np


class EusReadError(ValueError):
    """Raised when the source cannot be read as EusLisp data."""


class Symbol:
    """An EusLisp symbol. ``:foo`` keywords are symbols whose name starts ':'."""

    __slots__ = ("name",)

    def __init__(self, name):
        self.name = name

    @property
    def keywordp(self):
        return self.name.startswith(":")

    def __eq__(self, other):
        if isinstance(other, Symbol):
            return self.name == other.name
        if isinstance(other, str):
            return self.name == other
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        if result is NotImplemented:
            return result
        return not result

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f"Symbol({self.name!r})"


class Dotted:
    """A dotted pair ``(car . cdr)``.

    Generated models use these only for slot access -- ``(link . acentroid)``
    -- so the reader keeps the shape instead of building a cons cell.
    """

    __slots__ = ("car", "cdr")

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return f"Dotted({self.car!r}, {self.cdr!r})"


#: A token that ends a symbol.
_DELIM = set(" \t\r\n()\";")

_INT_RE = re.compile(r"^[+-]?\d+\.?$")
_FLOAT_RE = re.compile(r"^[+-]?(\d+\.\d*|\.\d+|\d+)([eEdD][+-]?\d+)?$")


def _atom(text):
    """Turn one token into a number or a symbol.

    Symbol names are lowercased: the EusLisp reader is case-insensitive, so
    ``:MediumSeaGreen`` and ``:mediumseagreen`` name the same symbol and both
    print lowercased.
    """
    if _INT_RE.match(text):
        # EusLisp writes integers as "12" and "12."; both are integers.
        return int(text.rstrip("."))
    if _FLOAT_RE.match(text):
        return float(text.replace("d", "e").replace("D", "e"))
    return Symbol(text.lower())


class Reader:
    """Reads forms out of one EusLisp source string."""

    def __init__(self, text, path=None):
        self.text = text
        self.path = path
        self.pos = 0
        self.length = len(text)

    # -- low level ---------------------------------------------------------
    def _error(self, message):
        line = self.text.count("\n", 0, self.pos) + 1
        where = "{}:{}".format(self.path or "<string>", line)
        return EusReadError(f"{where}: {message}")

    def _skip_blanks(self):
        text, n = self.text, self.length
        while self.pos < n:
            c = text[self.pos]
            if c in " \t\r\n\f":
                self.pos += 1
            elif c == ";":
                nl = text.find("\n", self.pos)
                self.pos = n if nl < 0 else nl + 1
            elif c == "#" and text.startswith("#|", self.pos):
                end = text.find("|#", self.pos + 2)
                if end < 0:
                    raise self._error("unterminated block comment")
                self.pos = end + 2
            else:
                return

    def _read_string(self):
        # self.pos is at the opening quote
        text = self.text
        i = self.pos + 1
        out = []
        while True:
            if i >= self.length:
                raise self._error("unterminated string")
            c = text[i]
            if c == "\\":
                out.append(text[i + 1])
                i += 2
            elif c == '"':
                i += 1
                break
            else:
                out.append(c)
                i += 1
        self.pos = i
        return "".join(out)

    def _read_numbers(self, kind):
        """Read the body of a ``#f(...)``/``#i(...)`` literal as a flat array.

        Number vectors hold no nested lists, so the matching paren is simply
        the next one -- which lets the huge vertex vectors in a mesh be parsed
        by numpy in one call instead of token by token.
        """
        end = self.text.find(")", self.pos)
        if end < 0:
            raise self._error(f"unterminated #{kind}( literal")
        body = self.text[self.pos:end]
        self.pos = end + 1
        tokens = body.split()
        if not tokens:
            return np.zeros(0, dtype=np.int64 if kind == "i" else np.float64)
        try:
            if kind == "i":
                return np.array(tokens, dtype=np.int64)
            return np.array(tokens, dtype=np.float64)
        except ValueError as e:
            raise self._error(f"bad #{kind}( literal: {e}")

    def _read_matrix(self):
        """Read the body of a ``#2f((...) (...))`` literal into an array."""
        rows = self.read()
        if not isinstance(rows, list) or not all(
                isinstance(r, list) for r in rows):
            raise self._error("#2f( expects a list of rows")
        try:
            return np.array(rows, dtype=np.float64)
        except ValueError as e:
            raise self._error(f"bad #2f( literal: {e}")

    # -- forms -------------------------------------------------------------
    def read(self):
        """Read one form. Raises :class:`EusReadError` at end of input."""
        self._skip_blanks()
        if self.pos >= self.length:
            raise self._error("unexpected end of input")
        text = self.text
        c = text[self.pos]

        if c == "(":
            self.pos += 1
            return self._read_list()
        if c == ")":
            raise self._error("unexpected ')'")
        if c == '"':
            return self._read_string()
        if c == "'":
            self.pos += 1
            return [Symbol("quote"), self.read()]
        if c == "`" or c == ",":
            raise self._error("backquote is not supported")
        if c == "#":
            nxt = text[self.pos + 1:self.pos + 2]
            if nxt == "'":
                self.pos += 2
                return [Symbol("function"), self.read()]
            if nxt in "fF":
                if text[self.pos + 2:self.pos + 3] != "(":
                    raise self._error("expected '(' after #f")
                self.pos += 3
                return self._read_numbers("f")
            if nxt in "iI":
                if text[self.pos + 2:self.pos + 3] != "(":
                    raise self._error("expected '(' after #i")
                self.pos += 3
                return self._read_numbers("i")
            if text[self.pos + 1:self.pos + 3] in ("2f", "2F"):
                if text[self.pos + 3:self.pos + 4] != "(":
                    raise self._error("expected '(' after #2f")
                self.pos += 3
                return self._read_matrix()
            if nxt == "\\":
                self.pos += 2
                if self.pos >= self.length:
                    raise self._error("unterminated character literal")
                start = self.pos
                self.pos += 1
                while self.pos < self.length and text[self.pos] not in _DELIM:
                    self.pos += 1
                return text[start:self.pos]
            raise self._error(f"unsupported reader macro '#{nxt}'")

        start = self.pos
        while self.pos < self.length and text[self.pos] not in _DELIM:
            self.pos += 1
        if self.pos == start:
            raise self._error(f"unexpected character {c!r}")
        return _atom(text[start:self.pos])

    def _read_list(self):
        items = []
        while True:
            self._skip_blanks()
            if self.pos >= self.length:
                raise self._error("unterminated list")
            if self.text[self.pos] == ")":
                self.pos += 1
                return items
            form = self.read()
            if isinstance(form, Symbol) and form.name == ".":
                cdr = self.read()
                self._skip_blanks()
                if self.pos >= self.length or self.text[self.pos] != ")":
                    raise self._error("bad dotted pair")
                self.pos += 1
                if len(items) != 1:
                    raise self._error("only (a . b) dotted pairs are supported")
                return Dotted(items[0], cdr)
            items.append(form)

    def read_all(self):
        """Read every form in the source."""
        forms = []
        while True:
            self._skip_blanks()
            if self.pos >= self.length:
                return forms
            forms.append(self.read())


def read_forms(text, path=None):
    """Read every top-level form out of ``text``."""
    return Reader(text, path=path).read_all()


def read_file(path):
    """Read every top-level form out of the EusLisp file at ``path``."""
    with open(path, errors="replace") as f:
        return read_forms(f.read(), path=path)
