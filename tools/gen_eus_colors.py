#!/usr/bin/env python
"""Generate ``urdfeus/eus_colors.py`` from jskeus' ``irteus/irtglrgb.l``.

``(send body :set-color :orange)`` in an EusLisp model resolves the colour
through ``gl::find-color``, which looks the name up in ``*face-colors*``. The
static parser has no irteusgl to ask, so the table is baked in -- regenerate it
with this script when jskeus changes it::

    python tools/gen_eus_colors.py ~/src/github.com/euslisp/jskeus
"""

import argparse
import os.path as osp
import sys

sys.path.insert(0, osp.dirname(osp.dirname(osp.abspath(__file__))))

from urdfeus.eus_reader import Symbol  # NOQA
from urdfeus.eus_reader import read_file

_HEADER = '''"""EusLisp colour names, generated from jskeus ``irteus/irtglrgb.l``.

``gl::find-color`` resolves a colour keyword against this table and falls back
to 0.5 grey when the name is unknown; :func:`find_color` does the same. Names
are compared case-insensitively because the EusLisp reader upcases symbols.

Do not edit by hand -- regenerate with ``tools/gen_eus_colors.py``.
"""

import numpy as np

#: Fallback of ``gl::find-color`` for an unknown colour name.
DEFAULT_COLOR = (0.5, 0.5, 0.5)

FACE_COLORS = {
'''

_FOOTER = '''}


def find_color(name):
    """Return the RGB of an EusLisp colour as a ``(3,)`` float array.

    Mirrors ``gl::find-color``: a float vector is used as it is, a list is
    taken to be 0-255 components, and anything unknown -- including ``nil``,
    which is how ``(send body :set-color (list))`` arrives -- yields 0.5 grey.
    """
    if isinstance(name, np.ndarray):
        if name.size == 0:
            return np.array(DEFAULT_COLOR, dtype=np.float64)
        return np.array(name, dtype=np.float64)[:3]
    if isinstance(name, (list, tuple)):
        if len(name) == 0:
            return np.array(DEFAULT_COLOR, dtype=np.float64)
        return np.array(name[:3], dtype=np.float64) / 255.0
    if name is None:
        return np.array(DEFAULT_COLOR, dtype=np.float64)
    key = str(name).lstrip(":").lower()
    return np.array(FACE_COLORS.get(key, DEFAULT_COLOR), dtype=np.float64)
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("jskeus", help="path to a jskeus checkout")
    parser.add_argument(
        "--out",
        default=osp.join(osp.dirname(osp.dirname(osp.abspath(__file__))),
                         "urdfeus", "eus_colors.py"))
    args = parser.parse_args()

    src = osp.join(args.jskeus, "irteus", "irtglrgb.l")
    colors = {}
    for form in read_file(src):
        for node in _walk(form):
            if not (isinstance(node, list) and len(node) > 1
                    and node[0] == Symbol("instance")
                    and node[1] == Symbol("colormaterial")):
                continue
            name = _keyword_value(node, ":name")
            diffuse = _keyword_value(node, ":diffuse")
            if name is None or diffuse is None:
                continue
            colors[str(name.name).lstrip(":").lower()] = tuple(
                float(v) for v in diffuse[:3])
    if not colors:
        raise SystemExit(f"no colormaterial entries found in {src}")

    with open(args.out, "w") as f:
        f.write(_HEADER)
        for name in sorted(colors):
            r, g, b = colors[name]
            f.write(f'    "{name}": ({r!r}, {g!r}, {b!r}),\n')
        f.write(_FOOTER)
    print(f"wrote {len(colors)} colours to {args.out}")


def _walk(form):
    yield form
    if isinstance(form, list):
        for item in form:
            yield from _walk(item)


def _keyword_value(form, keyword):
    for i, item in enumerate(form):
        if item == Symbol(keyword) and i + 1 < len(form):
            return form[i + 1]
    return None


if __name__ == "__main__":
    main()
