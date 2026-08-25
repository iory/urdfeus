#!/usr/bin/env python
"""Build the urdfeus wheel the browser converter installs into Pyodide.

The page used to ``micropip.install("urdfeus")`` from PyPI, which meant it ran
whatever was last released rather than what is in the tree. Building the wheel
into ``web/public/`` instead keeps the deployed converter in step with the
commit it was built from; ``web/public/wheel.json`` tells the worker its name.

Run it before ``npm run build`` (the Pages workflow does), or by hand for
``npm run dev``::

    python -m pip install build
    python tools/build_web_wheel.py
"""

import argparse
import glob
import json
import os
import os.path as osp
import shutil
import subprocess
import sys
import tempfile

_ROOT = osp.dirname(osp.dirname(osp.abspath(__file__)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--out", default=osp.join(_ROOT, "web", "public"),
        help="directory to place the wheel and wheel.json in")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory() as build_dir:
        subprocess.run(
            [sys.executable, "-m", "build", "--wheel", "--outdir", build_dir,
             _ROOT],
            check=True)
        wheels = glob.glob(osp.join(build_dir, "*.whl"))
        if len(wheels) != 1:
            raise SystemExit(
                f"expected one wheel, got {sorted(wheels)}")
        os.makedirs(args.out, exist_ok=True)
        for stale in glob.glob(osp.join(args.out, "urdfeus-*.whl")):
            os.remove(stale)
        shutil.copy(wheels[0], args.out)
        name = osp.basename(wheels[0])

    with open(osp.join(args.out, "wheel.json"), "w") as f:
        json.dump({"urdfeus": name}, f)
    print(f"staged {name} in {args.out}")


if __name__ == "__main__":
    main()
