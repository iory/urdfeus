#!/usr/bin/env python
"""Export every EusLisp model to a Draco-compressed URDF ROS package.

Converts the full model set used by the web gallery -- all jskeus
``*-robot.l`` / ``*-object.l`` / ``*-scene.l`` plus ``darwin.l`` and the
repository's ``euslisp/`` sample models -- to ``eusmodels/<name>/`` with
``eus2urdf(..., draco=True)``. Draco compression (``KHR_draco_mesh_compression``)
shrinks the dense glb meshes by roughly an order of magnitude while preserving
per-vertex colour, so the whole gallery becomes small enough to commit.

Usage::

    python tools/export_models.py [--out eusmodels] [--workers N]
"""

import argparse
from concurrent.futures import ProcessPoolExecutor
import glob
import os
import os.path as osp
import shutil

from urdfeus.eus2urdf import _ros_package_name
from urdfeus.eus2urdf import eus2urdf

_ROOT = osp.dirname(osp.dirname(osp.abspath(__file__)))

# Sample robots defined with defclass in jskeus' irteus/demo. These are not
# stock <name>-robot.l model files, so each needs its constructor (class name)
# and an explicit gallery name. Closed-link / scene / camera demos are omitted:
# closed kinematic loops cannot be expressed as a URDF tree.
#   (demo file, constructor/class, gallery name)
DEMO_MODELS = [
    ("sample-robot-model.l", "sample-robot", "sample-robot"),
    ("sample-arm-model.l", "sarmclass", "sample-arm-robot"),
    ("sample-multidof-arm-model.l", "sample-multidof-arm-robot",
     "sample-multidof-arm-robot"),
    # sample-hand reuses joint names (:j10/:j11) on every finger; eus2urdf
    # disambiguates the duplicates into unique URDF joint names.
    ("sample-hand-model.l", "sample-hand", "sample-hand"),
    ("special-joints.l", "sample-legged-robot-with-interlocking-joints",
     "sample-legged-robot"),
    ("crank-motion.l", "sample-crank", "sample-crank"),
]


def find_jskeus_models_dir():
    for pat in ("/opt/ros/*/share/euslisp/jskeus/eus/models",
                "/usr/share/euslisp/jskeus/eus/models"):
        for d in sorted(glob.glob(pat)):
            if glob.glob(osp.join(d, "*-robot.l")):
                return d
    return None


def collect_sources(jskeus_dir):
    """Return a list of (name, eus_path, constructor) for every model.

    ``constructor`` is None when it equals the file stem (the default).
    """
    sources = []
    for pat in ("*-robot.l", "*-object.l", "*-scene.l"):
        for p in sorted(glob.glob(osp.join(jskeus_dir, pat))):
            sources.append((osp.basename(p)[:-2], p, None))
    darwin = osp.join(jskeus_dir, "darwin.l")
    if osp.isfile(darwin):
        sources.append(("darwin", darwin, None))
    # defclass sample robots from irteus/demo (jskeus_dir is .../jskeus/eus/models)
    demo_dir = osp.join(osp.dirname(osp.dirname(jskeus_dir)), "irteus", "demo")
    for fn, ctor, name in DEMO_MODELS:
        p = osp.join(demo_dir, fn)
        if osp.isfile(p):
            sources.append((name, p, ctor))
    return sources


def _convert(args):
    name, eus_path, constructor, out_root = args
    # Use a valid ROS package name for both the directory and package:// URI so
    # the gallery (which keys on the directory name) stays consistent.
    pkg = _ros_package_name(name)
    out_dir = osp.join(out_root, pkg)
    shutil.rmtree(out_dir, ignore_errors=True)
    try:
        eus2urdf(eus_path, out_dir, package_name=pkg,
                 constructor=constructor, draco=True)
        return (pkg, True, "")
    except Exception as e:
        shutil.rmtree(out_dir, ignore_errors=True)
        return (pkg, False, repr(e)[:200])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", default=osp.join(_ROOT, "docs", "models"),
                        help="output directory for the model packages")
    parser.add_argument("--workers", type=int,
                        default=max(1, os.cpu_count() - 1))
    args = parser.parse_args()

    jskeus_dir = find_jskeus_models_dir()
    if jskeus_dir is None:
        raise SystemExit("jskeus models directory not found")
    sources = collect_sources(jskeus_dir)
    os.makedirs(args.out, exist_ok=True)
    print(f"exporting {len(sources)} models (draco) -> {args.out}")

    tasks = [(n, p, c, args.out) for n, p, c in sources]
    failures = []
    with ProcessPoolExecutor(max_workers=args.workers) as ex:
        for i, (name, ok, err) in enumerate(ex.map(_convert, tasks), 1):
            if not ok:
                failures.append((name, err))
            mark = "ok" if ok else "FAIL"
            print(f"[{i}/{len(tasks)}] {mark} {name}"
                  + ("" if ok else f"  {err}"))

    print(f"\ndone: {len(tasks) - len(failures)}/{len(tasks)} succeeded")
    if failures:
        print(f"{len(failures)} failures:")
        for name, err in failures:
            print(f"  {name}: {err}")
        raise SystemExit(1)


if __name__ == "__main__":
    main()
