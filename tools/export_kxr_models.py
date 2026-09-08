#!/usr/bin/env python
"""Export the kxreus robot set to URDF packages and EusLisp models.

kxreus (https://github.com/inabajsk/kxreus) builds its robots procedurally:
``*kxr-all-robot-names*`` in ``kxranimate.l`` names them and
``kxr-make-robot`` assembles each one out of ``kxrmodels.l`` at run time, so
there is no ``.l`` model file for ``eus2urdf`` to read. This tool
instantiates every robot inside irteusgl, dumps it with urdfeus'
``dump-object-json``, converts the dump into a URDF ROS package with
:func:`urdfeus.eus2urdf.eus2urdf_from_data`, and regenerates the EusLisp
model from that URDF with :func:`urdfeus.urdf2eus.urdf2eus`::

    <out>/urdfs/<name>/package.xml, urdf/<name>.urdf, meshes/*.glb
    <out>/models/<name>.l

kxreus' own route (``eus2collada-robot``) goes through openhrp3 and
``collada_to_urdf``, both ROS 1 packages; this one needs neither.

Usage::

    # back into the kxreus checkout it came from
    python tools/export_kxr_models.py --kxreus ~/src/github.com/inabajsk/kxreus

    # into the web gallery, alongside the jskeus models
    python tools/export_kxr_models.py --kxreus ~/src/github.com/inabajsk/kxreus \
        --out docs/models --layout packages --draco --set all
    python tools/make_gallery_site.py

``--layout packages`` drops each package straight into ``--out`` and skips the
``.l`` (the gallery reads URDF), and each package gets a ``collection.txt``
naming the model set, which is what the gallery filters on.
"""

import argparse
from concurrent.futures import ProcessPoolExecutor
import glob
import json
import os
import os.path as osp
import re
import shutil
import subprocess
import sys
import tempfile

from urdfeus.eus2urdf import dump_script_path
from urdfeus.eus2urdf import eus2urdf_from_data
from urdfeus.urdf2eus import urdf2eus

#: Prefix the name-listing script puts in front of each robot name so the
#: names can be picked out of irteusgl's very chatty startup output.
_NAME_MARKER = "KXRNAME "

#: Sidecar file naming which model set a package came from. The web gallery
#: (``tools/make_gallery_site.py``) reads it into the manifest so the models can
#: be filtered by where they came from; packages without one are jskeus'.
COLLECTION_FILE = "collection.txt"

#: EusLisp variables holding the robot name lists, by ``--set`` value.
NAME_LISTS = {
    "kxr": ["*kxr-all-robot-names*"],
    "khr": ["*khr-robot-names*"],
    "all": ["*kxr-all-robot-names*", "*khr-robot-names*"],
}


def find_module_dirs(kxreus_dir):
    """Return the compiled-module directories of a kxreus checkout.

    kxreus builds its EusLisp modules into ``<ARCHDIR>/obj`` and
    ``<ARCHDIR>/lib`` (``Linux64`` on a 64-bit Linux box, but the Makefile
    derives ARCHDIR from ``uname``, so it is not fixed). ``~/.eusrc`` already
    points at whichever checkout was built last, which need not be this one;
    putting these first in ``*load-path*`` makes this checkout win.

    Parameters
    ----------
    kxreus_dir : str
        Path to the kxreus checkout.

    Returns
    -------
    list of str
        Existing module directories, ``obj`` before ``lib``.
    """
    dirs = []
    for obj in sorted(glob.glob(osp.join(kxreus_dir, "*", "obj"))):
        if glob.glob(osp.join(obj, "kxrmodels.*")):
            dirs.append(obj)
            lib = osp.join(osp.dirname(obj), "lib")
            if osp.isdir(lib):
                dirs.append(lib)
    return dirs


def _load_path_form(kxreus_dir):
    """Return the EusLisp form that puts this checkout first on ``*load-path*``."""
    paths = [kxreus_dir + "/"] + [d + "/" for d in find_module_dirs(kxreus_dir)]
    quoted = " ".join(f'"{p}"' for p in paths)
    return f"(setq *load-path* (append (list {quoted}) *load-path*))"


def _preamble(kxreus_dir):
    """Return the EusLisp lines every runner script starts with."""
    return [
        _load_path_form(kxreus_dir),
        f'(setq *rcb4eus-dir* "{kxreus_dir}")',
        f'(load "{kxreus_dir}/kxranimate.l")',
    ]


def _run_irteusgl(lines, kxreus_dir, irteusgl, timeout):
    """Write ``lines`` to a temporary ``.l`` file and run it under irteusgl."""
    fd, path = tempfile.mkstemp(suffix=".l", prefix="export_kxr_")
    with os.fdopen(fd, "w") as f:
        f.write("\n".join(lines) + "\n")
    try:
        return subprocess.run(
            [irteusgl, path],
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=timeout,
            cwd=kxreus_dir,
        )
    finally:
        os.remove(path)


def robot_names(kxreus_dir, name_lists, irteusgl="irteusgl", timeout=600):
    """Return the robot names kxreus defines, read out of the running EusLisp.

    Parameters
    ----------
    kxreus_dir : str
        Path to the kxreus checkout.
    name_lists : list of str
        EusLisp variables to read, e.g. ``["*kxr-all-robot-names*"]``.
    irteusgl : str
        irteusgl executable.
    timeout : float
        Subprocess timeout in seconds.

    Returns
    -------
    list of str
        Robot names, in definition order, duplicates removed.
    """
    lines = _preamble(kxreus_dir)
    for var in name_lists:
        lines.append(
            f'(dolist (n {var}) (format t "{_NAME_MARKER}~A~%" n))')
    lines.append("(exit)")
    proc = _run_irteusgl(lines, kxreus_dir, irteusgl, timeout)
    out = proc.stdout.decode(errors="replace")
    names = []
    for line in out.splitlines():
        if line.startswith(_NAME_MARKER):
            name = line[len(_NAME_MARKER):].strip()
            if name and name not in names:
                names.append(name)
    if not names:
        raise RuntimeError(
            "no robot names read from {}\n--- stdout ---\n{}\n--- stderr ---\n{}"
            .format(kxreus_dir, out, proc.stderr.decode(errors="replace")))
    return names


def dump_robot(kxreus_dir, name, out_json, irteusgl="irteusgl", timeout=1800):
    """Instantiate one kxreus robot and dump it to urdfeus' JSON format.

    ``kxr-make-robot`` is called with ``:model nil``, which builds the robot
    from ``kxrmodels.l`` instead of loading ``models/<name>.l`` -- the file
    this tool is here to produce in the first place.

    Parameters
    ----------
    kxreus_dir : str
        Path to the kxreus checkout.
    name : str
        Robot name, e.g. ``"kxrl6"``.
    out_json : str
        Path the JSON dump is written to.
    irteusgl : str
        irteusgl executable.
    timeout : float
        Subprocess timeout in seconds.
    """
    lines = _preamble(kxreus_dir) + [
        f'(load "{dump_script_path()}")',
        f'(let ((r (kxr-make-robot "{name}" :model nil :viewer nil)))',
        '  (unless r (format *error-output* ";; kxr-make-robot returned nil~%")'
        + "    (exit 1))",
        f'  (send r :name "{name}")',
        "  (send r :angle-vector"
        + "    (instantiate float-vector (length (send r :angle-vector))))",
        f'  (dump-object-json r "{out_json}"))',
        "(exit)",
    ]
    proc = _run_irteusgl(lines, kxreus_dir, irteusgl, timeout)
    if not osp.exists(out_json) or osp.getsize(out_json) == 0:
        raise RuntimeError(
            ("irteusgl produced no dump for {}\n--- stdout ---\n{}\n"
             + "--- stderr ---\n{}").format(
                name,
                proc.stdout.decode(errors="replace")[-4000:],
                proc.stderr.decode(errors="replace")[-4000:]))


def _sanitize(name):
    """Return ``name`` reduced to what both ROS and EusLisp accept."""
    return re.sub(r"[^0-9a-z_]", "_", str(name).lower()) or "robot"


#: Where the two layouts put a package and its EusLisp model.
#:
#: ``kxreus`` mirrors what kxreus' own Makefile cleans up (``urdfs/`` next to
#: ``models/``); ``packages`` drops the packages straight into ``--out`` with no
#: ``.l``, which is the shape ``tools/make_gallery_site.py`` reads.
LAYOUTS = ("kxreus", "packages")


def _paths(out_dir, pkg, layout):
    """Return ``(package_dir, model_path)`` for one robot under ``layout``.

    ``model_path`` is None when the layout does not carry EusLisp models.
    """
    if layout == "kxreus":
        return (osp.join(out_dir, "urdfs", pkg),
                osp.join(out_dir, "models", pkg + ".l"))
    return (osp.join(out_dir, pkg), None)


def _export(task):
    """Export one robot; returns ``(name, ok, error)``.

    Runs in a worker process, so it takes and returns only picklable values
    and never raises.
    """
    name, opt = task
    pkg = _sanitize(name)
    urdf_pkg_dir, model_path = _paths(opt["out_dir"], pkg, opt["layout"])
    json_dir = osp.join(opt["out_dir"], "dumps")
    os.makedirs(json_dir, exist_ok=True)
    json_path = osp.join(json_dir, pkg + ".json")
    try:
        dump_robot(opt["kxreus_dir"], name, json_path,
                   irteusgl=opt["irteusgl"], timeout=opt["timeout"])
        with open(json_path) as f:
            data = json.load(f)
        shutil.rmtree(urdf_pkg_dir, ignore_errors=True)
        urdf_path = eus2urdf_from_data(
            data, urdf_pkg_dir, package_name=pkg, robot_name=pkg,
            mesh_format=opt["mesh_format"], draco=opt["draco"])
        if opt["collection"]:
            with open(osp.join(urdf_pkg_dir, COLLECTION_FILE), "w") as f:
                f.write(opt["collection"] + "\n")
        if model_path:
            os.makedirs(osp.dirname(model_path), exist_ok=True)
            with open(model_path, "w") as f:
                urdf2eus(urdf_path, robot_name=pkg, fp=f)
        return (pkg, True, "")
    except Exception as e:
        shutil.rmtree(urdf_pkg_dir, ignore_errors=True)
        if model_path and osp.exists(model_path):
            os.remove(model_path)
        return (pkg, False, repr(e)[:2000])
    finally:
        if not opt["keep_json"] and osp.exists(json_path):
            os.remove(json_path)


def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument(
        "--kxreus", default=os.environ.get("KXREUS_DIR"),
        help="kxreus checkout to read the models from "
        + "(default: $KXREUS_DIR)")
    parser.add_argument(
        "--out", default=None,
        help="where the packages are written (default: --kxreus)")
    parser.add_argument(
        "--layout", choices=LAYOUTS, default="kxreus",
        help="kxreus: <out>/urdfs/<name>/ + <out>/models/<name>.l; "
        + "packages: <out>/<name>/ only, the shape make_gallery_site.py "
        + "reads (default: kxreus)")
    parser.add_argument(
        "--collection", default="kxreus",
        help="value written to each package's " + COLLECTION_FILE
        + ", used as the gallery's source label (empty to skip)")
    parser.add_argument(
        "--set", dest="name_set", choices=sorted(NAME_LISTS), default="kxr",
        help="which robot list to export (default: kxr)")
    parser.add_argument(
        "--only", nargs="+", default=None,
        help="export just these robots instead of the whole list")
    parser.add_argument(
        "--mesh-format", default="glb",
        help="mesh extension passed to trimesh (default: glb)")
    parser.add_argument(
        "--draco", action="store_true",
        help="Draco-compress the glb meshes (needs DracoPy)")
    parser.add_argument("--irteusgl", default="irteusgl")
    parser.add_argument(
        "--timeout", type=float, default=1800,
        help="per-robot irteusgl timeout in seconds (default: 1800)")
    parser.add_argument(
        "--workers", type=int, default=1,
        help="parallel exports. The first run of any robot fills kxreus' "
        + "shared glbodies/ mesh cache, which concurrent irteusgl processes "
        + "would race on, so this defaults to 1; raise it once the cache is "
        + "warm (default: 1)")
    parser.add_argument(
        "--keep-dumps", action="store_true",
        help="keep the intermediate <out>/dumps/<name>.json files")
    args = parser.parse_args()

    if not args.kxreus:
        parser.error("--kxreus is required (or set $KXREUS_DIR)")
    kxreus_dir = osp.abspath(osp.expanduser(args.kxreus))
    if not osp.isfile(osp.join(kxreus_dir, "kxranimate.l")):
        parser.error(f"{kxreus_dir} does not look like a kxreus checkout "
                     + "(no kxranimate.l)")
    if not find_module_dirs(kxreus_dir):
        print(f"warning: no compiled modules under {kxreus_dir}/*/obj; "
              + "irteusgl will fall back to whatever ~/.eusrc points at",
              file=sys.stderr)
    out_dir = osp.abspath(osp.expanduser(args.out)) if args.out else kxreus_dir

    names = args.only or robot_names(
        kxreus_dir, NAME_LISTS[args.name_set], irteusgl=args.irteusgl)
    dest = f"{out_dir}/{{urdfs,models}}" if args.layout == "kxreus" \
        else f"{out_dir}/<name>"
    print(f"exporting {len(names)} robots from {kxreus_dir} -> {dest}")

    opt = {
        "kxreus_dir": kxreus_dir,
        "out_dir": out_dir,
        "layout": args.layout,
        "collection": args.collection,
        "mesh_format": args.mesh_format,
        "draco": args.draco,
        "irteusgl": args.irteusgl,
        "timeout": args.timeout,
        "keep_json": args.keep_dumps,
    }
    tasks = [(n, opt) for n in names]
    failures = []
    if args.workers > 1:
        with ProcessPoolExecutor(max_workers=args.workers) as ex:
            results = ex.map(_export, tasks)
            _report(results, len(tasks), failures)
    else:
        _report((_export(t) for t in tasks), len(tasks), failures)

    # With --keep-dumps off this is empty, and an empty dumps/ sitting among
    # the packages is one more directory for the gallery builder to walk.
    try:
        os.rmdir(osp.join(out_dir, "dumps"))
    except OSError:
        pass

    print(f"\ndone: {len(tasks) - len(failures)}/{len(tasks)} succeeded")
    if failures:
        print(f"{len(failures)} failures:")
        for name, err in failures:
            print(f"  {name}: {err}")
        raise SystemExit(1)


def _report(results, total, failures):
    """Print one line per finished export and collect the failures."""
    for i, (name, ok, err) in enumerate(results, 1):
        if not ok:
            failures.append((name, err))
        print("[{}/{}] {} {}{}".format(
            i, total, "ok" if ok else "FAIL", name,
            "" if ok else "  " + err.splitlines()[0][:200]), flush=True)


if __name__ == "__main__":
    main()
