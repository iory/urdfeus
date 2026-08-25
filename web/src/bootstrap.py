"""Python side of the browser converter.

Runs inside Pyodide. Installs urdfeus and its importable dependencies, then
exposes ``convert`` (URDF -> EusLisp) and ``convert_eus`` (EusLisp -> URDF)
for the worker to call.
"""
import io
import json
import os
import os.path as osp
import shutil

import micropip

# 0.3.31 is the first release that imports pysdfgen lazily. pysdfgen is a C++
# extension with no WebAssembly build, and every earlier release pulls it in at
# module scope, so `import skrobot.model` would fail outright. Pinned rather
# than left open because that failure is worth naming, not rediscovering.
SCIKIT_ROBOT = "scikit-robot>=0.3.31"


async def setup(urdfeus_wheel=None):
    """Install urdfeus and what it imports.

    ``urdfeus_wheel`` is the URL of the wheel the site was built with; without
    one the last PyPI release is used, which is what a bare ``npm run dev``
    gets.
    """
    # scikit-robot and urdfeus still declare native-only requirements
    # (pysdfgen, rtree) that the resolver cannot satisfy here, so install them
    # without dependency resolution and supply what they actually import.
    await micropip.install(["trimesh", "pycollada", "filelock"])
    await micropip.install(["pooch", "ordered-set", "cached-property"], deps=False)
    await micropip.install(SCIKIT_ROBOT, deps=False)
    await micropip.install(urdfeus_wheel or "urdfeus", deps=False)

    import urdfeus
    return urdfeus.__version__


WORKDIR = "/work"


def reset_workdir():
    if os.path.isdir(WORKDIR):
        shutil.rmtree(WORKDIR)
    os.makedirs(WORKDIR, exist_ok=True)


def write_file(relative_path, data):
    """Place one file inside the work directory, creating parents as needed."""
    path = os.path.join(WORKDIR, relative_path.lstrip("/"))
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as f:
        f.write(bytes(data))
    return path


def convert(urdf_name, robot_name=None, use_urdf_material=False):
    """Convert the staged URDF and return the EusLisp source as a string.

    skrobot resolves ``package://<pkg>/<rel>`` relative to the URDF's own
    directory, so staging meshes at ``<workdir>/<rel>`` is what makes the
    user's dropped folder line up with the URDF's references.
    """
    from urdfeus.urdf2eus import urdf2eus

    buf = io.StringIO()
    urdf2eus(
        os.path.join(WORKDIR, urdf_name),
        None,
        None,
        robot_name or None,
        fp=buf,
        use_cache=False,
        use_urdf_material=use_urdf_material,
    )
    return buf.getvalue()


OUTPUT_DIR = os.path.join(WORKDIR, "_urdf")


def convert_eus(eus_name, robot_name=None):
    """Convert a staged EusLisp model to a URDF package, without irteusgl.

    The ``static`` backend reads the ``.l`` file directly
    (:mod:`urdfeus.eus_parse`); irteusgl is a native program and cannot run
    here. Returns a JSON manifest -- the URDF text plus the names of the mesh
    files -- and leaves the meshes in the work directory for
    :func:`read_output` to hand over one at a time.
    """
    from urdfeus.eus2urdf import _ros_package_name
    from urdfeus.eus2urdf import eus2urdf

    shutil.rmtree(OUTPUT_DIR, ignore_errors=True)
    stem = osp.splitext(osp.basename(eus_name))[0]
    # The package name is what package:// URIs in the URDF resolve against, so
    # the download has to use the sanitized one, not the file stem.
    package_name = _ros_package_name(stem)
    urdf_path = eus2urdf(
        os.path.join(WORKDIR, eus_name),
        OUTPUT_DIR,
        package_name=package_name,
        robot_name=robot_name or None,
        backend="static",
    )
    meshes_dir = osp.join(OUTPUT_DIR, "meshes")
    meshes = sorted(os.listdir(meshes_dir)) if osp.isdir(meshes_dir) else []
    with open(urdf_path) as f:
        urdf = f.read()
    extras = {}
    for name in ("package.xml", "CMakeLists.txt"):
        path = osp.join(OUTPUT_DIR, name)
        if osp.isfile(path):
            with open(path) as f:
                extras[name] = f.read()
    return json.dumps({
        "urdf_name": osp.basename(urdf_path),
        "package_name": package_name,
        "urdf": urdf,
        "meshes": meshes,
        "extras": extras,
        "links": urdf.count("<link name="),
        "joints": urdf.count("<joint name="),
    })


def read_output(relative_path):
    """Bytes of one file of the generated package."""
    with open(osp.join(OUTPUT_DIR, relative_path), "rb") as f:
        return f.read()
