"""Python side of the browser converter.

Runs inside Pyodide. Installs urdfeus and its importable dependencies, then
exposes ``convert`` for the worker to call.
"""
import io
import os
import shutil

import micropip

# 0.3.31 is the first release that imports pysdfgen lazily. pysdfgen is a C++
# extension with no WebAssembly build, and every earlier release pulls it in at
# module scope, so `import skrobot.model` would fail outright. Pinned rather
# than left open because that failure is worth naming, not rediscovering.
SCIKIT_ROBOT = "scikit-robot>=0.3.31"


async def setup():
    # scikit-robot and urdfeus still declare native-only requirements
    # (pysdfgen, rtree) that the resolver cannot satisfy here, so install them
    # without dependency resolution and supply what they actually import.
    await micropip.install(["trimesh", "pycollada", "filelock"])
    await micropip.install(["pooch", "ordered-set", "cached-property"], deps=False)
    await micropip.install(SCIKIT_ROBOT, deps=False)
    await micropip.install("urdfeus", deps=False)

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
