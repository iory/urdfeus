"""Convert an EusLisp robot model to URDF (ROS package layout).

The EusLisp model is instantiated with ``irteusgl`` (see
``euslisp/eus2urdf-dump.l``) so that links/joints added procedurally in
``:init`` are captured, then the dumped kinematics and ``glvertices`` meshes
are converted to a URDF + Collada meshes laid out as a ROS package::

    <output_dir>/
      package.xml
      CMakeLists.txt
      urdf/<robot>.urdf
      meshes/<link>.<ext>

Mesh export is delegated to :mod:`trimesh` (``mesh.export``), so additional
formats can be supported simply by changing ``mesh_format``.
"""

import json
import os
import os.path as osp
import re
import shutil
import subprocess
import tempfile
import xml.etree.ElementTree as ET

import numpy as np
from skrobot.coordinates.math import matrix2rpy
import trimesh

from urdfeus.common import meter2millimeter

# Path to the irteusgl dump script shipped with the package. It is a
# program that irteusgl executes, not a template pasted into generated
# models, so it does not live under templates/ with those.
_DUMP_SCRIPT = osp.join(osp.dirname(__file__), "euslisp", "eus2urdf-dump.l")

# Joints whose |limit| reaches this sentinel are treated as unlimited.
_INF_LIMIT = 1e30


def dump_script_path():
    """Return the path of the irteusgl dump script shipped with urdfeus.

    A live irteusgl session can ``(load ...)`` it to get ``dump-object-json``,
    which writes the same JSON dump :func:`eus2urdf_from_data` consumes. The
    script only runs its own file+constructor conversion when
    ``*eus2urdf-model-path*`` is set, so loading it is side-effect free.
    """
    return _DUMP_SCRIPT


def _default_constructor_name(eus_path):
    """Return the conventional constructor name for an EusLisp model file.

    urdfeus-generated files define ``(defun <stem> () ...)`` where ``<stem>``
    is the file name without extension (e.g. ``robot.l`` -> ``robot``).
    """
    return osp.splitext(osp.basename(eus_path))[0]


def dump_eus_model(eus_path, constructor=None, irteusgl="irteusgl", timeout=600):
    """Instantiate an EusLisp model via irteusgl and return its dumped data.

    Parameters
    ----------
    eus_path : str
        Path to the ``.l`` model file.
    constructor : str or None
        Name of the constructor function. Defaults to the file stem.
    irteusgl : str
        irteusgl executable to use.
    timeout : float
        Subprocess timeout in seconds.

    Returns
    -------
    dict
        Parsed JSON dump (see ``euslisp/eus2urdf-dump.l`` for the schema).
    """
    eus_path = osp.abspath(eus_path)
    if not osp.exists(eus_path):
        raise FileNotFoundError(eus_path)
    if constructor is None:
        constructor = _default_constructor_name(eus_path)

    out_fd, out_path = tempfile.mkstemp(suffix=".json", prefix="eus2urdf_")
    os.close(out_fd)
    runner_fd, runner_path = tempfile.mkstemp(suffix=".l", prefix="eus2urdf_run_")
    # Add the model dir and its parent to *load-path* so both sibling
    # (require "x.l") and scene-style (load "models/x.l") references resolve.
    model_dir = osp.dirname(eus_path)
    parent_dir = osp.dirname(model_dir)
    # Scenes (scene-model) load their member objects with cwd-relative paths
    # like (load "models/foo.l"), which only resolve from the eus root (the
    # parent of the models dir). Regular models keep cwd at their own dir so
    # sibling (require "...") resolves. Detect scenes from the file header.
    run_cwd = model_dir
    try:
        with open(eus_path, errors="ignore") as _f:
            _head = _f.read(8192)
        if "scene-model" in _head or '(load "models/' in _head:
            run_cwd = parent_dir
    except OSError:
        pass
    runner = "\n".join([
        f'(setq *eus2urdf-model-path* "{eus_path}")',
        f'(setq *eus2urdf-constructor* "{constructor}")',
        f'(setq *eus2urdf-out-path* "{out_path}")',
        f'(setq *load-path* (append (list "{model_dir}" "{parent_dir}") '
        + '*load-path*))',
        f'(load "{_DUMP_SCRIPT}")',
        "",
    ])
    with os.fdopen(runner_fd, "w") as f:
        f.write(runner)

    try:
        proc = subprocess.run(
            [irteusgl, runner_path],
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=timeout,
            # Model dir for normal models (sibling require), eus root for scenes.
            cwd=run_cwd,
        )
        if not osp.exists(out_path) or osp.getsize(out_path) == 0:
            stdout = proc.stdout.decode(errors="replace")
            stderr = proc.stderr.decode(errors="replace")
            raise RuntimeError(
                f"irteusgl did not produce a dump for {eus_path} "
                + f"(constructor '{constructor}').\n"
                + "Make sure the model loads and the constructor name is "
                + "correct (override with --constructor).\n"
                + f"--- irteusgl stdout ---\n{stdout}\n"
                + f"--- irteusgl stderr ---\n{stderr}")
        with open(out_path) as f:
            data = json.load(f)
    finally:
        for p in (out_path, runner_path):
            if osp.exists(p):
                os.remove(p)
    return data


def load_eus_model(eus_path, constructor=None, irteusgl="irteusgl",
                   backend="auto", timeout=600):
    """Return the dump of an EusLisp model, via irteusgl or by parsing it.

    Parameters
    ----------
    eus_path : str
        Path to the ``.l`` model file.
    constructor : str or None
        Constructor function (or class) name. Defaults to the file stem.
    irteusgl : str
        irteusgl executable, used by the ``irteusgl`` and ``auto`` backends.
    backend : str
        ``irteusgl`` instantiates the model in EusLisp, which handles any
        model but needs EusLisp installed. ``static`` reads the file with
        :func:`urdfeus.eus_parse.parse_eus_model`, which needs nothing but
        only understands generated models. ``auto`` (the default) uses
        irteusgl when it is on PATH and falls back to the static parser.
    timeout : float
        irteusgl subprocess timeout in seconds.

    Returns
    -------
    dict
        The dump described by ``euslisp/eus2urdf-dump.l``.
    """
    if backend not in ("auto", "static", "irteusgl"):
        raise ValueError(
            "backend must be 'auto', 'static' or 'irteusgl', not "
            + repr(backend))
    if backend == "static" or (
            backend == "auto" and shutil.which(irteusgl) is None):
        from urdfeus.eus_parse import EusParseError
        from urdfeus.eus_parse import parse_eus_model
        try:
            return parse_eus_model(eus_path, constructor=constructor)
        except EusParseError as e:
            if backend == "static":
                raise
            raise RuntimeError(
                f"'{irteusgl}' is not on PATH and {eus_path} cannot be read "
                + "without it: " + str(e)
                + "\nInstall EusLisp (jskeus) to convert this model.")
    return dump_eus_model(eus_path, constructor=constructor,
                          irteusgl=irteusgl, timeout=timeout)


def _mat3(rows):
    return np.array(rows, dtype=np.float64).reshape(3, 3)


#: How far a decal is pushed off the surface it sits on, in metres. Small
#: enough to be invisible at any sane render scale, large enough to beat the
#: depth buffer's precision, and deliberately larger than ``_COPLANAR_TOL`` so
#: that what has been separated no longer counts as sharing a plane.
_DECAL_OFFSET = 5e-4


def _face_planes(mesh):
    """Per-face unit normal, plane offset and area, dropping degenerate faces.

    The normal is canonicalised (first significant component positive) so that
    two surfaces sharing a plane group together whichever way they are wound.
    """
    tri = np.asarray(mesh.vertices)[np.asarray(mesh.faces)]
    cross = np.cross(tri[:, 1] - tri[:, 0], tri[:, 2] - tri[:, 0])
    norm = np.linalg.norm(cross, axis=1)
    keep = norm > 1e-12
    if not keep.any():
        return None
    unit = cross[keep] / norm[keep][:, None]
    # Canonical sign: flip whichever normals point "negative" so that opposite
    # windings of one plane share a key.
    lead = np.zeros(len(unit))
    for axis in range(3):
        unset = lead == 0
        lead[unset] = unit[unset, axis]
    unit = unit * np.where(lead < 0, -1.0, 1.0)[:, None]
    offset = np.einsum("ij,ij->i", unit, tri[keep][:, 0])
    return unit, offset, norm[keep] / 2.0


#: Two surfaces closer than this (metres) are treated as sharing a plane.
_COPLANAR_TOL = 3e-4


def _link_plane_index(submeshes):
    """Map each plane the link occupies to the area each submesh puts on it.

    Faces are bucketed by quantised (normal, offset) rather than compared
    against one another: a tessellated surface gives almost every face its own
    plane, and a linear scan over accumulated planes turns quadratic in the
    face count -- minutes of stall on a floor or a robot link. Each face is
    also filed under the next offset bucket so a pair straddling a bucket
    boundary still meets somewhere.
    """
    index = {}
    for i, mesh in enumerate(submeshes):
        computed = _face_planes(mesh)
        if computed is None:
            continue
        unit, offset, area = computed
        keys = np.round(unit, 3)
        steps = np.floor(offset / _COPLANAR_TOL).astype(np.int64)
        for key, step, face_area in zip(map(tuple, keys), steps, area):
            for neighbour in (0, 1):
                per = index.setdefault((key, int(step) + neighbour), {})
                per[i] = per.get(i, 0.0) + float(face_area)
    return index


def _offset_coplanar_decals(submeshes):
    """Nudge detail parts off the surfaces they are flush against.

    EusLisp scene models build a detail -- a TV screen, a washing machine's
    lid, a label -- as its own body sitting exactly flush with the panel
    behind it, so both surfaces land on the same depth. Which one wins is then
    decided by floating point noise, and the detail shimmers as the camera
    moves. The coincidence lives in the geometry, so it follows the mesh into
    the URDF and reappears in every renderer downstream.

    These details are usually thin *boxes* rather than zero-thickness sheets,
    so the test is not "is this submesh flat" but "do two submeshes of this
    link share a plane". Whichever puts less area on the shared plane is taken
    to be the detail, and the whole submesh is moved ``_DECAL_OFFSET`` clear of
    the other -- moving all of it keeps the box closed rather than tearing one
    face off it.
    """
    if len(submeshes) < 2:
        return submeshes

    centroids = [np.asarray(mesh.vertices).mean(axis=0) for mesh in submeshes]
    # A part can sit flush against another on more than one face -- a screen
    # set into a recess touches the back of it and its sides -- so keep the
    # worst offender per (detail, plane normal) and move the part clear of all
    # of them. Normals are canonicalised, so opposing faces share a key and
    # cannot produce two offsets that cancel out.
    best = {}
    for (normal, _), per in _link_plane_index(submeshes).items():
        if len(per) < 2:
            continue
        detail = min(per, key=per.get)
        host = max(per, key=per.get)
        key = (detail, normal)
        if best.get(key, (0.0,))[0] < per[detail]:
            best[key] = (per[detail], np.array(normal, dtype=np.float64), host)

    shifts = {}
    for (detail, _), (_, normal, host) in best.items():
        away = centroids[detail] - centroids[host]
        direction = 1.0 if float(np.dot(normal, away)) >= 0 else -1.0
        shifts[detail] = shifts.get(detail, 0.0) + direction * normal

    for detail, shift in shifts.items():
        submeshes[detail].vertices = (
            np.asarray(submeshes[detail].vertices) + _DECAL_OFFSET * shift)
    return submeshes


def _build_link_mesh(link, link_pos, link_rot):
    """Build a :class:`trimesh.Trimesh` for one link in its local frame.

    Mesh vertices are stored in the glvertices-local frame (millimetres). They
    are transformed to world coordinates, then into the link-local frame and
    converted to metres so the URDF visual origin can stay at identity.
    """
    submeshes = []
    for body_meshes in link["meshes"]:
        for sm in body_meshes:
            vertices = np.array(sm["vertices"], dtype=np.float64)
            if vertices.shape[0] == 0:
                continue
            faces = np.array(sm["indices"], dtype=np.int64).reshape(-1, 3)
            glv_pos = np.array(sm["glv_pos"], dtype=np.float64)
            glv_rot = _mat3(sm["glv_rot"])

            # glvertices-local (mm) -> world (mm)
            world = vertices @ glv_rot.T + glv_pos
            # world (mm) -> link-local (mm) -> metres
            local = (world - link_pos) @ link_rot
            local = local / meter2millimeter

            mesh = trimesh.Trimesh(vertices=local, faces=faces, process=False)

            color = sm["diffuse"] if sm["diffuse"] is not None else sm["ambient"]
            if color is not None:
                rgba = np.clip(np.array(color) * 255.0, 0, 255).astype(np.uint8)
                if rgba.shape[0] == 3:
                    rgba = np.append(rgba, 255)
                mesh.visual.face_colors = np.tile(rgba, (len(mesh.faces), 1))
            submeshes.append(mesh)

    if not submeshes:
        return None
    if len(submeshes) == 1:
        return submeshes[0]
    return trimesh.util.concatenate(_offset_coplanar_decals(submeshes))


def _rodrigues(axis, theta):
    """Rotation matrix for angle ``theta`` (rad) about unit ``axis``."""
    a = np.asarray(axis, dtype=np.float64)
    a = a / np.linalg.norm(a)
    K = np.array([[0, -a[2], a[1]],
                  [a[2], 0, -a[0]],
                  [-a[1], a[0], 0]], dtype=np.float64)
    return np.eye(3) + np.sin(theta) * K + (1 - np.cos(theta)) * (K @ K)


def _joint_zero_child_pose(child_pose, jtype, axis, q):
    """Child link pose (mm) at joint value 0, backing out the dumped value ``q``.

    EusLisp clamps a joint to ``[min, max]``; when ``min > 0`` (e.g. PR2's
    torso) the dumped pose is not at joint value 0. URDF defines the joint
    origin at value 0 and adds the joint motion on top, so the dumped
    displacement must be removed to avoid double-counting.
    """
    c_pos, c_rot = child_pose
    if axis is None or q is None or q == 0.0 or jtype not in (
            "revolute", "continuous", "prismatic"):
        return c_pos, c_rot
    if jtype == "prismatic":
        a = np.asarray(axis, dtype=np.float64)
        a = a / np.linalg.norm(a)
        # axis is in the child frame; q is in mm (same units as positions).
        return c_pos - c_rot @ (q * a), c_rot
    # revolute / continuous: rotation about the axis through the joint origin,
    # so the origin position is unchanged and only the orientation is undone.
    # EusLisp rotational :joint-angle is in degrees, hence the deg2rad.
    r_undo = _rodrigues(axis, -np.deg2rad(q))
    return c_pos, c_rot @ r_undo


def _origin_xyz_rpy(parent_pose, child_pose):
    """URDF joint origin (parent frame -> child frame) from world poses."""
    p_pos, p_rot = parent_pose
    c_pos, c_rot = child_pose
    rel_pos = p_rot.T @ (c_pos - p_pos) / meter2millimeter
    rel_rot = p_rot.T @ c_rot
    rpy = matrix2rpy(rel_rot)
    return rel_pos, rpy


def _fmt_vec(vec):
    return " ".join(f"{v:.8g}" for v in vec)


def _classify_joint(joint, is_follower):
    """Return the URDF joint type for a dumped EusLisp joint."""
    jtype = joint["jtype"]
    linear = "linear" in jtype
    is_master = bool(joint["mimic"])
    min_v, max_v = joint["min"], joint["max"]

    # A multi-DOF joint (6dof, sphere) keeps its limits as float-vectors, which
    # the dump writes as null because they are not numbers. There is no 1-DOF
    # URDF joint to map such a joint onto: a free one is URDF's "floating", and
    # anything else -- including the immovable 6dof joint urdf2eus leaves at a
    # model's root -- is honestly just a fixed connection.
    if min_v is None or max_v is None:
        if joint["movable"] and "6dof" in jtype:
            return "floating"
        return "fixed"

    inf = abs(min_v) >= _INF_LIMIT or abs(max_v) >= _INF_LIMIT

    # Fixed joints are emitted by urdf2eus as rotational-joints with 0/0
    # limits that are excluded from :joint-list. Only collapse to "fixed" when
    # the joint genuinely carries no motion and no mimic relationship.
    if (not joint["movable"]) and (not is_master) and (not is_follower) \
            and abs(max_v - min_v) < 1e-9:
        return "fixed"
    if linear:
        return "prismatic"
    if inf:
        return "continuous"
    return "revolute"


#: Each principal moment is compared against ``mass * radius ** 2`` of the
#: link's own mesh, and anything below this fraction of that is a placeholder
#: rather than a mass distribution. A rod a thousand times longer than it is
#: thick, slender past anything a robot link is, still keeps its smallest
#: moment an order of magnitude above this; macra-robot declares ~1e-40 and
#: human-robot a single 1.0 g*mm^2 next to two moments of 2.07e8 g*mm^2, both
#: of which are several orders below it.
_INERTIA_FLOOR_FRACTION = 1e-7


def _inertia_defect(inertia, mass, mesh):
    """Return why ``inertia`` cannot be a rigid body's tensor, else None.

    A usable tensor is positive definite and its principal moments obey the
    triangle inequality. Several EusLisp models declare ``:inertia-tensor`` as
    a zero matrix (h3/h3s/h4/h6/h7, igoid2/igoid3, saizo) or placeholders
    (macra) while still declaring a non-zero ``:weight``. Mass without inertia
    is not a rigid body, so an importer handed it either refuses the body or
    silently substitutes a preset tensor that has nothing to do with the
    robot - both of which are worse than saying so here.

    Parameters
    ----------
    inertia : numpy.ndarray or None
        Candidate tensor in kg*m^2, or None when the link carries no
        ``:inertia-tensor`` at all.
    mass : float
        Link mass in kg.
    mesh : trimesh.Trimesh or None
        The link's mesh in link-local metres. Used only for a length scale, so
        the magnitude check is skipped when the link has no mesh.

    Returns
    -------
    str or None
        A one-line reason, or None when the tensor is usable as it stands.
    """
    if inertia is None:
        return "no :inertia-tensor in the EusLisp model"
    if not np.all(np.isfinite(inertia)):
        return "inertia tensor holds a non-finite component"
    if not np.any(inertia):
        return "all-zero inertia tensor"
    # Symmetric by construction (EusLisp stores the full 3x3), so the cheaper
    # symmetric eigensolver is the right one and returns ascending moments.
    moments = np.linalg.eigvalsh(0.5 * (inertia + inertia.T))
    if moments[0] <= 0.0:
        return ("inertia tensor is not positive definite (smallest principal"
                + f" moment {moments[0]:.3g})")
    small, mid, large = moments
    if small + mid < large * (1.0 - 1e-9):
        return ("principal moments violate the triangle inequality"
                + f" ({small:.3g} + {mid:.3g} < {large:.3g})")
    if mesh is not None:
        radius = 0.5 * float(np.linalg.norm(mesh.bounding_box.extents))
        floor = _INERTIA_FLOOR_FRACTION * mass * radius ** 2
        if small < floor:
            return (f"smallest principal moment {small:.3g} kg*m^2 is too"
                    + f" small for {mass:.3g} kg spread over"
                    + f" {2 * radius:.3g} m")
    return None


def _inertia_from_mesh(mesh, mass):
    """Inertia tensor of ``mesh`` at the uniform density that gives ``mass``.

    The tensor is taken about the mesh's own centre of mass and in link-local
    axes, which is the frame URDF's ``<inertia>`` is read in. Uniform density
    is a guess, but it is a guess derived from the link's actual shape, unlike
    the mass-scaled presets importers substitute when inertia is missing.

    Parameters
    ----------
    mesh : trimesh.Trimesh
        The link's mesh in link-local metres.
    mass : float
        Link mass in kg, taken from the EusLisp ``:weight``.

    Returns
    -------
    tuple of (numpy.ndarray, str) or None
        The 3x3 tensor in kg*m^2 and the geometry it came from, or None when
        the mesh encloses no usable volume.

    Notes
    -----
    glvertices meshes are rarely watertight (they are render geometry, often
    open shells), and the divergence-theorem volume integral trimesh uses is
    only defined on a closed surface. So the raw mesh is used when it is
    watertight and its convex hull, which is closed by construction, when it
    is not.
    """
    if mesh.is_watertight and mesh.volume > 0.0:
        body, source = mesh.copy(), "mesh"
    else:
        body, source = mesh.convex_hull.copy(), "convex hull of the mesh"
    if not body.is_watertight or body.volume <= 0.0:
        return None
    # density, not mass, is what trimesh scales the integral by; mass/volume
    # makes the resulting tensor match the declared mass exactly.
    body.density = mass / body.volume
    inertia = np.asarray(body.moment_inertia, dtype=np.float64)
    inertia = 0.5 * (inertia + inertia.T)
    if not np.all(np.isfinite(inertia)) or np.linalg.eigvalsh(inertia)[0] <= 0:
        return None
    return inertia, source


def _add_inertial(link_el, link, mesh=None):
    """Write ``<inertial>`` for one link, repairing an unusable tensor.

    Parameters
    ----------
    link_el : xml.etree.ElementTree.Element
        The ``<link>`` element to append to.
    link : dict
        One entry of the dump's ``links``, in EusLisp units (g, mm, g*mm^2).
    mesh : trimesh.Trimesh or None
        The link's mesh in link-local metres, used to recompute the tensor
        when the declared one cannot be used.

    Returns
    -------
    str or None
        A note naming the link and what was wrong, for the caller to report,
        or None when the declared inertia was written unchanged.
    """
    weight = link["weight"]
    if weight is None or weight <= 0.0:
        return None
    mass = weight / 1000.0  # g -> kg
    it = link["inertia"]
    inertia = (_mat3(it) / 1e9) if it is not None else None  # g*mm^2 -> kg*m^2

    note = None
    defect = _inertia_defect(inertia, mass, mesh)
    if defect is not None:
        computed = _inertia_from_mesh(mesh, mass) if mesh is not None else None
        if computed is None:
            # Writing mass with no usable inertia is what breaks importers, and
            # there is nothing here to compute one from, so the link is left
            # without <inertial> rather than with an impossible one.
            return (f"{link['name']}: {defect}, and no mesh to compute one"
                    + " from, so no <inertial> was written")
        inertia, source = computed
        note = (f"{link['name']}: {defect}; recomputed from the {source}"
                + " at uniform density")
        link_el.append(ET.Comment(
            f" inertia recomputed by urdfeus: {defect} "))

    inertial = ET.SubElement(link_el, "inertial")
    centroid = link["centroid"]
    xyz = (np.array(centroid) / meter2millimeter) if centroid is not None \
        else np.zeros(3)
    ET.SubElement(inertial, "origin", xyz=_fmt_vec(xyz), rpy="0 0 0")
    ET.SubElement(inertial, "mass", value=f"{mass:.8g}")
    i = inertia
    ET.SubElement(
        inertial, "inertia",
        ixx=f"{i[0, 0]:.8g}", ixy=f"{i[0, 1]:.8g}", ixz=f"{i[0, 2]:.8g}",
        iyy=f"{i[1, 1]:.8g}", iyz=f"{i[1, 2]:.8g}", izz=f"{i[2, 2]:.8g}",
    )
    return note


def _package_xml(package_name):
    pkg = ET.Element("package", format="2")
    ET.SubElement(pkg, "name").text = package_name
    ET.SubElement(pkg, "version").text = "0.0.0"
    ET.SubElement(pkg, "description").text = (
        f"URDF for {package_name}, generated by urdfeus (eus2urdf)."
    )
    ET.SubElement(pkg, "maintainer", email="someone@example.com").text = "auto"
    ET.SubElement(pkg, "license").text = "BSD"
    ET.SubElement(pkg, "buildtool_depend").text = "catkin"
    export = ET.SubElement(pkg, "export")
    ET.SubElement(export, "build_type").text = "catkin"
    return pkg


def _cmakelists(package_name):
    """Minimal catkin CMakeLists for a mesh/URDF-only description package.

    The package.xml declares a catkin build, which needs a CMakeLists.txt;
    this installs the urdf/ and meshes/ directories so the package builds and
    ``package://`` resolves after ``catkin build`` / ``catkin_make``.
    """
    return f"""cmake_minimum_required(VERSION 3.0.2)
project({package_name})
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${{CATKIN_PACKAGE_SHARE_DESTINATION}})
"""


def _indent_write(element, path):
    tree = ET.ElementTree(element)
    ET.indent(tree, space="  ")
    tree.write(path, encoding="utf-8", xml_declaration=True)


def eus2urdf(
    eus_path,
    output_dir,
    package_name=None,
    robot_name=None,
    constructor=None,
    mesh_format="glb",
    draco=False,
    irteusgl="irteusgl",
    backend="auto",
):
    """Convert an EusLisp model to a URDF ROS package.

    Parameters
    ----------
    eus_path : str
        Path to the EusLisp ``.l`` model.
    output_dir : str
        Directory of the ROS package to create.
    package_name : str or None
        ROS package name used in ``package://`` mesh paths. Defaults to the
        output directory's base name.
    robot_name : str or None
        ``<robot name>`` and urdf file stem. Defaults to the dumped robot name.
    constructor : str or None
        EusLisp constructor function name. Defaults to the file stem.
    mesh_format : str
        Mesh file extension understood by ``trimesh.export`` (default ``glb``).
        ``glb``/``ply``/``obj`` preserve per-face colours; ``dae`` does not
        (trimesh's Collada exporter flattens per-face colour into a texture),
        so single-colour meshes keep their colour but multi-colour meshes turn
        grey.
    draco : bool
        Compress glb meshes with Draco (``KHR_draco_mesh_compression``) via
        scikit-robot, preserving per-vertex colour. Shrinks dense meshes by
        roughly an order of magnitude. Forces ``mesh_format`` to ``glb`` and
        requires the ``DracoPy`` package; a glTF loader needs a Draco decoder
        to read the result.
    irteusgl : str
        irteusgl executable.
    backend : str
        How to read the model: ``irteusgl``, ``static`` (parse the file, no
        EusLisp needed) or ``auto``. See :func:`load_eus_model`.

    Returns
    -------
    str
        Path to the written ``.urdf`` file.
    """
    data = load_eus_model(eus_path, constructor=constructor,
                          irteusgl=irteusgl, backend=backend)
    return eus2urdf_from_data(
        data,
        output_dir,
        package_name=package_name,
        robot_name=robot_name,
        mesh_format=mesh_format,
        draco=draco,
    )


def eus2urdf_from_data(
    data,
    output_dir,
    package_name=None,
    robot_name=None,
    mesh_format="glb",
    draco=False,
):
    """Write a URDF ROS package from an already-dumped EusLisp model.

    This is the half of :func:`eus2urdf` that does not need ``irteusgl``: it
    takes the JSON dump described by ``euslisp/eus2urdf-dump.l`` and turns it
    into ``package.xml`` + ``urdf/`` + ``meshes/``. Callers that obtain the dump
    some other way -- most notably ``dump-object-json`` called from inside a
    live irteusgl session, which can convert objects built at runtime such as
    ``(make-cube 100 200 300)`` -- go through this entry point instead.

    Parameters
    ----------
    data : dict
        Parsed JSON dump (see ``euslisp/eus2urdf-dump.l`` for the schema).
    output_dir : str
        Directory of the ROS package to create.
    package_name : str or None
        ROS package name used in ``package://`` mesh paths. Defaults to the
        output directory's base name.
    robot_name : str or None
        ``<robot name>`` and urdf file stem. Defaults to ``data['robot_name']``.
    mesh_format : str
        Mesh file extension understood by ``trimesh.export`` (default ``glb``).
    draco : bool
        Compress glb meshes with Draco. See :func:`eus2urdf`.

    Returns
    -------
    str
        Path to the written ``.urdf`` file.
    """
    export_draco = None
    if draco:
        mesh_format = "glb"
        from skrobot.utils.draco import export_glb_with_draco
        from skrobot.utils.draco import is_dracopy_available
        if not is_dracopy_available():
            raise RuntimeError(
                "draco=True requires the DracoPy package "
                + "(pip install dracopy).")
        export_draco = export_glb_with_draco

    output_dir = osp.abspath(output_dir)
    if package_name is None:
        package_name = osp.basename(output_dir.rstrip("/"))
    # ROS package names must be lower_case_with_underscores (no dashes etc.).
    package_name = _ros_package_name(package_name)
    if robot_name is None:
        robot_name = data["robot_name"]

    meshes_dir = osp.join(output_dir, "meshes")
    urdf_dir = osp.join(output_dir, "urdf")
    os.makedirs(meshes_dir, exist_ok=True)
    os.makedirs(urdf_dir, exist_ok=True)

    link_pose = {}
    for link in data["links"]:
        link_pose[link["name"]] = (
            np.array(link["pos"], dtype=np.float64),
            _mat3(link["rot"]),
        )

    # Reverse mimic map: follower joint name -> (master, multiplier, offset).
    follower_mimic = {}
    for joint in data["joints"]:
        for f in (joint["mimic"] or []):
            follower_mimic[f["joint"]] = (
                joint["name"], f["multiplier"], f["offset"])

    robot_el = ET.Element("robot", name=robot_name)

    # EusLisp names (e.g. :torso-waist-y, o9_/eng2/room-table) contain ':' '-'
    # '/' etc. that are not valid URDF/ROS identifiers, so map every link and
    # joint name to a sanitized, unique one and use it everywhere a name is
    # referenced (link, joint, parent/child, mimic master, frames parent).
    _, link_names = _unique_name_map(data["links"])
    joint_unames, joint_names = _unique_name_map(data["joints"])

    # Links (+ meshes).
    used_fnames = set()
    inertia_notes = []
    for link in data["links"]:
        name = link["name"]
        link_el = ET.SubElement(robot_el, "link", name=link_names[name])

        # The mesh is built before <inertial> because it is what an unusable
        # declared inertia tensor is recomputed from.
        pos, rot = link_pose[name]
        mesh = _build_link_mesh(link, pos, rot)
        note = _add_inertial(link_el, link, mesh)
        if note is not None:
            inertia_notes.append(note)
        if mesh is not None:
            # _safe_name can collapse distinct link names to the same stem;
            # disambiguate so meshes never overwrite each other.
            stem = _safe_name(name)
            fname = f"{stem}.{mesh_format}"
            n = 1
            while fname in used_fnames:
                fname = f"{stem}_{n}.{mesh_format}"
                n += 1
            used_fnames.add(fname)
            if export_draco is not None:
                export_draco([mesh], osp.join(meshes_dir, fname))
            else:
                mesh.export(osp.join(meshes_dir, fname))
            uri = f"package://{package_name}/meshes/{fname}"
            for tag in ("visual", "collision"):
                el = ET.SubElement(link_el, tag)
                ET.SubElement(el, "origin", xyz="0 0 0", rpy="0 0 0")
                geom = ET.SubElement(el, "geometry")
                ET.SubElement(geom, "mesh", filename=uri)

    if inertia_notes:
        print(f"Warning: {len(inertia_notes)} link(s) declare a mass the"
              + " EusLisp inertia tensor cannot go with:")
        for note in inertia_notes:
            print(f"  {note}")

    # Joints. ``joint_unames`` already gives each joint a unique sanitized name
    # (handling both invalid characters and models that reuse a name across
    # parts, e.g. a hand reusing :j10/:j11 on every finger).
    for i, joint in enumerate(data["joints"]):
        name = joint["name"]
        parent = joint["parent"]
        child = joint["child"]
        is_follower = name in follower_mimic
        jtype = _classify_joint(joint, is_follower)

        joint_el = ET.SubElement(
            robot_el, "joint", name=joint_unames[i], type=jtype)
        ET.SubElement(joint_el, "parent", link=link_names[parent])
        ET.SubElement(joint_el, "child", link=link_names[child])

        child0 = _joint_zero_child_pose(
            link_pose[child], jtype, joint["axis"], joint.get("q"))
        pos, rpy = _origin_xyz_rpy(link_pose[parent], child0)
        ET.SubElement(joint_el, "origin", xyz=_fmt_vec(pos), rpy=_fmt_vec(rpy))

        if jtype != "fixed":
            axis = joint["axis"] if joint["axis"] is not None else [1.0, 0.0, 0.0]
            ET.SubElement(joint_el, "axis", xyz=_fmt_vec(axis))

        if jtype in ("revolute", "prismatic"):
            _add_limit(joint_el, joint, jtype)
        elif jtype == "continuous":
            # continuous joints still carry effort/velocity limits
            _add_limit(joint_el, joint, jtype)

        if is_follower:
            master, multiplier, offset = follower_mimic[name]
            ET.SubElement(joint_el, "mimic", joint=joint_names[master],
                          multiplier=f"{multiplier:.8g}", offset=f"{offset:.8g}")

    urdf_path = osp.join(urdf_dir, f"{_safe_name(robot_name)}.urdf")
    _indent_write(robot_el, urdf_path)
    _indent_write(_package_xml(package_name), osp.join(output_dir, "package.xml"))
    with open(osp.join(output_dir, "CMakeLists.txt"), "w") as f:
        f.write(_cmakelists(package_name))

    # Grasp/attention frames (eus :handle / :attention). URDF has no standard
    # frame tag, so write a sidecar frames.json with each frame's pose relative
    # to its parent link (so a viewer can attach it to that link).
    with open(osp.join(output_dir, "frames.json"), "w") as f:
        json.dump(frames_relative(data, link_names), f)
    # scene object name map (prefix -> name) for per-object labelling
    if data.get("scene_objects"):
        with open(osp.join(output_dir, "objects.json"), "w") as f:
            json.dump(data["scene_objects"], f)
    return urdf_path


def movable_joint_spec(data):
    """Describe the movable joints of a dump in the order EusLisp reports them.

    The i-th entry corresponds to the i-th joint of
    ``(eus2urdf-movable-joints obj)``, which is how a caller that logged joint
    angles from a live session lines them up with the URDF this dump produces.
    Matching by index rather than by name is deliberate: eus joint names are
    not unique across a model, so urdfeus renames some of them.

    Parameters
    ----------
    data : dict
        Parsed JSON dump.

    Returns
    -------
    list[tuple[str, float]]
        ``(urdf_joint_name, unit_scale)`` pairs. Multiplying the EusLisp joint
        value (degrees for rotational joints, millimetres for linear ones) by
        ``unit_scale`` gives the URDF value (radians, metres).
    """
    joint_unames, _ = _unique_name_map(data["joints"])
    follower = set()
    for joint in data["joints"]:
        for f in (joint["mimic"] or []):
            follower.add(f["joint"])
    spec = []
    for i, joint in enumerate(data["joints"]):
        if not joint["movable"]:
            continue
        jtype = _classify_joint(joint, joint["name"] in follower)
        if jtype not in ("revolute", "continuous", "prismatic"):
            # A multi-DOF joint carries a vector, which no single scale factor
            # describes and no URDF joint here can be driven by. EusLisp's
            # eus2urdf-movable-joints leaves the same ones out, which is what
            # keeps the two lists index-aligned.
            continue
        scale = (1.0 / meter2millimeter) if jtype == "prismatic" \
            else float(np.pi / 180.0)
        spec.append((joint_unames[i], scale))
    return spec


def frames_relative(data, link_names=None):
    """Convert dumped grasp/attention frames (world pose) to poses relative to
    their parent link, as ``[{name, kind, parent, xyz(m), rpy}]``.

    ``link_names`` optionally maps original link names to the sanitized URDF
    link names, so the ``parent`` field matches the link names in the URDF.
    """
    link_pose = {}
    for link in data["links"]:
        link_pose[link["name"]] = (
            np.array(link["pos"], dtype=np.float64), _mat3(link["rot"]))
    out = []
    for fr in data.get("frames", []):
        parent = fr.get("parent")
        f_pose = (np.array(fr["pos"], dtype=np.float64), _mat3(fr["rot"]))
        if parent in link_pose:
            xyz, rpy = _origin_xyz_rpy(link_pose[parent], f_pose)
            if link_names is not None:
                parent = link_names[parent]
        else:
            parent = None  # attach to world root
            xyz = f_pose[0] / meter2millimeter
            rpy = matrix2rpy(f_pose[1])
        out.append({"name": fr["name"], "kind": fr["kind"], "parent": parent,
                    "xyz": [round(float(x), 6) for x in xyz],
                    "rpy": [round(float(a), 6) for a in rpy]})
    return out


def _add_limit(joint_el, joint, jtype):
    linear = jtype == "prismatic"
    min_v, max_v = joint["min"], joint["max"]
    # eus: rotational limits in deg, linear limits in mm.
    if linear:
        lower = min_v / meter2millimeter
        upper = max_v / meter2millimeter
    else:
        lower = np.deg2rad(min_v)
        upper = np.deg2rad(max_v)
    effort = joint["torque"] if joint["torque"] is not None else 0.0
    velocity = joint["vel"] if joint["vel"] is not None else 0.0
    attrib = {"effort": f"{effort:.8g}", "velocity": f"{velocity:.8g}"}
    if jtype != "continuous":
        attrib["lower"] = f"{lower:.8g}"
        attrib["upper"] = f"{upper:.8g}"
    ET.SubElement(joint_el, "limit", **attrib)


def _safe_name(name):
    """Sanitize a link/robot name for use as a file name."""
    return re.sub(r"[^0-9A-Za-z_.-]", "_", name)


def _ros_name(name):
    """Sanitize an EusLisp link/joint name into a valid URDF/ROS identifier.

    Strips the leading ``:`` keyword marker, turns every character outside
    ``[A-Za-z0-9_]`` (``-``, ``/``, ``:``, whitespace, ...) into ``_``, and
    collapses runs of ``_``. Case is preserved (URDF identifiers are
    case-sensitive and have no lower-case requirement).
    """
    s = re.sub(r"[^0-9A-Za-z_]", "_", str(name).lstrip(":"))
    s = re.sub(r"_{2,}", "_", s).strip("_")
    if not s:
        return "link"
    if not (s[0].isalpha() or s[0] == "_"):
        s = "_" + s
    return s


def _ros_package_name(name):
    """Sanitize a name into a conventional ROS package name.

    Lower-case letters, digits and underscores only (per REP 144 / catkin_pkg);
    other characters such as ``-`` become ``_``.
    """
    s = re.sub(r"[^0-9A-Za-z_]", "_", str(name))
    s = re.sub(r"_{2,}", "_", s).strip("_").lower()
    return s or "package"


def _unique_name_map(items):
    """Map each item's original name to a unique sanitized URDF identifier.

    ``items`` is the list of dumped link/joint dicts (taken in order). Distinct
    originals that sanitize to the same string are disambiguated with a numeric
    suffix. Returns a list of the per-item unique names (parallel to ``items``)
    and a dict from original name to unique name (last occurrence wins, which is
    fine for mimic masters whose names are unique).
    """
    names = []
    by_orig = {}
    used = set()
    for it in items:
        base = _ros_name(it["name"])
        uniq = base
        n = 1
        while uniq in used:
            uniq = f"{base}_{n}"
            n += 1
        used.add(uniq)
        names.append(uniq)
        by_orig[it["name"]] = uniq
    return names, by_orig
