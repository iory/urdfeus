"""Round trip a model through EusLisp -> URDF -> EusLisp and compare the ends.

Both converters are tested on their own elsewhere: ``test_eus2urdf`` checks the
URDF against the EusLisp model it came from, and ``test_urdf_conversion``
checks that a generated ``.l`` loads. Neither closes the loop, and the loop is
where a unit or convention error hides: a factor of 1e9 between ``g*mm^2`` and
``kg*m^2``, degrees against radians, millimetres against metres, a flipped
joint axis. Each of those survives one leg looking plausible and only shows up
when the model comes back.

The comparison is between two ``irteusgl`` dumps, which is as close to "the
same model" as the two ends can be asked to be. ``dump-robot`` zeroes every
movable joint before dumping, so the dump is a kinematic snapshot at the zero
configuration: link poses, joint axes, types and limits. That set determines
forward kinematics at every other configuration too, which is why these tests
do not also sample joint angles.

Two differences are expected rather than regressions, and the tests allow for
exactly them:

- Names are sanitized on the way out (``:torso-waist-y`` becomes
  ``torso_waist_y``), so links and joints are matched through the same map
  ``eus2urdf`` uses rather than by name equality.
- A link whose declared inertia cannot belong to a rigid body comes back with
  the tensor ``eus2urdf`` computed from its mesh. The emitted URDF marks those
  links with a comment, which is what the inertia comparison skips on.
"""

import glob
import os
import os.path as osp
import shutil
import tempfile
import unittest
import xml.etree.ElementTree as ET

import numpy as np

from urdfeus.eus2urdf import _DECAL_OFFSET
from urdfeus.eus2urdf import _mat3
from urdfeus.eus2urdf import _unique_name_map
from urdfeus.eus2urdf import dump_eus_model
from urdfeus.eus2urdf import eus2urdf
from urdfeus.urdf2eus import urdf2eus

#: Coplanar decal submeshes are pushed off their host surface by
#: ``_DECAL_OFFSET`` metres, so a link's mesh may legitimately grow by that much
#: on each side of the round trip.
MESH_TOL_MM = 2.0 * _DECAL_OFFSET * 1000.0


def candidate_model_dirs():
    """Yield directories that may hold jskeus' models, most explicit first.

    Nothing here is a guess about where a particular machine keeps its
    checkout: the environment is asked first, then the interpreter is asked
    where it lives, and only the two paths a packaged jskeus installs into are
    spelled out.
    """
    env = os.environ.get("JSKEUS_MODELS_DIR")
    if env:
        yield env

    # A source build points EUSDIR at <jskeus>/eus, a packaged one at the
    # directory holding jskeus/.
    eusdir = os.environ.get("EUSDIR")
    if eusdir:
        yield osp.join(eusdir, "models")
        yield osp.join(eusdir, "jskeus", "eus", "models")

    # irteusgl sits at <jskeus>/eus/<arch>/bin/irteusgl, so the models are a
    # few directories up from whichever copy is on PATH. Walking up keeps this
    # working for a checkout anywhere on disk.
    binary = shutil.which("irteusgl")
    if binary:
        directory = osp.dirname(osp.realpath(binary))
        for _ in range(4):
            yield osp.join(directory, "models")
            directory = osp.dirname(directory)

    for pattern in ("/opt/ros/*/share/euslisp/jskeus/eus/models",
                    "/usr/share/euslisp/jskeus/eus/models"):
        yield from sorted(glob.glob(pattern))


def find_jskeus_models_dir():
    """Return the jskeus model directory, or None when it cannot be found."""
    for directory in candidate_model_dirs():
        if directory and glob.glob(osp.join(directory, "*-robot.l")):
            return directory
    return None


jskeus_models_dir = find_jskeus_models_dir()
jskeus_demo_dir = None
if jskeus_models_dir is not None:
    # <jskeus>/eus/models -> <jskeus>/irteus/demo
    candidate = osp.normpath(
        osp.join(jskeus_models_dir, "..", "..", "irteus", "demo"))
    if osp.isfile(osp.join(candidate, "special-joints.l")):
        jskeus_demo_dir = candidate


def is_irteusgl_available():
    return shutil.which("irteusgl") is not None


#: (label, file, constructor, robot name). The set is chosen for what each
#: model puts through the loop, not for coverage of the gallery: h3 for a
#: rotational-only robot whose inertia tensors are all repaired, the cupboard
#: for a prismatic joint and for two mirrored door meshes of identical size,
#: sample-arm for a linear joint inside an arm, special-joints for interlocking
#: (mimic) joints, and the multidof arm for a joint with more than one degree
#: of freedom. ``robot_name`` is given explicitly because a demo class whose
#: EusLisp name is nil would otherwise name the URDF -- and therefore the
#: regenerated constructor -- "nil".
MODELS = [
    ("h3", "models/h3-robot.l", None, "h3"),
    ("cupboard", "models/73b2-cupboard-without-door-object.l", None,
     "cupboard"),
    ("sample-arm", "demo/sample-arm-model.l", "sarmclass", "samplearm"),
    ("interlock", "demo/special-joints.l",
     "sample-legged-robot-with-interlocking-joints", "interlock"),
    ("multidof", "demo/sample-multidof-arm-model.l",
     "sample-multidof-arm-robot", "multidof"),
]


def model_path(locator):
    """Absolute path of a MODELS locator, or None when its directory is absent."""
    kind, name = locator.split("/", 1)
    root = jskeus_models_dir if kind == "models" else jskeus_demo_dir
    if root is None:
        return None
    path = osp.join(root, name)
    return path if osp.isfile(path) else None


def world_mesh_points(link):
    """Mesh vertices of a dumped link in world millimetres, or None."""
    points = []
    for body in link["meshes"]:
        for submesh in body:
            vertices = np.array(submesh["vertices"], dtype=np.float64)
            if vertices.size == 0:
                continue
            vertices = vertices.reshape(-1, 3)
            points.append(vertices @ _mat3(submesh["glv_rot"]).T
                          + np.array(submesh["glv_pos"], dtype=np.float64))
    return np.vstack(points) if points else None


def principal_moments(tensor):
    matrix = _mat3(tensor)
    return np.linalg.eigvalsh(0.5 * (matrix + matrix.T))


def is_usable_inertia(tensor):
    if not tensor:
        return False
    matrix = _mat3(tensor)
    if not np.any(matrix):
        return False
    small, mid, large = principal_moments(tensor)
    return small > 0.0 and small + mid >= large * (1.0 - 1e-9)


def repaired_links(urdf_path):
    """Link names the converter marked as carrying a recomputed inertia.

    The marker is an XML comment, and ElementTree's default parser drops
    comments, so the tree has to be built with a builder that keeps them.
    """
    parser = ET.XMLParser(target=ET.TreeBuilder(insert_comments=True))
    names = set()
    for link in ET.parse(urdf_path, parser=parser).getroot().findall("link"):
        for child in link:
            if (child.tag is ET.Comment
                    and "inertia recomputed by urdfeus" in (child.text or "")):
                names.add(link.get("name"))
    return names


@unittest.skipUnless(is_irteusgl_available(), "irteusgl not available")
@unittest.skipUnless(jskeus_models_dir is not None, "jskeus models not present")
class TestEusUrdfEusRoundTrip(unittest.TestCase):

    def setUp(self):
        self.tmp = tempfile.mkdtemp(prefix="urdfeus_roundtrip_")

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def _round_trip(self, label, path, constructor, robot_name):
        """Convert out and back; return (original dump, regenerated dump, urdf)."""
        package = osp.join(self.tmp, f"{label}_pkg")
        urdf_path = eus2urdf(path, package, package_name="pkg",
                             constructor=constructor, robot_name=robot_name)
        # package:// has no meaning without a ROS workspace, and urdf2eus has
        # to open the meshes, so point the URDF at them directly.
        absolute = osp.join(self.tmp, f"{label}_abs.urdf")
        with open(absolute, "w") as f:
            f.write(open(urdf_path).read().replace("package://pkg/",
                                                   package + "/"))
        regenerated = osp.join(self.tmp, f"{label}_regen.l")
        with open(regenerated, "w") as f:
            urdf2eus(absolute, fp=f)
        return (dump_eus_model(path, constructor=constructor),
                dump_eus_model(regenerated, constructor=robot_name),
                urdf_path)

    def _check(self, label, locator, constructor, robot_name):
        path = model_path(locator)
        if path is None:
            self.skipTest(f"{locator} not present")
        before, after, urdf_path = self._round_trip(
            label, path, constructor, robot_name)
        repaired = repaired_links(urdf_path)

        self.assertEqual(len(before["links"]), len(after["links"]),
                         f"{label}: link count changed")
        self.assertEqual(len(before["joints"]), len(after["joints"]),
                         f"{label}: joint count changed")

        _, link_names = _unique_name_map(before["links"])
        joint_unames, _ = _unique_name_map(before["joints"])
        after_links = {link["name"]: link for link in after["links"]}
        after_joints = {joint["name"]: joint for joint in after["joints"]}

        for link in before["links"]:
            name = link_names[link["name"]]
            self.assertIn(name, after_links, f"{label}: lost link {name}")
            back = after_links[name]
            where = f"{label}:{name}"

            self.assertLess(
                np.linalg.norm(np.array(link["pos"]) - np.array(back["pos"])),
                1e-2, f"{where} world position moved")
            self.assertLess(
                np.abs(_mat3(link["rot"]) - _mat3(back["rot"])).max(), 1e-5,
                f"{where} world rotation changed")

            self.assertAlmostEqual(link["weight"] or 0.0, back["weight"] or 0.0,
                                   places=6, msg=f"{where} mass changed")
            if link["centroid"] and back["centroid"]:
                self.assertLess(
                    np.linalg.norm(np.array(link["centroid"])
                                   - np.array(back["centroid"])),
                    1e-3, f"{where} centroid moved")

            if name in repaired:
                # The tensor was deliberately replaced on the way out; what has
                # to survive is that both ends hold a usable one.
                self.assertTrue(is_usable_inertia(back["inertia"]),
                                f"{where} came back with an unusable inertia")
            elif is_usable_inertia(link["inertia"]):
                original = _mat3(link["inertia"])
                returned = _mat3(back["inertia"])
                self.assertLess(
                    np.abs(original - returned).max() / np.abs(original).max(),
                    1e-5, f"{where} inertia drifted")

            points, back_points = world_mesh_points(link), world_mesh_points(back)
            self.assertEqual(points is None, back_points is None,
                             f"{where} gained or lost its mesh")
            if points is not None:
                box = np.vstack([points.min(axis=0), points.max(axis=0)])
                back_box = np.vstack([back_points.min(axis=0),
                                      back_points.max(axis=0)])
                self.assertLess(np.abs(box - back_box).max(), MESH_TOL_MM,
                                f"{where} mesh moved or changed size")

        for i, joint in enumerate(before["joints"]):
            name = joint_unames[i]
            self.assertIn(name, after_joints, f"{label}: lost joint {name}")
            back = after_joints[name]
            where = f"{label}:{name}"

            self.assertEqual(joint["jtype"], back["jtype"],
                             f"{where} joint type changed")
            self.assertLess(
                np.abs(np.array(joint["axis"]) - np.array(back["axis"])).max(),
                1e-9, f"{where} joint axis changed")
            self.assertEqual(bool(joint.get("mimic")), bool(back.get("mimic")),
                             f"{where} lost or gained a mimic master")
            for bound in ("min", "max"):
                value, returned = joint.get(bound), back.get(bound)
                self.assertEqual(value is None, returned is None,
                                 f"{where} {bound} appeared or vanished")
                if value is None:
                    continue
                if abs(value) > 1e29 or abs(returned) > 1e29:
                    # Unlimited on both sides; the sentinel itself may differ.
                    self.assertGreater(abs(returned), 1e29,
                                       f"{where} {bound} gained a limit")
                    continue
                # Degrees for rotational joints, millimetres for linear ones,
                # which is what catches a degree/radian or mm/m slip.
                self.assertAlmostEqual(value, returned, places=4,
                                       msg=f"{where} {bound} changed")

    def test_round_trip(self):
        for label, locator, constructor, robot_name in MODELS:
            with self.subTest(model=label):
                self._check(label, locator, constructor, robot_name)

    def test_second_pass_repairs_nothing(self):
        """A model that has been through the loop needs no inertia repair.

        Every tensor the first pass wrote is a usable one, so converting the
        regenerated model again must not trip the repair. If it does, the check
        that decides what counts as unusable disagrees with the tensors the
        repair itself produces.
        """
        path = model_path("models/h3-robot.l")
        if path is None:
            self.skipTest("h3-robot.l not present")
        _, _, first_urdf = self._round_trip("pass1", path, None, "h3")
        self.assertTrue(repaired_links(first_urdf),
                        "h3 is expected to need the repair on the first pass")
        regenerated = osp.join(self.tmp, "pass1_regen.l")
        # The regenerated file defines (defun h3 ...), not one named after the
        # file, so the constructor has to be named explicitly.
        second = eus2urdf(regenerated, osp.join(self.tmp, "pass2_pkg"),
                          package_name="pkg", constructor="h3",
                          robot_name="h3")
        self.assertEqual(repaired_links(second), set(),
                         "second pass repaired an inertia the first pass wrote")
