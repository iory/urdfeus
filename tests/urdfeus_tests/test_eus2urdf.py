import glob
import os
import os.path as osp
import shutil
import subprocess
import tempfile
import unittest
import xml.etree.ElementTree as ET

import numpy as np
from skrobot.model import RobotModel
import trimesh

from urdfeus.eus2urdf import _add_inertial
from urdfeus.eus2urdf import _ros_package_name
from urdfeus.eus2urdf import _unique_name_map
from urdfeus.eus2urdf import dump_eus_model
from urdfeus.eus2urdf import eus2urdf

# Repository-root euslisp/ directory holding test models.
euslisp_dir = osp.abspath(
    osp.join(osp.dirname(__file__), "..", "..", "euslisp"))


def is_irteusgl_available():
    return shutil.which("irteusgl") is not None


def repo_models_available():
    """True if the sample EusLisp model lives under the repo's euslisp/ dir."""
    return osp.isfile(
        osp.join(euslisp_dir, "yamaguchi_6axis_arm_nejineji.l"))


def find_jskeus_models_dir():
    """Locate the jskeus model directory if installed, else return None."""
    patterns = [
        os.environ.get("JSKEUS_MODELS_DIR", ""),
        "/opt/ros/*/share/euslisp/jskeus/eus/models",
        "/usr/share/euslisp/jskeus/eus/models",
        osp.expanduser("~/ros/*/devel/share/euslisp/jskeus/eus/models"),
    ]
    for pat in patterns:
        if not pat:
            continue
        for d in sorted(glob.glob(pat)):
            if glob.glob(osp.join(d, "*-robot.l")):
                return d
    return None


def _convert_one(args):
    """Convert a single jskeus model to a temp dir (used by the full sweep)."""
    path, package = args
    out = tempfile.mkdtemp(prefix="jk_test_")
    try:
        eus2urdf(path, out, package_name=package)
        return (package, True, "")
    except Exception as e:
        return (package, False, repr(e)[:160])
    finally:
        shutil.rmtree(out, ignore_errors=True)


@unittest.skipUnless(is_irteusgl_available(), "irteusgl not available")
@unittest.skipUnless(repo_models_available(), "euslisp/ sample models not present")
class TestEus2Urdf(unittest.TestCase):

    model_name = "yamaguchi_6axis_arm_nejineji"

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.eus_path = osp.join(euslisp_dir, f"{self.model_name}.l")

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_eus2urdf_roundtrip(self):
        out_dir = osp.join(self.tmp, "pkg")
        urdf_path = eus2urdf(self.eus_path, out_dir, package_name="pkg")

        # ROS package layout.
        self.assertTrue(osp.isfile(urdf_path))
        self.assertTrue(osp.isfile(osp.join(out_dir, "package.xml")))
        self.assertTrue(osp.isdir(osp.join(out_dir, "meshes")))

        # Expected kinematics straight from the EusLisp model. URDF link names
        # are sanitized, so key by the sanitized name to match.
        data = dump_eus_model(self.eus_path)
        _, link_names = _unique_name_map(data["links"])
        eus_pos = {link_names[link["name"]]: np.array(link["pos"])
                   for link in data["links"]}

        # Load generated URDF (resolve package:// to the local package dir).
        urdf = open(urdf_path).read().replace("package://pkg/", out_dir + "/")
        abs_path = osp.join(out_dir, "urdf", "_abs.urdf")
        with open(abs_path, "w") as f:
            f.write(urdf)
        robot = RobotModel()
        with open(abs_path) as f:
            robot.load_urdf_file(f)

        # Every link must reproduce the EusLisp world position (mm) at zero pose.
        self.assertEqual(len(robot.link_list), len(data["links"]))
        for link in robot.link_list:
            self.assertIn(link.name, eus_pos)
            err = np.linalg.norm(link.worldpos() * 1000.0 - eus_pos[link.name])
            self.assertLess(err, 1e-3, f"link {link.name} world-pos mismatch")

    def test_visual_meshes_exist(self):
        # Every visual mesh referenced by the URDF must exist on disk, and the
        # model must export at least one (geometry-less dummy links may have
        # no visual).
        out_dir = osp.join(self.tmp, "visual_pkg")
        urdf_path = eus2urdf(self.eus_path, out_dir, package_name="visual_pkg")

        import xml.etree.ElementTree as ET
        root = ET.parse(urdf_path).getroot()
        meshes = root.findall("link/visual/geometry/mesh")
        self.assertGreater(len(meshes), 0, "no visual meshes exported")
        for mesh in meshes:
            rel = mesh.get("filename").replace("package://visual_pkg/", "")
            self.assertTrue(
                osp.isfile(osp.join(out_dir, rel)),
                f"missing mesh file {rel}")

    def test_mesh_colors_preserved(self):
        # The default glb format must keep the model's multiple colors
        # (a base link with colored connectors has more than one color).
        import trimesh
        out_dir = osp.join(self.tmp, "color_pkg")
        eus2urdf(self.eus_path, out_dir, package_name="color_pkg")
        mesh = trimesh.load(
            osp.join(out_dir, "meshes", "base_link.glb"),
            force="mesh", process=False)
        colors = mesh.visual.vertex_colors
        distinct = {tuple(c) for c in colors}
        self.assertGreater(len(distinct), 1, "expected multiple mesh colors")

    def test_cli(self):
        out_dir = osp.join(self.tmp, "cli_pkg")
        result = subprocess.run(
            ["eus2urdf", self.eus_path, out_dir, "--package-name", "cli_pkg"],
            capture_output=True)
        self.assertEqual(
            result.returncode, 0, result.stderr.decode(errors="replace"))
        self.assertTrue(
            osp.isfile(osp.join(out_dir, "urdf", f"{self.model_name}.urdf")))


jskeus_models_dir = find_jskeus_models_dir()


@unittest.skipUnless(is_irteusgl_available(), "irteusgl not available")
@unittest.skipUnless(jskeus_models_dir is not None, "jskeus models not found")
class TestEus2UrdfJskeus(unittest.TestCase):
    """Convert stock jskeus models (sample robots and object models).

    These exercise classic-jskeus quirks the urdfeus-generated models do not:
    class-only constructors (no zero-arg defun), keyword joint axes (:z/:-z),
    faceset geometry, and cascaded-link objects with articulated parts.
    """

    # A representative spread: keyword-axis sample robots, a large humanoid,
    # an articulated object (kettle handle) and rigid objects.
    REPRESENTATIVE = (
        "h4-robot", "taro-robot", "human-robot",
        "kettle-object", "chair-object", "ball-object",
    )

    def setUp(self):
        self.tmp = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def _convert_and_validate(self, name):
        eus_path = osp.join(jskeus_models_dir, name + ".l")
        if not osp.isfile(eus_path):
            self.skipTest(f"{name} not present in {jskeus_models_dir}")
        out_dir = osp.join(self.tmp, name)
        urdf_path = eus2urdf(eus_path, out_dir, package_name=name)

        data = dump_eus_model(eus_path)
        _, link_names = _unique_name_map(data["links"])
        eus_pos = {link_names[link["name"]]: np.array(link["pos"])
                   for link in data["links"]}

        urdf = open(urdf_path).read().replace(
            f"package://{_ros_package_name(name)}/", out_dir + "/")
        abs_path = osp.join(out_dir, "urdf", "_abs.urdf")
        with open(abs_path, "w") as f:
            f.write(urdf)
        robot = RobotModel()
        with open(abs_path) as f:
            robot.load_urdf_file(f)

        self.assertEqual(len(robot.link_list), len(data["links"]))
        for link in robot.link_list:
            err = np.linalg.norm(link.worldpos() * 1000.0 - eus_pos[link.name])
            self.assertLess(err, 1e-2, f"{name}:{link.name} world-pos mismatch")

    def test_representative_models(self):
        for name in self.REPRESENTATIVE:
            with self.subTest(model=name):
                self._convert_and_validate(name)

    @unittest.skipUnless(
        os.environ.get("URDFEUS_TEST_ALL_JSKEUS"),
        "set URDFEUS_TEST_ALL_JSKEUS=1 to convert every jskeus model")
    def test_all_models(self):
        from concurrent.futures import ProcessPoolExecutor

        models = sorted(
            glob.glob(osp.join(jskeus_models_dir, "*-robot.l"))
            + glob.glob(osp.join(jskeus_models_dir, "*-object.l")))
        tasks = [(p, osp.basename(p)[:-2]) for p in models]
        failures = []
        with ProcessPoolExecutor(max_workers=os.cpu_count()) as ex:
            for name, ok, err in ex.map(_convert_one, tasks):
                if not ok:
                    failures.append((name, err))
        self.assertFalse(
            failures, f"{len(failures)}/{len(tasks)} failed: {failures[:10]}")


class TestInertialRepair(unittest.TestCase):
    """``<inertial>`` for links whose EusLisp tensor cannot be used.

    jskeus models such as h3/h7 declare ``:inertia-tensor`` as a zero matrix
    and macra/human declare placeholders next to a real mass, so these run
    without irteusgl on a hand-built dump entry.
    """

    EXTENTS = (0.1, 0.2, 0.3)  # metres
    MASS = 2.0  # kg

    def setUp(self):
        self.mesh = trimesh.creation.box(extents=self.EXTENTS)
        a, b, c = self.EXTENTS
        self.analytic = self.MASS / 12.0 * np.diag(
            [b * b + c * c, a * a + c * c, a * a + b * b])

    def _link(self, inertia_kgm2):
        """A dump ``links`` entry in EusLisp units (g, mm, g*mm^2)."""
        return {
            "name": ":test-link",
            "weight": self.MASS * 1000.0,
            "centroid": [0.0, 0.0, 0.0],
            "inertia": None if inertia_kgm2 is None
            else (np.asarray(inertia_kgm2) * 1e9).tolist(),
        }

    def _emitted(self, link_el):
        inertia = link_el.find("inertial/inertia")
        return np.array([
            [float(inertia.get("ixx")), float(inertia.get("ixy")),
             float(inertia.get("ixz"))],
            [float(inertia.get("ixy")), float(inertia.get("iyy")),
             float(inertia.get("iyz"))],
            [float(inertia.get("ixz")), float(inertia.get("iyz")),
             float(inertia.get("izz"))]])

    def test_usable_tensor_is_written_unchanged(self):
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(self.analytic), self.mesh)
        self.assertIsNone(note)
        np.testing.assert_allclose(
            self._emitted(link_el), self.analytic, rtol=1e-7)

    def test_zero_tensor_is_recomputed_from_the_mesh(self):
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(np.zeros((3, 3))), self.mesh)
        self.assertIn("all-zero", note)
        # A box is watertight and convex, so the recomputed tensor is the
        # analytic one; any real link only gets its convex hull.
        np.testing.assert_allclose(
            self._emitted(link_el), self.analytic, rtol=1e-6)
        self.assertAlmostEqual(
            float(link_el.find("inertial/mass").get("value")), self.MASS)

    def test_missing_tensor_without_a_mesh_writes_no_inertial(self):
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(None), None)
        self.assertIn("no mesh", note)
        self.assertIsNone(link_el.find("inertial"))

    def test_placeholder_moment_is_recomputed(self):
        # human-robot declares izz = 1.0 g*mm^2 beside ixx = iyy = 2.07e8.
        placeholder = np.diag([self.analytic[0, 0], self.analytic[0, 0], 1e-9])
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(placeholder), self.mesh)
        self.assertIn("too small", note)
        np.testing.assert_allclose(
            self._emitted(link_el), self.analytic, rtol=1e-6)

    def test_non_positive_definite_tensor_is_recomputed(self):
        negative = np.diag([-self.analytic[0, 0], self.analytic[1, 1],
                            self.analytic[2, 2]])
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(negative), self.mesh)
        self.assertIn("not positive definite", note)
        np.testing.assert_allclose(
            self._emitted(link_el), self.analytic, rtol=1e-6)

    def test_triangle_inequality_violation_is_recomputed(self):
        # Two small moments that cannot add up to the third.
        broken = np.diag([1e-4, 1e-4, 1.0])
        link_el = ET.Element("link")
        note = _add_inertial(link_el, self._link(broken), self.mesh)
        self.assertIn("triangle inequality", note)
        np.testing.assert_allclose(
            self._emitted(link_el), self.analytic, rtol=1e-6)

    def test_a_thin_rod_is_left_alone(self):
        # The magnitude floor must not fire on a real, very slender body: a rod
        # 1000x longer than it is thick still has a usable smallest moment.
        length, radius = 1.0, 5e-4
        rod = trimesh.creation.cylinder(radius=radius, height=length)
        mass = 1.0
        tensor = np.diag([
            mass * (3 * radius ** 2 + length ** 2) / 12.0,
            mass * (3 * radius ** 2 + length ** 2) / 12.0,
            mass * radius ** 2 / 2.0])
        link = self._link(tensor)
        link["weight"] = mass * 1000.0
        link_el = ET.Element("link")
        self.assertIsNone(_add_inertial(link_el, link, rod))
