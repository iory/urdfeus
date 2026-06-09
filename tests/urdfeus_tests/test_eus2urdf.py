import glob
import os
import os.path as osp
import shutil
import subprocess
import tempfile
import unittest

import numpy as np
from skrobot.model import RobotModel

from urdfeus.eus2urdf import dump_eus_model
from urdfeus.eus2urdf import eus2urdf

# Repository-root euslisp/ directory holding test models.
euslisp_dir = osp.abspath(
    osp.join(osp.dirname(__file__), "..", "..", "euslisp"))


def is_irteusgl_available():
    return shutil.which("irteusgl") is not None


def repo_models_available():
    """True if the sample EusLisp models live under the repo's euslisp/ dir."""
    return osp.isfile(
        osp.join(euslisp_dir, "yamaguchi_4axis_arm_nejineji_short.l"))


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

    model_name = "yamaguchi_4axis_arm_nejineji_short"

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

        # Expected kinematics straight from the EusLisp model.
        data = dump_eus_model(self.eus_path)
        eus_pos = {link["name"]: np.array(link["pos"]) for link in data["links"]}

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

    def test_procedural_links_and_plain_bodies(self):
        # yamaguchi-arm.l adds rigid legs / vacuum pads in :init using plain
        # make-cube bodies (no glvertices). All such links and their primitive
        # geometry must be captured.
        eus_path = osp.join(euslisp_dir, "yamaguchi-arm.l")
        out_dir = osp.join(self.tmp, "yarm")
        urdf_path = eus2urdf(eus_path, out_dir, package_name="yarm")

        import xml.etree.ElementTree as ET
        root = ET.parse(urdf_path).getroot()
        link_names = [link.get("name") for link in root.findall("link")]
        # procedurally-added links exist
        for name in ("rigid_leg0_link", "vacuum_pad", "base_cube"):
            self.assertIn(name, link_names)
        # every link has a visual mesh (plain bodies are exported too)
        for link in root.findall("link"):
            self.assertIsNotNone(
                link.find("visual"),
                f"link {link.get('name')} has no visual")

        # plain cube geometry has the expected size (make-cube 10 10 1 -> mm).
        import trimesh
        leg = trimesh.load(
            osp.join(out_dir, "meshes", "rigid_leg0_link.glb"), force="mesh")
        extents = leg.bounds[1] - leg.bounds[0]
        np.testing.assert_allclose(
            sorted(extents), [0.001, 0.01, 0.01], atol=1e-4)

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
        eus_pos = {link["name"]: np.array(link["pos"]) for link in data["links"]}

        urdf = open(urdf_path).read().replace(
            f"package://{name}/", out_dir + "/")
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
