import glob
import os
import os.path as osp
import shutil
import tempfile
import unittest

import numpy as np

from urdfeus.eus2urdf import dump_eus_model
from urdfeus.eus2urdf import eus2urdf
from urdfeus.eus2urdf import load_eus_model
from urdfeus.eus_parse import EusParseError
from urdfeus.eus_parse import parse_eus_model
from urdfeus.eus_reader import Dotted
from urdfeus.eus_reader import read_forms
from urdfeus.eus_reader import Symbol

# Repository-root euslisp/ directory holding test models.
euslisp_dir = osp.abspath(
    osp.join(osp.dirname(__file__), "..", "..", "euslisp"))
sample_model = osp.join(euslisp_dir, "yamaguchi_6axis_arm_nejineji.l")


def is_irteusgl_available():
    return shutil.which("irteusgl") is not None


def find_jskeus_models_dir():
    patterns = [
        os.environ.get("JSKEUS_MODELS_DIR", ""),
        "/opt/ros/*/share/euslisp/jskeus/eus/models",
        "/usr/share/euslisp/jskeus/eus/models",
        osp.expanduser("~/src/github.com/euslisp/jskeus/eus/models"),
    ]
    for pattern in patterns:
        if not pattern:
            continue
        for directory in sorted(glob.glob(pattern)):
            if glob.glob(osp.join(directory, "*-object.l")):
                return directory
    return None


class TestEusReader(unittest.TestCase):

    def test_atoms(self):
        forms = read_forms('(setq a 1 b -2.5 c "s" d :key)')
        self.assertEqual(forms[0][0], Symbol("setq"))
        self.assertEqual(forms[0][2], 1)
        self.assertEqual(forms[0][4], -2.5)
        self.assertEqual(forms[0][6], "s")
        self.assertTrue(forms[0][8].keywordp)

    def test_symbols_are_case_insensitive(self):
        # The EusLisp reader upcases symbols, so :Peru and :peru are one name.
        self.assertEqual(read_forms(":Peru")[0], read_forms(":peru")[0])

    def test_vector_and_matrix_literals(self):
        forms = read_forms("(#f(1 2 3.5) #i(4 5) #2f((1 0)(0 1)))")
        self.assertTrue(np.allclose(forms[0][0], [1, 2, 3.5]))
        self.assertEqual(list(forms[0][1]), [4, 5])
        self.assertTrue(np.allclose(forms[0][2], np.eye(2)))

    def test_dotted_pair(self):
        form = read_forms("(setq (obj . slot) 1)")[0]
        self.assertIsInstance(form[1], Dotted)
        self.assertEqual(form[1].cdr, Symbol("slot"))

    def test_comments_are_skipped(self):
        self.assertEqual(read_forms(";; comment\n(a) ; trailing\n"),
                         [[Symbol("a")]])


@unittest.skipUnless(osp.isfile(sample_model), "sample model not present")
class TestParseGeneratedModel(unittest.TestCase):
    """The static parser on the repository's own urdfeus-generated model."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.data = parse_eus_model(sample_model)

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_kinematics(self):
        data = self.data
        self.assertEqual(data["robot_name"], "yamaguchi_6axis_arm_nejineji")
        self.assertGreater(len(data["links"]), 1)
        names = {link["name"] for link in data["links"]}
        self.assertEqual(data["root_link"] in names, True)
        # Every joint connects two dumped links, and the tree has one root.
        rooted = [link for link in data["links"] if link["parent"] is None
                  or link["parent"] not in names]
        self.assertEqual(len(rooted), 1)
        for joint in data["joints"]:
            self.assertIn(joint["parent"], names)
            self.assertIn(joint["child"], names)
            self.assertEqual(joint["q"], 0.0)

    def test_meshes_have_geometry(self):
        vertices = sum(len(sub["vertices"])
                       for link in self.data["links"]
                       for body in link["meshes"] for sub in body)
        self.assertGreater(vertices, 0)

    def test_static_backend_writes_urdf(self):
        out_dir = osp.join(self.tmp, "pkg")
        urdf_path = eus2urdf(sample_model, out_dir, package_name="pkg",
                             backend="static")
        self.assertTrue(osp.isfile(urdf_path))
        self.assertTrue(osp.isfile(osp.join(out_dir, "package.xml")))
        self.assertGreater(len(os.listdir(osp.join(out_dir, "meshes"))), 0)

    def test_unknown_constructor(self):
        with self.assertRaises(EusParseError):
            parse_eus_model(sample_model, constructor="no-such-thing")


def _assert_dumps_match(case, reference, parsed):
    """The two dumps must agree, allowing for irteusgl's print precision.

    irteusgl writes floats with about six significant digits, so it is the
    less precise of the two and the comparison is relative.
    """

    def close(a, b, tol=1e-4):
        case.assertEqual(a is None, b is None)
        if a is None:
            return
        a, b = np.asarray(a, np.float64), np.asarray(b, np.float64)
        case.assertEqual(a.shape, b.shape)
        case.assertTrue(np.allclose(a, b, atol=tol, rtol=1e-5))

    case.assertEqual(reference["robot_name"], parsed["robot_name"])
    case.assertEqual(reference["root_link"], parsed["root_link"])
    case.assertEqual([link["name"] for link in reference["links"]],
                     [link["name"] for link in parsed["links"]])
    for a, b in zip(reference["links"], parsed["links"]):
        case.assertEqual(a["parent"], b["parent"])
        case.assertEqual(a["joint"], b["joint"])
        close(a["pos"], b["pos"])
        close(a["rot"], b["rot"])
        close(a["centroid"], b["centroid"])
        close(a["weight"], b["weight"], tol=1e-6)
        submeshes_a = [sub for body in a["meshes"] for sub in body]
        submeshes_b = [sub for body in b["meshes"] for sub in body]
        case.assertEqual(len(submeshes_a), len(submeshes_b))
        for mesh_a, mesh_b in zip(submeshes_a, submeshes_b):
            close(mesh_a["ambient"], mesh_b["ambient"])
            case.assertEqual(mesh_a["indices"], mesh_b["indices"])
            close(mesh_a["vertices"], mesh_b["vertices"], tol=1e-3)
    case.assertEqual([joint["name"] for joint in reference["joints"]],
                     [joint["name"] for joint in parsed["joints"]])
    for a, b in zip(reference["joints"], parsed["joints"]):
        case.assertEqual(a["jtype"], b["jtype"])
        case.assertEqual(a["movable"], b["movable"])
        case.assertEqual((a["parent"], a["child"]), (b["parent"], b["child"]))
        close(a["axis"], b["axis"])
        close(a["min"], b["min"])
        close(a["max"], b["max"])
    case.assertEqual([frame["name"] for frame in reference["frames"]],
                     [frame["name"] for frame in parsed["frames"]])


@unittest.skipUnless(is_irteusgl_available(), "irteusgl not available")
@unittest.skipUnless(osp.isfile(sample_model), "sample model not present")
class TestAgainstIrteusgl(unittest.TestCase):
    """The parser must report what irteusgl reports for the same model."""

    def test_generated_robot(self):
        _assert_dumps_match(self, dump_eus_model(sample_model),
                            parse_eus_model(sample_model))

    def test_jskeus_objects(self):
        models_dir = find_jskeus_models_dir()
        if models_dir is None:
            self.skipTest("jskeus models not found")
        # One of each shape the object generator emits: a single link, a
        # multi-link object with joints and handles, and one with a texture.
        for name in ("audio_shelf-object.l",
                     "73b2-cupboard-without-door-object.l",
                     "arrow-object.l"):
            path = osp.join(models_dir, name)
            if not osp.isfile(path):
                continue
            with self.subTest(model=name):
                _assert_dumps_match(self, dump_eus_model(path),
                                    parse_eus_model(path))

    def test_auto_backend_matches_irteusgl(self):
        # With irteusgl on PATH, 'auto' must go through it.
        self.assertEqual(
            load_eus_model(sample_model, backend="auto")["robot_name"],
            dump_eus_model(sample_model)["robot_name"])


class TestUnsupportedInput(unittest.TestCase):

    def test_scene_is_reported(self):
        models_dir = find_jskeus_models_dir()
        if models_dir is None:
            self.skipTest("jskeus models not found")
        scene = osp.join(models_dir, "room73b2-scene.l")
        if not osp.isfile(scene):
            self.skipTest("scene model not found")
        with self.assertRaises(EusParseError) as caught:
            parse_eus_model(scene)
        self.assertIn("scene", str(caught.exception))

    def test_hand_written_model_is_not_guessed(self):
        # A model that computes its links is outside the subset: the parser
        # must say so rather than return an empty or half-built model.
        source = """(defclass foo-object :super cascaded-link :slots ())
(defmethod foo-object
  (:init (&rest args)
    (send-super* :init :name "foo" args)
    (setq links (list (compute-links)))
    self))
"""
        path = osp.join(tempfile.mkdtemp(), "foo-object.l")
        with open(path, "w") as f:
            f.write(source)
        try:
            with self.assertRaises(EusParseError):
                parse_eus_model(path)
        finally:
            shutil.rmtree(osp.dirname(path), ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
