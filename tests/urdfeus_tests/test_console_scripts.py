import os
import os.path as osp
import shutil
import subprocess
import tempfile
import unittest

from skrobot.data import fetch_urdfpath

data_dir = osp.abspath(osp.dirname(__file__))


def run_command(cmd):
    kwargs = {}
    kwargs["stdout"] = subprocess.PIPE
    kwargs["stderr"] = subprocess.PIPE
    result = subprocess.run(cmd, shell=True, **kwargs)
    return result


def is_open3d_available():
    """Mesh simplification goes through open3d, which has no wheel past cp312."""
    try:
        import open3d  # noqa: F401
    except ImportError:
        return False
    return True


def is_euslisp_available():
    """Check if roseus/euslisp is available."""
    result = run_command("which roseus")
    return result.returncode == 0


class TestConsoleScripts(unittest.TestCase):
    tmp_output = None
    urdfpath = None

    @classmethod
    def setUpClass(cls):
        cls.tmp_output = tempfile.TemporaryDirectory()
        os.environ["SKROBOT_CACHE_DIR"] = cls.tmp_output.name
        cls.urdfpath = fetch_urdfpath()

    def test_mesh2eus(self):
        target_mesh = osp.join(osp.dirname(self.urdfpath), "meshes", "base_link.dae")
        output_eus_path = osp.join(osp.dirname(self.urdfpath), "meshes", "base_link.l")

        result = run_command(f"mesh2eus {target_mesh} {output_eus_path}")
        assert result.returncode == 0

    @unittest.skipUnless(is_open3d_available(), "open3d not available")
    def test_mesh2eus_with_voxel_size(self):
        target_mesh = osp.join(osp.dirname(self.urdfpath), "meshes", "base_link.dae")
        output_eus_path = osp.join(osp.dirname(self.urdfpath), "meshes", "base_link.l")

        result = run_command(
            f"mesh2eus {target_mesh} {output_eus_path} --voxel-size 0.001")
        assert result.returncode == 0

    def test_urdf2eus(self):
        output_eus_path = osp.join(osp.dirname(self.urdfpath), "fetch.l")

        result = run_command(f"urdf2eus {self.urdfpath} {output_eus_path}")
        assert result.returncode == 0

    @unittest.skipUnless(is_open3d_available(), "open3d not available")
    def test_urdf2eus_with_voxel_size(self):
        output_eus_path = osp.join(osp.dirname(self.urdfpath), "fetch.l")
        yaml_path = osp.join(data_dir, "fetch.yaml")

        cmds = [
            f"urdf2eus {self.urdfpath} {output_eus_path} --voxel-size 0.001",
            f"urdf2eus {self.urdfpath} {output_eus_path}"
            + f" --voxel-size 0.001 --yaml-path {yaml_path}",
        ]
        for cmd in cmds:
            result = run_command(cmd)
            assert result.returncode == 0

    def test_urdf2eus_custom_name(self):
        """Test urdf2eus command with custom robot name."""
        temp_dir = tempfile.mkdtemp()
        output_eus_path = osp.join(temp_dir, "custom_robot.l")
        yaml_path = osp.join(data_dir, "fetch.yaml")

        # Test with valid custom name
        valid_name_cmds = [
            f"urdf2eus {self.urdfpath} {output_eus_path} --name my_robot",
            f"urdf2eus {self.urdfpath} {output_eus_path} --name robot-v1 --yaml-path {yaml_path}",
        ]
        if is_open3d_available():
            valid_name_cmds.append(
                f"urdf2eus {self.urdfpath} {output_eus_path}"
                + " --name _test_robot --voxel-size 0.001")

        for cmd in valid_name_cmds:
            with self.subTest(cmd=cmd):
                result = run_command(cmd)
                self.assertEqual(result.returncode, 0,
                               f"Command failed: {cmd}\nstderr: {result.stderr.decode()}")

                # Check if the custom name appears in the output file
                if osp.exists(output_eus_path):
                    with open(output_eus_path) as f:
                        content = f.read()
                        # Extract name from command
                        name = cmd.split('--name ')[1].split()[0]
                        self.assertIn(f"defun {name}", content)
                        self.assertIn(f"defclass {name}-robot", content)

        # Test with invalid custom names
        invalid_name_cmds = [
            f"urdf2eus {self.urdfpath} {output_eus_path} --name 123invalid",
            f"urdf2eus {self.urdfpath} {output_eus_path} --name 'robot name'",
            f"urdf2eus {self.urdfpath} {output_eus_path} --name robot.invalid",
            f"urdf2eus {self.urdfpath} {output_eus_path} --name if",
            f"urdf2eus {self.urdfpath} {output_eus_path} --name defun",
        ]

        for cmd in invalid_name_cmds:
            with self.subTest(cmd=cmd):
                result = run_command(cmd)
                self.assertNotEqual(result.returncode, 0,
                                  f"Command should have failed: {cmd}")
                self.assertIn("Invalid robot name", result.stderr.decode())

        # Clean up
        shutil.rmtree(temp_dir, ignore_errors=True)

    @unittest.skipUnless(is_euslisp_available(), "roseus not available")
    def test_pqp_collision_distance_with_robot_object(self):
        """Test that pqp-collision-distance works correctly with robot object.

        This test verifies that collision detection uses the actual mesh geometry
        rather than the default 10x10x10mm cube from collada-body.
        The test performs edge-case collision checks at the mesh boundary.
        """
        target_mesh = osp.join(
            osp.dirname(self.urdfpath), "meshes", "base_link.dae")
        temp_dir = tempfile.mkdtemp()
        output_eus_path = osp.join(temp_dir, "test_collision.l")

        try:
            # Generate euslisp file from mesh
            result = run_command(f"mesh2eus {target_mesh} {output_eus_path}")
            self.assertEqual(result.returncode, 0,
                             f"mesh2eus failed: {result.stderr.decode()}")

            # Find the robot creation function name from generated file
            # mesh2eus uses mesh internal name, not filename
            robot_func_name = None
            with open(output_eus_path) as f:
                for line in f:
                    if line.strip().startswith('(defun ') and 'jacobian' not in line:
                        # Extract function name: (defun NAME () ...)
                        robot_func_name = line.split()[1]
                        break

            self.assertIsNotNone(robot_func_name,
                                 "Could not find robot function in generated file")

            # Run collision test using external euslisp template
            # Pass parameters via environment variables
            test_template = osp.join(data_dir, "test_pqp_collision.l")
            env_vars = f"EUS_FILE_PATH={output_eus_path} ROBOT_FUNC_NAME={robot_func_name}"
            result = run_command(f"{env_vars} roseus {test_template}")

            # Parse results
            stdout = result.stdout.decode()
            self.assertEqual(result.returncode, 0,
                             f"roseus failed: {result.stderr.decode()}\n{stdout}")

            # Find result line
            result_line = None
            for line in stdout.split('\n'):
                if line.startswith('RESULT:'):
                    result_line = line.replace('RESULT:', '')
                    break

            self.assertIsNotNone(result_line,
                                 f"No result found in output: {stdout}")

            values = result_line.split(',')
            self.assertEqual(len(values), 7,
                             f"Unexpected result format: {result_line}")

            dist_outside_robot = float(values[0])
            dist_outside_link = float(values[1])
            dist_center_robot = float(values[2])
            dist_center_link = float(values[3])
            dist_large_robot = float(values[4])
            dist_large_link = float(values[5])
            robot_has_pqpmodel = int(values[6])

            # Assertions
            # 1. Robot should have pqpmodel (our fix)
            self.assertEqual(robot_has_pqpmodel, 1,
                             "Robot object should have pqpmodel set")

            # 2. Cube far outside should have positive distance
            self.assertGreater(dist_outside_robot, 0,
                               "Cube outside mesh should have positive distance")
            self.assertGreater(dist_outside_link, 0,
                               "Cube outside mesh should have positive distance")

            # 3. MOST IMPORTANT: robot and link should give SAME results
            # This is the main fix - previously robot used 10x10x10 cube
            # which gave different (incorrect) results
            self.assertEqual(dist_outside_robot, dist_outside_link,
                             "Robot and link should give same result for outside cube")
            self.assertEqual(dist_center_robot, dist_center_link,
                             "Robot and link should give same result for center cube")
            self.assertEqual(dist_large_robot, dist_large_link,
                             "Robot and link should give same result for large cube")

        finally:
            shutil.rmtree(temp_dir, ignore_errors=True)
