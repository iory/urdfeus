import os
import os.path as osp
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

        cmds = [
            f"mesh2eus {target_mesh} {output_eus_path}",
            f"mesh2eus {target_mesh} {output_eus_path} --voxel-size 0.001",
        ]
        for cmd in cmds:
            result = run_command(cmd)
            assert result.returncode == 0

    def test_urdf2eus(self):
        output_eus_path = osp.join(osp.dirname(self.urdfpath), "fetch.l")
        yaml_path = osp.join(data_dir, "fetch.yaml")

        cmds = [
            f"urdf2eus {self.urdfpath} {output_eus_path}",
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
            f"urdf2eus {self.urdfpath} {output_eus_path} --name _test_robot --voxel-size 0.001",
        ]

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
        import shutil
        shutil.rmtree(temp_dir, ignore_errors=True)
