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
