import os.path as osp
import unittest

from skrobot.data import fetch_urdfpath
from skrobot.data import pr2_urdfpath

from urdfeus.urdf2eus import urdf2eus

data_dir = osp.abspath(osp.dirname(__file__))


def is_open3d_available():
    """Mesh simplification goes through open3d, which has no wheel past cp312."""
    try:
        import open3d  # noqa: F401
    except ImportError:
        return False
    return True


class TestURDF2EUS(unittest.TestCase):
    def test_urdf2eus(self):
        urdf2eus(fetch_urdfpath())
        urdf2eus(fetch_urdfpath(), osp.join(data_dir, "fetch.yaml"))
        urdf2eus(pr2_urdfpath())
        urdf2eus(pr2_urdfpath(), osp.join(data_dir, "pr2.yaml"))

    @unittest.skipUnless(is_open3d_available(), "open3d not available")
    def test_urdf2eus_with_vertex_clustering(self):
        urdf2eus(pr2_urdfpath(), simplify_vertex_clustering_voxel_size=0.001)
