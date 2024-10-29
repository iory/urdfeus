import os.path as osp
import unittest

from skrobot.data import fetch_urdfpath
from skrobot.data import pr2_urdfpath

from urdfeus.urdf2eus import urdf2eus

data_dir = osp.abspath(osp.dirname(__file__))


class TestURDF2EUS(unittest.TestCase):
    def test_urdf2eus(self):
        urdf2eus(fetch_urdfpath())
        urdf2eus(fetch_urdfpath(), osp.join(data_dir, "fetch.yaml"))
        urdf2eus(pr2_urdfpath())

        urdf2eus(pr2_urdfpath(), simplify_vertex_clustering_voxel_size=0.001)
