import unittest

from skrobot.data import fetch_urdfpath
from skrobot.data import pr2_urdfpath

from urdfeus.urdf2eus import urdf2eus


class TestURDF2EUS(unittest.TestCase):

    def test_urdf2eus(self):
        urdf2eus(fetch_urdfpath())
        urdf2eus(pr2_urdfpath())
