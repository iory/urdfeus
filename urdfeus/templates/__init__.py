import os.path as osp

data_dir = osp.abspath(osp.dirname(__file__))


def get_euscollada_string():
    with open(osp.join(data_dir, "euscollada-robot.l")) as f:
        return f.read()


def get_urdfeus_glvertices_string():
    with open(osp.join(data_dir, "urdfeus-glvertices.l")) as f:
        return f.read()
