import os.path as osp
import sys
import tempfile

from urdfeus.templates.urdf_template import urdf_template
from urdfeus.urdf2eus import urdf2eus


def mesh2eus(mesh_path, simplify_vertex_clustering_voxel_size=None,
             fp=sys.stdout, mesh_name=None):
    if mesh_name is None:
        mesh_name, _ = osp.splitext(osp.basename(mesh_path))
    collision_mesh_filepath = osp.abspath(mesh_path)
    visual_mesh_filepath = osp.abspath(mesh_path)

    formatted_urdf = urdf_template.format(
        mesh_name=mesh_name,
        collision_mesh_filepath=collision_mesh_filepath,
        visual_mesh_filepath=visual_mesh_filepath
    )

    with tempfile.NamedTemporaryFile(delete=True) as temp:
        temp_urdf_path = temp.name
        temp.write(formatted_urdf.encode('utf-8'))
        temp.seek(0)
        voxel_size = simplify_vertex_clustering_voxel_size
        urdf2eus(temp_urdf_path,
                 simplify_vertex_clustering_voxel_size=voxel_size,
                 fp=fp)
