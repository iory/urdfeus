#!/usr/bin/env python

import argparse

from urdfeus.mesh2eus import mesh2eus


def main():
    parser = argparse.ArgumentParser(description='Convert mesh to Euslisp')
    parser.add_argument('input_mesh_path', type=str,
                        help='Input mesh file path')
    parser.add_argument('output_euslisp_path', type=str,
                        help='Output Euslisp path')
    parser.add_argument(
        '--simplify-vertex-clustering-voxel-size', '--voxel-size',
        default=None,
        type=float,
        help='Specifies the voxel size for the simplify_vertex_clustering'
        ' function in open3d. When this value is provided, '
        'it is used as the voxel size in the function to perform '
        'mesh simplification. This process reduces the complexity'
        ' of the mesh by clustering vertices within the specified voxel size.')
    args = parser.parse_args()
    with open(args.output_euslisp_path, 'w') as f:
        mesh2eus(args.input_mesh_path,
                 args.simplify_vertex_clustering_voxel_size,
                 fp=f)


if __name__ == '__main__':
    main()
