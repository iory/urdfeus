#!/usr/bin/env python

import argparse

from urdfeus.urdf2eus import urdf2eus


def main():
    parser = argparse.ArgumentParser(description="Convert URDF to Euslisp")
    parser.add_argument("input_urdf_path", type=str, help="Input URDF path")
    parser.add_argument("output_euslisp_path", type=str, help="Output Euslisp path")
    parser.add_argument("--yaml-path", type=str, default=None, help="Config yaml path")
    parser.add_argument(
        "--simplify-vertex-clustering-voxel-size",
        "--voxel-size",
        default=None,
        type=float,
        help="Specifies the voxel size for the simplify_vertex_clustering"
        + " function in open3d. When this value is provided, "
        + "it is used as the voxel size in the function to perform "
        + "mesh simplification. This process reduces the complexity"
        + " of the mesh by clustering vertices within the specified voxel size.",
    )
    args = parser.parse_args()
    with open(args.output_euslisp_path, "w") as f:
        urdf2eus(
            args.input_urdf_path,
            args.yaml_path,
            args.simplify_vertex_clustering_voxel_size,
            fp=f,
        )


if __name__ == "__main__":
    main()
