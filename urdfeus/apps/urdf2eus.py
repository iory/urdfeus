#!/usr/bin/env python

import argparse

from urdfeus.urdf2eus import urdf2eus


def main():
    parser = argparse.ArgumentParser(description="Convert URDF to Euslisp")
    parser.add_argument("input_urdf_path", type=str, help="Input URDF path")
    parser.add_argument("output_euslisp_path", type=str, help="Output Euslisp path")
    parser.add_argument("--yaml-path", type=str, default=None, help="Config yaml path")
    parser.add_argument("--name", type=str, default=None,
                       help="Custom robot name for EusLisp functions (defun <name>). "
                       + "Must be a valid EusLisp identifier (letters, digits, _, - only). "
                       + "If not specified, uses the robot name from URDF.")
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
    parser.add_argument(
        "--no-cache",
        action="store_true",
        help="Disable mesh caching. By default, processed mesh data is cached "
        + "to speed up repeated conversions of the same URDF file.",
    )
    parser.add_argument(
        "--use-urdf-material",
        action="store_true",
        help="Use the color specified in the URDF <material> tag instead of "
        + "the mesh's internal color. This prevents splitting the mesh by "
        + "face color and uses the material color loaded by skrobot.",
    )
    args = parser.parse_args()

    with open(args.output_euslisp_path, "w") as f:
        urdf2eus(
            args.input_urdf_path,
            args.yaml_path,
            args.simplify_vertex_clustering_voxel_size,
            args.name,
            fp=f,
            use_cache=not args.no_cache,
            use_urdf_material=args.use_urdf_material,
        )


if __name__ == "__main__":
    main()
