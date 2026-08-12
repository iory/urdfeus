#!/usr/bin/env python

import argparse

from urdfeus.apps.common import add_diagnostic_arguments
from urdfeus.apps.common import handle_doctor
from urdfeus.eus2urdf import eus2urdf


def main():
    parser = argparse.ArgumentParser(
        description="Convert an EusLisp robot model to a URDF ROS package "
        + "(package.xml + urdf/ + meshes/).")
    parser.add_argument("input_euslisp_path", type=str, nargs="?",
                        help="Input EusLisp .l model path")
    parser.add_argument("output_dir", type=str, nargs="?",
                        help="Output ROS package directory to create")
    parser.add_argument(
        "--package-name", type=str, default=None,
        help="ROS package name used in package:// mesh paths. "
        + "Defaults to the output directory's base name.")
    parser.add_argument(
        "--name", type=str, default=None,
        help="Robot name (<robot name> and urdf file stem). "
        + "Defaults to the robot name reported by the EusLisp model.")
    parser.add_argument(
        "--constructor", type=str, default=None,
        help="EusLisp constructor function name. "
        + "Defaults to the model file name without extension.")
    parser.add_argument(
        "--mesh-format", type=str, default="glb",
        help="Mesh file format/extension understood by trimesh.export "
        + "(e.g. glb, ply, obj, dae, stl). Default: glb. "
        + "glb/ply/obj preserve per-face colors; dae flattens them to a "
        + "texture (multi-color meshes become gray) and stl has no color.")
    parser.add_argument(
        "--draco", action="store_true",
        help="Compress glb meshes with Draco (KHR_draco_mesh_compression) "
        + "via scikit-robot, preserving per-vertex color. Shrinks dense "
        + "meshes by ~an order of magnitude. Forces glb and needs DracoPy; "
        + "a glTF loader needs a Draco decoder to read the output.")
    parser.add_argument(
        "--irteusgl", type=str, default="irteusgl",
        help="irteusgl executable to use.")
    add_diagnostic_arguments(parser)
    args = parser.parse_args()
    if handle_doctor(args, parser, ['input_euslisp_path', 'output_dir']):
        return

    urdf_path = eus2urdf(
        args.input_euslisp_path,
        args.output_dir,
        package_name=args.package_name,
        robot_name=args.name,
        constructor=args.constructor,
        mesh_format=args.mesh_format,
        draco=args.draco,
        irteusgl=args.irteusgl,
    )
    print(f"Wrote URDF: {urdf_path}")


if __name__ == "__main__":
    main()
