#!/usr/bin/env python

import argparse

from urdfeus.mesh2eus import mesh2eus


def main():
    parser = argparse.ArgumentParser(description='Convert mesh to Euslisp')
    parser.add_argument('input_mesh_path', type=str,
                        help='Input mesh file path')
    parser.add_argument('output_euslisp_path', type=str,
                        help='Output Euslisp path')
    args = parser.parse_args()
    with open(args.output_euslisp_path, 'w') as f:
        mesh2eus(args.input_mesh_path,
                 fp=f)


if __name__ == '__main__':
    main()