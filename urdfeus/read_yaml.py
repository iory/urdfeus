import sys

import yaml

from urdfeus.common import meter2millimeter


def read_config_from_yaml(
        robot, config_file,
        fp=sys.stdout, add_link_suffix=True, add_joint_suffix=True):
    print('\n', end='', file=fp)
    with open(config_file, 'r') as file:
        doc = yaml.load(file, Loader=yaml.FullLoader)

    for limb in [k for k in doc.keys() if k.endswith('-end-coords')]:
        limb_name = limb.rstrip('-end-coords')
        end_coords_parent_name = doc[f"{limb_name}-end-coords"]["parent"]

        if add_link_suffix:
            print(f"     (setq {limb_name}-end-coords (make-cascoords :coords (send {end_coords_parent_name}_lk :copy-worldcoords) :name :{limb_name}-end-coords))", file=fp)  # NOQA
        else:
            print(f"     (setq {limb_name}-end-coords (make-cascoords :coords (send {end_coords_parent_name} :copy-worldcoords) :name {limb_name}-end-coords))", file=fp)  # NOQA

        try:
            n = doc[f"{limb_name}-end-coords"]["translate"]
            values = [meter2millimeter * val for val in n[:3]]
            print(f"     (send {limb_name}-end-coords :translate (float-vector {' '.join(map(str, values))}))", file=fp)  # NOQA
        except Exception as _:  # NOQA
            pass

        try:
            n = doc[f"{limb_name}-end-coords"]["rotate"]
            if n:
                values = [val for val in n[:3]]
                rotation_value = (3.141592653589793/180) * n[3]
                print(f"     (send {limb_name}-end-coords :rotate {rotation_value} (float-vector {' '.join(map(str, values))}))", file=fp)  # NOQA
        except Exception as _:  # NOQA
            pass

        if add_link_suffix:
            print(f"     (send {end_coords_parent_name}_lk :assoc {limb_name}-end-coords)", file=fp)  # NOQA
        else:
            print(f"     (send {end_coords_parent_name} :assoc {limb_name}-end-coords)", file=fp)  # NOQA

    print("", file=fp)
    print("     ;; limbs", file=fp)

    limb_candidates = [k for k in doc.keys()
                       if not k.endswith('-coords')
                       and not k.endswith('-vector')]
    limb_order = [(limb, idx) for idx, limb in enumerate(limb_candidates)]
    limb_order.sort(key=lambda x: x[1])

    limbs = []
    for limb, _ in limb_order:
        limb_name = limb
        tmp_link_names = []
        tmp_joint_names = []
        try:
            limb_doc = doc[limb_name]
        except Exception as _:  # NOQA
            continue

        for item in limb_doc:
            for key, value in item.items():
                if key in robot.__dict__:
                    joint = robot.__dict__[key]
                    tmp_joint_names.append(key)
                    tmp_link_names.append(joint.child_link.name)
        if tmp_link_names:
            print(f"     (setq {limb_name} (list", end="", file=fp)
            if add_link_suffix:
                for link in tmp_link_names:
                    print(f" {link}_lk", end="", file=fp)
            else:
                for link in tmp_link_names:
                    print(f" {link}", end="", file=fp)
            print("))", file=fp)
            print("", file=fp)

            print(f"     (setq {limb_name}-root-link", file=fp)
            print(f"           (labels ((find-parent (l) (if (find (send l :parent) {limb_name}) (find-parent (send l :parent)) l)))", file=fp)  # NOQA
            print(f"             (find-parent (car {limb_name}))))", file=fp)
        limbs.append((limb, (tmp_link_names, tmp_joint_names)))
    print("", file=fp)
    print("     ;; links", file=fp)
    if add_link_suffix:
        print(f"     (setq links (list {robot.__dict__['root_link'].name}_lk", end="", file=fp)  # NOQA
    else:
        print(f"     (setq links (list {robot.__dict__['root_link'].name}", end="", file=fp)  # NOQA

    for limb, (link_names, joint_names) in limbs:
        if add_link_suffix:
            for link in link_names:
                print(f" {link}_lk", end="", file=fp)
        else:
            for link in link_names:
                print(f" {link}", end="", file=fp)

    print("))", file=fp)
    print("", file=fp)
    print("     ;; joint-list", file=fp)
    print("     (setq joint-list (list", end="", file=fp)
    for limb, (link_names, joint_names) in limbs:
        if add_joint_suffix:
            for joint in joint_names:
                print(f" {joint}_jt", end="", file=fp)
        else:
            for joint in joint_names:
                print(f" {joint}", end="", file=fp)
    print("))", file=fp)
    print("", file=fp)
