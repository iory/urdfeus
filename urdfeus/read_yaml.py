import sys

import yaml

from urdfeus.common import meter2millimeter


def read_config_from_yaml(
        robot, config_file,
        fp=sys.stdout, add_link_suffix=True, add_joint_suffix=True):
    print('\n', end='', file=fp)
    with open(config_file, 'r') as file:
        doc = yaml.load(file, Loader=yaml.FullLoader)

    limb_names = []
    for limb in [k for k in doc.keys() if k.endswith('-end-coords')]:
        suffix_to_remove = '-end-coords'
        limb_name = limb[:-len(suffix_to_remove)]
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
        limb_names.append(limb_name)

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

    print("     ;; init-ending\n", file=fp)
    print("     (send self :init-ending) ;; :urdf\n", file=fp)
    print("     ;; overwrite bodies to return draw-things links not (send link :bodies)\n", file=fp)  # NOQA
    print("     (setq bodies (flatten (mapcar #'(lambda (b) (if (find-method b :bodies) (send b :bodies))) (list", end="", file=fp)  # NOQA
    for link in robot.link_list:
        if add_link_suffix:
            print(f" {link.name}_lk", end="", file=fp)
        else:
            print(f" {link.name}", end="", file=fp)
    print("))))\n", file=fp)

    print("     (when (member :reset-pose (send self :methods))", file=fp)
    print("           (send self :reset-pose)) ;; :set reset-pose\n", file=fp)
    print("     self)) ;; end of :init", file=fp)

    if 'angle-vector' in doc:
        n = doc['angle-vector']
        if len(n) > 0:
            print("  ;; pre-defined pose methods\n", file=fp)

        for name, v in n.items():
            limbs_symbols = " ".join([f":{limb[0]}" for limb in limbs])
            print(f"\n    (:{name} (&optional (limbs '({limbs_symbols})))\n",
                  file=fp)
            print(f"      \"Predefined pose named {name}.\"\n", file=fp)
            print("      (unless (listp limbs) (setq limbs (list limbs)))\n",
                  file=fp)
            print("      (dolist (limb limbs)\n", file=fp)
            print("        (case limb", file=fp)

            i_joint = 0
            for limb in limbs:
                limb_name = limb[0]
                print(f"\n          (:{limb_name} (send self limb :angle-vector (float-vector", file=fp)  # NOQA
                joint_names = limb[1][1]

                for j in range(len(joint_names)):
                    try:
                        angle_value = v[i_joint]
                        print(f" {angle_value}", file=fp)
                        i_joint += 1
                    except IndexError as e:  # NOQA
                        sys.stderr.write("****** Angle-vector may be shorter than joint-list, please fix .yaml ******\n")  # NOQA
                        while j < len(joint_names):
                            print(" 0.0", file=fp)  # padding dummy
                            j += 1

                print(")))", file=fp)

            print("\n          (t (format t \"Unknown limb is passed: ~a~%\" limb))", file=fp)  # NOQA
            print("))\n      (send self :angle-vector))", file=fp)
    return limb_names
