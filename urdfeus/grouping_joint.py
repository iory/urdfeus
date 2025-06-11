import collections

from skrobot.model import RobotModel
from skrobot.utils.urdf import no_mesh_load_mode


def group_joints_by_branches(parent_map):
    """Analyzes joint parent-child relationships to group them into chains.

    This function uses a parent map to determine the robot's structure
    and splits the joints into groups. A new group is started at a root
    (a joint with no parent) and at any joint that is a child of a
    "branching point" (a joint with more than one child).

    Parameters
    ----------
    parent_map : dict
        A dictionary mapping each child joint name (str) to its parent
        joint name (str). Root joints should map to None.
        Example: {'shoulder_pan_joint': 'torso_lift_joint', 'base_joint': None}

    Returns
    -------
    list of list of str
        A list of kinematic chains. Each chain is represented as a list
        of joint names.
    """
    # Step 1: Build helper data structures from the parent map
    children_map = collections.defaultdict(list)
    all_joints = set()
    for child, parent in parent_map.items():
        all_joints.add(child)
        if parent is not None:
            all_joints.add(parent)
            children_map[parent].append(child)

    # Step 2: Identify branching points (joints with multiple children)
    branching_points = {p for p, c in children_map.items() if len(c) > 1}

    # Step 3: Identify group starters (roots or children of branching points)
    group_starters = set()
    for joint in all_joints:
        parent = parent_map.get(joint)
        if parent is None:  # This is a root of a chain
            group_starters.add(joint)
        elif parent in branching_points:  # The parent is a branching point
            group_starters.add(joint)

    # Step 4: Traverse from each starter to form groups, stopping at the next branch
    final_groups = []
    processed_joints = set()

    # Sort starters for a consistent output order
    for starter in sorted(group_starters):
        if starter in processed_joints:
            continue

        group = []
        queue = collections.deque([starter])
        while queue:
            current_joint = queue.popleft()
            if current_joint in processed_joints:
                continue
            group.append(current_joint)
            processed_joints.add(current_joint)

            if current_joint in branching_points:
                continue

            for child in children_map.get(current_joint, []):
                if child not in processed_joints:
                    queue.append(child)
        if group:
            final_groups.append(group)

    # Step 5: Identify and add the main trunk (e.g., torso)
    all_grouped_joints = {j for g in final_groups for j in g}
    main_starters = {j for j, p in parent_map.items() if p is None}
    for starter in sorted(main_starters):
        if starter in all_grouped_joints:
            continue
        group = []
        current_joint = starter
        while current_joint and current_joint not in all_grouped_joints:
            group.append(current_joint)
            all_grouped_joints.add(current_joint)
            children = children_map.get(current_joint, [])
            if len(children) == 1 and children[0] not in all_grouped_joints:
                current_joint = children[0]
            else:
                break
        if group:
            final_groups.insert(0, group)

    return final_groups


def extract_kinematic_groups_from_urdf(urdf_path):
    """Extracts kinematic joint groups from a URDF file.

    This function loads a robot model, determines the parent-child
    relationships for its non-fixed joints, and organizes them into
    kinematic chains.

    Parameters
    ----------
    urdf_path : str
        The file path to the URDF file.

    Returns
    -------
    kinematic_groups : list of list of str
        A list of kinematic chains, where each chain is a list of joint names.
    num_total_joints : int
        The total number of non-fixed joints in the robot model.
    """
    with no_mesh_load_mode():
        robot_model = RobotModel.from_urdf(urdf_path, include_mimic_joints=False)

    # Create a complete map of every joint to its non-fixed parent.
    # Root joints will have a parent of None.
    parent_map = {}
    mimic_joint_names = {j.name for j in robot_model.urdf_robot_model.joints
                         if j.mimic is not None}
    for joint in robot_model.joint_list:
        parent_joint_name = None
        parent_link = joint.parent_link
        while parent_link:
            if parent_link.joint is not None and parent_link.joint.type != 'fixed' \
                and parent_link.joint.name not in mimic_joint_names:
                parent_joint_name = parent_link.joint.name
                break
            parent_link = parent_link.parent_link
        parent_map[joint.name] = parent_joint_name

    # Pass the structured map directly, avoiding string conversion.
    kinematic_groups = group_joints_by_branches(parent_map)
    return robot_model, kinematic_groups, len(robot_model.joint_list)


def create_config(urdf_filepath, output_filepath):
    robot_model, groups, _ = extract_kinematic_groups_from_urdf(urdf_filepath)

    total_joints_in_groups = 0
    for group in groups:
        if len(group) > 1:
            total_joints_in_groups += len(group)
    limb_config = []
    for i, group in enumerate(groups):
        limb_config.append(f"limb{i}:")
        for joint in group:
            limb_config.append(f"  - {joint} : limb{i}-{joint.replace('_', '-')}")
        limb_config.append(f'limb{i}-end-coords:')
        limb_config.append(f'  parent: {getattr(robot_model, joint).child_link.name}')
        limb_config.append('  translate: [0, 0, 0]')
        limb_config.append('  rotate: [1, 0, 0, 0]')

    with open(output_filepath, 'w') as f:
        f.write("\n".join(limb_config))


if __name__ == '__main__':
    from skrobot.data import fetch_urdfpath
    from skrobot.data import panda_urdfpath
    from skrobot.data import pr2_urdfpath
    example_urdf_paths = [
        pr2_urdfpath(),
        fetch_urdfpath(),
        panda_urdfpath(),
    ]

    for file_path in example_urdf_paths:
        try:
            print(f"Processing: {file_path}")
            robot_model, groups, total_joint_count = extract_kinematic_groups_from_urdf(file_path)

            total_joints_in_groups = 0
            for group in groups:
                if len(group) > 1:
                    print(group)
                    total_joints_in_groups += len(group)
            limb_config = []
            for i, group in enumerate(groups):
                limb_config.append(f"limb{i}:")
                for joint in group:
                    limb_config.append(f"  - {joint} : limb{i}-{joint.replace('_', '-')}")
                limb_config.append(f'limb{i}-end-coords:')
                limb_config.append(f'  parent: {robot_model.__dict__[joint].child_link.name}')
                limb_config.append('  translate: [0, 0, 0]')
                limb_config.append('  rotate: [1, 0, 0, 0]')
            print("\n".join(limb_config))
            print(f'Total joints in kinematic groups: {total_joints_in_groups}/{total_joint_count}')
        except Exception as e:
            print(f"Could not process {file_path}. Error: {e}")
            pass
