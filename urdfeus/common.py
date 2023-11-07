from skrobot.model import FixedJoint
from skrobot.model import Joint


meter2millimeter = 1000.0


def is_linear_joint(joint: Joint) -> bool:
    return joint.__class__.__name__ == 'LinearJoint'


def is_fixed_joint(joint: Joint) -> bool:
    return joint.__class__.__name__ == 'FixedJoint'


def collect_all_joints_of_robot(robot):
    """Collects and returns all joints of the robot, including fixed joints.

    """
    all_fixed_joints = [value for _, value in robot.__dict__.items()
                        if isinstance(value, FixedJoint)]
    all_joints = robot.joint_list + all_fixed_joints
    return all_joints
