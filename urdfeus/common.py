from skrobot.model import Joint


meter2millimeter = 1000.0


def is_linear_joint(joint: Joint) -> bool:
    return joint.__class__.__name__ == 'LinearJoint'


def is_fixed_joint(joint: Joint) -> bool:
    return joint.__class__.__name__ == 'FixedJoint'
