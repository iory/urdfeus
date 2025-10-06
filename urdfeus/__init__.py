# flake8: noqa

import sys


if (sys.version_info[0] == 3 and sys.version_info[1] >= 7) \
    or sys.version_info[0] > 3:
    import importlib.metadata

    def determine_version(module_name):
        return importlib.metadata.version(module_name)

    __version__ = determine_version('urdfeus')
else:
    import pkg_resources

    def determine_version(module_name):
        return pkg_resources.get_distribution(module_name).version

    __version__ = determine_version('urdfeus')