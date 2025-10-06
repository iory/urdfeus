# flake8: noqa

import sys

# Import compatibility handling
try:
    from .compatibility import handle_import_error, handle_compatibility_error
except ImportError:
    # Fallback if compatibility module is not available
    def handle_import_error(e, exit_on_error=True):
        if exit_on_error:
            print(f"Import error: {e}", file=sys.stderr)
            sys.exit(1)
        else:
            raise ImportError(f"Import error: {e}") from e

    def handle_compatibility_error(e, exit_on_error=True):
        if exit_on_error:
            print(f"Compatibility error: {e}", file=sys.stderr)
            sys.exit(1)
        else:
            raise e

# Test critical dependencies early to provide helpful error messages
try:
    import numpy
    import scipy
    import trimesh
    import skrobot
except (ImportError, AttributeError, ValueError) as e:
    handle_compatibility_error(e, exit_on_error=False)

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