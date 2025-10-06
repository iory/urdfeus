"""NumPy/SciPy compatibility handling utilities."""

import sys


def show_compatibility_error(error_msg, file=sys.stderr):
    """Show NumPy/SciPy compatibility error message with solutions."""
    print("Error: NumPy/SciPy compatibility issue detected.", file=file)
    print("", file=file)
    print("This error typically occurs when you have:", file=file)
    print("- NumPy 2.x with SciPy compiled for NumPy 1.x", file=file)
    print("- Or incompatible versions of NumPy/SciPy", file=file)
    print("", file=file)
    print("To fix this issue, try one of the following solutions:", file=file)
    print("", file=file)
    print("1. Downgrade NumPy to version < 2.0:", file=file)
    print("   pip install 'numpy<2.0'", file=file)
    print("", file=file)
    print("2. Upgrade SciPy to a version compatible with NumPy 2.x:", file=file)
    print("   pip install --upgrade scipy", file=file)
    print("", file=file)
    print("3. Reinstall all dependencies in a clean environment:", file=file)
    print("   pip uninstall numpy scipy trimesh scikit-robot", file=file)
    print("   pip install numpy scipy trimesh scikit-robot", file=file)
    print("", file=file)
    print("4. Use a virtual environment with compatible versions:", file=file)
    print("   python -m venv urdfeus_env", file=file)
    print("   source urdfeus_env/bin/activate  # Linux/Mac", file=file)
    print("   # urdfeus_env\\Scripts\\activate  # Windows", file=file)
    print("   pip install urdfeus", file=file)
    print("", file=file)
    print(f"Original error: {error_msg}", file=file)


def is_compatibility_error(error_msg):
    """Check if an error is a NumPy/SciPy compatibility issue."""
    error_lower = error_msg.lower()
    compatibility_indicators = [
        "numpy" in error_lower and ("dtype size changed" in error_lower or "binary incompatibility" in error_lower),
        "_ARRAY_API not found" in error_msg,
        "numpy version" in error_lower and ("required" in error_lower or "detected" in error_lower),
        "cannot be run in numpy" in error_lower,
        "compiled using numpy" in error_lower,
        "scipy" in error_lower and "numpy" in error_lower,
        "trimesh" in error_lower and ("numpy" in error_lower or "scipy" in error_lower)
    ]
    return any(compatibility_indicators)


def handle_import_error(e, exit_on_error=True):
    """Handle import errors with appropriate messaging.

    Args:
        e: The exception that was raised
        exit_on_error: If True, call sys.exit(1) after showing error (for CLI tools)
                      If False, raise ImportError (for library usage)
    """
    error_msg = str(e)
    if is_compatibility_error(error_msg):
        show_compatibility_error(error_msg)
        if exit_on_error:
            sys.exit(1)
        else:
            raise ImportError(f"NumPy/SciPy compatibility issue: {error_msg}") from e
    else:
        if exit_on_error:
            print(f"Import error: {error_msg}", file=sys.stderr)
            sys.exit(1)
        else:
            raise ImportError(f"Import error: {error_msg}") from e


def handle_compatibility_error(e, exit_on_error=True):
    """Handle any type of compatibility error (not just ImportError).

    Args:
        e: The exception that was raised (ImportError, AttributeError, ValueError, etc.)
        exit_on_error: If True, call sys.exit(1) after showing error (for CLI tools)
                      If False, re-raise the original exception
    """
    error_msg = str(e)
    if is_compatibility_error(error_msg):
        show_compatibility_error(error_msg)
        if exit_on_error:
            sys.exit(1)
        else:
            # Re-raise the original exception type
            raise type(e)(f"NumPy/SciPy compatibility issue: {error_msg}") from e
    else:
        # Not a compatibility issue, re-raise the original exception
        raise e
