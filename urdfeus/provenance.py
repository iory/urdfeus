"""Report which code and which environment produced a conversion.

A bug report against urdfeus is only actionable if it says what actually ran,
and that is easy to get wrong: a ``pip install --user`` shim in ``~/.local/bin``
and a checkout in the current directory can be different versions of the same
package, and ``import urdfeus`` picks whichever the working directory happens to
favour.  Recording the resolved module paths alongside the version numbers makes
that unambiguous.
"""

import importlib
import os
import shutil
import sys

#: Packages whose version tends to matter when a conversion looks wrong.
REPORTED_PACKAGES = ("urdfeus", "skrobot", "trimesh", "numpy")

#: Environment variables that change how a URDF resolves.
REPORTED_ENVIRONMENT = ("ROS_PACKAGE_PATH", "ROS_DISTRO", "AMENT_PREFIX_PATH")

#: Filled in by :func:`package_reports`, read by :func:`doctor_lines`.
_shadow_notes = {}


def _package_version(module):
    version = getattr(module, "__version__", None)
    if version is not None:
        return str(version)
    import importlib.metadata as metadata

    # skrobot ships as scikit-robot; ask metadata for the distribution name.
    for name in {module.__name__, module.__name__.replace("_", "-")}:
        try:
            return metadata.version(name)
        except metadata.PackageNotFoundError:
            continue
    try:
        packages = metadata.packages_distributions().get(module.__name__) or []
        if packages:
            return metadata.version(packages[0])
    except Exception:
        pass
    return "unknown"


def _summarise_path_variable(value, limit=160):
    """Keep a search path readable.

    A sourced ROS workspace can put dozens of entries in one variable, which
    buries the rest of the report; the entry count plus the first element is
    what actually tells you which workspace is in play.
    """
    if value is None:
        return "(unset)"
    if len(value) <= limit:
        return value
    entries = value.split(os.pathsep)
    return f"{len(entries)} entries, first {entries[0]}"


def _shadowing_note(module):
    """Warn when the imported module is not the installed distribution.

    ``__version__`` is read from installed metadata, so running a checkout that
    shadows an older installed copy reports the *installed* version number
    against the *checkout's* code.  That combination is what makes a bug report
    impossible to reproduce, so say so explicitly.  An editable install trips
    this too, which is fair: its recorded version is frozen at install time and
    drifts from the working tree just the same.

    Parameters
    ----------
    module : module
        Imported module.

    Returns
    -------
    note : str or None
        Warning text, or None when the two agree.
    """
    import importlib.metadata as metadata

    path = getattr(module, "__file__", None)
    if path is None:
        return None
    try:
        packages = metadata.packages_distributions().get(module.__name__) or []
        if not packages:
            return None
        installed = metadata.distribution(packages[0]).locate_file(
            module.__name__)
    except Exception:
        return None
    module_dir = os.path.dirname(os.path.realpath(path))
    if module_dir == os.path.realpath(str(installed)):
        return None
    return (f"imported from {module_dir}, not from the installed"
            + f" distribution at {installed} (an editable install, or a"
            + " checkout ahead of sys.path); the version above comes from"
            + " installed metadata and can lag this code")


def package_reports():
    """Describe each reported package.

    Returns
    -------
    reports : list of tuple
        ``(name, version, path)`` per package, with ``path`` telling which copy
        of it was imported.  Packages that fail to import are reported rather
        than raising, so a broken environment can still be described.
    """
    reports = []
    for name in REPORTED_PACKAGES:
        try:
            module = importlib.import_module(name)
        except Exception as e:
            reports.append((name, "not importable", f"{type(e).__name__}: {e}"))
            continue
        reports.append((name, _package_version(module),
                        getattr(module, "__file__", "unknown")))
        note = _shadowing_note(module)
        if note:
            _shadow_notes[name] = note
        else:
            _shadow_notes.pop(name, None)
    return reports


def header_lines():
    """Provenance compact enough to sit at the top of every generated file."""
    lines = [f"python is {sys.executable} ({sys.version.split()[0]})"]
    for name, version, path in package_reports():
        lines.append(f"{name} {version} from {path}")
        note = _shadow_notes.get(name)
        if note:
            lines.append(f"  ! {note}")
    return lines


def doctor_lines():
    """Everything worth pasting into a bug report."""
    lines = [f"python: {sys.executable}",
             "python version: {}".format(sys.version.replace("\n", " ")),
             f"platform: {sys.platform}"]
    for name, version, path in package_reports():
        lines.append(f"{name}: {version} ({path})")
        note = _shadow_notes.get(name)
        if note:
            lines.append(f"  ! {note}")
    for key in REPORTED_ENVIRONMENT:
        lines.append(f"{key}: {_summarise_path_variable(os.environ.get(key))}")
    for command in ("irteusgl", "roseus"):
        lines.append("{}: {}".format(
            command, shutil.which(command) or "(not on PATH)"))
    return lines


def doctor_report():
    """The doctor output as one printable block."""
    return "\n".join(doctor_lines())
