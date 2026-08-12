"""Flags every urdfeus command line shares."""

import urdfeus
from urdfeus.provenance import doctor_report


def add_diagnostic_arguments(parser):
    """Add ``--version`` and ``--doctor`` to a parser.

    Parameters
    ----------
    parser : argparse.ArgumentParser
        Parser to extend.
    """
    parser.add_argument(
        "--version", action="version",
        version=f"urdfeus {urdfeus.__version__}")
    parser.add_argument(
        "--doctor", action="store_true",
        help="Print which python, urdfeus and dependencies would be used, "
        + "then exit. Paste this into a bug report.")


def handle_doctor(args, parser, required):
    """Run ``--doctor``, or complain about the arguments it made optional.

    ``--doctor`` has to work without the input and output paths, so those are
    declared with ``nargs='?'``; this puts the requirement back for every other
    invocation.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed arguments.
    parser : argparse.ArgumentParser
        Parser to report errors through.
    required : list of str
        Names of the positional arguments that are otherwise mandatory.

    Returns
    -------
    handled : bool
        True when the doctor report was printed and the caller should stop.
    """
    if args.doctor:
        print(doctor_report())
        return True
    missing = [name for name in required if getattr(args, name) is None]
    if missing:
        parser.error("the following arguments are required: "
                     + ", ".join(missing))
    return False
