"""Read a generated EusLisp model file without running irteusgl.

:func:`parse_eus_model` produces the same dump dictionary that
``euslisp/eus2urdf-dump.l`` writes from a live irteusgl session, so it can feed
:func:`urdfeus.eus2urdf.eus2urdf_from_data` directly. That makes ``eus2urdf``
usable where EusLisp is not installed -- including inside Pyodide, where the
browser converter runs.

Only *generated* models are in scope. Both shapes the model generators emit are
covered:

``euscollada`` / ``urdfeus`` models (robots converted from URDF or Collada)
    geometry as inline ``gl::glvertices`` meshes, links placed by
    ``(send link :transform world-cds)``.

jskeus object models (``eus/models/*-object.l``)
    geometry as ``faceset`` / ``face`` vertex lists, links placed by
    ``:newcoords`` before being assoc'd.

Everything else -- a hand-written model that computes its links, a scene that
``load``s other files -- raises :class:`EusParseError` rather than being
approximated: the interpreted subset is deliberately small, and a form outside
it is an error, never a skipped statement.

The parser reproduces the EusLisp semantics that decide a model's shape
(``:assoc`` preserving world pose, ``:transform`` / ``:newcoords`` /
``:rotate``, ``:init-ending`` wiring links from joints) rather than
pattern-matching the generator's output line by line.

One deliberate difference from the irteusgl dump: that dump zeroes every
movable joint and reports the resulting joint value, which EusLisp clamps into
``[min, max]``, together with the link poses that clamping produced. This parser
reports ``q = 0`` and the untouched poses instead. The two are equivalent for
the URDF, which defines its joint origins at joint value 0:
:func:`urdfeus.eus2urdf._joint_zero_child_pose` backs the clamped value out of
the dumped pose and arrives at exactly the poses reported here.
"""

import os.path as osp

import numpy as np

from urdfeus.eus_colors import find_color
from urdfeus.eus_reader import Dotted
from urdfeus.eus_reader import read_file
from urdfeus.eus_reader import Symbol


class EusParseError(RuntimeError):
    """Raised when a model file falls outside the interpreted subset."""


#: What the dump writes for an unbounded joint limit (EusLisp ``*inf*``).
_INF = 1e30

#: Locally defined functions (``labels``) share the variable scope, under a
#: prefix that no EusLisp symbol can collide with.
_LOCAL_FUNCTION_PREFIX = "#function:"

_UNSET = object()


# ---------------------------------------------------------------------------
# coordinates
# ---------------------------------------------------------------------------

def _coords(pos=None, rot=None):
    """A 4x4 homogeneous matrix from a position and/or rotation."""
    m = np.eye(4)
    if rot is not None:
        m[:3, :3] = np.asarray(rot, dtype=np.float64).reshape(3, 3)
    if pos is not None:
        m[:3, 3] = np.asarray(pos, dtype=np.float64).reshape(3)
    return m


def _axis_vector(axis):
    """Normalize an EusLisp joint/rotation axis to a float vector.

    Classic jskeus models write ``:z`` / ``:-z``; generated ones write a
    ``float-vector``.
    """
    if axis is None:
        return None
    if isinstance(axis, np.ndarray):
        return np.asarray(axis, dtype=np.float64)
    if isinstance(axis, Symbol):
        table = {
            ":x": (1, 0, 0), ":-x": (-1, 0, 0),
            ":y": (0, 1, 0), ":-y": (0, -1, 0),
            ":z": (0, 0, 1), ":-z": (0, 0, -1),
        }
        vec = table.get(axis.name)
        if vec is None:
            return None
        return np.array(vec, dtype=np.float64)
    if isinstance(axis, (list, tuple)):
        if all(isinstance(v, (int, float)) for v in axis):
            return np.array(axis, dtype=np.float64)
        return None
    return None


def _rotation_matrix(theta, axis):
    """Rotation of ``theta`` radians about ``axis`` (Rodrigues)."""
    a = _axis_vector(axis)
    if a is None:
        raise EusParseError(f"cannot rotate about axis {axis!r}")
    norm = np.linalg.norm(a)
    if norm == 0:
        raise EusParseError("cannot rotate about a zero axis")
    a = a / norm
    K = np.array([[0, -a[2], a[1]],
                  [a[2], 0, -a[0]],
                  [-a[1], a[0], 0]], dtype=np.float64)
    return np.eye(3) + np.sin(theta) * K + (1 - np.cos(theta)) * (K @ K)


class CascadedCoords:
    """The subset of EusLisp's ``cascaded-coords`` the models exercise.

    ``matrix`` is the pose relative to ``parent`` (world pose when there is no
    parent), which is what makes descendants follow a parent's motion without
    any explicit propagation.
    """

    def __init__(self, name=None, matrix=None):
        self.name = name
        self.parent = None
        self.descendants = []
        self.matrix = np.eye(4) if matrix is None else matrix
        #: ``:put`` / ``:get`` property list.
        self.properties = {}
        #: Slots declared by a class defined in the model file.
        self.slots = {}
        #: Name of the class this was instantiated from, when the file
        #: defines it -- that is what makes its methods reachable.
        self.eus_class = None

    def worldmatrix(self):
        if self.parent is None:
            return self.matrix
        return self.parent.worldmatrix() @ self.matrix

    def worldpos(self):
        return self.worldmatrix()[:3, 3]

    def worldrot(self):
        return self.worldmatrix()[:3, :3]

    def assoc(self, child):
        """Attach ``child`` keeping its world pose, as ``:assoc`` does."""
        if child in self.descendants:
            return child
        world = child.worldmatrix()
        if child.parent is not None:
            child.parent.descendants.remove(child)
        child.matrix = np.linalg.inv(self.worldmatrix()) @ world
        child.parent = self
        # EusLisp pushes onto descendants, so the newest child comes first.
        self.descendants.insert(0, child)
        return child

    def newcoords(self, matrix):
        self.matrix = np.array(matrix, dtype=np.float64)
        return self

    def transform(self, matrix, wrt=":local"):
        if wrt in (":local", "local"):
            self.matrix = self.matrix @ matrix
        elif wrt in (":parent", "parent", ":world", "world"):
            # cascaded-coords treats :world as :parent for a root coords, and
            # the models only transform coords that have no parent yet.
            if self.parent is not None and wrt in (":world", "world"):
                raise EusParseError(
                    ":transform :world on a coords with a parent is"
                    + " not supported")
            self.matrix = matrix @ self.matrix
        else:
            raise EusParseError(f":transform wrt {wrt!r}")
        return self

    def move_to(self, matrix, wrt=":local"):
        if wrt in (":local", "local"):
            self.matrix = self.matrix @ matrix
        elif wrt in (":parent", "parent"):
            self.matrix = np.array(matrix, dtype=np.float64)
        elif wrt in (":world", "world"):
            if self.parent is None:
                self.matrix = np.array(matrix, dtype=np.float64)
            else:
                self.matrix = np.linalg.inv(self.parent.worldmatrix()) @ matrix
        else:
            raise EusParseError(f":move-to wrt {wrt!r}")
        return self

    def rotate(self, theta, axis, wrt=":local"):
        rot = _rotation_matrix(theta, axis)
        if wrt in (":local", "local"):
            self.matrix[:3, :3] = self.matrix[:3, :3] @ rot
        elif wrt in (":parent", "parent", ":world", "world"):
            self.matrix[:3, :3] = rot @ self.matrix[:3, :3]
        else:
            raise EusParseError(f":rotate wrt {wrt!r}")
        return self

    def translate(self, vec, wrt=":local"):
        vec = np.asarray(vec, dtype=np.float64)
        if wrt in (":local", "local"):
            self.matrix[:3, 3] = self.matrix[:3, 3] + self.matrix[:3, :3] @ vec
        elif wrt in (":parent", "parent", ":world", "world"):
            self.matrix[:3, 3] = self.matrix[:3, 3] + vec
        else:
            raise EusParseError(f":translate wrt {wrt!r}")
        return self

    def copy_worldcoords(self):
        return Coords(matrix=self.worldmatrix().copy())

    def copy_coords(self):
        return Coords(matrix=self.matrix.copy())


class Coords(CascadedCoords):
    """A plain ``coordinates`` value (``make-coords``)."""


class GLVertices(CascadedCoords):
    """``gl::glvertices``: a list of coloured triangle submeshes."""

    def __init__(self, mesh_list=None, name=None):
        super().__init__(name=name)
        self.mesh_list = mesh_list or []


class Body(CascadedCoords):
    """A ``faceset`` / ``body`` / ``collada-body``.

    ``faces`` holds each face's vertices in the body's own frame; ``glvertices``
    is the mesh a ``collada-body`` carries instead.
    """

    def __init__(self, name=None, faces=None, glbody=False):
        super().__init__(name=name)
        self.faces = faces or []
        self.face_color = None
        self.glvertices = None
        #: True for a ``collada-body``. The dump reads such a body's mesh from
        #: its glvertices only -- when it has none it contributes no geometry,
        #: rather than falling back to the placeholder cube EusLisp gives it.
        self.glbody = glbody


class Link(CascadedCoords):
    """A ``bodyset-link``."""

    def __init__(self, name=None, bodies=None):
        super().__init__(name=name)
        self.bodies = bodies or []
        self.weight = 0.0
        self.acentroid = None
        self.inertia_tensor = None
        self.joint = None
        self.parent_link = None
        self.child_links = []

    def add_child_link(self, link):
        if link is not None and link not in self.child_links:
            # EusLisp's :add-child-links pushes, newest first.
            self.child_links.insert(0, link)


class Joint:
    """A ``rotational-joint`` / ``linear-joint`` (and their mimic variants)."""

    def __init__(self, cls_name, name=None):
        self.cls_name = cls_name
        self.eus_class = cls_name
        self.name = name
        self.parent_link = None
        self.child_link = None
        self.axis = None
        self.min_angle = None
        self.max_angle = None
        self.max_joint_velocity = 0.0
        self.max_joint_torque = 0.0
        self.mimic_joints = None
        self.default_coords = None


class MimicJointParam:
    """A ``mimic-joint-param``: a follower joint plus multiplier and offset."""

    def __init__(self, joint, multiplier=1.0, offset=0.0):
        self.eus_class = None
        self.slots = {}
        self.joint = joint
        self.multiplier = multiplier
        self.offset = offset


class Model(CascadedCoords):
    """The instantiated model: a ``cascaded-link`` and its slots.

    ``links`` and ``joint-list`` are ordinary EusLisp slots that the model's
    ``:init`` fills in, so they are read back out of :attr:`slots` rather than
    kept separately.
    """

    def __init__(self, cls, name=None):
        super().__init__(name=name)
        self.cls = cls

    @property
    def links(self):
        return self.slots.get("links") or []

    @property
    def joint_list(self):
        return self.slots.get("joint-list") or []


# ---------------------------------------------------------------------------
# the interpreted subset
# ---------------------------------------------------------------------------

class _Class:
    """A ``defclass`` read from the file."""

    def __init__(self, name, super_name, slots):
        self.name = name
        self.super_name = super_name
        self.slots = slots
        self.methods = {}


#: Classes the parser implements itself instead of interpreting their methods.
#: Their EusLisp definitions live in irteus, not in the model file (the few the
#: generator copies into the file are re-implementations of the same classes).
_NATIVE_CLASSES = {
    "coordinates", "cascaded-coords", "bodyset-link", "bodyset",
    "rotational-joint", "linear-joint",
    "rotational-mimic-joint", "linear-mimic-joint", "mimic-joint-param",
    "collada-body", "faceset", "face", "hole", "body",
    "gl::glvertices", "gl::urdfeus-glvertices", "glvertices",
}

#: Superclass of each joint class, for ``derivedp``.
_JOINT_SUPERS = {
    "rotational-joint": "joint",
    "linear-joint": "joint",
    "rotational-mimic-joint": "rotational-joint",
    "linear-mimic-joint": "linear-joint",
    "joint": "cascaded-coords",
}

#: Model superclasses whose ``:init`` the parser supplies natively.
_MODEL_ROOTS = {"cascaded-link", "robot-model"}

#: Class names a model may name as a value, typically as the second argument
#: of ``derivedp``.
_KNOWN_CLASSES = (_NATIVE_CLASSES | _MODEL_ROOTS | set(_JOINT_SUPERS)
                  | set(_JOINT_SUPERS.values())
                  | {"gl::glbody", "propertied-object", "scene-model"})

#: Messages handled natively on a model even when the file defines a method of
#: the same name: they either need the parser's own bookkeeping
#: (``:init-ending``) or only affect state the dump does not read.
_MODEL_NATIVE_MESSAGES = {
    ":make-collision-model-for-links", ":reset-pose",
    ":worldcoords", ":assoc", ":dissoc", ":move-to", ":methods", ":slots",
    ":name", ":links", ":joint-list", ":transform", ":newcoords",
}


class _Env:
    """A lexical scope.

    Inside a method, a name that is not lexically bound is a slot of the
    object the method runs on -- ``owner`` -- which is how the generated
    models write to ``links``, ``joint-list`` or ``glvertices``.
    """

    def __init__(self, parent=None, owner=None):
        self.vars = {}
        self.parent = parent
        self.owner = owner

    def root_owner(self):
        env = self
        while env is not None:
            if env.owner is not None:
                return env.owner
            env = env.parent
        return None

    def lookup(self, name):
        env = self
        while env is not None:
            if name in env.vars:
                return env.vars[name]
            env = env.parent
        owner = self.root_owner()
        if owner is not None:
            try:
                return _slot_get(owner, name)
            except EusParseError:
                return _UNSET
        return _UNSET

    def assign(self, name, value):
        env = self
        while env is not None:
            if name in env.vars:
                env.vars[name] = value
                return
            env = env.parent
        owner = self.root_owner()
        if owner is not None:
            _slot_set(owner, name, value)
            return
        self.vars[name] = value


def _sym(value):
    return value.name if isinstance(value, Symbol) else value


def _is_sym(form, name):
    return isinstance(form, Symbol) and form.name == name


def _keyword_args(args):
    """Split an evaluated argument list into positionals and keywords."""
    positional = []
    keywords = {}
    i = 0
    while i < len(args):
        arg = args[i]
        if isinstance(arg, Symbol) and arg.keywordp and i + 1 < len(args):
            keywords[arg.name] = args[i + 1]
            i += 2
        else:
            positional.append(arg)
            i += 1
    return positional, keywords


class Face:
    """One ``face`` of a ``faceset``: its outer contour, in the body frame.

    ``polygon :init`` stores ``(append (last ver) ver)`` -- the contour with
    its final vertex repeated in front -- and that is the list
    ``(send face :vertices)`` returns and the dump fan-triangulates from, so
    the same rotation is applied here. ``holes`` are kept for completeness but
    never triangulated: ``:vertices`` returns the outer contour alone.
    """

    def __init__(self, vertices, holes=None):
        vertices = np.asarray(vertices, dtype=np.float64).reshape(-1, 3)
        if len(vertices) > 0:
            vertices = np.vstack([vertices[-1:], vertices])
        self.vertices = vertices
        self.holes = holes or []


def _as_matrix(value):
    """The 4x4 of a coords-like value, as EusLisp's ``transform-coords`` sees it."""
    if isinstance(value, CascadedCoords):
        return value.matrix
    if isinstance(value, np.ndarray) and value.shape == (4, 4):
        return value
    raise EusParseError(f"coordinates expected, got {value!r}")


def _quaternion2matrix(q):
    q = np.asarray(q, dtype=np.float64)
    if q.shape != (4,):
        raise EusParseError("quaternion2matrix expects 4 elements")
    from skrobot.coordinates.math import quaternion2matrix
    return quaternion2matrix(q)  # wxyz, as in EusLisp


def _numeric(value, what):
    if isinstance(value, bool) or not isinstance(value, (int, float,
                                                         np.floating,
                                                         np.integer)):
        raise EusParseError(f"{what} expects a number, got {value!r}")
    return float(value)


def _slot_get(obj, name):
    """Read one EusLisp slot of a runtime object."""
    if isinstance(obj, MimicJointParam):
        if name in _MIMIC_SLOTS:
            return getattr(obj, name)
        if name in obj.slots:
            return obj.slots[name]
        raise EusParseError(
            f"cannot read slot '{name}' of mimic-joint-param")
    if isinstance(obj, Joint):
        attr = _JOINT_SLOTS.get(name)
        if attr is not None:
            return getattr(obj, attr)
    elif isinstance(obj, Link):
        attr = _LINK_SLOTS.get(name)
        if attr is not None:
            return getattr(obj, attr)
    elif isinstance(obj, Body):
        if name in ("gl::aglvertices", "aglvertices", "glvertices"):
            return obj.glvertices
    if isinstance(obj, CascadedCoords):
        if name in obj.slots:
            return obj.slots[name]
        if name == "name":
            return obj.name
    raise EusParseError(
        f"cannot read slot '{name}' of {type(obj).__name__}")


def _slot_set(obj, name, value):
    """Write one EusLisp slot of a runtime object."""
    if isinstance(obj, MimicJointParam):
        if name in _MIMIC_SLOTS:
            setattr(obj, name, value)
        else:
            obj.slots[name] = value
        return value
    if isinstance(obj, Joint):
        attr = _JOINT_SLOTS.get(name)
        if attr is not None:
            setattr(obj, attr, value)
            return value
    elif isinstance(obj, Link):
        attr = _LINK_SLOTS.get(name)
        if attr is not None:
            setattr(obj, attr, value)
            return value
    elif isinstance(obj, Body):
        if name in ("gl::aglvertices", "aglvertices", "glvertices"):
            obj.glvertices = value
            return value
    if isinstance(obj, CascadedCoords):
        obj.slots[name] = value
        return value
    raise EusParseError(
        f"cannot set slot '{name}' of {type(obj).__name__}")


#: Slot name -> attribute, for the slots models read and write directly.
_JOINT_SLOTS = {
    "axis": "axis", "default-coords": "default_coords",
    "mimic-joints": "mimic_joints", "name": "name",
    "min-angle": "min_angle", "max-angle": "max_angle",
    "child-link": "child_link", "parent-link": "parent_link",
}

_MIMIC_SLOTS = ("joint", "multiplier", "offset")

_LINK_SLOTS = {
    "acentroid": "acentroid", "weight": "weight",
    "inertia-tensor": "inertia_tensor", "joint": "joint",
    "parent-link": "parent_link", "child-links": "child_links",
    "bodies": "bodies", "name": "name",
}


class Interpreter:
    """Evaluates the generated subset of EusLisp needed to build a model."""

    def __init__(self, forms, path=None):
        self.path = path
        self.classes = {}
        self.functions = {}
        self.globals = {}
        self.required = []
        for form in forms:
            self._toplevel(form)

    # -- reading definitions ----------------------------------------------
    def _toplevel(self, form):
        if not isinstance(form, list) or not form:
            return
        head = form[0]
        if not isinstance(head, Symbol):
            return
        if head.name == "defclass":
            self._defclass(form)
        elif head.name == "defmethod":
            self._defmethod(form)
        elif head.name == "defun":
            if len(form) >= 3 and isinstance(form[1], Symbol):
                self.functions[form[1].name] = (form[2], form[3:])
        elif head.name in ("require", "load"):
            self.required.append(form)
        # Everything else at top level (provide, in-package, defvar, comments
        # turned into forms) does not affect the model instance we build.

    def _defclass(self, form):
        name = _sym(form[1])
        super_name = None
        slots = []
        i = 2
        while i < len(form):
            key = form[i]
            if _is_sym(key, ":super") and i + 1 < len(form):
                super_name = _sym(form[i + 1])
                i += 2
            elif _is_sym(key, ":slots") and i + 1 < len(form):
                for slot in form[i + 1]:
                    if isinstance(slot, Symbol):
                        slots.append(slot.name)
                    elif isinstance(slot, list) and slot \
                            and isinstance(slot[0], Symbol):
                        slots.append(slot[0].name)
                i += 2
            else:
                i += 1
        self.classes[name] = _Class(name, super_name, slots)

    def _defmethod(self, form):
        name = _sym(form[1])
        cls = self.classes.get(name)
        if cls is None:
            cls = _Class(name, None, [])
            self.classes[name] = cls
        for method in form[2:]:
            if isinstance(method, list) and method \
                    and isinstance(method[0], Symbol):
                cls.methods[method[0].name] = (method[1], method[2:])

    # -- class helpers ------------------------------------------------------
    def class_chain(self, cls_name):
        """Names of ``cls_name`` and its superclasses, most derived first."""
        chain = []
        seen = set()
        name = cls_name
        while name is not None and name not in seen:
            chain.append(name)
            seen.add(name)
            cls = self.classes.get(name)
            if cls is not None and cls.super_name:
                name = cls.super_name
            elif name in _JOINT_SUPERS:
                name = _JOINT_SUPERS[name]
            else:
                name = None
        return chain

    def find_method(self, cls_name, message):
        for name in self.class_chain(cls_name):
            cls = self.classes.get(name)
            if cls is not None and message in cls.methods:
                return name, cls.methods[message]
        return None, None

    def all_slots(self, cls_name):
        slots = []
        for name in reversed(self.class_chain(cls_name)):
            cls = self.classes.get(name)
            if cls is not None:
                slots.extend(cls.slots)
        return slots

    def all_messages(self, cls_name):
        messages = []
        for name in self.class_chain(cls_name):
            cls = self.classes.get(name)
            if cls is not None:
                for message in cls.methods:
                    if message not in messages:
                        messages.append(message)
        return messages

    # -- evaluation --------------------------------------------------------
    def ev(self, form, env):
        if isinstance(form, Symbol):
            return self._symbol_value(form, env)
        if isinstance(form, Dotted):
            return self._get_slot(self.ev(form.car, env), _sym(form.cdr))
        if isinstance(form, list):
            if not form:
                return None
            head = form[0]
            if isinstance(head, Symbol):
                special = _SPECIAL_FORMS.get(head.name)
                if special is not None:
                    return special(self, form, env)
                args = [self.ev(a, env) for a in form[1:]]
                return self.call(head.name, args, env)
            raise EusParseError(
                f"cannot call {head!r}: only named functions are supported")
        # numbers, strings, arrays and characters evaluate to themselves
        return form

    def _symbol_value(self, sym, env):
        name = sym.name
        if name == "nil":
            return None
        if name == "t":
            return True
        if name.startswith(":"):
            return sym
        if name == "*inf*":
            return float("inf")
        if name == "*-inf*":
            return float("-inf")
        value = env.lookup(name)
        if value is not _UNSET:
            return value
        if name in self.globals:
            return self.globals[name]
        if name in self.classes or name in _KNOWN_CLASSES:
            return sym
        raise EusParseError(f"unbound variable '{name}'")

    def progn(self, body, env):
        result = None
        for form in body:
            result = self.ev(form, env)
        return result

    # -- slots -------------------------------------------------------------
    def _get_slot(self, obj, name):
        return _slot_get(obj, name)

    def _set_slot(self, obj, name, value):
        return _slot_set(obj, name, value)

    # -- calling -----------------------------------------------------------
    def call(self, name, args, env):
        local = env.lookup(_LOCAL_FUNCTION_PREFIX + name)
        if local is not _UNSET:
            lambda_list, body, closure = local
            fn_env = _Env(parent=closure)
            self._bind(lambda_list, args, fn_env)
            return self.progn(body, fn_env)
        handler = _FUNCTIONS.get(name)
        if handler is not None:
            return handler(self, args, env)
        if name in self.functions:
            lambda_list, body = self.functions[name]
            fn_env = _Env()
            self._bind(lambda_list, args, fn_env)
            return self.progn(body, fn_env)
        raise EusParseError(f"unsupported function '{name}'")

    def _bind(self, lambda_list, args, env):
        """Bind a lambda list to already-evaluated arguments."""
        mode = "required"
        args = list(args)
        keywords = None
        index = 0
        i = 0
        while i < len(lambda_list):
            item = lambda_list[i]
            i += 1
            if isinstance(item, Symbol) and item.name.startswith("&"):
                mode = item.name
                if mode == "&key" and keywords is None:
                    positional, keywords = _keyword_args(args[index:])
                    args = args[:index] + positional
                continue
            if mode == "&rest":
                env.vars[_sym(item)] = args[index:]
                index = len(args)
                continue
            if mode == "&key":
                keyword, var, default = self._key_spec(item)
                if keywords is not None and keyword in keywords:
                    env.vars[var] = keywords[keyword]
                else:
                    env.vars[var] = self.ev(default, env) \
                        if default is not None else None
                continue
            if mode == "&allow-other-keys":
                continue
            if mode == "&aux":
                var, default = self._optional_spec(item)
                env.vars[var] = self.ev(default, env) \
                    if default is not None else None
                continue
            var, default = self._optional_spec(item)
            if index < len(args):
                env.vars[var] = args[index]
                index += 1
            elif mode == "&optional":
                env.vars[var] = self.ev(default, env) \
                    if default is not None else None
            else:
                raise EusParseError(f"missing argument '{var}'")

    @staticmethod
    def _optional_spec(item):
        if isinstance(item, Symbol):
            return item.name, None
        if isinstance(item, list) and item:
            return _sym(item[0]), item[1] if len(item) > 1 else None
        raise EusParseError(f"bad lambda list entry {item!r}")

    @staticmethod
    def _key_spec(item):
        """``var`` / ``(var default)`` / ``((:keyword var) default)``."""
        if isinstance(item, Symbol):
            return ":" + item.name, item.name, None
        if isinstance(item, list) and item:
            head = item[0]
            default = item[1] if len(item) > 1 else None
            if isinstance(head, Symbol):
                return ":" + head.name, head.name, default
            if isinstance(head, list) and len(head) == 2:
                return _sym(head[0]), _sym(head[1]), default
        raise EusParseError(f"bad &key entry {item!r}")

    # -- messages ----------------------------------------------------------
    def _forward(self, value, args, env):
        """``forward-message-to``: extra arguments are a message for ``value``."""
        if args:
            return self.send(value, args[0], list(args[1:]), env)
        return value

    def send(self, obj, message, args, env):
        name = _sym(message)
        if not isinstance(name, str) or not name.startswith(":"):
            raise EusParseError(f"message expected, got {message!r}")
        if obj is None:
            raise EusParseError(f"cannot send {name} to nil")
        if isinstance(obj, Model):
            return self._send_model(obj, name, args, env)
        defining, method = self.user_method(obj, name)
        if method is not None:
            return self.invoke(obj, defining, method, args)
        return self._send_builtin(obj, name, args, env)

    def user_method(self, obj, message):
        """The file-defined method for ``message``, if the object has one."""
        cls_name = getattr(obj, "eus_class", None)
        if cls_name is None:
            return None, None
        return self.find_method(cls_name, message)

    def _send_builtin(self, obj, name, args, env):
        if isinstance(obj, Model):
            return self._send_model(obj, name, args, env)
        if isinstance(obj, Link):
            return self._send_link(obj, name, args, env)
        if isinstance(obj, Body):
            return self._send_body(obj, name, args, env)
        if isinstance(obj, GLVertices):
            return self._send_glvertices(obj, name, args, env)
        if isinstance(obj, Joint):
            return self._send_joint(obj, name, args, env)
        if isinstance(obj, MimicJointParam):
            return self._send_mimic(obj, name, args, env)
        if isinstance(obj, CascadedCoords):
            return self._send_coords(obj, name, args, env)
        raise EusParseError(
            f"cannot send {name} to {type(obj).__name__}")

    def _send_coords(self, obj, name, args, env):
        if name == ":assoc":
            obj.assoc(args[0])
            return args[0]
        if name == ":dissoc":
            child = args[0]
            if child in obj.descendants:
                child.matrix = child.worldmatrix()
                obj.descendants.remove(child)
                child.parent = None
            return child
        if name == ":transform":
            wrt = _sym(args[1]) if len(args) > 1 else ":local"
            return obj.transform(_as_matrix(args[0]), wrt)
        if name == ":newcoords":
            return obj.newcoords(_as_matrix(args[0]))
        if name == ":move-to":
            wrt = _sym(args[1]) if len(args) > 1 else ":local"
            return obj.move_to(_as_matrix(args[0]), wrt)
        if name == ":rotate":
            theta = _numeric(args[0], ":rotate")
            axis = args[1] if len(args) > 1 else None
            wrt = _sym(args[2]) if len(args) > 2 else ":local"
            return obj.rotate(theta, axis, wrt)
        if name == ":translate":
            wrt = _sym(args[1]) if len(args) > 1 else ":local"
            return obj.translate(args[0], wrt)
        if name in (":worldcoords", ":copy-worldcoords"):
            return obj.copy_worldcoords()
        if name in (":copy-coords", ":coords"):
            return obj.copy_coords()
        if name == ":worldpos":
            return obj.worldpos()
        if name in (":transform-vector", ":rotate-vector",
                    ":inverse-transform-vector", ":inverse-rotate-vector"):
            world = obj.worldmatrix()
            vec = np.asarray(args[0], dtype=np.float64)
            if name == ":transform-vector":
                return world[:3, :3] @ vec + world[:3, 3]
            if name == ":rotate-vector":
                return world[:3, :3] @ vec
            if name == ":inverse-transform-vector":
                return world[:3, :3].T @ (vec - world[:3, 3])
            return world[:3, :3].T @ vec
        if name == ":worldrot":
            return obj.worldrot()
        if name == ":name":
            if args:
                obj.name = args[0]
            return obj.name
        if name == ":parent":
            return obj.parent
        if name == ":put":
            obj.properties[_sym(args[0])] = args[1] if len(args) > 1 else None
            return obj.properties[_sym(args[0])]
        if name == ":get":
            return obj.properties.get(_sym(args[0]))
        if name in (":changed", ":update", ":worldcoords-update",
                    ":make-collisionmodel"):
            return None
        raise EusParseError(
            f"unsupported message {name} for {type(obj).__name__}")

    def _send_link(self, obj, name, args, env):
        if name == ":weight":
            if args:
                obj.weight = _numeric(args[0], ":weight")
            return obj.weight
        if name == ":inertia-tensor":
            if args:
                obj.inertia_tensor = np.asarray(args[0], dtype=np.float64)
            return obj.inertia_tensor
        if name in (":centroid", ":acentroid"):
            if args:
                obj.acentroid = np.asarray(args[0], dtype=np.float64)
            return obj.acentroid
        if name == ":bodies":
            if args:
                return [self.send(b, args[0], list(args[1:]), env)
                        for b in obj.bodies]
            return obj.bodies
        if name == ":joint":
            return self._forward(obj.joint, args, env)
        if name == ":add-joint":
            obj.joint = args[0]
            return obj.joint
        if name == ":parent-link":
            return self._forward(obj.parent_link, args, env)
        if name == ":add-parent-link":
            obj.parent_link = args[0]
            return obj.parent_link
        if name == ":child-links":
            return obj.child_links
        if name == ":add-child-links":
            obj.add_child_link(args[0])
            return obj.child_links
        return self._send_coords(obj, name, args, env)

    def _send_body(self, obj, name, args, env):
        if name in (":glvertices", ":aglvertices"):
            return self._forward(obj.glvertices, args, env)
        if name == ":set-color":
            obj.face_color = find_color(args[0] if args else None)
            if obj.glvertices is not None:
                self.send(obj.glvertices, Symbol(":set-color"), args, env)
            return obj.face_color
        if name == ":faces":
            return obj.faces
        if name == ":paste-texture-to-face":
            # Textures live outside the dump (and outside URDF), so pasting
            # one changes nothing this parser reports.
            return None
        if name == ":get" and _sym(args[0]) == ":face-color":
            return obj.face_color
        return self._send_coords(obj, name, args, env)

    def _send_glvertices(self, obj, name, args, env):
        if name == ":calc-normals":
            # Only meshes that ship no normals are touched, and then the
            # vertices are expanded one per index (irtgl computes flat
            # normals). The normals themselves are not part of the dump.
            for mesh in obj.mesh_list:
                if mesh.get("normals") is not None:
                    continue
                indices, vertices = mesh["indices"], mesh["vertices"]
                if len(indices) > vertices.shape[0]:
                    mesh["vertices"] = vertices[indices]
                    mesh["indices"] = np.arange(len(indices), dtype=np.int64)
                mesh["normals"] = True
            return None
        if name == ":set-color":
            color = find_color(args[0] if args else None)
            for mesh in obj.mesh_list:
                mesh["ambient"] = color
                mesh["diffuse"] = color
            return color
        if name == ":filename":
            return None
        return self._send_coords(obj, name, args, env)

    def _send_joint(self, obj, name, args, env):
        simple = {
            ":name": "name", ":axis": "axis",
            ":min-angle": "min_angle", ":max-angle": "max_angle",
            ":max-joint-velocity": "max_joint_velocity",
            ":max-joint-torque": "max_joint_torque",
        }
        if name in simple:
            attr = simple[name]
            if args:
                setattr(obj, attr, args[0])
            return getattr(obj, attr)
        if name == ":child-link":
            return self._forward(obj.child_link, args, env)
        if name == ":parent-link":
            return self._forward(obj.parent_link, args, env)
        if name == ":joint-angle":
            # The dump zeroes joints and this parser reports the untouched
            # pose, so a joint value is never set while reading a model.
            if args:
                raise EusParseError(
                    "setting :joint-angle is outside the interpreted subset")
            return 0.0
        if name == ":default-coords":
            return obj.default_coords
        if name == ":mimic-joints":
            return obj.mimic_joints
        raise EusParseError(f"unsupported joint message {name}")

    def _send_mimic(self, obj, name, args, env):
        if name == ":joint":
            return self._forward(obj.joint, args, env)
        if name == ":multiplier":
            return self._forward(obj.multiplier, args, env)
        if name == ":offset":
            return self._forward(obj.offset, args, env)
        raise EusParseError(
            f"unsupported mimic-joint-param message {name}")

    def _send_model(self, obj, name, args, env):
        if name in (":make-collision-model-for-links", ":reset-pose",
                    ":init-sensors", ":update-descendants"):
            return None
        if name == ":methods":
            return [Symbol(m) for m in self.all_messages(obj.cls.name)]
        if name == ":slots":
            # An alist of (slot-name . value), which is how a generated
            # :init-ending finds the joints that live in a slot.
            return [Dotted(Symbol(slot), obj.slots.get(slot))
                    for slot in self.all_slots(obj.cls.name)]
        if name == ":init-ending" and self.find_method(
                obj.cls.name, name)[1] is None:
            return self.init_ending(obj)
        if name == ":links":
            return self._forward_all(obj.links, args, env)
        if name == ":joint-list":
            return self._forward_all(obj.joint_list, args, env)
        if name not in _MODEL_NATIVE_MESSAGES:
            defining, method = self.find_method(obj.cls.name, name)
            if method is not None:
                return self.invoke(obj, defining, method, args)
        return self._send_coords(obj, name, args, env)

    def _forward_all(self, items, args, env):
        if args:
            return [self.send(i, args[0], list(args[1:]), env)
                    for i in (items or [])]
        return items

    def invoke(self, obj, defining_class, method, args):
        """Evaluate one user-defined method body."""
        lambda_list, body = method
        env = _Env(owner=obj)
        env.vars["self"] = obj
        env.vars["%defining-class%"] = defining_class
        self._bind(lambda_list, args, env)
        return self.progn(body, env)

    # -- instantiation -----------------------------------------------------
    def eus_instance(self, cls_designator, args, env):
        cls_name = _sym(cls_designator)
        if not args or not isinstance(args[0], Symbol) \
                or not args[0].keywordp:
            raise EusParseError(
                f"instance of {cls_name} without a message")
        message, rest = args[0].name, list(args[1:])
        chain = self.class_chain(cls_name)
        for base in chain:
            if base in _NATIVE_CLASSES:
                return self._native_instance(cls_name, base, message, rest,
                                             env)
        for base in chain:
            if base in _MODEL_ROOTS:
                return self._model_instance(cls_name, message, rest)
        if "scene-model" in chain:
            raise EusParseError(
                f"'{cls_name}' is a scene: it composes objects loaded from"
                + " other files, which this parser does not resolve."
                + " Convert it with irteusgl (backend='irteusgl').")
        raise EusParseError(
            f"cannot instantiate '{cls_name}': not a model or a known"
            + " EusLisp class")

    def _model_instance(self, cls_name, message, args):
        cls = self.classes.get(cls_name)
        if cls is None:
            raise EusParseError(f"unknown class '{cls_name}'")
        model = Model(cls)
        for slot in self.all_slots(cls_name):
            model.slots.setdefault(slot, None)
        for slot in ("links", "joint-list", "bodies", "end-coords-list",
                     "sensors"):
            model.slots.setdefault(slot, None)
        defining, method = self.find_method(cls_name, message)
        if method is None:
            raise EusParseError(
                f"class '{cls_name}' has no {message} method")
        self.invoke(model, defining, method, args)
        return model

    def _native_instance(self, cls_name, base, message, args, env):
        """Build an instance of an EusLisp class the parser implements.

        ``base`` is the built-in class; ``cls_name`` is what the model asked
        for. When the file defines ``cls_name`` itself -- an older
        collada2eus model gives every mesh its own ``collada-body`` subclass
        -- its ``:init`` runs on top of the built-in object.
        """
        if message not in (":init", ":create"):
            raise EusParseError(
                f"unsupported {message} message for {cls_name}")
        positional, keywords = _keyword_args(args)
        make = getattr(self, "_make_" + base.replace("::", "_")
                       .replace("-", "_"), None)
        if make is None:
            raise EusParseError(f"unsupported class '{base}'")
        obj = make(positional, keywords, env)
        obj.eus_class = cls_name
        if isinstance(obj, Joint):
            # The dump names the joint type after its own class.
            obj.cls_name = cls_name
        if isinstance(obj, Body) and obj.glbody:
            obj.glbody = self._reports_glvertices(cls_name)
        if cls_name in _NATIVE_CLASSES:
            # The generators paste re-implementations of the irteus classes
            # into the model file; the built-in behaviour is the real one.
            return obj
        for slot in self.all_slots(cls_name):
            obj.slots.setdefault(slot, None)
        defining, method = self.find_method(cls_name, message)
        if method is None:
            return obj
        result = self.invoke(obj, defining, method, args)
        if isinstance(result, (CascadedCoords, Joint, MimicJointParam)):
            return result
        return obj

    def _reports_glvertices(self, cls_name):
        """Whether a body exposes its mesh through ``:glvertices``.

        ``gl::glbody`` provides that method, so a ``collada-body`` derived
        from it dumps its glvertices. The ``collada-body :super body`` an
        older collada2eus writes has no such method, and the dump reads that
        body's faces instead.
        """
        chain = self.class_chain(cls_name)
        if "gl::glbody" in chain:
            return True
        return any(":glvertices" in self.classes[name].methods
                   for name in chain if name in self.classes)

    # -- native constructors ------------------------------------------------
    def _make_coordinates(self, positional, keywords, env):
        return Coords(
            name=keywords.get(":name"),
            matrix=_coords(keywords.get(":pos"), keywords.get(":rot")))

    def _make_cascaded_coords(self, positional, keywords, env):
        return CascadedCoords(
            name=keywords.get(":name"),
            matrix=_coords(keywords.get(":pos"), keywords.get(":rot")))

    def _make_bodyset_link(self, positional, keywords, env):
        if not positional:
            raise EusParseError("bodyset-link :init needs a coords")
        coords = positional[0]
        bodies = keywords.get(":bodies") or []
        link = Link(name=keywords.get(":name"), bodies=list(bodies))
        link.matrix = _as_matrix(coords.copy_worldcoords())
        # EusLisp defaults, overwritten below when the model passes its own.
        link.weight = 1.0
        link.acentroid = np.zeros(3)
        link.inertia_tensor = np.eye(3)
        if ":weight" in keywords:
            link.weight = _numeric(keywords[":weight"], ":weight")
        if ":centroid" in keywords:
            link.acentroid = np.asarray(keywords[":centroid"],
                                        dtype=np.float64)
        if ":inertia-tensor" in keywords:
            link.inertia_tensor = np.asarray(keywords[":inertia-tensor"],
                                             dtype=np.float64)
        if bodies:
            # Only the first body is assoc'd: the rest are expected to hang
            # off it already, which is what the generated models do.
            link.assoc(bodies[0])
        return link

    _make_bodyset = _make_bodyset_link

    def _make_joint(self, cls_name, positional, keywords):
        """A joint, with the defaults irteus' ``joint :init`` applies."""
        linear = "linear" in cls_name
        joint = Joint(cls_name, name=keywords.get(":name"))
        joint.parent_link = keywords.get(":parent-link")
        joint.child_link = keywords.get(":child-link")
        axis = keywords.get(":axis", Symbol(":z"))
        # linear-joint resolves a keyword axis at init; rotational-joint keeps
        # the keyword, and the dump resolves it when it writes the axis.
        joint.axis = _axis_vector(axis) if linear else axis
        joint.min_angle = keywords.get(":min", -90.0)
        joint.max_angle = keywords.get(":max", 90.0)
        joint.max_joint_velocity = keywords.get(
            ":max-joint-velocity", np.pi / 4 if linear else 5)
        joint.max_joint_torque = keywords.get(":max-joint-torque", 100)
        joint.mimic_joints = keywords.get(":mimic-joints")
        if joint.child_link is not None:
            joint.default_coords = joint.child_link.copy_coords()
        return joint

    def _make_rotational_joint(self, positional, keywords, env):
        return self._make_joint("rotational-joint", positional, keywords)

    def _make_linear_joint(self, positional, keywords, env):
        return self._make_joint("linear-joint", positional, keywords)

    def _make_rotational_mimic_joint(self, positional, keywords, env):
        return self._make_joint("rotational-mimic-joint", positional, keywords)

    def _make_linear_mimic_joint(self, positional, keywords, env):
        return self._make_joint("linear-mimic-joint", positional, keywords)

    def _make_mimic_joint_param(self, positional, keywords, env):
        if not positional:
            raise EusParseError("mimic-joint-param :init needs a joint")
        return MimicJointParam(positional[0],
                               keywords.get(":multiplier", 1.0),
                               keywords.get(":offset", 0.0))

    def _make_collada_body(self, positional, keywords, env):
        return Body(name=keywords.get(":name"), glbody=True)

    def _make_faceset(self, positional, keywords, env):
        faces = keywords.get(":faces") or []
        body = Body(name=keywords.get(":name"), faces=list(faces))
        if ":color" in keywords:
            body.face_color = find_color(keywords[":color"])
        return body

    _make_body = _make_faceset

    def _make_face(self, positional, keywords, env):
        return Face(keywords.get(":vertices") or [],
                    holes=keywords.get(":holes"))

    def _make_hole(self, positional, keywords, env):
        return Face(keywords.get(":vertices") or [])

    def _make_gl_glvertices(self, positional, keywords, env):
        mesh_list = positional[0] if positional else None
        return GLVertices(mesh_list=self._mesh_list(mesh_list))

    _make_gl_urdfeus_glvertices = _make_gl_glvertices
    _make_glvertices = _make_gl_glvertices

    @staticmethod
    def _mesh_list(mesh_list):
        """Turn ``gl::glvertices`` submesh alists into plain dictionaries."""
        meshes = []
        for description in mesh_list or []:
            entry = {"ambient": None, "diffuse": None, "indices": None,
                     "vertices": None, "normals": None}
            for item in description:
                if not isinstance(item, list) or not item:
                    continue
                key = _sym(item[0])
                value = item[1] if len(item) > 1 else None
                if key == ":indices":
                    entry["indices"] = np.asarray(value, dtype=np.int64)
                elif key == ":vertices":
                    entry["vertices"] = np.asarray(
                        value, dtype=np.float64).reshape(-1, 3)
                elif key == ":normals":
                    entry["normals"] = np.asarray(
                        value, dtype=np.float64).reshape(-1, 3)
                elif key == ":material":
                    for material in value or []:
                        if isinstance(material, list) and material:
                            mkey = _sym(material[0])
                            if mkey in (":ambient", ":diffuse"):
                                entry[mkey[1:]] = np.asarray(
                                    material[1], dtype=np.float64)
            if entry["vertices"] is None or len(entry["vertices"]) == 0:
                continue
            if entry["indices"] is None:
                raise EusParseError("glvertices submesh without :indices")
            meshes.append(entry)
        return meshes

    # -- model finalisation -------------------------------------------------
    def init_ending(self, model):
        """``:init-ending``: wire the link tree from the joints."""
        for joint in model.joint_list:
            self._wire_joint(joint)
        if "euscollada-robot" in self.class_chain(model.cls.name):
            # euscollada-robot also wires up joints that live in a slot but
            # not in joint-list -- that is how its fixed joints reach the tree.
            for slot in self.all_slots(model.cls.name):
                value = model.slots.get(slot)
                if isinstance(value, Joint) and value not in model.joint_list:
                    self._wire_joint(value)
        return None

    @staticmethod
    def _wire_joint(joint):
        if joint.child_link is None or joint.parent_link is None:
            raise EusParseError(
                f"joint '{joint.name}' has no parent or child link")
        joint.child_link.joint = joint
        joint.child_link.parent_link = joint.parent_link
        joint.parent_link.add_child_link(joint.child_link)

    # -- class introspection -----------------------------------------------
    def send_super(self, env, message, args):
        """``send-super``: the same message, resolved above the defining class."""
        obj = env.lookup("self")
        defining = env.lookup("%defining-class%")
        if obj is _UNSET or defining is _UNSET:
            raise EusParseError("send-super outside a method")
        name = _sym(message)
        for cls_name in self.class_chain(defining)[1:]:
            cls = self.classes.get(cls_name)
            if cls is not None and name in cls.methods:
                return self.invoke(obj, cls_name, cls.methods[name], args)
        if isinstance(obj, Model):
            if name == ":init-ending":
                return self.init_ending(obj)
            if name == ":init":
                # cascaded-link/robot-model :init only records the name; the
                # rest of the model is built by the method that called us.
                _, keywords = _keyword_args(args)
                if ":name" in keywords:
                    obj.name = keywords[":name"]
                return obj
        # No user method above the defining class: the built-in one applies.
        return self._send_builtin(obj, name, args, _Env())

    def class_names_of(self, obj):
        """Class chain of a runtime object, for ``derivedp``."""
        if isinstance(obj, Model):
            return self.class_chain(obj.cls.name)
        if isinstance(obj, Joint):
            return self.class_chain(obj.cls_name)
        if isinstance(obj, Link):
            return ["bodyset-link", "bodyset", "cascaded-coords",
                    "coordinates"]
        if isinstance(obj, Body):
            if obj.glbody:
                return ["collada-body", "gl::glbody", "body", "faceset",
                        "cascaded-coords", "coordinates"]
            return ["faceset", "cascaded-coords", "coordinates"]
        if isinstance(obj, GLVertices):
            return ["gl::glvertices", "cascaded-coords", "coordinates"]
        if isinstance(obj, MimicJointParam):
            return ["mimic-joint-param", "propertied-object"]
        if isinstance(obj, Coords):
            return ["coordinates"]
        if isinstance(obj, CascadedCoords):
            return ["cascaded-coords", "coordinates"]
        return []

    def derivedp(self, obj, cls_name):
        return cls_name in self.class_names_of(obj)

    def has_method(self, obj, message):
        """``find-method``, for the messages the dump asks about."""
        if isinstance(obj, Model):
            if self.find_method(obj.cls.name, message)[1] is not None:
                return True
            return message in _MODEL_NATIVE_MESSAGES
        if isinstance(obj, Body):
            if message == ":glvertices":
                return obj.glbody
            return message in (":faces", ":name", ":set-color", ":get")
        if isinstance(obj, Link):
            return message in (":bodies", ":joint", ":name", ":weight",
                               ":child-links", ":parent-link")
        return False

    def instantiate(self, constructor):
        """Build the model, resolving ``constructor`` as a function or class.

        Mirrors ``eus2urdf-instantiate``: generated models define
        ``(defun <name> () ...)``, while some jskeus sample models only define
        the class.
        """
        name = str(constructor).lower()
        if name in self.functions:
            lambda_list, body = self.functions[name]
            env = _Env()
            self._bind(lambda_list, [], env)
            result = self.progn(body, env)
        elif name in self.classes:
            result = self.eus_instance(Symbol(name), [Symbol(":init")],
                                       _Env())
        else:
            raise EusParseError(
                f"'{constructor}' is neither a function nor a class in this file")
        if not isinstance(result, Model):
            raise EusParseError(
                f"'{constructor}' did not build a model")
        return result



# ---------------------------------------------------------------------------
# special forms
# ---------------------------------------------------------------------------

def _sf_quote(interp, form, env):
    return form[1]


class _Closure:
    """A ``#'(lambda ...)`` together with the scope it was written in."""

    __slots__ = ("body", "env", "lambda_list")

    def __init__(self, lambda_list, body, env):
        self.lambda_list = lambda_list
        self.body = body
        self.env = env


def _sf_function(interp, form, env):
    target = form[1]
    if isinstance(target, list) and target and _is_sym(target[0], "lambda"):
        return _Closure(target[1], target[2:], env)
    return target


def _sf_lambda(interp, form, env):
    return _Closure(form[1], form[2:], env)


def _sf_let(interp, form, env):
    scope = _Env(parent=env)
    for binding in form[1]:
        if isinstance(binding, Symbol):
            scope.vars[binding.name] = None
        else:
            scope.vars[_sym(binding[0])] = interp.ev(binding[1], env) \
                if len(binding) > 1 else None
    return interp.progn(form[2:], scope)


def _sf_let_star(interp, form, env):
    scope = _Env(parent=env)
    for binding in form[1]:
        if isinstance(binding, Symbol):
            scope.vars[binding.name] = None
        else:
            scope.vars[_sym(binding[0])] = interp.ev(binding[1], scope) \
                if len(binding) > 1 else None
    return interp.progn(form[2:], scope)


def _sf_setq(interp, form, env):
    value = None
    items = form[1:]
    if len(items) % 2:
        raise EusParseError("setq needs an even number of forms")
    for i in range(0, len(items), 2):
        target, value_form = items[i], items[i + 1]
        value = interp.ev(value_form, env)
        if isinstance(target, Symbol):
            env.assign(target.name, value)
        elif isinstance(target, Dotted):
            interp._set_slot(interp.ev(target.car, env), _sym(target.cdr),
                             value)
        else:
            raise EusParseError(f"cannot setq {target!r}")
    return value


def _sf_progn(interp, form, env):
    return interp.progn(form[1:], env)


def _sf_prog1(interp, form, env):
    result = interp.ev(form[1], env)
    interp.progn(form[2:], env)
    return result


def _sf_if(interp, form, env):
    if interp.ev(form[1], env) is not None:
        return interp.ev(form[2], env)
    if len(form) > 3:
        return interp.progn(form[3:], env)
    return None


def _sf_when(interp, form, env):
    if interp.ev(form[1], env) is not None:
        return interp.progn(form[2:], env)
    return None


def _sf_unless(interp, form, env):
    if interp.ev(form[1], env) is None:
        return interp.progn(form[2:], env)
    return None


def _sf_cond(interp, form, env):
    for clause in form[1:]:
        if _is_sym(clause[0], "t"):
            return interp.progn(clause[1:], env)
        test = interp.ev(clause[0], env)
        if test is not None:
            if len(clause) == 1:
                return test
            return interp.progn(clause[1:], env)
    return None


def _sf_case(interp, form, env):
    key = interp.ev(form[1], env)
    for clause in form[2:]:
        keys = clause[0]
        if _is_sym(keys, "t"):
            return interp.progn(clause[1:], env)
        candidates = keys if isinstance(keys, list) else [keys]
        for candidate in candidates:
            if _eql(key, candidate):
                return interp.progn(clause[1:], env)
    return None


def _sf_and(interp, form, env):
    result = True
    for sub in form[1:]:
        result = interp.ev(sub, env)
        if result is None:
            return None
    return result


def _sf_or(interp, form, env):
    for sub in form[1:]:
        result = interp.ev(sub, env)
        if result is not None:
            return result
    return None


def _sf_dolist(interp, form, env):
    spec = form[1]
    var = _sym(spec[0])
    items = interp.ev(spec[1], env) or []
    scope = _Env(parent=env)
    for item in items:
        scope.vars[var] = item
        interp.progn(form[2:], scope)
    if len(spec) > 2:
        return interp.ev(spec[2], scope)
    return None


def _sf_dotimes(interp, form, env):
    spec = form[1]
    var = _sym(spec[0])
    count = int(_numeric(interp.ev(spec[1], env), "dotimes"))
    scope = _Env(parent=env)
    for i in range(count):
        scope.vars[var] = i
        interp.progn(form[2:], scope)
    if len(spec) > 2:
        return interp.ev(spec[2], scope)
    return None


def _sf_labels(interp, form, env):
    """``labels`` / ``flet``: locally defined functions."""
    scope = _Env(parent=env)
    for definition in form[1]:
        name = _sym(definition[0])
        scope.vars[_LOCAL_FUNCTION_PREFIX + name] = (
            definition[1], definition[2:], scope)
    return interp.progn(form[2:], scope)


_SPECIAL_FORMS = {
    "quote": _sf_quote,
    "labels": _sf_labels,
    "lambda": _sf_lambda,
    "flet": _sf_labels,
    "function": _sf_function,
    "let": _sf_let,
    "let*": _sf_let_star,
    "setq": _sf_setq,
    "progn": _sf_progn,
    "prog1": _sf_prog1,
    "if": _sf_if,
    "when": _sf_when,
    "unless": _sf_unless,
    "cond": _sf_cond,
    "case": _sf_case,
    "and": _sf_and,
    "or": _sf_or,
    "dolist": _sf_dolist,
    "dotimes": _sf_dotimes,
}


# ---------------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------------

def _eql(a, b):
    if isinstance(a, Symbol) or isinstance(b, Symbol):
        return _sym(a) == _sym(b)
    if isinstance(a, np.ndarray) or isinstance(b, np.ndarray):
        return a is b
    return a == b


def _truth(value):
    return True if value else None


def _flatten(items):
    out = []
    for item in items or []:
        if isinstance(item, list):
            out.extend(_flatten(item))
        elif item is not None:
            out.append(item)
    return out


def _fn(name):
    def register(func):
        _FUNCTIONS[name] = func
        return func
    return register


_FUNCTIONS = {}


def _simple(name, func):
    _FUNCTIONS[name] = lambda interp, args, env: func(*args)


_simple("list", lambda *args: list(args))
_simple("car", lambda seq: seq.car if isinstance(seq, Dotted) else (
    seq[0] if seq else None))
_simple("cdr", lambda seq: seq.cdr if isinstance(seq, Dotted) else (
    list(seq[1:]) if seq else None))
_simple("eq", lambda a, b: _truth(a is b or _eql(a, b)))
_simple("eql", lambda a, b: _truth(a is b or _eql(a, b)))
_simple("equal", lambda a, b: _truth(a is b or _eql(a, b)))
_simple("cadr", lambda seq: seq[1] if seq and len(seq) > 1 else None)
_simple("caddr", lambda seq: seq[2] if seq and len(seq) > 2 else None)
_simple("first", lambda seq: seq[0] if seq else None)
_simple("rest", lambda seq: list(seq[1:]) if seq else None)
_simple("nth", lambda n, seq: seq[n] if seq is not None and n < len(seq)
        else None)
_simple("elt", lambda seq, n: seq[n])
_simple("length", lambda seq: 0 if seq is None else len(seq))
_simple("null", lambda value: _truth(value is None))
_simple("not", lambda value: _truth(value is None))
_simple("reverse", lambda seq: list(reversed(seq or [])))
_simple("flatten", _flatten)
_simple("float-vector", lambda *args: np.array(args, dtype=np.float64))
_simple("unit-matrix", lambda n=3: np.eye(int(n)))
_simple("make-matrix", lambda r, c: np.zeros((int(r), int(c))))
_simple("array-entity", lambda array: array)
_simple("quaternion2matrix", _quaternion2matrix)
_simple("deg2rad", lambda x: np.deg2rad(x))
_simple("rad2deg", lambda x: np.rad2deg(x))
_simple("float", float)
_simple("abs", abs)
_simple("scale", lambda s, vec: np.asarray(vec, dtype=np.float64) * s)
_simple("v+", lambda a, b: np.asarray(a) + np.asarray(b))
_simple("v-", lambda a, b: np.asarray(a) - np.asarray(b))
_simple("transpose", lambda m: np.asarray(m).T)
_simple("diagonal", lambda v: np.diag(np.asarray(v, dtype=np.float64)))
# EusLisp's *epsilon*, the default tolerance of eps=.
_simple("eps=", lambda a, b, tolerance=1.0e-10: _truth(abs(a - b) < tolerance))


@_fn("append")
def _fn_append(interp, args, env):
    out = []
    for arg in args:
        out.extend(arg or [])
    return out


@_fn("+")
def _fn_add(interp, args, env):
    total = 0
    for arg in args:
        total = total + arg
    return total


@_fn("-")
def _fn_sub(interp, args, env):
    if len(args) == 1:
        return -args[0]
    total = args[0]
    for arg in args[1:]:
        total = total - arg
    return total


@_fn("*")
def _fn_mul(interp, args, env):
    total = 1
    for arg in args:
        total = total * arg
    return total


@_fn("/")
def _fn_div(interp, args, env):
    total = args[0]
    for arg in args[1:]:
        total = total / arg
    return total


@_fn("member")
def _fn_member(interp, args, env):
    item, seq = args[0], args[1] or []
    for i, other in enumerate(seq):
        if _eql(item, other):
            return list(seq[i:])
    return None


_FUNCTIONS["memq"] = _fn_member


@_fn("assoc")
def _fn_assoc(interp, args, env):
    key, alist = args[0], args[1] or []
    for entry in alist:
        if isinstance(entry, list) and entry and _eql(key, entry[0]):
            return entry
        if isinstance(entry, Dotted) and _eql(key, entry.car):
            return entry
    return None


@_fn("fvector-replace")
def _fn_fvector_replace(interp, args, env):
    dst, src = args[0], np.asarray(args[1], dtype=np.float64)
    flat = np.asarray(dst).reshape(-1)
    if src.size > flat.size:
        raise EusParseError("fvector-replace: source is larger than target")
    flat[:src.size] = src
    return dst


def _coords_from_keywords(keywords):
    """The pose ``coordinates :init`` builds from its keywords.

    Follows the same order of precedence: ``:coords`` / ``:at`` replaces the
    pose outright, otherwise ``:angle`` (with ``:axis``) rotates the
    ``:pos`` / ``:rot`` pose in its own frame.
    """
    for unsupported in (":euler", ":rpy", ":4x4"):
        if unsupported in keywords:
            raise EusParseError(
                f"make-coords {unsupported} is outside the interpreted subset")
    given = keywords.get(":coords", keywords.get(":at"))
    if given is not None:
        return np.array(_as_matrix(given), dtype=np.float64)
    matrix = _coords(keywords.get(":pos"), keywords.get(":rot"))
    angle = keywords.get(":angle")
    if angle is None:
        return matrix
    axis = keywords.get(":axis")
    angles = angle if isinstance(angle, list) else [angle]
    axes = axis if isinstance(axis, list) else [axis]
    for i, theta in enumerate(angles):
        matrix[:3, :3] = matrix[:3, :3] @ _rotation_matrix(
            _numeric(theta, "make-coords :angle"),
            axes[i] if i < len(axes) else axes[-1])
    return matrix


@_fn("make-coords")
def _fn_make_coords(interp, args, env):
    _, keywords = _keyword_args(args)
    return Coords(name=keywords.get(":name"),
                  matrix=_coords_from_keywords(keywords))


@_fn("make-cascoords")
def _fn_make_cascoords(interp, args, env):
    _, keywords = _keyword_args(args)
    return CascadedCoords(name=keywords.get(":name"),
                          matrix=_coords_from_keywords(keywords))


@_fn("instance")
def _fn_instance(interp, args, env):
    return interp.eus_instance(args[0], args[1:], env)


@_fn("instance*")
def _fn_instance_star(interp, args, env):
    spliced = list(args[1:-1]) + list(args[-1] or [])
    return interp.eus_instance(args[0], spliced, env)


@_fn("send")
def _fn_send(interp, args, env):
    return interp.send(args[0], args[1], list(args[2:]), env)


@_fn("send*")
def _fn_send_star(interp, args, env):
    rest = list(args[2:-1]) + list(args[-1] or [])
    return interp.send(args[0], args[1], rest, env)


@_fn("send-all")
def _fn_send_all(interp, args, env):
    return [interp.send(obj, args[1], list(args[2:]), env)
            for obj in (args[0] or [])]


@_fn("send-super")
def _fn_send_super(interp, args, env):
    return interp.send_super(env, args[0], list(args[1:]))


@_fn("send-super*")
def _fn_send_super_star(interp, args, env):
    rest = list(args[1:-1]) + list(args[-1] or [])
    return interp.send_super(env, args[0], rest)


@_fn("forward-message-to")
def _fn_forward_message_to(interp, args, env):
    return interp._forward(args[0], list(args[1] or []), env)


@_fn("forward-message-to-all")
def _fn_forward_message_to_all(interp, args, env):
    targets, message = args[0] or [], list(args[1] or [])
    if not message:
        return targets
    return [interp._forward(t, message, env) for t in targets]


@_fn("replace-object")
def _fn_replace_object(interp, args, env):
    dest, src = args[0], args[1]
    if type(dest) is not type(src):
        raise EusParseError(
            f"replace-object between {type(dest).__name__} and {type(src).__name__} is not supported")
    # replace-object copies slot values; the class of dest is untouched, so
    # the attributes that stand for it stay as they are.
    identity = ("cls_name", "eus_class", "glbody")
    dest.__dict__.update(
        {k: v for k, v in src.__dict__.items() if k not in identity})
    return dest


@_fn("derivedp")
def _fn_derivedp(interp, args, env):
    return _truth(interp.derivedp(args[0], _sym(args[1])))


@_fn("find-method")
def _fn_find_method(interp, args, env):
    return _truth(interp.has_method(args[0], _sym(args[1])))


@_fn("error")
def _fn_error(interp, args, env):
    raise EusParseError("model raised an error: {}".format(
        " ".join(str(a) for a in args)))


def _apply(interp, function, args, env):
    """Call a ``#'(lambda ...)`` closure or a named function."""
    if isinstance(function, _Closure):
        scope = _Env(parent=function.env)
        interp._bind(function.lambda_list, args, scope)
        return interp.progn(function.body, scope)
    if isinstance(function, Symbol):
        return interp.call(function.name, args, env)
    raise EusParseError(f"cannot call {function!r}")


@_fn("funcall")
def _fn_funcall(interp, args, env):
    return _apply(interp, args[0], list(args[1:]), env)


@_fn("apply")
def _fn_apply(interp, args, env):
    spliced = list(args[1:-1]) + list(args[-1] or [])
    return _apply(interp, args[0], spliced, env)


@_fn("mapcar")
def _fn_mapcar(interp, args, env):
    function, sequences = args[0], [seq or [] for seq in args[1:]]
    return [_apply(interp, function, list(items), env)
            for items in zip(*sequences)]


@_fn("mapcan")
def _fn_mapcan(interp, args, env):
    out = []
    for item in _fn_mapcar(interp, args, env):
        if isinstance(item, list):
            out.extend(item)
        elif item is not None:
            out.append(item)
    return out


@_fn("remove-if")
def _fn_remove_if(interp, args, env):
    function, seq = args[0], args[1] or []
    return [i for i in seq if _apply(interp, function, [i], env) is None]


@_fn("remove-if-not")
def _fn_remove_if_not(interp, args, env):
    function, seq = args[0], args[1] or []
    return [i for i in seq if _apply(interp, function, [i], env) is not None]


@_fn("remove")
def _fn_remove(interp, args, env):
    item, seq = args[0], args[1] or []
    return [i for i in seq if not _eql(item, i)]


@_fn("every")
def _fn_every(interp, args, env):
    function, sequences = args[0], [seq or [] for seq in args[1:]]
    for items in zip(*sequences):
        if _apply(interp, function, list(items), env) is None:
            return None
    return True


@_fn("find")
def _fn_find(interp, args, env):
    positional, keywords = _keyword_args(args)
    if keywords:
        raise EusParseError("find with {} is not supported".format(
            ", ".join(sorted(keywords))))
    item, seq = positional[0], positional[1] or []
    for other in seq:
        if _eql(item, other) or item is other:
            return other
    return None


def _store(result, destination):
    """EusLisp math functions optionally write into a destination array."""
    if destination is None:
        return result
    destination[...] = result
    return destination


@_fn("m*")
def _fn_matrix_multiply(interp, args, env):
    result = np.asarray(args[0], dtype=np.float64) @ np.asarray(
        args[1], dtype=np.float64)
    return _store(result, args[2] if len(args) > 2 else None)


@_fn("m+")
def _fn_matrix_add(interp, args, env):
    result = np.asarray(args[0], dtype=np.float64) + np.asarray(
        args[1], dtype=np.float64)
    return _store(result, args[2] if len(args) > 2 else None)


@_fn("m-")
def _fn_matrix_sub(interp, args, env):
    result = np.asarray(args[0], dtype=np.float64) - np.asarray(
        args[1], dtype=np.float64)
    return _store(result, args[2] if len(args) > 2 else None)


@_fn("scale-matrix")
def _fn_scale_matrix(interp, args, env):
    result = float(args[0]) * np.asarray(args[1], dtype=np.float64)
    return _store(result, args[2] if len(args) > 2 else None)


@_fn("v*")
def _fn_cross(interp, args, env):
    result = np.cross(np.asarray(args[0], dtype=np.float64),
                      np.asarray(args[1], dtype=np.float64))
    return _store(result, args[2] if len(args) > 2 else None)


@_fn("v.")
def _fn_dot(interp, args, env):
    return float(np.dot(np.asarray(args[0], dtype=np.float64),
                        np.asarray(args[1], dtype=np.float64)))


@_fn("norm")
def _fn_norm(interp, args, env):
    return float(np.linalg.norm(np.asarray(args[0], dtype=np.float64)))


@_fn("normalize-vector")
def _fn_normalize_vector(interp, args, env):
    vec = np.asarray(args[0], dtype=np.float64)
    length = np.linalg.norm(vec)
    # EusLisp leaves a zero vector alone rather than dividing by zero.
    result = vec if length == 0 else vec / length
    return _store(result, args[1] if len(args) > 1 else None)


@_fn("outer-product-matrix")
def _fn_outer_product_matrix(interp, args, env):
    x, y, z = np.asarray(args[0], dtype=np.float64)[:3]
    result = np.array([[0.0, -z, y], [z, 0.0, -x], [-y, x, 0.0]])
    return _store(result, args[1] if len(args) > 1 else None)


@_fn("matrix-exponent")
def _fn_matrix_exponent(interp, args, env):
    """Rotation matrix of a rotation vector, as ``matrix-exponent`` builds it."""
    omega = np.asarray(args[0], dtype=np.float64)
    p = float(args[1]) if len(args) > 1 else 1.0
    angle = np.linalg.norm(omega)
    if angle == 0:
        # EusLisp normalizes a zero vector to itself, leaving the identity.
        return np.eye(3)
    return _rotation_matrix(angle * p, omega / angle)


@_fn("make-camera-from-param")
def _fn_make_camera_from_param(interp, args, env):
    """A camera sensor, reduced to the coordinate frame it is mounted at.

    ``camera-model`` is not a link, so ``collect-all-links`` never reaches it
    and neither its body nor its projection appears in the dump; all that
    matters here is that it lands where the model attaches it.
    """
    _, keywords = _keyword_args(args)
    parent = keywords.get(":parent-coords")
    camera = CascadedCoords(name=keywords.get(":name"))
    if parent is not None:
        camera.newcoords(parent.worldmatrix())
    tx = float(keywords.get(":tx", 0.0))
    ty = float(keywords.get(":ty", 0.0))
    camera.translate(np.array([-tx, -ty, 0.0]))
    if parent is not None:
        parent.assoc(camera)
    return camera


@_fn("gl::transparent")
def _fn_transparent(interp, args, env):
    """``gl::transparent`` only sets the alpha of a body's face colour.

    The dump carries RGB alone, so this changes nothing it reports.
    """
    return None


_FUNCTIONS["transparent"] = _fn_transparent


@_fn("warn")
def _fn_warn(interp, args, env):
    return None


@_fn("format")
def _fn_format(interp, args, env):
    return None


# ---------------------------------------------------------------------------
# dumping
# ---------------------------------------------------------------------------

def _name_str(value):
    """How EusLisp's ``(format nil "~A" name)`` renders a link/joint name."""
    if value is None:
        return "nil"
    if isinstance(value, Symbol):
        return value.name
    return str(value)


def _vec(value):
    return None if value is None else [float(v) for v in np.asarray(value)]


def _mat(value):
    if value is None:
        return None
    return [[float(v) for v in row] for row in np.asarray(value)]


def _limit(value):
    """A joint limit as the dump writes it: a number, or null for multi-DOF."""
    if isinstance(value, bool) or not isinstance(
            value, (int, float, np.floating, np.integer)):
        return None
    value = float(value)
    if value > _INF:
        return _INF
    if value < -_INF:
        return -_INF
    return value


def _dump_glvertices(glv):
    pos = _vec(glv.worldpos())
    rot = _mat(glv.worldrot())
    submeshes = []
    for mesh in glv.mesh_list:
        submeshes.append({
            "ambient": _vec(mesh["ambient"]),
            "diffuse": _vec(mesh["diffuse"]),
            "glv_pos": pos,
            "glv_rot": rot,
            # tolist() rather than a comprehension: these are the biggest
            # arrays in a dump by far, and converting them element by element
            # in Python costs more than everything else here put together.
            "indices": mesh["indices"].tolist(),
            "vertices": mesh["vertices"].tolist(),
        })
    return submeshes


def _dump_plain_body(body):
    """A non-collada body as one world-coordinate triangle soup.

    Faces are fan-triangulated and holes ignored, which is what
    ``dump-plain-body`` does: ``(send face :vertices)`` returns the outer
    contour alone.
    """
    world = body.worldmatrix()
    rot, pos = world[:3, :3], world[:3, 3]
    triangles = []
    for face in body.faces:
        vertices = face.vertices @ rot.T + pos
        count = len(vertices)
        if count > 2 and np.linalg.norm(vertices[0] - vertices[-1]) < 1e-4:
            count -= 1
        for i in range(count - 2):
            triangles.append(vertices[0])
            triangles.append(vertices[i + 1])
            triangles.append(vertices[i + 2])
    if not triangles:
        return []
    color = body.face_color
    rgba = [0.7, 0.7, 0.7, 1.0] if color is None else \
        [float(color[0]), float(color[1]), float(color[2]), 1.0]
    return [{
        "ambient": rgba,
        "diffuse": rgba,
        "glv_pos": [0.0, 0.0, 0.0],
        "glv_rot": [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
        "indices": list(range(len(triangles))),
        "vertices": np.asarray(triangles, dtype=np.float64).tolist(),
    }]


def _dump_link(link, prefix=""):
    meshes = []
    for body in link.bodies:
        if body.glbody:
            if body.glvertices is not None:
                meshes.append(_dump_glvertices(body.glvertices))
        elif body.faces:
            meshes.append(_dump_plain_body(body))
    parent = link.parent
    return {
        "name": prefix + _name_str(link.name),
        "parent": prefix + _name_str(parent.name) if parent is not None
        else None,
        "joint": prefix + _name_str(link.joint.name)
        if link.joint is not None else None,
        "pos": _vec(link.worldpos()),
        "rot": _mat(link.worldrot()),
        "weight": link.weight,
        "centroid": _vec(link.acentroid),
        "inertia": _mat(link.inertia_tensor),
        "meshes": meshes,
    }


def _dump_joint(joint, movable, prefix=""):
    mimic = None
    if joint.mimic_joints:
        mimic = [{
            "joint": _name_str(param.joint.name),
            "multiplier": float(param.multiplier),
            "offset": float(param.offset),
        } for param in joint.mimic_joints]
    return {
        "name": prefix + _name_str(joint.name),
        "jtype": joint.cls_name,
        "movable": bool(movable),
        "axis": _vec(_axis_vector(joint.axis)),
        "min": _limit(joint.min_angle),
        "max": _limit(joint.max_angle),
        # This parser reports the untouched pose, so every joint is at 0; see
        # the module docstring for why that matches the irteusgl dump's URDF.
        "q": 0.0,
        "parent": prefix + _name_str(joint.parent_link.name),
        "child": prefix + _name_str(joint.child_link.name),
        "vel": joint.max_joint_velocity,
        "torque": joint.max_joint_torque,
        "mimic": mimic,
    }


def _object_root(model):
    """Topmost ``bodyset-link`` of a model, as ``object-root`` finds it."""
    if not model.links:
        raise EusParseError("model has no links")
    root = model.links[0]
    while isinstance(root.parent, Link):
        root = root.parent
    return root


def _collect_all_links(root):
    links = []

    def walk(link):
        links.append(link)
        for child in link.child_links:
            walk(child)

    walk(root)
    return links


def _dump_frames(interp, model, prefix=""):
    """Grasp handles and attention points, in world coordinates."""
    frames = []
    for message, kind in ((":handle", "handle"), (":attention", "attention")):
        if interp.find_method(model.cls.name, message)[1] is None:
            continue
        coords_list = interp.send(model, Symbol(message), [], _Env())
        if not isinstance(coords_list, list):
            continue
        for coords in coords_list:
            if not isinstance(coords, CascadedCoords):
                continue
            parent = coords.parent
            frames.append({
                "name": prefix + _name_str(coords.name),
                "kind": kind,
                "parent": prefix + _name_str(parent.name)
                if parent is not None and parent.name is not None else None,
                "pos": _vec(coords.worldpos()),
                "rot": _mat(coords.worldrot()),
            })
    return frames


def dump_model(interp, model):
    """Build the dump dictionary for an instantiated model."""
    root = _object_root(model)
    links = _collect_all_links(root)
    joint_list = model.joint_list
    joints = [link.joint for link in links
              if link.joint is not None and link.joint.parent_link in links]
    return {
        "robot_name": _name_str(model.name),
        "root_link": _name_str(root.name),
        "links": [_dump_link(link) for link in links],
        "joints": [_dump_joint(joint, joint in joint_list)
                   for joint in joints],
        "frames": _dump_frames(interp, model),
    }


# ---------------------------------------------------------------------------
# public API
# ---------------------------------------------------------------------------

def default_constructor_name(eus_path):
    """The conventional constructor name for a model file: its stem."""
    return osp.splitext(osp.basename(eus_path))[0]


def parse_eus_model(eus_path, constructor=None):
    """Read a generated EusLisp model file into an eus2urdf dump dictionary.

    Parameters
    ----------
    eus_path : str
        Path to the ``.l`` model file.
    constructor : str or None
        Name of the constructor function or model class. Defaults to the file
        stem, which is what the generators emit.

    Returns
    -------
    dict
        The same structure ``euslisp/eus2urdf-dump.l`` writes as JSON, ready
        for :func:`urdfeus.eus2urdf.eus2urdf_from_data`.

    Raises
    ------
    EusParseError
        If the file uses EusLisp beyond the generated subset, in which case
        the model has to be dumped with irteusgl instead.
    """
    forms = read_file(eus_path)
    interp = Interpreter(forms, path=eus_path)
    if constructor is None:
        constructor = default_constructor_name(eus_path)
    model = interp.instantiate(constructor)
    return dump_model(interp, model)
