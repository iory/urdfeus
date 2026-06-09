#!/usr/bin/env python
"""Build the static web gallery assets (thumbnails + manifest) under docs/.

Reads the converted ROS packages in ``eusmodels/`` (produced by exporting every
EusLisp model with ``eus2urdf``), renders one preview WebP per model with
pyrender, and writes ``docs/thumbnails/<name>.webp`` + ``docs/manifest.json``.
The viewer itself is the static ``docs/index.html``.

Usage::

    python tools/make_gallery_site.py [--eusmodels DIR] [--docs DIR]
"""

import argparse
import glob
import json
import os

os.environ.setdefault("PYOPENGL_PLATFORM", "egl")
from concurrent.futures import ProcessPoolExecutor
import os.path as osp
import sys
import xml.etree.ElementTree as ET

import numpy as np

_ROOT = osp.dirname(osp.dirname(osp.abspath(__file__)))
DOCS = osp.join(_ROOT, "docs")
EUSMODELS = osp.join(_ROOT, "docs", "models")


def _kind(name):
    if name.endswith(("-scene", "_scene")):
        return "scene"
    if name.endswith(("-object", "_object")):
        return "object"
    return "robot"


_UP = np.array([0, 0, 1.0])


def _look_at(eye, target, up=_UP):
    f = target - eye
    f = f / np.linalg.norm(f)
    s = np.cross(f, up)
    s = s / np.linalg.norm(s)
    u = np.cross(s, f)
    m = np.eye(4)
    m[:3, 0] = s
    m[:3, 1] = u
    m[:3, 2] = -f
    m[:3, 3] = eye
    return m


def separate_coplanar(mesh, eps, group_cap=64):
    """Nudge coplanar, overlapping faces apart so the depth buffer can resolve
    them, removing z-fighting (the speckled/garbled look on e.g. TV screens).

    Some EusLisp models place a face exactly on top of another at the same
    depth -- e.g. a black TV screen flush on its frame, or a panel drawn twice.
    Coplanar faces at identical depth flicker per-pixel (z-fighting) no matter
    the depth precision. Instead of deleting geometry, this keeps the largest
    face of each overlapping coplanar cluster in place (the background) and
    pushes smaller overlapping faces outward along their normal by a small
    multiple of ``eps`` (so a screen/decal cleanly renders on top of its frame).

    Returns the mesh unchanged when no coplanar overlap is found. ``group_cap``
    bounds the O(n^2) overlap test per plane (big flat facesets are skipped --
    they are not z-fighting decals).
    """
    import collections

    import trimesh
    v, f = mesh.vertices, mesh.faces
    if len(f) < 2:
        return mesh
    n, cen, area = mesh.face_normals, mesh.triangles_center, mesh.area_faces
    groups = collections.defaultdict(list)
    for i in range(len(f)):
        key = (tuple(np.round(n[i], 2)), round(float(np.dot(n[i], cen[i])), 4))
        groups[key].append(i)
    layer = np.zeros(len(f), dtype=int)
    changed = False
    for g in groups.values():
        if len(g) < 2 or len(g) > group_cap:
            continue
        nv = n[g[0]]
        ax = np.cross(nv, [1.0, 0, 0])
        if np.linalg.norm(ax) < 1e-6:
            ax = np.cross(nv, [0, 1.0, 0])
        ax = ax / np.linalg.norm(ax)
        ay = np.cross(nv, ax)
        order = sorted(g, key=lambda i: -area[i])   # largest (background) first
        tri2 = {i: np.array([[np.dot(v[j], ax), np.dot(v[j], ay)] for j in f[i]])
                for i in order}

        def _inside(p, t):
            (x1, y1), (x2, y2), (x3, y3) = t
            d = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
            if abs(d) < 1e-12:
                return False
            a = ((y2 - y3) * (p[0] - x3) + (x3 - x2) * (p[1] - y3)) / d
            b = ((y3 - y1) * (p[0] - x3) + (x1 - x3) * (p[1] - y3)) / d
            return a >= -1e-6 and b >= -1e-6 and (1 - a - b) >= -1e-6

        placed = []
        for i in order:
            c2 = tri2[i].mean(0)
            over = [layer[k] for k in placed if _inside(c2, tri2[k])]
            if over:
                layer[i] = max(over) + 1
                changed = True
            placed.append(i)
    if not changed:
        return mesh
    # unmerge to per-face vertices so offsetting one face never moves another
    fv = v[f].reshape(-1, 3, 3).astype(np.float64)
    for i in range(len(f)):
        if layer[i]:
            fv[i] += n[i] * (layer[i] * eps)
    out = trimesh.Trimesh(vertices=fv.reshape(-1, 3),
                          faces=np.arange(len(f) * 3).reshape(-1, 3), process=False)
    try:
        out.visual.vertex_colors = mesh.visual.vertex_colors[f].reshape(-1, 4)
    except Exception:
        pass
    return out


def _load_glb_with_colors(path):
    """Load a (possibly Draco-compressed) glb as a Trimesh with vertex colours.

    trimesh's Draco import collapses a multi-colour COLOR_0 into a flat texture
    (so meshes with several colours in one primitive, e.g. an RGB axis arrow,
    render grey). For Draco glb we decode the buffer with DracoPy directly to
    recover per-vertex colours; plain glb falls back to trimesh.
    """
    import json
    import struct

    import trimesh
    with open(path, "rb") as f:
        data = f.read()
    if data[:4] == b"glTF":
        off = 12
        jlen = struct.unpack("<II", data[off:off + 8])[0]
        off += 8
        gltf = json.loads(data[off:off + jlen])
        off += jlen
        blen = struct.unpack("<II", data[off:off + 8])[0]
        off += 8
        chunk = data[off:off + blen]
        prim = gltf["meshes"][0]["primitives"][0]
        dext = prim.get("extensions", {}).get("KHR_draco_mesh_compression")
        if dext is not None:
            import DracoPy
            bv = gltf["bufferViews"][dext["bufferView"]]
            o = bv.get("byteOffset", 0)
            dm = DracoPy.decode(chunk[o:o + bv["byteLength"]])
            verts = np.asarray(dm.points, dtype=np.float64).reshape(-1, 3)
            faces = np.asarray(dm.faces, dtype=np.int64).reshape(-1, 3)
            mesh = trimesh.Trimesh(vertices=verts, faces=faces, process=False)
            cols = np.asarray(dm.colors) if getattr(dm, "colors", None) is not None \
                else np.zeros((0,))
            if len(cols) == len(verts):
                mesh.visual.vertex_colors = cols.reshape(len(verts), -1)
            return mesh
    return trimesh.load(path, force="mesh", process=False)


def _render_thumb(model_dir, urdf_rel, out_path, size=256):
    import xml.etree.ElementTree as ET

    from PIL import Image
    import pyrender
    from skrobot.model import RobotModel
    name = osp.basename(model_dir)
    urdf = osp.join(model_dir, urdf_rel)
    txt = open(urdf).read().replace(f"package://{name}/", model_dir + "/")
    abs_ = osp.join(model_dir, "urdf", "_thumb_abs.urdf")
    with open(abs_, "w") as f:
        f.write(txt)
    robot = RobotModel()
    with open(abs_) as f:
        robot.load_urdf_file(f)
    os.remove(abs_)
    # skrobot loads meshes via trimesh (which drops Draco multi-colours), so use
    # skrobot only for the per-link world transforms and load the mesh geometry
    # ourselves with _load_glb_with_colors to keep colours faithful.
    link_xform = {lk.name: lk.worldcoords().T() for lk in robot.link_list}
    link_meshes = {}
    for link_el in ET.fromstring(txt).findall("link"):
        files = [m.get("filename")
                 for m in link_el.findall("visual/geometry/mesh")]
        if files:
            link_meshes[link_el.get("name")] = files
    # Flat, near-shadowless lighting (in the spirit of scikit-robot's pyrender
    # viewer: high ambient + one weak directional) so the mesh vertex colours
    # render faithfully instead of being blown out / desaturated by strong
    # directional lights.
    scene = pyrender.Scene(bg_color=[0.16, 0.17, 0.20], ambient_light=[0.8] * 3)
    bounds = []
    for link_name, files in link_meshes.items():
        transform = link_xform.get(link_name)
        if transform is None:
            continue
        for fn in files:
            m = _load_glb_with_colors(fn)
            if m is None or len(m.vertices) == 0:
                continue
            m.apply_transform(transform)
            bounds.append(m.bounds)
            # de-fight coplanar overlapping faces (TV screens on frames, etc.)
            eps = float(np.linalg.norm(m.bounds[1] - m.bounds[0])) * 0.0015
            m = separate_coplanar(m, eps)
            scene.add(pyrender.Mesh.from_trimesh(m, smooth=False))
    if not bounds:
        raise RuntimeError("no geometry")
    pts = np.array(bounds).reshape(-1, 3)
    center = (pts.min(0) + pts.max(0)) / 2
    diag = float(np.linalg.norm(pts.max(0) - pts.min(0)))
    dist = diag * 1.5
    az, el = 0.9, 0.32
    eye = center + dist * np.array(
        [np.cos(az) * np.cos(el), np.sin(az) * np.cos(el), np.sin(el)])
    cam = pyrender.PerspectiveCamera(yfov=np.pi / 4, znear=max(diag * 0.05, 1e-3),
                                     zfar=diag * 8)
    scene.add(cam, pose=_look_at(eye, center))
    scene.add(pyrender.DirectionalLight(color=np.ones(3), intensity=0.5),
              pose=_look_at(center + dist * np.array([0.3, 0.3, 1.0]), center))
    renderer = pyrender.OffscreenRenderer(size, size)
    color, _ = renderer.render(scene)
    renderer.delete()
    # Lossless WebP: ~2.5x smaller than PNG for these flat-background renders.
    Image.fromarray(color).save(out_path, "WEBP", lossless=True, method=6)


def _worker(args):
    model_dir, thumb_dir = args
    name = osp.basename(model_dir)
    try:
        urdfs = [u for u in glob.glob(osp.join(model_dir, "urdf", "*.urdf"))
                 if not u.endswith(("_abs.urdf", "_thumb_abs.urdf"))]
        if not urdfs:
            return None
        urdf = urdfs[0]
        urdf_rel = osp.relpath(urdf, model_dir).replace(os.sep, "/")
        root = ET.parse(urdf).getroot()
        joints = root.findall("joint")
        movable = [j for j in joints
                   if j.get("type") in ("revolute", "prismatic", "continuous")]
        _render_thumb(model_dir, urdf_rel, osp.join(thumb_dir, name + ".webp"))
        # Relative file list for client-side zip download of the package.
        files = []
        for pat in ("package.xml", "CMakeLists.txt", "urdf/*.urdf", "meshes/*",
                    "frames.json", "objects.json"):
            for p in sorted(glob.glob(osp.join(model_dir, pat))):
                rel = osp.relpath(p, model_dir).replace(os.sep, "/")
                if rel.endswith(("_abs.urdf", "_thumb_abs.urdf")):
                    continue
                files.append(rel)
        return {"name": name, "kind": _kind(name), "links": len(root.findall("link")),
                "joints": len(joints), "movable": len(movable), "urdf": urdf_rel,
                "files": files}
    except Exception as e:
        print("FAIL", name, repr(e)[:80], flush=True)
        return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--eusmodels", default=EUSMODELS)
    parser.add_argument("--docs", default=DOCS)
    parser.add_argument("--workers", type=int, default=max(1, os.cpu_count() - 1))
    args = parser.parse_args()

    thumb_dir = osp.join(args.docs, "thumbnails")
    os.makedirs(thumb_dir, exist_ok=True)
    dirs = sorted(d for d in glob.glob(osp.join(args.eusmodels, "*")) if osp.isdir(d))
    if not dirs:
        raise SystemExit(f"no model packages under {args.eusmodels}")
    print(f"rendering {len(dirs)} thumbnails ...")
    sys.stdout.flush()
    entries = []
    tasks = [(d, thumb_dir) for d in dirs]
    with ProcessPoolExecutor(max_workers=args.workers) as ex:
        for i, r in enumerate(ex.map(_worker, tasks), 1):
            if r:
                entries.append(r)
            if i % 100 == 0:
                print(f"  [{i}/{len(dirs)}]", flush=True)
    entries.sort(key=lambda e: (e["kind"], e["name"]))
    with open(osp.join(args.docs, "manifest.json"), "w") as f:
        json.dump(entries, f)
    print(f"done: {len(entries)} models -> {args.docs}/manifest.json")


if __name__ == "__main__":
    main()
