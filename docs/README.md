# urdfeus model gallery (web)

Interactive browser gallery of the **657 EusLisp models** converted to URDF by
`eus2urdf` (jskeus robots/objects/scenes + darwin + irteus/demo sample robots).
Search and
filter the grid, click a model to view it in 3D, and drag the joint sliders to
articulate it (open drawers/doors, pose arms, …).

- `index.html` — single-page gallery + viewer (three.js + [urdf-loader], CDN, no build step)
  - grid with search / type filter; click a model for a full-screen 3D viewer
  - joint sliders (articulate doors/drawers/arms), **Frames** (eus `:handle`
    grasp poses + `:attention` points; shown by default, markers always on,
    names appear on hover), a **Move** gizmo (click an object →
    translate/rotate it; e.g. rearrange furniture in a scene), and **hover an
    object to see its name** (per-object in scenes, via `objects.json`)
  - deep links: `?m=<name>` opens a model, add `&frames=1` to show grasp frames
- `manifest.json` — model list (name, kind, link/joint counts, urdf path)
- `thumbnails/` — one preview WebP per model (lossless, ~2.5x smaller than PNG)
- `models/` — the converted ROS packages: `<name>/urdf/*.urdf`,
  `<name>/meshes/*.glb`, and `<name>/frames.json` (grasp/attention frames)

## Run locally

```bash
cd docs
python3 -m http.server 8000
# open http://localhost:8000/  (deep link: ?m=h7-robot)
```

Everything the viewer needs lives under `docs/` (`index.html`, `manifest.json`,
`thumbnails/`, and the model packages under `models/`), so no symlink or extra
setup is required.

## Deploy to GitHub Pages

Enable Pages: repo *Settings → Pages → Build from a branch → `main` `/docs`*.
The whole site is self-contained under `docs/` and committed (Draco-compressed
`.glb` keeps `models/` to ~30 MB), so Pages serves it as-is — no git-lfs or
external hosting needed.

Regenerate everything (after changing the converter or model set):

```bash
python3 tools/export_models.py        # (re)writes docs/models/<name>/ (Draco glb)
python3 tools/make_gallery_site.py    # rebuilds docs/thumbnails/ + manifest.json
```

[urdf-loader]: https://github.com/gkjohnson/urdf-loaders
