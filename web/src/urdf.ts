/**
 * URDF mesh-reference handling.
 *
 * A URDF names its meshes by package-relative path, which says nothing about
 * where they sit on the machine doing the converting. These helpers pull the
 * references out of the document and resolve them against whatever files were
 * supplied, so the result can be staged in Pyodide's virtual filesystem with
 * the layout the URDF expects.
 */

export interface MeshRef {
  /** filename attribute exactly as it appears in the URDF. */
  fullPath: string;
  /** Path with any `package://<pkg>/` prefix stripped. */
  relativePath: string;
  /** Basename, used as the primary matching key. */
  filename: string;
  matched: boolean;
  matchedFile: File | null;
}

const ASSET_EXTENSIONS = [
  ".stl", ".dae", ".obj", ".glb", ".gltf", ".mtl", ".ply",
  ".png", ".jpg", ".jpeg", ".tga", ".bmp", ".tiff",
];

export function isAssetFile(name: string): boolean {
  const lower = name.toLowerCase();
  return ASSET_EXTENSIONS.some((ext) => lower.endsWith(ext));
}

/** Extract every distinct mesh reference from a URDF document. */
export function parseUrdfMeshRefs(xmlString: string): MeshRef[] {
  const doc = new DOMParser().parseFromString(xmlString, "text/xml");
  if (doc.querySelector("parsererror")) {
    throw new Error("Could not parse the file as XML - is it really a URDF?");
  }
  const refs = new Map<string, MeshRef>();

  doc.querySelectorAll("mesh").forEach((el) => {
    const filename = el.getAttribute("filename");
    if (!filename) return;
    const match = filename.match(/^package:\/\/[^/]+\/(.+)$/);
    const relativePath = match ? match[1] : filename.replace(/^\.?\//, "");
    const basename = relativePath.split("/").pop() || relativePath;
    if (!refs.has(filename)) {
      refs.set(filename, {
        fullPath: filename,
        relativePath,
        filename: basename,
        matched: false,
        matchedFile: null,
      });
    }
  });
  return Array.from(refs.values());
}

/**
 * Resolve mesh references against a pool of user-supplied files.
 *
 * Matching is by basename because a URDF's package-relative paths rarely line
 * up with wherever the user keeps the meshes. When several files share a
 * basename, a candidate whose directory path also matches wins - that is what
 * makes picking a whole mesh directory behave sensibly.
 */
export function matchMeshFiles(meshes: MeshRef[], files: File[]): MeshRef[] {
  const byName = new Map<string, File[]>();
  for (const file of files) {
    const key = file.name.toLowerCase();
    const bucket = byName.get(key);
    if (bucket) bucket.push(file);
    else byName.set(key, [file]);
  }

  return meshes.map((mesh) => {
    const candidates = byName.get(mesh.filename.toLowerCase());
    if (!candidates || candidates.length === 0) return mesh;

    let best = candidates[0];
    // Strip the leading directory of the URDF-side path before comparing: the
    // user's folder is usually the package's mesh dir, not the package root.
    const tail = mesh.relativePath.split("/").slice(1).join("/");
    for (const candidate of candidates) {
      const rel = relativePathOf(candidate);
      if (rel && tail && rel.includes(tail)) {
        best = candidate;
        break;
      }
    }
    return { ...mesh, matched: true, matchedFile: best };
  });
}

/** `webkitRelativePath` is only populated by a directory picker. */
export function relativePathOf(file: File): string {
  return (file as File & { webkitRelativePath?: string }).webkitRelativePath || "";
}

/** Recursively collect files from a drag-and-drop of folders. */
export async function filesFromDataTransfer(dt: DataTransfer): Promise<File[]> {
  const entries: FileSystemEntry[] = [];
  for (const item of Array.from(dt.items)) {
    const entry = item.webkitGetAsEntry?.();
    if (entry) entries.push(entry);
  }
  if (entries.length === 0) return Array.from(dt.files);

  const out: File[] = [];
  const walk = async (entry: FileSystemEntry): Promise<void> => {
    if (entry.isFile) {
      const file = await new Promise<File>((resolve, reject) =>
        (entry as FileSystemFileEntry).file(resolve, reject),
      );
      out.push(file);
      return;
    }
    const reader = (entry as FileSystemDirectoryEntry).createReader();
    // readEntries yields at most 100 entries per call, so drain it in a loop.
    for (;;) {
      const batch = await new Promise<FileSystemEntry[]>((resolve, reject) =>
        reader.readEntries(resolve, reject),
      );
      if (batch.length === 0) break;
      for (const child of batch) await walk(child);
    }
  };
  for (const entry of entries) await walk(entry);
  return out;
}
