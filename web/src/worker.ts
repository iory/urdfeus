/// <reference lib="webworker" />
/**
 * Runs Pyodide off the main thread. Converting a full humanoid takes seconds of
 * solid CPU, which would otherwise freeze the page.
 */
import bootstrapSource from "./bootstrap.py?raw";

export const PYODIDE_VERSION = "314.0.4";
const PYODIDE_CDN = `https://cdn.jsdelivr.net/pyodide/v${PYODIDE_VERSION}/full/`;

/** Packages Pyodide ships itself; loading them up front avoids a PyPI round trip. */
const BUILTIN_PACKAGES = [
  "micropip", "numpy", "scipy", "networkx", "lxml", "pyyaml", "pillow",
];

export interface StagedFile {
  path: string;
  bytes: Uint8Array;
}

export type WorkerRequest =
  | { type: "init" }
  | {
      type: "convert";
      urdfName: string;
      files: StagedFile[];
      robotName: string;
      useUrdfMaterial: boolean;
    }
  | {
      type: "convert-eus";
      eusName: string;
      files: StagedFile[];
      robotName: string;
    };

/** A URDF package built from an EusLisp model, meshes and all. */
export interface UrdfPackage {
  urdfName: string;
  packageName: string;
  urdf: string;
  /** Mesh file name -> glb bytes, as written under meshes/. */
  meshes: Record<string, Uint8Array>;
  /** package.xml and CMakeLists.txt, so the download is a usable package. */
  extras: Record<string, string>;
  links: number;
  joints: number;
}

export type WorkerResponse =
  | { type: "progress"; message: string }
  | { type: "ready"; urdfeusVersion: string }
  | { type: "result"; source: string; elapsedMs: number }
  | { type: "urdf-result"; result: UrdfPackage; elapsedMs: number }
  | { type: "error"; message: string };

interface PyodideApi {
  loadPackage(names: string[]): Promise<void>;
  runPythonAsync(code: string): Promise<unknown>;
  globals: { get(name: string): unknown; set(name: string, value: unknown): void };
}

let pyodide: PyodideApi | null = null;

const post = (message: WorkerResponse) => self.postMessage(message);

/** URL of the wheel staged by tools/build_web_wheel.py, if the site has one. */
async function stagedWheelUrl(): Promise<string | null> {
  try {
    // Relative to the site root, not to the worker bundle: the worker lives
    // under assets/ in a build and under src/ in dev.
    const manifestUrl = new URL(
      `${import.meta.env.BASE_URL}wheel.json`,
      self.location.origin,
    );
    const response = await fetch(manifestUrl);
    if (!response.ok) return null;
    const manifest = (await response.json()) as { urdfeus?: string };
    if (!manifest.urdfeus) return null;
    return new URL(manifest.urdfeus, manifestUrl).href;
  } catch {
    return null;
  }
}

async function init(): Promise<string> {
  if (pyodide) return "";
  post({ type: "progress", message: "Downloading the Python runtime…" });
  // Imported from the CDN rather than bundled: the runtime is ~33 MB of wasm
  // and wheels, which does not belong in a Pages artifact.
  const { loadPyodide } = await import(
    /* @vite-ignore */ `${PYODIDE_CDN}pyodide.mjs`
  );
  const py: PyodideApi = await loadPyodide({ indexURL: PYODIDE_CDN });

  post({ type: "progress", message: "Loading numpy / scipy / lxml…" });
  await py.loadPackage(BUILTIN_PACKAGES);

  post({ type: "progress", message: "Installing urdfeus and scikit-robot…" });
  await py.runPythonAsync(bootstrapSource);
  // The site ships the wheel built from the same commit; without one (a bare
  // `npm run dev`), fall back to the last release on PyPI.
  const wheel = await stagedWheelUrl();
  py.globals.set("urdfeus_wheel", wheel);
  const version = String(
    await py.runPythonAsync("await setup(urdfeus_wheel)"),
  );

  pyodide = py;
  return version;
}

self.onmessage = async (event: MessageEvent<WorkerRequest>) => {
  const request = event.data;
  try {
    if (request.type === "init") {
      const urdfeusVersion = await init();
      post({ type: "ready", urdfeusVersion });
      return;
    }

    const urdfeusVersion = await init();
    post({ type: "ready", urdfeusVersion });

    const py = pyodide!;
    post({ type: "progress", message: "Staging files…" });
    const reset = py.globals.get("reset_workdir") as () => void;
    const writeFile = py.globals.get("write_file") as (
      path: string,
      data: Uint8Array,
    ) => void;
    reset();
    for (const file of request.files) {
      writeFile(file.path, file.bytes);
    }

    post({ type: "progress", message: "Converting…" });
    const started = Date.now();

    if (request.type === "convert-eus") {
      const convertEus = py.globals.get("convert_eus") as (
        eusName: string,
        robotName: string | null,
      ) => string;
      const readOutput = py.globals.get("read_output") as (
        path: string,
      ) => Uint8Array;
      const manifest = JSON.parse(
        convertEus(request.eusName, request.robotName || null),
      ) as Omit<UrdfPackage, "meshes" | "urdfName" | "packageName"> & {
        urdf_name: string;
        package_name: string;
        meshes: string[];
        extras: Record<string, string>;
      };
      const meshes: Record<string, Uint8Array> = {};
      for (const name of manifest.meshes) {
        // Copied out of the Pyodide heap: the buffer it hands back is a view
        // that the next call may invalidate.
        meshes[name] = new Uint8Array(readOutput(`meshes/${name}`));
      }
      post({
        type: "urdf-result",
        result: {
          urdfName: manifest.urdf_name,
          packageName: manifest.package_name,
          urdf: manifest.urdf,
          meshes,
          extras: manifest.extras,
          links: manifest.links,
          joints: manifest.joints,
        },
        elapsedMs: Date.now() - started,
      });
      return;
    }

    const convert = py.globals.get("convert") as (
      urdfName: string,
      robotName: string | null,
      useUrdfMaterial: boolean,
    ) => string;
    const source = convert(
      request.urdfName,
      request.robotName || null,
      request.useUrdfMaterial,
    );
    post({ type: "result", source, elapsedMs: Date.now() - started });
  } catch (error) {
    post({
      type: "error",
      message: pythonMessage(
        error instanceof Error ? error.message : String(error),
      ),
    });
  }
};

/**
 * The readable part of a Pyodide failure.
 *
 * Python errors arrive as a whole traceback through the Pyodide frames, which
 * says nothing to someone converting a model. Everything from the last
 * exception line on is the message that was written for a person; the frames
 * above it are dropped.
 */
export function pythonMessage(text: string): string {
  const lines = text.trimEnd().split("\n");
  let start = -1;
  for (let i = lines.length - 1; i >= 0; i--) {
    if (/^[\w.]*(Error|Exception|Warning):/.test(lines[i].trim())) {
      start = i;
      break;
    }
  }
  if (start < 0) return text;
  return lines
    .slice(start)
    .join("\n")
    .replace(/^[\w.]*(?:Error|Exception|Warning):\s*/, "");
}
