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
    };

export type WorkerResponse =
  | { type: "progress"; message: string }
  | { type: "ready"; urdfeusVersion: string }
  | { type: "result"; source: string; elapsedMs: number }
  | { type: "error"; message: string };

interface PyodideApi {
  loadPackage(names: string[]): Promise<void>;
  runPythonAsync(code: string): Promise<unknown>;
  globals: { get(name: string): unknown };
}

let pyodide: PyodideApi | null = null;

const post = (message: WorkerResponse) => self.postMessage(message);

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
  const version = String(await py.runPythonAsync("await setup()"));

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
      message: error instanceof Error ? error.message : String(error),
    });
  }
};
