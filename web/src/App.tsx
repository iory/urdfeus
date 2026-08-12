import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  type MeshRef,
  filesFromDataTransfer,
  isAssetFile,
  matchMeshFiles,
  parseUrdfMeshRefs,
  relativePathOf,
} from "./urdf";
import type { WorkerRequest, WorkerResponse } from "./worker";

type Phase = "idle" | "booting" | "converting" | "done" | "error";

export default function App() {
  const [urdfName, setUrdfName] = useState<string | null>(null);
  const [urdfText, setUrdfText] = useState<string>("");
  const [meshes, setMeshes] = useState<MeshRef[]>([]);
  const [robotName, setRobotName] = useState("");
  const [useUrdfMaterial, setUseUrdfMaterial] = useState(false);
  const [phase, setPhase] = useState<Phase>("idle");
  const [progress, setProgress] = useState("");
  const [result, setResult] = useState<string | null>(null);
  const [elapsed, setElapsed] = useState(0);
  const [error, setError] = useState<string | null>(null);
  const [dragging, setDragging] = useState(false);

  // The accumulated asset pool is only ever read to re-run matching, never
  // rendered, so it stays out of render state.
  const poolRef = useRef<File[]>([]);
  const workerRef = useRef<Worker | null>(null);
  const urdfInput = useRef<HTMLInputElement>(null);
  const meshFileInput = useRef<HTMLInputElement>(null);
  const meshDirInput = useRef<HTMLInputElement>(null);

  useEffect(() => {
    const worker = new Worker(new URL("./worker.ts", import.meta.url), {
      type: "module",
    });
    worker.onmessage = (event: MessageEvent<WorkerResponse>) => {
      const message = event.data;
      if (message.type === "progress") setProgress(message.message);
      else if (message.type === "result") {
        setResult(message.source);
        setElapsed(message.elapsedMs);
        setPhase("done");
      } else if (message.type === "error") {
        setError(message.message);
        setPhase("error");
      }
    };
    workerRef.current = worker;
    return () => worker.terminate();
  }, []);

  const matchedCount = useMemo(
    () => meshes.filter((mesh) => mesh.matched).length,
    [meshes],
  );

  const acceptUrdf = useCallback(async (file: File) => {
    const text = await file.text();
    let refs: MeshRef[];
    try {
      refs = parseUrdfMeshRefs(text);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setPhase("error");
      return;
    }
    setUrdfName(file.name);
    setUrdfText(text);
    poolRef.current = [];
    setMeshes(refs);
    setResult(null);
    setError(null);
    setPhase("idle");
  }, []);

  const addAssets = useCallback((files: File[], filterByExtension: boolean) => {
    const accepted = filterByExtension
      ? files.filter((file) => isAssetFile(file.name))
      : files;
    poolRef.current = [...poolRef.current, ...accepted];
    setMeshes((current) => matchMeshFiles(current, poolRef.current));
  }, []);

  const onDrop = useCallback(
    async (event: React.DragEvent) => {
      event.preventDefault();
      setDragging(false);
      const files = await filesFromDataTransfer(event.dataTransfer);
      const urdf = files.find((file) => file.name.toLowerCase().endsWith(".urdf"));
      if (urdf) {
        await acceptUrdf(urdf);
        const rest = files.filter((file) => file !== urdf);
        // A dropped folder usually carries the URDF and its meshes together,
        // so wire up whatever came along instead of asking twice.
        if (rest.length > 0) addAssets(rest, true);
        return;
      }
      if (!urdfName) {
        setError("No .urdf file in that drop.");
        setPhase("error");
        return;
      }
      addAssets(files, true);
    },
    [acceptUrdf, addAssets, urdfName],
  );

  const convert = useCallback(async () => {
    if (!urdfName) return;
    setPhase("booting");
    setError(null);
    setResult(null);
    const files: { path: string; bytes: Uint8Array }[] = [
      { path: urdfName, bytes: new TextEncoder().encode(urdfText) },
    ];
    for (const mesh of meshes) {
      if (!mesh.matched || !mesh.matchedFile) continue;
      files.push({
        path: mesh.relativePath,
        bytes: new Uint8Array(await mesh.matchedFile.arrayBuffer()),
      });
    }
    setPhase("converting");
    const request: WorkerRequest = {
      type: "convert",
      urdfName,
      files,
      robotName,
      useUrdfMaterial,
    };
    workerRef.current?.postMessage(request);
  }, [urdfName, urdfText, meshes, robotName, useUrdfMaterial]);

  const download = useCallback(() => {
    if (!result || !urdfName) return;
    const blob = new Blob([result], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = `${(robotName || urdfName.replace(/\.urdf$/i, "")).replace(/[^\w-]/g, "_")}.l`;
    anchor.click();
    URL.revokeObjectURL(url);
  }, [result, urdfName, robotName]);

  const busy = phase === "booting" || phase === "converting";
  const missing = meshes.length - matchedCount;

  return (
    <main>
      <header>
        <h1>
          urdf2eus <span>in your browser</span>
        </h1>
        <a href="../">← model gallery</a>
      </header>

      <p className="lede">
        Converts URDF to an EusLisp model entirely on this page - urdfeus runs as
        WebAssembly, so nothing is uploaded anywhere.
      </p>

      <section
        className={`dropzone${dragging ? " dragging" : ""}`}
        onDragOver={(event) => {
          event.preventDefault();
          setDragging(true);
        }}
        onDragLeave={() => setDragging(false)}
        onDrop={onDrop}
        onClick={() => urdfInput.current?.click()}
      >
        <strong>{urdfName ?? "Drop a .urdf file or folder here"}</strong>
        <span>
          {urdfName
            ? `${meshes.length} mesh reference${meshes.length === 1 ? "" : "s"} found`
            : "or click to select - a folder brings its meshes along"}
        </span>
      </section>
      <input
        ref={urdfInput}
        type="file"
        accept=".urdf,.xml"
        hidden
        onChange={(event) => {
          const file = event.target.files?.[0];
          if (file) void acceptUrdf(file);
        }}
      />

      {meshes.length > 0 && (
        <section className="panel">
          <div className="panel-head">
            <h2>
              Meshes <em>{matchedCount}/{meshes.length} matched</em>
            </h2>
            <div className="actions">
              <button type="button" onClick={() => meshFileInput.current?.click()}>
                Select files
              </button>
              <button type="button" onClick={() => meshDirInput.current?.click()}>
                Select directory
              </button>
            </div>
          </div>
          <input
            ref={meshFileInput}
            type="file"
            multiple
            hidden
            accept=".stl,.dae,.obj,.glb,.gltf,.mtl,.ply,.png,.jpg,.jpeg,.tga,.bmp,.tiff"
            onChange={(event) =>
              addAssets(Array.from(event.target.files ?? []), true)
            }
          />
          <input
            ref={meshDirInput}
            type="file"
            hidden
            // Non-standard but universally supported; React has no typing for it.
            {...({ webkitdirectory: "", directory: "" } as Record<string, string>)}
            onChange={(event) =>
              addAssets(Array.from(event.target.files ?? []), false)
            }
          />
          <ul className="meshlist">
            {meshes.map((mesh) => (
              <li key={mesh.fullPath} className={mesh.matched ? "ok" : "missing"}>
                <span className="mark">{mesh.matched ? "✓" : "!"}</span>
                <code>{mesh.relativePath}</code>
                {mesh.matchedFile && (
                  <small>{relativePathOf(mesh.matchedFile) || mesh.matchedFile.name}</small>
                )}
              </li>
            ))}
          </ul>
          {missing > 0 && (
            <p className="warn">
              {missing} mesh{missing === 1 ? "" : "es"} still missing. Converting
              now would drop {missing === 1 ? "that link's" : "those links'"} geometry.
            </p>
          )}
        </section>
      )}

      {urdfName && (
        <section className="panel options">
          <label>
            Robot name
            <input
              type="text"
              value={robotName}
              placeholder="(from the URDF)"
              onChange={(event) => setRobotName(event.target.value)}
            />
          </label>
          <label className="check">
            <input
              type="checkbox"
              checked={useUrdfMaterial}
              onChange={(event) => setUseUrdfMaterial(event.target.checked)}
            />
            Use <code>&lt;material&gt;</code> colours instead of per-face mesh colours
          </label>
          <button type="button" className="primary" disabled={busy} onClick={convert}>
            {busy ? "Working…" : "Convert to EusLisp"}
          </button>
        </section>
      )}

      {busy && (
        <p className="status">
          {progress}
          <br />
          <small>First run downloads ~33 MB of Python runtime; later runs reuse it.</small>
        </p>
      )}

      {error && (
        <section className="panel error">
          <h2>Conversion failed</h2>
          <pre>{error}</pre>
        </section>
      )}

      {result && (
        <section className="panel">
          <div className="panel-head">
            <h2>
              Result <em>{result.length.toLocaleString()} chars in {(elapsed / 1000).toFixed(1)}s</em>
            </h2>
            <div className="actions">
              <button type="button" className="primary" onClick={download}>
                Download .l
              </button>
            </div>
          </div>
          <pre className="preview">{result.slice(0, 4000)}</pre>
        </section>
      )}
    </main>
  );
}
