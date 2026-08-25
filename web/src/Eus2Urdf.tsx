import { useCallback, useEffect, useRef, useState } from "react";
import Viewer from "./Viewer";
import { fetchModel } from "./remote";
import { filesFromDataTransfer } from "./urdf";
import type { UrdfPackage, WorkerRequest, WorkerResponse } from "./worker";

type Phase = "idle" | "fetching" | "booting" | "converting" | "done" | "error";

/** Zipping is only needed when someone actually downloads, so it loads then. */
const JSZIP_URL = "https://cdn.jsdelivr.net/npm/jszip@3.10.1/+esm";

/** EusLisp -> URDF: drop a .l model, view it, take the ROS package away. */
export default function Eus2Urdf({ worker }: { worker: Worker | null }) {
  const [eusName, setEusName] = useState<string | null>(null);
  const [eusBytes, setEusBytes] = useState<Uint8Array | null>(null);
  const [robotName, setRobotName] = useState("");
  const [phase, setPhase] = useState<Phase>("idle");
  const [progress, setProgress] = useState("");
  const [result, setResult] = useState<UrdfPackage | null>(null);
  const [elapsed, setElapsed] = useState(0);
  const [error, setError] = useState<string | null>(null);
  // Only a failed conversion means the model was outside the readable subset;
  // a failed fetch says nothing about the model.
  const [errorKind, setErrorKind] = useState<"fetch" | "convert">("convert");
  const [dragging, setDragging] = useState(false);
  const [url, setUrl] = useState("");
  const fileInput = useRef<HTMLInputElement>(null);
  // Lets a ?url= link start the conversion without loadFromUrl having to
  // depend on convert, which depends on the state loadFromUrl is setting.
  const convertRef = useRef<((file: {
    name: string;
    bytes: Uint8Array;
  }) => void) | null>(null);

  useEffect(() => {
    if (!worker) return;
    worker.onmessage = (event: MessageEvent<WorkerResponse>) => {
      const message = event.data;
      if (message.type === "progress") setProgress(message.message);
      else if (message.type === "urdf-result") {
        setResult(message.result);
        setElapsed(message.elapsedMs);
        setPhase("done");
      } else if (message.type === "error") {
        setError(message.message);
        setErrorKind("convert");
        setPhase("error");
      }
    };
    return () => {
      worker.onmessage = null;
    };
  }, [worker]);

  const accept = useCallback(async (file: File) => {
    setEusName(file.name);
    setEusBytes(new Uint8Array(await file.arrayBuffer()));
    setResult(null);
    setError(null);
    setPhase("idle");
  }, []);

  const loadFromUrl = useCallback(async (input: string, andConvert = false) => {
    if (!input.trim()) return;
    setUrl(input);
    setResult(null);
    setError(null);
    setPhase("fetching");
    setProgress("Fetching the model…");
    try {
      const file = await fetchModel(input, (loaded, total) => {
        const mb = (loaded / 1e6).toFixed(1);
        setProgress(
          total
            ? `Fetching the model… ${mb} MB of ${(total / 1e6).toFixed(1)} MB`
            : `Fetching the model… ${mb} MB`,
        );
      });
      setEusName(file.name);
      setEusBytes(file.bytes);
      setPhase("idle");
      if (andConvert) convertRef.current?.(file);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setErrorKind("fetch");
      setPhase("error");
    }
  }, []);

  useEffect(() => {
    // ?url=... makes a model shareable as a plain link: fetch it and show it
    // without asking, which is what following such a link means.
    const shared = new URLSearchParams(window.location.search).get("url");
    if (shared) void loadFromUrl(shared, true);
  }, [loadFromUrl]);

  const onDrop = useCallback(
    async (event: React.DragEvent) => {
      event.preventDefault();
      setDragging(false);
      const dropped = event.dataTransfer.getData("text/uri-list")
        || event.dataTransfer.getData("text/plain");
      const files = await filesFromDataTransfer(event.dataTransfer);
      const model = files.find((file) => file.name.toLowerCase().endsWith(".l"));
      if (model) {
        await accept(model);
        return;
      }
      // A link dragged from a repository page works as well as a file.
      if (dropped && /^https?:\/\//i.test(dropped.trim())) {
        await loadFromUrl(dropped.trim());
        return;
      }
      setError("No .l file or link in that drop.");
      setErrorKind("fetch");
      setPhase("error");
    },
    [accept, loadFromUrl],
  );

  const convert = useCallback(
    (file?: { name: string; bytes: Uint8Array }) => {
      const name = file?.name ?? eusName;
      const bytes = file?.bytes ?? eusBytes;
      if (!name || !bytes) return;
      setPhase("booting");
      setError(null);
      setResult(null);
      const request: WorkerRequest = {
        type: "convert-eus",
        eusName: name,
        files: [{ path: name, bytes }],
        robotName,
      };
      setPhase("converting");
      worker?.postMessage(request);
    },
    [worker, eusName, eusBytes, robotName],
  );

  useEffect(() => {
    convertRef.current = convert;
  }, [convert]);

  const download = useCallback(async () => {
    if (!result) return;
    const { default: JSZip } = await import(/* @vite-ignore */ JSZIP_URL);
    const zip = new JSZip();
    const root = zip.folder(result.packageName)!;
    root.file(`urdf/${result.urdfName}`, result.urdf);
    for (const [name, bytes] of Object.entries(result.meshes)) {
      root.file(`meshes/${name}`, bytes);
    }
    // package.xml and CMakeLists.txt: without them the package:// URIs in the
    // URDF resolve nowhere.
    for (const [name, text] of Object.entries(result.extras ?? {})) {
      root.file(name, text);
    }
    const blob: Blob = await zip.generateAsync({ type: "blob" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = `${result.packageName}.zip`;
    anchor.click();
    URL.revokeObjectURL(url);
  }, [result]);

  const busy = phase === "fetching" || phase === "booting"
    || phase === "converting";
  // Running the converter in WebAssembly costs roughly half a minute per
  // 10 MB of model, which is worth saying before someone starts waiting.
  const megabytes = (eusBytes?.length ?? 0) / 1e6;
  const slow = megabytes > 8;

  return (
    <>
      <section
        className={`dropzone${dragging ? " dragging" : ""}`}
        onDragOver={(event) => {
          event.preventDefault();
          setDragging(true);
        }}
        onDragLeave={() => setDragging(false)}
        onDrop={onDrop}
        onClick={() => fileInput.current?.click()}
      >
        <strong>{eusName ?? "Drop an EusLisp .l model here"}</strong>
        <span>
          {eusName
            ? `${((eusBytes?.length ?? 0) / 1024).toFixed(0)} KB - meshes are inside the file`
            : "or click to select - a generated model carries its own geometry"}
        </span>
      </section>

      <section className="panel urlbar">
        <label>
          …or paste a link to one
          <input
            type="url"
            value={url}
            placeholder="https://github.com/owner/repo/blob/main/robot.l"
            onChange={(event) => setUrl(event.target.value)}
            onKeyDown={(event) => {
              if (event.key === "Enter") void loadFromUrl(url);
            }}
          />
        </label>
        <button type="button" disabled={busy || !url.trim()}
                onClick={() => void loadFromUrl(url)}>
          Fetch
        </button>
      </section>
      <input
        ref={fileInput}
        type="file"
        accept=".l"
        hidden
        onChange={(event) => {
          const file = event.target.files?.[0];
          if (file) void accept(file);
        }}
      />

      {eusName && (
        <section className="panel options">
          <label>
            Robot name
            <input
              type="text"
              value={robotName}
              placeholder="(from the model)"
              onChange={(event) => setRobotName(event.target.value)}
            />
          </label>
          <button type="button" className="primary" disabled={busy}
                  onClick={() => convert()}>
            {busy ? "Working…" : "Convert to URDF"}
          </button>
          {slow && !result && (
            <p className="warn">
              {megabytes.toFixed(0)} MB of model - expect a minute or two, and
              a busy tab while it runs.
            </p>
          )}
        </section>
      )}

      {busy && (
        <p className="status">
          {progress}
          {phase !== "fetching" && (
            <>
              <br />
              <small>
                First run downloads ~33 MB of Python runtime; later runs reuse it.
              </small>
            </>
          )}
        </p>
      )}

      {error && (
        <section className="panel error">
          <h2>Conversion failed</h2>
          <pre>{error}</pre>
          {errorKind === "convert" && (
            <p className="warn">
              Only generated models can be read here: euscollada/urdfeus robots
              and jskeus objects carry their geometry as data. A model that
              builds its links with code, or a scene that loads other files,
              needs <code>eus2urdf</code> with irteusgl.
            </p>
          )}
        </section>
      )}

      {result && (
        <section className="panel">
          <div className="panel-head">
            <h2>
              {result.urdfName}{" "}
              <em>
                {result.links} links · {result.joints} joints ·{" "}
                {Object.keys(result.meshes).length} meshes in{" "}
                {(elapsed / 1000).toFixed(1)}s
              </em>
            </h2>
            <div className="actions">
              <button type="button" className="primary" onClick={() => void download()}>
                Download package (.zip)
              </button>
            </div>
          </div>
          <Viewer pkg={result} />
          <pre className="preview">{result.urdf.slice(0, 4000)}</pre>
        </section>
      )}
    </>
  );
}
