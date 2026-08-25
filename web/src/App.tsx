import { useEffect, useState } from "react";
import Eus2Urdf from "./Eus2Urdf";
import Urdf2Eus from "./Urdf2Eus";

type Mode = "urdf2eus" | "eus2urdf";

const LEDE: Record<Mode, string> = {
  urdf2eus:
    "Converts URDF to an EusLisp model entirely on this page - urdfeus runs as WebAssembly, so nothing is uploaded anywhere.",
  eus2urdf:
    "Reads a generated EusLisp model back into a URDF package and shows it - again entirely on this page, so nothing is uploaded anywhere.",
};

/** A ?url= link points at a model file, so it opens on the direction that
 *  takes one. */
function initialMode(): Mode {
  if (typeof window === "undefined") return "urdf2eus";
  return new URLSearchParams(window.location.search).has("url")
    ? "eus2urdf"
    : "urdf2eus";
}

export default function App() {
  const [mode, setMode] = useState<Mode>(initialMode);
  const [worker, setWorker] = useState<Worker | null>(null);

  useEffect(() => {
    // One Pyodide worker serves both directions: the runtime download is the
    // expensive part, and switching tabs should not pay it twice.
    const instance = new Worker(new URL("./worker.ts", import.meta.url), {
      type: "module",
    });
    setWorker(instance);
    return () => instance.terminate();
  }, []);

  return (
    <main>
      <header>
        <h1>
          urdfeus <span>in your browser</span>
        </h1>
        <a href="../">← model gallery</a>
      </header>

      <nav className="tabs">
        <button
          type="button"
          className={mode === "urdf2eus" ? "on" : ""}
          onClick={() => setMode("urdf2eus")}
        >
          URDF → EusLisp
        </button>
        <button
          type="button"
          className={mode === "eus2urdf" ? "on" : ""}
          onClick={() => setMode("eus2urdf")}
        >
          EusLisp → URDF
        </button>
      </nav>

      <p className="lede">{LEDE[mode]}</p>

      {mode === "urdf2eus" ? (
        <Urdf2Eus worker={worker} />
      ) : (
        <Eus2Urdf worker={worker} />
      )}
    </main>
  );
}
