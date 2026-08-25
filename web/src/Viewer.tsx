import { useCallback, useEffect, useRef, useState } from "react";
import type { UrdfPackage } from "./worker";

/** CDN modules, matching the import map in index.html. */
const THREE_URL = "https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js";
const ADDONS = "https://cdn.jsdelivr.net/npm/three@0.160.0/examples/jsm/";
const URDF_LOADER_URL =
  "https://cdn.jsdelivr.net/npm/urdf-loader@0.12.7/src/URDFLoader.js";

interface JointHandle {
  name: string;
  lower: number;
  upper: number;
  continuous: boolean;
  set(value: number): void;
}

/** Point every mesh reference at a blob URL of the mesh we hold in memory. */
function inlineMeshes(pkg: UrdfPackage): { urdf: string; urls: string[] } {
  const urls: string[] = [];
  const urdf = pkg.urdf.replace(
    /filename="([^"]+)"/g,
    (whole, filename: string) => {
      const name = filename.split("/").pop() ?? "";
      const bytes = pkg.meshes[name];
      if (!bytes) return whole;
      const url = URL.createObjectURL(
        new Blob([bytes as BlobPart], { type: "model/gltf-binary" }),
      );
      urls.push(url);
      return `filename="${url}"`;
    },
  );
  return { urdf, urls };
}

/**
 * Shows a converted model: orbit the camera, drag a slider to move a joint.
 *
 * three.js and urdf-loader come from the same CDN the gallery uses, loaded
 * only once a model is actually ready to show.
 */
export default function Viewer({ pkg }: { pkg: UrdfPackage }) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [joints, setJoints] = useState<JointHandle[]>([]);
  const [values, setValues] = useState<Record<string, number>>({});
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    let disposed = false;
    let frameId = 0;
    let cleanup = () => {};

    (async () => {
      const THREE = await import(/* @vite-ignore */ THREE_URL);
      const { OrbitControls } = await import(
        /* @vite-ignore */ `${ADDONS}controls/OrbitControls.js`
      );
      const { GLTFLoader } = await import(
        /* @vite-ignore */ `${ADDONS}loaders/GLTFLoader.js`
      );
      const URDFLoader = (await import(/* @vite-ignore */ URDF_LOADER_URL))
        .default;
      if (disposed) return;

      const renderer = new THREE.WebGLRenderer({ canvas, antialias: true });
      renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
      const scene = new THREE.Scene();
      scene.background = new THREE.Color(0x13151a);
      // URDF is Z-up, and so is the gallery's camera.
      const camera = new THREE.PerspectiveCamera(45, 1, 0.01, 100);
      camera.up.set(0, 0, 1);
      scene.add(new THREE.HemisphereLight(0xffffff, 0x333844, 2.2));
      const sun = new THREE.DirectionalLight(0xffffff, 1.4);
      sun.position.set(1, -1.4, 2);
      scene.add(sun);
      const grid = new THREE.GridHelper(10, 20, 0x2e333d, 0x23272f);
      grid.rotation.x = Math.PI / 2;
      scene.add(grid);

      const controls = new OrbitControls(camera, renderer.domElement);
      controls.enableDamping = true;

      const { urdf, urls } = inlineMeshes(pkg);
      const manager = new THREE.LoadingManager();
      const loader = new URDFLoader(manager);
      loader.workingPath = "";
      loader.loadMeshCb = (
        path: string,
        mgr: unknown,
        done: (mesh: unknown, err?: unknown) => void,
      ) => {
        new GLTFLoader(mgr).load(
          path,
          (gltf: { scene: { traverse(fn: (o: any) => void): void } }) => {
            gltf.scene.traverse((object: any) => {
              if (object.isMesh) {
                object.material.metalness = 0;
                object.material.roughness = 0.85;
              }
            });
            done(gltf.scene);
          },
          undefined,
          (err: unknown) => done(null, err),
        );
      };

      let robot: any;
      try {
        robot = loader.parse(urdf);
      } catch (err) {
        setError(err instanceof Error ? err.message : String(err));
        return;
      }
      scene.add(robot);

      const handles: JointHandle[] = Object.entries(robot.joints ?? {})
        .filter(([, joint]: [string, any]) => joint.jointType !== "fixed")
        .map(([name, joint]: [string, any]) => {
          const continuous = joint.jointType === "continuous";
          return {
            name,
            lower: continuous ? -Math.PI : Number(joint.limit?.lower ?? -Math.PI),
            upper: continuous ? Math.PI : Number(joint.limit?.upper ?? Math.PI),
            continuous,
            set: (value: number) => {
              joint.setJointValue(value);
            },
          };
        });
      setJoints(handles);
      setValues(Object.fromEntries(handles.map((h) => [h.name, 0])));

      const frameModel = () => {
        const box = new THREE.Box3().setFromObject(robot);
        if (box.isEmpty()) return;
        const center = box.getCenter(new THREE.Vector3());
        const size = box.getSize(new THREE.Vector3());
        const radius = Math.max(size.x, size.y, size.z) || 1;
        controls.target.copy(center);
        camera.position.set(
          center.x + radius * 1.3,
          center.y - radius * 1.6,
          center.z + radius * 1.1,
        );
        camera.near = radius / 100;
        camera.far = radius * 60;
        camera.updateProjectionMatrix();
        grid.position.set(center.x, center.y, box.min.z);
        grid.scale.setScalar(Math.max(1, radius * 2) / 10);
      };
      manager.onLoad = frameModel;
      // Geometry-less models never trigger onLoad, so frame them anyway.
      setTimeout(frameModel, 800);

      const resize = () => {
        const width = canvas.clientWidth || 640;
        const height = canvas.clientHeight || 360;
        renderer.setSize(width, height, false);
        camera.aspect = width / height;
        camera.updateProjectionMatrix();
      };
      resize();
      const observer = new ResizeObserver(resize);
      observer.observe(canvas);

      const tick = () => {
        frameId = requestAnimationFrame(tick);
        controls.update();
        renderer.render(scene, camera);
      };
      tick();

      cleanup = () => {
        cancelAnimationFrame(frameId);
        observer.disconnect();
        controls.dispose();
        renderer.dispose();
        urls.forEach((url) => URL.revokeObjectURL(url));
      };
    })().catch((err) => {
      setError(err instanceof Error ? err.message : String(err));
    });

    return () => {
      disposed = true;
      cleanup();
    };
  }, [pkg]);

  const moveJoint = useCallback((handle: JointHandle, value: number) => {
    handle.set(value);
    setValues((current) => ({ ...current, [handle.name]: value }));
  }, []);

  const resetJoints = useCallback(() => {
    joints.forEach((handle) => handle.set(0));
    setValues(Object.fromEntries(joints.map((handle) => [handle.name, 0])));
  }, [joints]);

  return (
    <div className="viewer">
      {error ? (
        <p className="warn">3D preview unavailable: {error}</p>
      ) : (
        <canvas ref={canvasRef} className="viewcanvas" />
      )}
      {joints.length > 0 && (
        <div className="joints">
          <div className="panel-head">
            <h3>
              Joints <em>{joints.length} movable</em>
            </h3>
            <button type="button" onClick={resetJoints}>
              Reset
            </button>
          </div>
          {joints.map((handle) => (
            <label key={handle.name} className="joint">
              <span>{handle.name}</span>
              <input
                type="range"
                min={handle.lower}
                max={handle.upper}
                step={(handle.upper - handle.lower) / 200 || 0.01}
                value={values[handle.name] ?? 0}
                onChange={(event) =>
                  moveJoint(handle, Number(event.target.value))
                }
              />
              <em>{(values[handle.name] ?? 0).toFixed(3)}</em>
            </label>
          ))}
        </div>
      )}
    </div>
  );
}
