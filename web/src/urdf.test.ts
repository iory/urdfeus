import { describe, expect, it } from "vitest";
import { isAssetFile, matchMeshFiles, parseUrdfMeshRefs } from "./urdf";

const urdf = (body: string) =>
  `<?xml version="1.0"?><robot name="t">${body}</robot>`;

/** jsdom's File has no webkitRelativePath, so attach one the way a picker would. */
function fileWithPath(name: string, relativePath: string): File {
  const file = new File(["x"], name);
  Object.defineProperty(file, "webkitRelativePath", { value: relativePath });
  return file;
}

describe("parseUrdfMeshRefs", () => {
  it("strips the package:// prefix and keeps the package-relative path", () => {
    const [ref] = parseUrdfMeshRefs(
      urdf(`<link name="a"><visual><geometry>
        <mesh filename="package://some_description/meshes/base_v0/base.stl"/>
      </geometry></visual></link>`),
    );
    expect(ref.relativePath).toBe("meshes/base_v0/base.stl");
    expect(ref.filename).toBe("base.stl");
    expect(ref.matched).toBe(false);
  });

  it("keeps plain relative paths as they are", () => {
    const [ref] = parseUrdfMeshRefs(
      urdf(`<link name="a"><visual><geometry>
        <mesh filename="./meshes/link.dae"/></geometry></visual></link>`),
    );
    expect(ref.relativePath).toBe("meshes/link.dae");
  });

  it("reports each distinct reference once even when reused", () => {
    const refs = parseUrdfMeshRefs(
      urdf(`<link name="a">
        <visual><geometry><mesh filename="package://p/m/x.stl"/></geometry></visual>
        <collision><geometry><mesh filename="package://p/m/x.stl"/></geometry></collision>
      </link>
      <link name="b"><visual><geometry>
        <mesh filename="package://p/m/y.stl"/></geometry></visual></link>`),
    );
    expect(refs.map((r) => r.filename)).toEqual(["x.stl", "y.stl"]);
  });

  it("rejects input that is not XML", () => {
    expect(() => parseUrdfMeshRefs("not a urdf at all")).toThrow(/URDF/);
  });
});

describe("matchMeshFiles", () => {
  const refs = () =>
    parseUrdfMeshRefs(
      urdf(`<link name="a"><visual><geometry>
        <mesh filename="package://robot/meshes/arm/link1.stl"/>
      </geometry></visual></link>`),
    );

  it("matches on basename regardless of where the file came from", () => {
    const matched = matchMeshFiles(refs(), [new File(["x"], "link1.stl")]);
    expect(matched[0].matched).toBe(true);
  });

  it("is case insensitive", () => {
    const matched = matchMeshFiles(refs(), [new File(["x"], "LINK1.STL")]);
    expect(matched[0].matched).toBe(true);
  });

  it("prefers the candidate whose directory path also lines up", () => {
    const wrong = fileWithPath("link1.stl", "pick/meshes/leg/link1.stl");
    const right = fileWithPath("link1.stl", "pick/meshes/arm/link1.stl");
    const matched = matchMeshFiles(refs(), [wrong, right]);
    expect(matched[0].matchedFile).toBe(right);
  });

  it("leaves unmatched references alone", () => {
    const matched = matchMeshFiles(refs(), [new File(["x"], "other.stl")]);
    expect(matched[0].matched).toBe(false);
    expect(matched[0].matchedFile).toBeNull();
  });
});

describe("isAssetFile", () => {
  it("accepts meshes and textures, rejects everything else", () => {
    expect(isAssetFile("a.STL")).toBe(true);
    expect(isAssetFile("a.dae")).toBe(true);
    expect(isAssetFile("a.png")).toBe(true);
    expect(isAssetFile("README.md")).toBe(false);
  });
});
