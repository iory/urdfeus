/**
 * Fetching a model straight from a URL.
 *
 * A generated `.l` carries its own geometry, so a link to one is all the page
 * needs -- no download-then-drop round trip. GitHub serves file *pages* rather
 * than file contents, so a github.com URL is rewritten to the raw one, which
 * is also the only form that answers with the CORS header a page needs.
 */

/** The URL a file's bytes actually live at, given a link to it. */
export function rawFileUrl(input: string): string {
  const url = new URL(input.trim());
  const parts = url.pathname.split("/").filter(Boolean);

  if (url.hostname === "github.com" || url.hostname === "www.github.com") {
    // /<owner>/<repo>/blob/<ref...>/<path> - "raw" appears in older links.
    const kind = parts[2];
    if ((kind === "blob" || kind === "raw") && parts.length > 4) {
      const rest = parts.slice(3).join("/");
      return `https://raw.githubusercontent.com/${parts[0]}/${parts[1]}/${rest}`;
    }
  }
  if (url.hostname === "gist.github.com" && parts.length >= 2) {
    // A gist page; its files hang off /raw.
    return `https://gist.githubusercontent.com/${parts[0]}/${parts[1]}/raw`;
  }
  // Already a raw/direct link, or some other host: use it as it stands, minus
  // the fragment, which is a line anchor rather than part of the file.
  url.hash = "";
  return url.href;
}

/** File name to show for a URL. */
export function fileNameOf(url: string): string {
  try {
    const name = new URL(url).pathname.split("/").filter(Boolean).pop();
    return name && name.length > 0 ? decodeURIComponent(name) : "model.l";
  } catch {
    return "model.l";
  }
}

export interface FetchedFile {
  name: string;
  bytes: Uint8Array;
}

/** Download a model file, reporting progress while it arrives. */
export async function fetchModel(
  input: string,
  onProgress?: (loadedBytes: number, totalBytes: number | null) => void,
): Promise<FetchedFile> {
  let target: string;
  try {
    target = rawFileUrl(input);
  } catch {
    throw new Error(`Not a URL: ${input}`);
  }

  let response: Response;
  try {
    response = await fetch(target);
  } catch {
    // fetch hides the reason, and for a page like this it is nearly always
    // the host declining cross-origin reads.
    throw new Error(
      `Could not fetch ${target} - the server did not allow this page to `
      + "read it (CORS). Download the file and drop it here instead.",
    );
  }
  if (!response.ok) {
    throw new Error(`${target} returned ${response.status} ${response.statusText}`);
  }

  const declared = Number(response.headers.get("content-length"));
  const total = Number.isFinite(declared) && declared > 0 ? declared : null;
  if (!response.body || !onProgress) {
    const buffer = await response.arrayBuffer();
    return { name: fileNameOf(target), bytes: new Uint8Array(buffer) };
  }

  const reader = response.body.getReader();
  const chunks: Uint8Array[] = [];
  let loaded = 0;
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    loaded += value.length;
    onProgress(loaded, total);
  }
  const bytes = new Uint8Array(loaded);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.length;
  }
  return { name: fileNameOf(target), bytes };
}
