import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// The gallery is served from https://iory.github.io/urdfeus/ and the Pages
// workflow drops this build at /convert, so assets must resolve from there.
export default defineConfig({
  base: "/urdfeus/convert/",
  plugins: [react()],
  test: {
    environment: "jsdom",
    include: ["src/**/*.test.ts"],
  },
});
