import { fileURLToPath } from "node:url";
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import links from "starlight-links-validator";
import hypha from "starlight-theme-hypha";

// wiretype's documentation site, published to GitHub Pages by
// .github/workflows/docs.yml. `make docs` builds it into _build/docs, odoc's
// reference from the .mli files into public/reference/ first.
//
// Every program a page shows is a whole file under examples/, built with the
// repository, which the page imports with `?raw` into Starlight's <Code> --
// so a page cannot show code that does not compile. A missing file, or a link
// to a page or a heading that is not there, fails the build.
const repository = fileURLToPath(new URL("..", import.meta.url));

export default defineConfig({
  site: "https://hyphatech.github.io",
  base: "/wiretype",
  outDir: `${repository}_build/docs`,
  vite: {
    resolve: {
      // Pages import an example as "@examples/<file>.ml?raw".
      alias: { "@examples": `${repository}examples` },
    },
  },
  integrations: [
    starlight({
      title: "wiretype",
      description: "Define your JSON once and share it across your backend, front end and AI agents.",
      logo: { src: "starlight-theme-hypha/assets/hypha.webp" },
      favicon: "/favicon-32.png",
      head: [
        { tag: "link", attrs: { rel: "icon", type: "image/png", sizes: "16x16", href: "/wiretype/favicon-16.png" } },
        { tag: "link", attrs: { rel: "apple-touch-icon", sizes: "180x180", href: "/wiretype/apple-touch-icon.png" } },
      ],
      plugins: [
        hypha({
          brand: "hypha",
          github: "hyphatech/wiretype",
          tabs: { "Start here": "rocket", Guides: "book-open", Reference: "library" },
          links: [{ label: "Hypha Stack", href: "https://hyphatech.io/stack", icon: "layers" }],
        }),
        // The reference is odoc's, in public/, which the validator cannot see.
        links({
          components: [["Feature", "href"]],
          exclude: ["**/reference/wiretype/**", "**/reference/ppx_wiretype/**"],
        }),
      ],
      sidebar: [
        { label: "Start here", items: ["install", "quick-start"] },
        {
          label: "Guides",
          items: [
            "guide/by-hand",
            "guide/deriver",
            "guide/reading",
            "guide/writing",
            "guide/bounds",
            "guide/kinds",
            "guide/json-schema",
            "guide/zod",
            "guide/melange",
            "guide/walking",
            "guide/what-it-does-not-do",
          ],
        },
        { label: "Reference", items: ["reference"] },
      ],
    }),
  ],
});
