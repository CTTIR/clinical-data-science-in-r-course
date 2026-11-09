// Tag co-occurrence heatmap.
//
// For the top-N most-frequent tags in the currently filtered node set,
// build an N×N matrix where cell[i][j] is the number of tutorials
// tagged with both. Diagonal = total count for that tag.
//
// Click a cell (i, j) → set tags filter to {tag_i, tag_j} (only one tag
// when i === j). The matrix is recomputed on every filter change so the
// heatmap reflects the active subset.
//
// Pure SVG + JS — no D3 — keeps the bundle thin and consistent with the
// rest of the overview module.

import { makeMatcher } from "./state.js";

const PADDING = { top: 110, right: 16, bottom: 16, left: 110 };
const CELL = 18;
const GAP = 1;
const MAX_LABEL_CHARS = 14;

export function createHeatmap({ controller, graph }) {
  const host = document.getElementById("heatmap");
  const nInput = document.getElementById("heatmap-n");
  const nReadout = document.getElementById("heatmap-n-readout");
  const wrapper = document.getElementById("heatmap-wrapper");
  if (!host || !nInput) return { update() {} };

  let topN = clampN(parseInt(nInput.value, 10) || 20);
  nReadout && (nReadout.textContent = String(topN));

  let lastState = controller.state;
  let lastHits = null;

  nInput.addEventListener("input", () => {
    topN = clampN(parseInt(nInput.value, 10) || 20);
    if (nReadout) nReadout.textContent = String(topN);
    render(lastState, lastHits);
  });

  // Render only when the heatmap is open — saves work for users who
  // never expand it.
  const details = wrapper?.closest?.("details");
  details?.addEventListener("toggle", () => {
    if (details.open) render(lastState, lastHits);
  });

  function update(state, searchHits) {
    lastState = state;
    lastHits = searchHits;
    if (!details || details.open) render(state, searchHits);
  }

  function render(state, searchHits) {
    const matches = makeMatcher(state, searchHits);
    const filtered = graph.nodes.filter(matches);

    // 1. Tag frequencies in the filtered subset.
    const freq = new Map();
    for (const n of filtered) {
      for (const t of n.tags) freq.set(t, (freq.get(t) || 0) + 1);
    }
    const tags = [...freq.entries()]
      .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
      .slice(0, topN)
      .map(([id]) => id);

    if (tags.length === 0) {
      host.replaceChildren(makeEmptyMessage("No tags in current filter."));
      return;
    }

    // 2. Co-occurrence matrix.
    const idx = new Map(tags.map((t, i) => [t, i]));
    const matrix = Array.from({ length: tags.length }, () => new Array(tags.length).fill(0));
    for (const n of filtered) {
      const present = n.tags.filter(t => idx.has(t));
      for (const a of present) {
        const ia = idx.get(a);
        for (const b of present) {
          const ib = idx.get(b);
          matrix[ia][ib] += 1;
        }
      }
    }

    let max = 0;
    for (const row of matrix) for (const v of row) if (v > max) max = v;
    if (max === 0) {
      host.replaceChildren(makeEmptyMessage("No tag co-occurrences in current filter."));
      return;
    }

    host.replaceChildren(buildSvg(tags, matrix, max, controller));
  }

  return { update };
}

function clampN(n) { return Math.max(10, Math.min(50, n | 0)); }

function buildSvg(tags, matrix, max, controller) {
  const N = tags.length;
  const sizePx = N * (CELL + GAP) - GAP;
  const width = PADDING.left + sizePx + PADDING.right;
  const height = PADDING.top + sizePx + PADDING.bottom;

  // Read theme from CSS vars. The accent gives us the high-end of the
  // colour scale; lerping in sRGB is fine for a heatmap legend.
  const cs = getComputedStyle(document.documentElement);
  const accent = cs.getPropertyValue("--accent").trim() || "#1a73e8";
  const surface = cs.getPropertyValue("--surface").trim() || "#f1f3f5";
  const fg = cs.getPropertyValue("--fg").trim() || "#222";
  const fgAlt = cs.getPropertyValue("--fg-alt").trim() || "#666";

  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  svg.setAttribute("role", "img");
  svg.setAttribute("aria-label", `Tag co-occurrence heatmap, ${N} tags`);
  svg.style.maxWidth = "100%";
  svg.style.height = "auto";

  // Column labels (rotated).
  for (let i = 0; i < N; i++) {
    const x = PADDING.left + i * (CELL + GAP) + CELL / 2;
    const y = PADDING.top - 6;
    const text = svgEl("text", {
      x, y,
      "text-anchor": "start",
      transform: `rotate(-55 ${x} ${y})`,
      "font-size": "10",
      fill: fgAlt,
    });
    text.textContent = truncate(tags[i]);
    svg.appendChild(text);
  }

  // Row labels.
  for (let i = 0; i < N; i++) {
    const x = PADDING.left - 6;
    const y = PADDING.top + i * (CELL + GAP) + CELL / 2 + 3;
    const text = svgEl("text", {
      x, y,
      "text-anchor": "end",
      "font-size": "10",
      fill: fgAlt,
    });
    text.textContent = truncate(tags[i]);
    svg.appendChild(text);
  }

  // Cells.
  for (let i = 0; i < N; i++) {
    for (let j = 0; j < N; j++) {
      const v = matrix[i][j];
      const t = v / max;
      const fill = t === 0 ? surface : lerp(surface, accent, t);
      const rect = svgEl("rect", {
        x: PADDING.left + j * (CELL + GAP),
        y: PADDING.top + i * (CELL + GAP),
        width: CELL,
        height: CELL,
        fill,
        rx: 2,
        role: "button",
        tabindex: v > 0 ? "0" : "-1",
        "aria-label": `${tags[i]} and ${tags[j]}: ${v} tutorials`,
        style: v > 0 ? "cursor: pointer;" : "",
      });
      const title = svgEl("title", {});
      title.textContent = `${tags[i]} × ${tags[j]}: ${v}`;
      rect.appendChild(title);
      if (v > 0) {
        const handler = () => {
          // Replace tag selection (rather than additive toggle) so a
          // heatmap click is a deliberate "show me this intersection".
          const wantTags = i === j ? [tags[i]] : [tags[i], tags[j]];
          controller.replaceCategory("tags", wantTags);
        };
        rect.addEventListener("click", handler);
        rect.addEventListener("keydown", (ev) => {
          if (ev.key === "Enter" || ev.key === " ") { ev.preventDefault(); handler(); }
        });
      }
      svg.appendChild(rect);
    }
  }

  return svg;
}

function svgEl(tag, attrs) {
  const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [k, v] of Object.entries(attrs)) el.setAttribute(k, String(v));
  return el;
}

function makeEmptyMessage(text) {
  const p = document.createElement("p");
  p.className = "heatmap-empty";
  p.textContent = text;
  return p;
}

function truncate(s) {
  return s.length > MAX_LABEL_CHARS ? s.slice(0, MAX_LABEL_CHARS - 1) + "…" : s;
}

// Linear sRGB-space interpolation between two #RRGGBB strings.
function lerp(a, b, t) {
  const ca = parseHex(a), cb = parseHex(b);
  const r = Math.round(ca[0] + (cb[0] - ca[0]) * t);
  const g = Math.round(ca[1] + (cb[1] - ca[1]) * t);
  const bl = Math.round(ca[2] + (cb[2] - ca[2]) * t);
  return `rgb(${r}, ${g}, ${bl})`;
}

function parseHex(s) {
  const m = s.trim().match(/^#?([0-9a-f]{6})$/i);
  if (!m) return [200, 200, 200];
  const n = parseInt(m[1], 16);
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
}
