// Courses overview entry point. Same architecture as the tutorials
// overview module — single shared filterState, OR-within / AND-across,
// vis-network with opacity-based filtering, Pagefind search, native
// dual-handle range slider (no-op when min==max), heatmap, mobile nav.
// The data shape is identical (artifacts/graph.json) — only the data
// model differs (4 courses, no labels axis).

import { createState } from "./state.js";
import { createGraph, bindResetButton } from "./graph.js";
import { createLegend } from "./legend.js";
import { createList } from "./list.js";
import { createSearch } from "./search.js";
import { createSlider } from "./slider.js";
import { createHeatmap } from "./heatmap.js";
import { createMobileNav } from "./mobile-nav.js";
import { createAutocomplete } from "./autocomplete.js";

function detectRoot() {
  const brand = document.querySelector(".navbar-brand[href]");
  if (brand) {
    const h = brand.getAttribute("href");
    if (h && h !== "#") return h.endsWith("/") ? h : h.replace(/[^/]*$/, "");
  }
  return "";
}

async function loadGraph(root) {
  const candidates = [root + "artifacts/graph.json", "artifacts/graph.json", "../artifacts/graph.json"];
  for (const url of candidates) {
    try {
      const r = await fetch(url, { cache: "no-cache" });
      if (r.ok) return await r.json();
    } catch (_) { /* try next */ }
  }
  throw new Error("artifacts/graph.json not reachable");
}

async function bootstrap() {
  const root = detectRoot();

  let graph;
  try { graph = await loadGraph(root); }
  catch (e) {
    document.getElementById("tutorial-list").textContent =
      "Failed to load activity network. Reload the page or report at https://github.com/CTTIR/clinical-data-science-in-r-course/issues.";
    console.error(e);
    return;
  }

  const topicById = new Map(graph.topics.map(t => [t.id, t]));
  const minYear = 0, maxYear = 0;  // courses don't track dates; slider is a no-op
  const controller = createState({ minYear, maxYear });

  const networkContainer = document.getElementById("tutorial-network");
  const graphView = createGraph({ container: networkContainer, graph, controller, root, topicById });
  const legend    = createLegend({ root, graph, controller, topicById });
  const list      = createList({ root, graph, topicById });
  const slider    = createSlider({ controller, minYear, maxYear });
  const heatmap   = createHeatmap({ controller, graph });
  const mobileNav = createMobileNav({ graph, topicById, root });
  // Title-only autocomplete dropdown on the search input. Independent
  // of Pagefind: typing surfaces lab-title matches instantly for direct
  // navigation, while Pagefind keeps driving the linked filter.
  createAutocomplete({ graph, root, topicById });

  bindResetButton(document.getElementById("network-reset"), graphView);

  const announcer = document.getElementById("a11y-announcer");
  let announceTimer = null;
  function announce(state, total, matching) {
    if (!announcer) return;
    clearTimeout(announceTimer);
    announceTimer = setTimeout(() => {
      const filters = [];
      if (state.topics.size) filters.push(`${state.topics.size} course${state.topics.size > 1 ? "s" : ""}`);
      if (state.tags.size)   filters.push(`${state.tags.size} tag${state.tags.size > 1 ? "s" : ""}`);
      if (state.query)       filters.push(`search "${state.query}"`);
      announcer.textContent = filters.length
        ? `${matching} of ${total} activities match (${filters.join(", ")}).`
        : `Showing all ${total} activities.`;
    }, 400);
  }

  let searchResult = { hits: null, snippets: new Map(), order: [] };

  function fanOut(s) {
    graphView.update(s, searchResult.hits);
    legend.update(s, searchResult.hits);
    list.update(s, searchResult.hits, searchResult.order, searchResult.snippets);
    slider.update(s);
    heatmap.update(s, searchResult.hits);
    mobileNav.update(s, searchResult.hits);

    let matching = 0;
    const m = (n) => {
      if (s.topics.size && !s.topics.has(n.topic)) return false;
      if (s.tags.size && !n.tags.some(t => s.tags.has(t))) return false;
      if (searchResult.hits && !searchResult.hits.has(n.id)) return false;
      return true;
    };
    for (const n of graph.nodes) if (m(n)) matching++;
    announce(s, graph.nodes.length, matching);
  }

  const search = createSearch({
    root, graph, controller,
    onResults(result) {
      searchResult = result;
      document.getElementById("search-status").textContent = result.error || "";
      fanOut(controller.state);
    },
  });

  let lastQuery = null;
  controller.subscribe((s) => {
    if (s.query !== lastQuery) {
      lastQuery = s.query;
      search.update(s);
    }
    fanOut(s);
  });

  controller.hydrateFromURL();
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", bootstrap);
} else {
  bootstrap();
}
