// vis-network rendering. Filtering is OPACITY-BASED: non-matching nodes
// fade to ~10% but stay in the simulation so positions are stable.

import { makeMatcher } from "./state.js";

const FADED = 0.10;
const FULL = 1.0;

export function createGraph({ container, graph, controller, root, topicById }) {
  /* eslint-disable no-undef */
  if (typeof vis === "undefined") {
    container.textContent = "Network library failed to load.";
    return { update() {}, fit() {} };
  }

  const idToIndex = new Map();
  graph.nodes.forEach((n, i) => idToIndex.set(n.id, i));

  const visNodes = graph.nodes.map((n, i) => {
    const hex = topicById.get(n.topic)?.color || "#888";
    return {
      id: i,
      label: n.title.length > 28 ? n.title.slice(0, 26) + "…" : n.title,
      title: `${n.title}\n${topicById.get(n.topic)?.label ?? n.topic}\n${(n.tags || []).slice(0, 6).join(", ")}`,
      // Pin background and border to the same hex used by the topic
      // chip swatches so the legend and the network read identically.
      // vis-network's string-color shortcut otherwise derives a
      // slightly darker border, making the rendered dot drift from
      // the swatch colour.
      color: { background: hex, border: hex, highlight: { background: hex, border: hex }, hover: { background: hex, border: hex } },
      nodeRef: n,
      url: root + n.url,
    };
  });

  const visEdges = graph.edges.map((e, i) => {
    const edge = {
      id: i,
      from: idToIndex.get(e.source),
      to: idToIndex.get(e.target),
      value: e.weight,
    };
    if (e.dashed) {
      // 1-tag-overlap edges: dashed, lower opacity, thinner. Communicates
      // a weaker relation without crowding out the solid (≥2-tag) edges.
      edge.dashes = [4, 4];
      edge.value = 0.6;
    }
    return edge;
  });

  const dataset = {
    nodes: new vis.DataSet(visNodes),
    edges: new vis.DataSet(visEdges),
  };

  const network = new vis.Network(container, dataset, themedOptions());

  network.on("click", (params) => {
    if (!params.nodes.length) return;
    const id = params.nodes[0];
    const node = dataset.nodes.get(id);
    if (!node) return;
    if (params.event.srcEvent.shiftKey) {
      controller.toggle("topics", node.nodeRef.topic);
      return;
    }
    if (params.event.srcEvent.altKey) {
      for (const tag of node.nodeRef.tags) controller.toggle("tags", tag);
      return;
    }
    if (node.url) window.location.href = node.url;
  });

  // Theme reaction: MutationObserver on the html element's data-bs-theme
  // attribute. More robust than the legacy click handler on Quarto's
  // toggle button — also catches OS-preference changes and any
  // programmatic toggles.
  const obs = new MutationObserver(() => {
    network.setOptions(themedOptions());
  });
  obs.observe(document.documentElement, { attributes: true, attributeFilter: ["data-bs-theme", "class"] });

  function themedOptions() {
    const cs = getComputedStyle(document.documentElement);
    const fg = cs.getPropertyValue("--fg").trim() || "#222";
    const accent = cs.getPropertyValue("--accent").trim() || "#1a73e8";
    const bg = cs.getPropertyValue("--bg").trim() || "#ffffff";
    const isDark = (() => {
      const m = bg.match(/^#?([0-9a-f]{6})$/i);
      if (!m) return false;
      const n = parseInt(m[1], 16);
      return ((n >> 16 & 255) + (n >> 8 & 255) + (n & 255)) / 3 < 128;
    })();
    return {
      // size matches the .chip-swatch (0.65rem ≈ 10px) so the network
      // dot reads as the same circle as the legend swatch.
      // borderWidth: 0 — any visible 1px stroke darkens a small dot
      // and made the rendered colour drift from the chip swatch hex.
      nodes: { shape: "dot", size: 10, font: { size: 10, color: fg }, borderWidth: 0 },
      edges: {
        color: { color: isDark ? "rgba(200,200,200,0.18)" : "rgba(80,80,80,0.22)", highlight: accent },
        smooth: false,
      },
      physics: {
        solver: "forceAtlas2Based",
        forceAtlas2Based: { gravitationalConstant: -40, springLength: 90, springConstant: 0.05 },
        stabilization: { iterations: 200 },
      },
      interaction: { hover: true, tooltipDelay: 200, navigationButtons: false },
    };
  }

  function update(state, searchHits) {
    const matches = makeMatcher(state, searchHits);
    const matched = new Array(graph.nodes.length);
    for (let i = 0; i < graph.nodes.length; i++) matched[i] = matches(graph.nodes[i]);

    const nodeUpdates = visNodes.map((vn, i) => ({
      id: vn.id,
      opacity: matched[i] ? FULL : FADED,
    }));
    dataset.nodes.update(nodeUpdates);

    // An edge is "lit" only when both endpoints match.
    const edgeUpdates = visEdges.map((ve) => ({
      id: ve.id,
      hidden: !(matched[ve.from] && matched[ve.to]) && (state.topics.size + state.tags.size + state.labels.size > 0 || !!searchHits),
    }));
    dataset.edges.update(edgeUpdates);
  }

  return {
    update,
    fit() { network.fit(); },
    network,
  };
}

export function bindResetButton(btn, network) {
  if (!btn) return;
  btn.addEventListener("click", () => network.fit());
}
