// Renders a topic-grouped, semantic <nav> of every tutorial. Always
// present in the DOM (visually-hidden on desktop) so screen readers
// have a clean linear path through the catalogue regardless of the
// network graph. Becomes visible below 768px where vis-network is
// unusable.
//
// Each <details> defaults closed except the topic the user has
// currently selected (single-topic case). On filter change we re-render
// in place so the visible/expanded subset matches what's on screen.

import { makeMatcher } from "./state.js";

export function createMobileNav({ graph, topicById, root }) {
  const host = document.getElementById("all-tutorials-nav");
  if (!host) return { update() {} };

  function render(state, searchHits) {
    const matches = makeMatcher(state, searchHits);
    const byTopic = new Map();
    for (const t of graph.topics) byTopic.set(t.id, []);
    for (const n of graph.nodes) {
      if (matches(n)) byTopic.get(n.topic)?.push(n);
    }

    host.replaceChildren();
    const onlyTopic = state.topics.size === 1 ? [...state.topics][0] : null;

    for (const topic of graph.topics) {
      const items = byTopic.get(topic.id) || [];
      if (items.length === 0) continue;
      const det = document.createElement("details");
      det.open = onlyTopic === topic.id || items.length <= 8;

      const sum = document.createElement("summary");
      const swatch = document.createElement("span");
      swatch.className = "nav-swatch";
      swatch.style.background = topic.color;
      sum.appendChild(swatch);
      sum.appendChild(document.createTextNode(`${topic.label} (${items.length})`));
      det.appendChild(sum);

      const ul = document.createElement("ul");
      items.sort((a, b) => a.title.localeCompare(b.title));
      for (const t of items) {
        const li = document.createElement("li");
        const a = document.createElement("a");
        a.href = root + t.url;
        a.textContent = t.title;
        li.appendChild(a);
        ul.appendChild(li);
      }
      det.appendChild(ul);
      host.appendChild(det);
    }

    if (host.children.length === 0) {
      const p = document.createElement("p");
      p.className = "nav-empty";
      p.textContent = "No tutorials match the current filters.";
      host.appendChild(p);
    }
  }

  return { update: render };
}
