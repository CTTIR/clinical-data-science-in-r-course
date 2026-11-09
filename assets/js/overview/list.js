// Filtered lab list. Sorted by course → week → session by default.
// When Pagefind supplies an order hint (search active), use that instead.

import { makeMatcher } from "./state.js";

const MAX_RENDER = 200;

export function createList({ root, graph, topicById }) {
  const container = document.getElementById("tutorial-list");
  const empty = document.getElementById("tutorial-list-empty");

  function update(state, searchHits, orderHint = null, snippets = null) {
    const matches = makeMatcher(state, searchHits);
    let items = graph.nodes.filter(matches);

    if (orderHint && orderHint.length) {
      const rank = new Map(orderHint.map((id, i) => [id, i]));
      items.sort((a, b) => (rank.get(a.id) ?? 1e9) - (rank.get(b.id) ?? 1e9));
    } else {
      items.sort((a, b) =>
        a.topic.localeCompare(b.topic) ||
        (a.week ?? 0) - (b.week ?? 0) ||
        (a.session ?? 0) - (b.session ?? 0)
      );
    }

    container.replaceChildren();
    if (items.length === 0) {
      if (empty) empty.hidden = false;
      return;
    }
    if (empty) empty.hidden = true;

    const frag = document.createDocumentFragment();
    for (const t of items.slice(0, MAX_RENDER)) {
      frag.appendChild(renderCard(t, root, topicById, snippets?.get(t.id)));
    }
    container.appendChild(frag);
  }

  return { update };
}

function renderCard(t, root, topicById, snippetHTML) {
  const card = document.createElement("article");
  card.className = "tutorial-card";

  const topic = topicById.get(t.topic);
  const pill = document.createElement("span");
  pill.className = "topic-pill";
  const bg = topic?.color || "#888";
  pill.style.background = bg;
  pill.style.color = readableText(bg);
  pill.textContent = topic?.label || t.topic;

  const meta = document.createElement("span");
  meta.className = "lab-meta";
  meta.textContent = t.kind + (t.week ? ` · W${t.week}` : "");

  const h3 = document.createElement("h3");
  const a = document.createElement("a");
  a.href = root + t.url;
  a.textContent = t.title;
  h3.appendChild(a);

  const head = document.createElement("div");
  head.className = "card-head";
  head.appendChild(pill);
  head.appendChild(meta);

  card.appendChild(head);
  card.appendChild(h3);

  if (snippetHTML) {
    const p = document.createElement("p");
    p.className = "desc desc-snippet";
    p.innerHTML = snippetHTML;
    card.appendChild(p);
  }

  if (t.tags && t.tags.length) {
    const tags = document.createElement("div");
    tags.className = "tags";
    for (const tag of t.tags.slice(0, 6)) {
      const s = document.createElement("span");
      s.className = "tag";
      s.textContent = tag;
      tags.appendChild(s);
    }
    card.appendChild(tags);
  }
  return card;
}

function readableText(hex) {
  const m = String(hex || "").match(/^#?([0-9a-f]{3}|[0-9a-f]{6})$/i);
  if (!m) return "#fff";
  let h = m[1];
  if (h.length === 3) h = h.split("").map(c => c + c).join("");
  const r = parseInt(h.slice(0, 2), 16) / 255;
  const g = parseInt(h.slice(2, 4), 16) / 255;
  const b = parseInt(h.slice(4, 6), 16) / 255;
  const lin = (c) => (c <= 0.03928 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4));
  const L = 0.2126 * lin(r) + 0.7152 * lin(g) + 0.0722 * lin(b);
  return L > 0.5 ? "#111" : "#fff";
}
