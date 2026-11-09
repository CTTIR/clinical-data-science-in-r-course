// Topic / tag / label chips. Each chip is keyboard-accessible (button
// element with role+tabindex), shows a live count of matching tutorials
// within the current filter, and toggles its category on click/Enter/Space.
//
// "Live count" excludes the chip's own category from the predicate so a
// user can see how many tutorials a chip *would* add. Same convention
// faceted-search UIs typically use.

import { makeMatcher } from "./state.js";

const FREQ_TAG_THRESHOLD = 3;

export function createLegend({ root, graph, controller, topicById }) {
  const topicBar = document.getElementById("topic-filter-bar");
  const tagBar = document.getElementById("tag-filter-bar");
  const labelBar = document.getElementById("label-filter-bar");
  const summary = document.getElementById("filter-summary-text");
  const resetBtn = document.getElementById("filter-reset");

  const topicChips = new Map();
  const tagChips = new Map();
  const labelChips = new Map();

  // Topics — ordered by topics.yml order field (already sorted by graph.json).
  for (const t of graph.topics) {
    const chip = makeChip({
      label: t.label,
      count: t.count,
      colorSwatch: t.color,
      onToggle: () => controller.toggle("topics", t.id),
      // Accessible name must start with visible label per WCAG 2.5.3.
      // Suffix carries the toggle hint without breaking the rule.
      ariaLabel: `${t.label}, topic filter`,
    });
    chip.dataset.value = t.id;
    topicBar.appendChild(chip);
    topicChips.set(t.id, chip);
  }

  // Tags — frequency-thresholded, sorted by count desc.
  for (const t of graph.tags) {
    if (t.count < FREQ_TAG_THRESHOLD) break;
    const chip = makeChip({
      label: t.label,
      count: t.count,
      onToggle: () => controller.toggle("tags", t.id),
      ariaLabel: `${t.label}, tag filter`,
    });
    chip.dataset.value = t.id;
    tagBar.appendChild(chip);
    tagChips.set(t.id, chip);
  }

  // Labels — only render the bar if the dataset has any.
  if (graph.labels.length === 0 && labelBar) {
    labelBar.style.display = "none";
  } else if (labelBar) {
    for (const l of graph.labels) {
      const chip = makeChip({
        label: l.label,
        count: l.count,
        onToggle: () => controller.toggle("labels", l.id),
        ariaLabel: `${l.label}, label filter`,
      });
      chip.dataset.value = l.id;
      labelBar.appendChild(chip);
      labelChips.set(l.id, chip);
    }
  }

  resetBtn?.addEventListener("click", () => controller.reset());

  function update(state, searchHits) {
    // Live counts use a "category-relaxed" predicate per chip-class.
    const counts = recomputeCounts(graph, state, searchHits);

    for (const [id, chip] of topicChips) updateChip(chip, state.topics.has(id), counts.topics.get(id) ?? 0);
    for (const [id, chip] of tagChips)   updateChip(chip, state.tags.has(id),   counts.tags.get(id)   ?? 0);
    for (const [id, chip] of labelChips) updateChip(chip, state.labels.has(id), counts.labels.get(id) ?? 0);

    if (summary) {
      const matching = counts.totalMatching;
      summary.textContent = controller.isActive()
        ? `Showing ${matching} of ${graph.nodes.length} activities.`
        : `Browse all ${graph.nodes.length} activities.`;
    }
    if (resetBtn) resetBtn.hidden = !controller.isActive();
  }

  return { update };
}

function makeChip({ label, count, colorSwatch, onToggle, ariaLabel }) {
  const btn = document.createElement("button");
  btn.type = "button";
  btn.className = "filter-chip";
  btn.setAttribute("aria-pressed", "false");
  btn.setAttribute("aria-label", ariaLabel);
  btn.dataset.active = "false";
  if (colorSwatch) {
    const sw = document.createElement("span");
    sw.className = "chip-swatch";
    sw.style.background = colorSwatch;
    btn.appendChild(sw);
  }
  btn.appendChild(document.createTextNode(label));
  const c = document.createElement("span");
  c.className = "chip-count";
  c.textContent = String(count);
  btn.appendChild(c);
  btn.addEventListener("click", onToggle);
  return btn;
}

function updateChip(chip, active, count) {
  chip.dataset.active = active ? "true" : "false";
  chip.setAttribute("aria-pressed", active ? "true" : "false");
  const c = chip.querySelector(".chip-count");
  if (c) c.textContent = String(count);
  chip.classList.toggle("is-empty", !active && count === 0);
}

function recomputeCounts(graph, state, searchHits) {
  const topics = new Map();
  const tags = new Map();
  const labels = new Map();

  // Predicates that *exclude* the chip's own category so a chip's count
  // shows what selecting it would add (relative to the rest of the filter).
  const matchExceptTopics = makeMatcher({ ...state, topics: new Set() }, searchHits);
  const matchExceptTags   = makeMatcher({ ...state, tags:   new Set() }, searchHits);
  const matchExceptLabels = makeMatcher({ ...state, labels: new Set() }, searchHits);
  const matchAll = makeMatcher(state, searchHits);

  let totalMatching = 0;
  for (const n of graph.nodes) {
    if (matchAll(n)) totalMatching++;
    if (matchExceptTopics(n)) topics.set(n.topic, (topics.get(n.topic) || 0) + 1);
    if (matchExceptTags(n))   for (const t of n.tags)  tags.set(t, (tags.get(t) || 0) + 1);
    if (matchExceptLabels(n)) for (const l of n.labels) labels.set(l, (labels.get(l) || 0) + 1);
  }
  return { topics, tags, labels, totalMatching };
}
