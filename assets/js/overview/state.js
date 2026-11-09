// Single shared filter state for the overview page.
//
// Combination logic: OR within a category, AND across categories.
//   - topics:  node passes if topics is empty OR node.topic ∈ topics
//   - tags:    node passes if tags   is empty OR node.tags ∩ tags ≠ ∅
//   - labels:  node passes if labels is empty OR node.labels ∩ labels ≠ ∅
//   - year:    node passes if dateFrom ≤ node.year ≤ dateTo
//   - query:   not enforced here — Pagefind layer (Phase 4) intersects later
//
// URL params: ?topics=a,b&tags=x,y&labels=l1&from=2024&to=2026&q=foo
// Updates fire `state.subscribe(fn)` listeners and write history via
// `replaceState` so history is not polluted by every keystroke.

const KEYS = ["topics", "tags", "labels", "from", "to", "q"];

export function createState({ minYear, maxYear }) {
  const listeners = new Set();
  const s = {
    topics: new Set(),
    tags: new Set(),
    labels: new Set(),
    query: "",
    dateFrom: minYear,
    dateTo: maxYear,
    _bounds: { minYear, maxYear },
  };

  function notify() {
    for (const fn of listeners) fn(s);
    writeURL();
  }

  function readURL() {
    const p = new URLSearchParams(location.search);
    for (const cat of ["topics", "tags", "labels"]) {
      s[cat].clear();
      const raw = p.get(cat);
      if (raw) for (const v of raw.split(",").filter(Boolean)) s[cat].add(v);
    }
    s.query = p.get("q") || "";
    const f = parseInt(p.get("from"), 10);
    const t = parseInt(p.get("to"), 10);
    setBounds(f, t);
  }

  function setBounds(from, to) {
    const clamp = (value, fallback) => Number.isFinite(value)
      ? Math.min(maxYear, Math.max(minYear, value)) : fallback;
    const a = clamp(from, minYear), b = clamp(to, maxYear);
    s.dateFrom = Math.min(a, b);
    s.dateTo = Math.max(a, b);
  }

  function writeURL() {
    const p = new URLSearchParams(location.search);
    for (const k of KEYS) p.delete(k);
    if (s.topics.size) p.set("topics", [...s.topics].join(","));
    if (s.tags.size) p.set("tags", [...s.tags].join(","));
    if (s.labels.size) p.set("labels", [...s.labels].join(","));
    if (s.query) p.set("q", s.query);
    if (s.dateFrom !== minYear) p.set("from", String(s.dateFrom));
    if (s.dateTo !== maxYear) p.set("to", String(s.dateTo));
    const qs = p.toString();
    const url = qs ? `${location.pathname}?${qs}${location.hash}`
                   : `${location.pathname}${location.hash}`;
    history.replaceState(null, "", url);
  }

  return {
    state: s,
    subscribe(fn) { listeners.add(fn); return () => listeners.delete(fn); },
    hydrateFromURL() { readURL(); notify(); },
    toggle(category, value) {
      const set = s[category];
      if (!(set instanceof Set)) return;
      if (set.has(value)) set.delete(value); else set.add(value);
      notify();
    },
    replaceCategory(category, values) {
      const set = s[category];
      if (!(set instanceof Set)) return;
      set.clear();
      for (const v of values) set.add(v);
      notify();
    },
    setQuery(q) { s.query = q || ""; notify(); },
    setRange(from, to) { setBounds(from, to); notify(); },
    reset() {
      s.topics.clear(); s.tags.clear(); s.labels.clear();
      s.query = "";
      s.dateFrom = minYear; s.dateTo = maxYear;
      notify();
    },
    isActive() {
      return s.topics.size > 0 || s.tags.size > 0 || s.labels.size > 0
          || !!s.query
          || s.dateFrom !== minYear || s.dateTo !== maxYear;
    },
  };
}

// Pure predicate. `searchHits` is a Set of node ids the search layer
// considers a match — or null when no query is active.
export function makeMatcher(state, searchHits) {
  return function matches(node) {
    if (state.topics.size && !state.topics.has(node.topic)) return false;
    if (state.tags.size) {
      let any = false;
      for (const t of node.tags) if (state.tags.has(t)) { any = true; break; }
      if (!any) return false;
    }
    if (state.labels.size) {
      let any = false;
      for (const l of node.labels) if (state.labels.has(l)) { any = true; break; }
      if (!any) return false;
    }
    if (node.year < state.dateFrom || node.year > state.dateTo) return false;
    if (searchHits && !searchHits.has(node.id)) return false;
    return true;
  };
}
