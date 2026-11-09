// Title-only autocomplete for the overview search input.
//
// Sits next to the Pagefind-backed full-text search:
//   - Pagefind drives the linked filter (debounced, full-body match).
//   - Autocomplete is an instant ARIA combobox dropdown that matches
//     ONLY against tutorial titles, ranks substring matches, and
//     navigates to the chosen tutorial on Enter or click.
//
// Why both: title-only autocomplete answers "I know the title" without
// waiting on Pagefind's WASM round-trip, and snaps directly to the page
// instead of just filtering the list. Pagefind still owns full-text
// queries that drive the filter.
//
// Accessibility: ARIA combobox pattern (input + listbox), arrow-key
// navigation, aria-activedescendant for focus, Enter to confirm,
// Escape/blur to close.

const MAX_SUGGESTIONS = 10;
const MIN_QUERY = 1;

export function createAutocomplete({ graph, root, topicById }) {
  const input = document.getElementById("overview-search");
  if (!input) return;

  // Build the listbox once. Position absolutely under the input so it
  // overlays content below without reflowing the page.
  const list = document.createElement("ul");
  list.id = "overview-autocomplete";
  list.className = "autocomplete-list";
  list.setAttribute("role", "listbox");
  list.hidden = true;

  // Combobox wiring on the input.
  input.setAttribute("role", "combobox");
  input.setAttribute("aria-autocomplete", "list");
  input.setAttribute("aria-controls", list.id);
  input.setAttribute("aria-expanded", "false");

  // Insert listbox right after the input. Wrap the input in a relative
  // container so absolute positioning pins to it.
  const wrap = document.createElement("div");
  wrap.className = "autocomplete-wrap";
  input.parentNode.insertBefore(wrap, input);
  wrap.appendChild(input);
  wrap.appendChild(list);

  // Pre-lower-case titles for fast substring matching.
  const index = graph.nodes.map(n => ({
    id: n.id,
    title: n.title,
    titleLower: (n.title || "").toLowerCase(),
    url: n.url,
    topic: n.topic,
  }));

  let activeIndex = -1;
  let currentMatches = [];

  function score(query, item) {
    // Earlier matches rank higher; exact prefix beats mid-word match.
    const idx = item.titleLower.indexOf(query);
    if (idx < 0) return -1;
    return 1000 - idx - item.title.length * 0.01;
  }

  function search(raw) {
    const q = (raw || "").trim().toLowerCase();
    if (q.length < MIN_QUERY) return [];
    const scored = [];
    for (const item of index) {
      const s = score(q, item);
      if (s > 0) scored.push({ item, s });
    }
    scored.sort((a, b) => b.s - a.s);
    return scored.slice(0, MAX_SUGGESTIONS).map(x => x.item);
  }

  function highlight(title, query) {
    const q = query.toLowerCase();
    const t = title;
    const i = t.toLowerCase().indexOf(q);
    if (i < 0) return [document.createTextNode(t)];
    const before = document.createTextNode(t.slice(0, i));
    const mark = document.createElement("mark");
    mark.textContent = t.slice(i, i + q.length);
    const after = document.createTextNode(t.slice(i + q.length));
    return [before, mark, after];
  }

  function render(matches, query) {
    list.replaceChildren();
    currentMatches = matches;
    if (matches.length === 0) {
      list.hidden = true;
      input.setAttribute("aria-expanded", "false");
      input.removeAttribute("aria-activedescendant");
      return;
    }
    matches.forEach((m, i) => {
      const li = document.createElement("li");
      li.id = `overview-autocomplete-${i}`;
      li.className = "autocomplete-item";
      li.setAttribute("role", "option");
      li.setAttribute("aria-selected", "false");
      li.dataset.url = root + m.url;

      const topic = topicById.get(m.topic);
      if (topic) {
        const pill = document.createElement("span");
        pill.className = "autocomplete-pill";
        pill.style.background = topic.color;
        pill.style.color = readableText(topic.color);
        pill.textContent = topic.label;
        li.appendChild(pill);
      }

      const title = document.createElement("span");
      title.className = "autocomplete-title";
      for (const node of highlight(m.title, query)) title.appendChild(node);
      li.appendChild(title);

      li.addEventListener("mousedown", (ev) => {
        // mousedown beats blur — without this the listbox closes before
        // the click registers.
        ev.preventDefault();
        navigate(li);
      });
      list.appendChild(li);
    });
    list.hidden = false;
    input.setAttribute("aria-expanded", "true");
    setActive(0);
  }

  function setActive(i) {
    if (currentMatches.length === 0) {
      activeIndex = -1;
      return;
    }
    const items = list.querySelectorAll(".autocomplete-item");
    if (activeIndex >= 0 && items[activeIndex]) {
      items[activeIndex].setAttribute("aria-selected", "false");
      items[activeIndex].classList.remove("is-active");
    }
    activeIndex = ((i % currentMatches.length) + currentMatches.length) % currentMatches.length;
    const next = items[activeIndex];
    if (next) {
      next.setAttribute("aria-selected", "true");
      next.classList.add("is-active");
      input.setAttribute("aria-activedescendant", next.id);
      next.scrollIntoView({ block: "nearest" });
    }
  }

  function navigate(li) {
    const url = li?.dataset?.url;
    if (url) window.location.href = url;
  }

  function close() {
    list.hidden = true;
    input.setAttribute("aria-expanded", "false");
    input.removeAttribute("aria-activedescendant");
    activeIndex = -1;
    currentMatches = [];
  }

  input.addEventListener("input", () => {
    const q = input.value;
    render(search(q), q.trim());
  });

  input.addEventListener("keydown", (ev) => {
    if (list.hidden && ev.key !== "ArrowDown") return;
    switch (ev.key) {
      case "ArrowDown": {
        if (list.hidden) {
          // Open dropdown on ArrowDown if there's a query.
          const q = input.value;
          render(search(q), q.trim());
        } else {
          ev.preventDefault();
          setActive(activeIndex + 1);
        }
        break;
      }
      case "ArrowUp":
        ev.preventDefault();
        setActive(activeIndex - 1);
        break;
      case "Enter": {
        if (activeIndex >= 0) {
          ev.preventDefault();
          const items = list.querySelectorAll(".autocomplete-item");
          navigate(items[activeIndex]);
        }
        // No active selection: let the form / Pagefind path proceed.
        break;
      }
      case "Escape":
        ev.preventDefault();
        close();
        break;
      case "Tab":
        close();
        break;
    }
  });

  input.addEventListener("blur", () => {
    // Delay so a click on a suggestion still registers.
    setTimeout(close, 120);
  });

  document.addEventListener("click", (ev) => {
    if (!wrap.contains(ev.target)) close();
  });
}

// Same readable-text helper used by list.js. Duplicated here so the
// autocomplete module is self-contained.
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
