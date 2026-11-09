// Year range slider — two stacked native <input type="range"> elements
// driving controller.setRange. Native inputs give us free keyboard
// access (Tab, Arrow keys, Home/End), screen-reader announcements via
// aria-label, and theming via CSS. Cross-handle drag is prevented by
// clamping in the input handler. A 60ms debounce avoids replaying every
// pixel of a fast drag.
//
// No external dependency: a 60-line custom slider in place of noUiSlider
// keeps the bundle thin and avoids vendoring binary files.

const DEBOUNCE_MS = 60;

export function createSlider({ controller, minYear, maxYear }) {
  const host = document.getElementById("year-slider");
  const readout = document.getElementById("year-readout");
  if (!host) return { update() {} };

  // Single-year dataset: nothing useful to slide. Hide the row.
  if (minYear == null || maxYear == null || minYear === maxYear) {
    host.hidden = true;
    if (readout) readout.textContent = String(minYear ?? "");
    return { update() {} };
  }
  host.hidden = false;

  host.innerHTML = `
    <div class="range-track">
      <div class="range-fill" data-fill></div>
      <input type="range" class="range-input range-from"
             min="${minYear}" max="${maxYear}" value="${minYear}" step="1"
             aria-label="From year">
      <input type="range" class="range-input range-to"
             min="${minYear}" max="${maxYear}" value="${maxYear}" step="1"
             aria-label="To year">
    </div>`;

  const fromInput = host.querySelector(".range-from");
  const toInput   = host.querySelector(".range-to");
  const fill      = host.querySelector("[data-fill]");

  function clamp() {
    let f = +fromInput.value;
    let t = +toInput.value;
    if (f > t) {
      // The handle the user is currently driving wins; the other follows.
      if (document.activeElement === fromInput) t = f; else f = t;
      fromInput.value = f;
      toInput.value = t;
    }
  }

  function paint() {
    const f = +fromInput.value;
    const t = +toInput.value;
    if (readout) readout.textContent = f === t ? `${f}` : `${f} – ${t}`;
    const span = maxYear - minYear || 1;
    fill.style.left  = ((f - minYear) / span) * 100 + "%";
    fill.style.right = (100 - ((t - minYear) / span) * 100) + "%";
    // z-boost the lower handle when both sit on the same value so the
    // user can still pick it up.
    fromInput.style.zIndex = f === t ? 3 : 2;
  }

  let timer = null;
  function debouncedCommit() {
    clearTimeout(timer);
    timer = setTimeout(() => {
      const s = controller.state;
      const f = +fromInput.value, t = +toInput.value;
      if (s.dateFrom !== f || s.dateTo !== t) controller.setRange(f, t);
    }, DEBOUNCE_MS);
  }

  for (const inp of [fromInput, toInput]) {
    inp.addEventListener("input", () => { clamp(); paint(); debouncedCommit(); });
    inp.addEventListener("change", () => { clamp(); paint(); debouncedCommit(); });
  }

  function update(state) {
    const f = Math.max(minYear, Math.min(maxYear, state.dateFrom));
    const t = Math.max(minYear, Math.min(maxYear, state.dateTo));
    if (+fromInput.value !== f) fromInput.value = f;
    if (+toInput.value !== t)   toInput.value   = t;
    paint();
  }

  paint();
  return { update };
}
