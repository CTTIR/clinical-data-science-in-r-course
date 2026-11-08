# Instructor notes · overview

This folder is excluded from the rendered course website (`_quarto.yml` renders only `index.qmd`, `syllabus.qmd`,
`lessons/` and `exercises/`). It is nevertheless **public** in the source repository. Exclusion from rendering is
not access control.

**No examination questions belong here.** The item pool for the individual examination in unit 31, the answer key
and any real case material are kept in a separate, access-controlled environment chosen by the course team. This
folder contains only the blueprint (distribution and intended reasoning per slot, see
[`assessment-blueprint.md`](assessment-blueprint.md)) and notes for teaching.

## Files

| File | Content |
|---|---|
| `w1.md` … `w8.md` | One note per workshop: timetable with time buffers, preparation, typical mistakes, answers to lab questions, paper fallback, open points with responsible role |
| `assessment-blueprint.md` | 20 slots of the examination mapped to area, learning objective and expected reasoning; no items |

## Before the first run

| Task | Responsible role | Status |
|---|---|---|
| Assessment and participation conditions for the run (grading, attendance, resits) | Course lead | *open* |
| Item pool for unit 31 written, reviewed and stored in a private environment | Course lead with examination office | *open* |
| Clinical review of all cases (wrist injury, spinal cord injury, emergency department flow) | Clinical co-lead | *open* |
| Review of W7 legal anchors against the primary texts on the date of the run | Data protection officer, regulatory affairs | *open*; status of this version: 8 November 2026 |
| Guests for W7 (data protection, medical device regulation, information security) | Course lead | *open* |
| Accessibility check of the rendered site (alt texts, contrast, print view) | Course team | *open* |
| Test run of all labs on the participants' typical laptops | Course team | *open*; tested on Linux, R 4.6.1 |
| Decision on the optional SNAIL scale (language version, permission) | Course lead | *open* |

## Rendering and checks

```sh
quarto render
test ! -e _site/instructor && echo "instructor notes not published"
for f in r/w*.R; do Rscript "$f" || echo "FAILED: $f"; done
```

Each lab script stops with an error if a target value is not met. After changing a script in `r/`, render the
corresponding lab explicitly (`quarto render exercises/lab-wN-….qmd`), because `freeze: auto` only watches the
`.qmd` file.

## Course team roles (proposal)

- **Course lead:** overall responsibility, assessment, examination.
- **Clinical co-lead:** cases, clinical plausibility, W3, W5 and W8 discussions.
- **Methods co-lead:** R labs, W4–W6, reproducibility checks in unit 24.
- **Guests in W7:** data protection, medical device regulation, information security.
