# Clinical Data Science in R · Course

A course in four parts on clinical AI and translational trauma research: eight workshops (W1–W8) of four teaching
units of 45 minutes each, 32 units in total. Participants learn to read a finding, carry out the appropriate check in
R and defend a limited recommendation to patients, researchers or hospital management.

Course website: <https://cttir.github.io/clinical-data-science-in-r-course/>

Companion book: [Clinical Data Science in R](https://cttir.github.io/clinical-data-science-in-r/). Each
workshop requires the core readings of two book chapters (B01–B28); course and book share cases, identifiers and
figure style.

## For whom

Medical students, clinical researchers and doctoral researchers who want to judge data from clinical care and run
their own analyses in R. Basic clinical concepts are enough; R is introduced in the first workshop.

## Structure

| Part | Workshops | Teaching units |
|---|---|---|
| I · Clinical questions and data | W1 From clinical case to testable question · W2 Language models and scientific evidence | 01–08 |
| II · Testing and comparing models | W3 Diagnostics, prediction and external evidence · W4 What imaging, gait and tissue models measure | 09–16 |
| III · Justifying complex models | W5 Causal questions, trajectories and phenotypes · W6 Pharmacometrics, forecasting and simulation | 17–24 |
| IV · Translating models into practice | W7 Regulation, data protection and a security incident · W8 Model colloquium, decision and review | 25–32 |

The repository keeps the layout of [coursegen](https://github.com/r-heller/coursegen):

| Path | Content |
|---|---|
| `index.qmd`, `syllabus.qmd` | Home page and syllabus |
| `lessons/` | One page per workshop (W1–W8) |
| `exercises/` | R labs (`lab-w1` … `lab-w6`), case sheets (`casebook-w2`, `casebook-w7`), project pages and the formative self-test |
| `r/` | `style.R` (shared with the companion book), `course.R` (lab helpers) and one stand-alone script per lab |
| `data/` | Synthetic teaching tables written by the labs when the site is rendered |
| `instructor/` | Instructor notes per workshop; **not rendered**, but visible in this public repository |
| `editorial/course-book.csv` | Mapping of teaching unit → workshop → book chapter (required/supplementary) → lab or file |

Examination questions are not part of this repository. The public course contains newly developed practice cases
only (`exercises/self-test.qmd`).

## Layout and learning network

The site shares the light/dark themes, typography, cards and interactive overview components of [CTTIR courses](https://cttir.github.io/courses/). The overview connects all eight workshops and fourteen exercises through shared topics, with linked filters, full-text search, a tag heatmap and mobile navigation. `scripts/build_graph.py` derives the activity titles and targets from the published sources; `scripts/check_site.py` verifies the built links, network and search artifacts.

## Building locally

Requirements: [Quarto](https://quarto.org/) and R with the packages ggplot2, scales, glmnet, dagitty, lme4,
mclust, cluster, deSolve, simmer, class and knitr/rmarkdown. Tested with Quarto 1.9.38 and R 4.6.1 (ggplot2 4.0.3,
glmnet 5.0, lme4 2.0.1, dagitty 0.3.4, mclust 6.1.3, cluster 2.1.8.3, deSolve 1.42, simmer 4.4.7, class 7.3.24,
knitr 1.51) on 8 November 2026.

```sh
npm ci                # pinned search and browser tooling
quarto render          # website into _site/
quarto preview
```

R results are stored with `freeze: auto` in `_freeze/`. A full `quarto render` re-runs a lab only when its `.qmd`
file changes. The labs read their code from the scripts in `r/` (via `knitr::read_chunk()`), so after changing a
script, render the corresponding lab explicitly, for example `quarto render exercises/lab-w3-diagnostic-accuracy.qmd`,
which always re-executes the code.

Every lab script also runs on its own in a fresh R session and stops with an error if a target value is not met:

```sh
Rscript r/w1-case-table.R
Rscript r/w2-next-word.R
Rscript r/w3-diagnostic-accuracy.R
Rscript r/w4-spectra-animal-split.R
Rscript r/w5-causality-trajectories.R
Rscript r/w6-forecast-pk-simulation.R
```

Stand-alone runs write their figures and tables to `./output` (or to the folder named in the environment variable
`CDSR_OUTPUT`). Before distributing the site, check that `_site/` contains no `instructor/` folder.

## Data

All examples use synthetic data generated with a fixed seed, or explicitly cited published figures. The repository
contains no patient data, no unpublished project results and no examination questions. Animals are fictitious and
pharmacokinetic parameters virtual; nothing in the course is a dosing recommendation.

## Use of LLM tools

Where applicable, LLM tools were used for narrowly defined, non-authorial tasks: copyediting, prose refinement,
Markdown/LaTeX formatting, boilerplate scaffolding (such as CI configurations and build scripts), and code
refactoring. Tools included [Chat AI](https://kisski.gwdg.de/leistungen/2-02-llm-service/), the KISSKI (GWDG)
service, and self-hosted models served through a local vLLM architecture on an Ubuntu Linux workstation with Intel
processors and NVIDIA GPUs. Self-hosted use also includes models that were further trained (fine-tuned) or
weight-edited in-house for specific tasks. All self-hosted models ran locally, with no data sent to third parties.

All scientific claims, methodological choices, analyses, interpretations and conclusions are the author's own. All
content and references were revised and verified by the author; every reference was checked against its DOI,
ISBN or official source.

## Licence

Texts, tables and figures: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) (see
[`LICENSE-CONTENT.md`](LICENSE-CONTENT.md)). Code (R scripts, chunks, configuration): MIT licence (see
[`LICENSE`](LICENSE)). Cited legal texts, guidelines and publications remain subject to the terms of their
publishers.

## Citation

> Heller, R. (2026). *Clinical Data Science in R · Course.* <https://cttir.github.io/clinical-data-science-in-r-course/>
