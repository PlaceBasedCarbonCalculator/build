## Quick orientation for AI coding agents

This repository contains the main analysis pipeline for the Carbon & Place site. It's an R-based, targets-driven data pipeline that produces GeoJSON, PMTiles and bulk JSON/CSV outputs under `outputdata/`.

Keep guidance compact and actionable. When editing, reference the files and folders mentioned below.

### Big picture
- Primary entry: `_targets.R` — defines the full pipeline as many `tar_target(...)` units.
- See the documentation for the R package `targets`: https://books.ropensci.org/targets/ for details on how it works.
- Functions used by targets live in `R/` (run `tar_source()` to load them interactively).
- One-off scripts and visual helpers are in `RScripts/` (for example `RScripts/mermaid.R` which produces `mermaid.txt`).
- Inputs: `parameters.json` points to two external repos: `inputdata` and `inputdatasecure`. Many targets will fail if these are not present.
- The input data is about the United Kingdom and the purpose of the pipeline is to produce spatial data outputs releated to consumption based carbon emissions, boundaries, and related geographic data such as housing, transport, and land use.

Why this matters: most work is about composing and wiring targets, not building new CLI services or APIs. Changes should preserve reproducibility of targets and the shape of target outputs in `outputdata/`.

### Developer workflows & important commands
- Local interactive work (recommended): open `build.Rproj` in RStudio and use `library(targets)`.
- Run entire pipeline: `tar_make()` (may take days and requires large RAM/CPU resources).
- Run a single target and its prerequisites: `tar_make(TARGET_NAME)`.
- Visualise pipeline: `tar_visnetwork(T)` or run `RScripts/mermaid.R` and paste `mermaid.txt` into mermaid.live for a readable flowchart.
- Load a completed target into the environment for inspection: `tar_load(TARGET_NAME)`.
- To allow the pipeline to continue past missing secure data, set `tar_option_set(error = "continue")` in `_targets.R`.

Note on resources: the full pipeline expects >36 cores and >256GB RAM and produces >70GB of outputs. Use small or mock inputs for faster iteration.

### Project-specific conventions and patterns
- Targets are named in snake_case and frequently combine outputs from multiple earlier targets. Example: `bounds_lsoa_GB_generalised` depends on `bounds_lsoa21_generalised` and `bounds_dz11`.
- Reusable functions live in `R/` and are referenced directly by targets — prefer editing functions here rather than embedding long code blocks inside `_targets.R`.
- `RScripts/` contains auxiliary scripts that are not part of the pipeline; use them for testing/visual checks only.
- Outputs live in `outputdata/` with predictable subfolders: `pmtiles`, GeoJSON files, zipped JSON and bulk data folders.

### Integration points and external dependencies
- External repos: `inputdata` (open inputs) and `inputdatasecure` (sensitive inputs). `parameters.json` must be updated to point to local clones of those repos.
- System tools: `tippecanoe` required to produce PMTiles (on Linux/Mac or via WSL on Windows).
- R packages: the README lists required packages and R >=4.3.3. Functions rely on spatial packages such as `sf`, `terra`, `rmapshaper`, `osmextract`, `nngeo` and parallelism packages `furrr`/`future`.

### Editing guidance for AI agents
- When changing data-shaping code, update the corresponding target name and ensure downstream targets still match expected inputs.
- Prefer small, incremental edits to functions in `R/` and run `tar_make(TARGET_NAME)` for the smallest affected target — do not run full pipeline unless necessary.
- If adding new heavy targets, add documentation in `_targets.R` adjacent to the new target and update `RScripts/mermaid.R` output by running it.
- Preserve `parameters.json` keys and shape; it's the main configuration for input repo paths. If you must change config keys, update README and any code that reads them.

### Files to consult for context
- `_targets.R` — pipeline entry and dependency graph
- `R/` — all reusable functions called by targets
- `RScripts/mermaid.R` and `mermaid.txt` — pipeline visualization helper
- `parameters.json` — local paths to `inputdata` and `inputdatasecure`
- `README.md` — environment, packages and high-level run notes

If anything here is unclear or you need deeper behaviour from specific targets, tell me which target or function to inspect and I'll extract examples and call patterns.
