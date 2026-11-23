# AJURI Dashboard · Shinylive

Internal research dashboard for AJURI staff. R runs in the user's browser
through Shinylive/WebAssembly; hosting serves static files without an R server.

## Data loading

The opening screen offers CSV upload and a disabled API option marked
**Em breve · Coming soon**. No API credentials are collected. CSVs are processed
in browser memory and must be selected again after a reload. **Trocar dados**
starts a fresh session. Select the reference date before loading.

Upload the REDCap export directly: all instruments and events, **CSV (labels)** or
**CSV (raw data)**, with variable names (not field labels) in the header. The
importer:

- detects the delimiter (`,`, `;`, tab or `|`) and UTF-8/Latin-1 encoding;
- collapses longitudinal and repeating-instrument rows (`redcap_event_name`,
  `redcap_repeat_*`) to one row per `record_id`, keeping the latest non-blank
  value of each field (so the latest *Avaliação Clínica Atualizada* wins);
- merges checkbox columns (`field___code`, `Marcado`/`Checked`/`1`) into one
  comma-separated label column, using the bundled data dictionary;
- maps raw codes to dictionary labels, and form-status codes to
  `Incomplete`/`Unverified`/`Complete`;
- accepts dates as `YYYY-MM-DD` or `DD/MM/YYYY` (`MM/DD/YYYY` when a value proves it).

**Trial arm is optional.** REDCap does not store it. Add a `trial_arm` column
(`Intervencao`/`Controle`, or blinded codes such as `A`/`B`) to show arm
breakdowns; without it the arm KPI and arm stratification are hidden.
**Clusters:** each `polo_base` is a cluster unless a `cluster_id` column is given.

A one-row-per-participant analysis CSV (the previous format) still works. Required
columns appear in `shinylive/required-columns.txt`, plus `tansferencia_1` (or alias
`transferencia_1`); `sequela_alta_2`/`obito_alta_2` (retired in the 2026-11
dictionary) are optional. The header-only template is available on the opening
screen. Interpret clinical summaries alongside data completeness.

**Never commit data.** Real exports live in Box, outside this repository.
`.gitignore` blocks `/data/` and data file types; only the dictionary and the
empty template under `shinylive/` are tracked.

Optional targets default to 190 total participants, 96 per arm, 168 without
inflation, and 10 per cluster. The CSV size limit is 50 MB; practical limits depend
on device memory. The original enrollment, exploration, summary CSV download,
projection, follow-up and adverse-event views are retained.

## Run and test locally

Install development dependencies in R:

```r
install.packages(c(
  "shinylive", "shiny", "bslib", "dplyr", "tidyr", "purrr", "tibble",
  "ggplot2", "stringr", "forcats", "lubridate", "gt", "gtsummary",
  "labelled", "plotly", "scales", "bsicons", "readr", "testthat"
))
```

From this repository's root:

```sh
Rscript tests/test_shinylive.R
Rscript tests/test_simulated_data.R
Rscript scripts/export_shinylive.R
python3 -m http.server 4388 --bind 127.0.0.1 --directory _site
```

Open `http://127.0.0.1:4388`. This serves static files only. Opening through
`file://` is unsupported. The first load requires time and network access to
initialize R and its packages. For native R development, use
`shiny::runApp("shinylive")`. Tests generate synthetic data in memory and do not
require data from the original Shiny repository.

## Simulated upload data

`data/Simulated_Data.csv` contains 190 synthetic participants generated from the
latest dictionary (currently `AJURI_DataDictionary_2026-11-10.csv`). Use it with the
CSV upload option. It contains all 362 dictionary fields, in dictionary order,
plus the trial/cluster metadata fields needed by the dashboard. It uses labelled
values, comma-separated checkbox labels, and ISO dates/timestamps for the
analysis CSV, regardless of REDCap's date-entry display format.

Regenerate from the repository root:

```sh
Rscript scripts/generate_data.R
Rscript tests/test_simulated_data.R
```

The generator discovers the latest dated dictionary in `data/` or
`shinylive/data/`, uses a fixed random seed, and defaults to reference date
2026-08-31. Override `AJURI_REFERENCE_DATE` to generate a different snapshot and
`AJURI_SIMULATED_OUTPUT` to choose another output file. Validation checks categories,
numeric bounds, branch conditions, date types, uncollected follow-up fields,
reproducibility, and Shinylive import compatibility. The current dictionary's
`outro_reclas_doenca_conco` checkbox has no defined choices; it remains blank.

The synthetic CSV is kept outside `shinylive/`, is git-ignored, and is excluded
from the exported site; tests regenerate it in a temporary directory. All names and records in it are generated for testing.

## GitHub Pages

Repository: [AJURI-Dasbhoard-Shiny-Live](https://github.com/souzajvp/AJURI-Dasbhoard-Shiny-Live).
The source branch is `codex/shinylive-dashboard`. Pushes to this branch or `main`
and pull requests run tests and export the site without publishing.

When ready to publish:

1. Push the source branch to GitHub.
2. Set **Settings → Pages → Build and deployment → Source → GitHub Actions**.
3. Run **AJURI Shinylive Pages** manually on the desired branch and enable
   **Publish this build to GitHub Pages**.

The exporter bundles only `shinylive/` into ignored `_site/`. Every file inside
`shinylive/` is public in the exported site. Keep participant data and credentials
outside it. Only dictionary metadata (the app reads the newest dated
`shinylive/data/AJURI_DataDictionary_*.csv`), the empty CSV template, code and
logo are included. The original server-based version remains in the separate
`ajuri-dashboard` repository.

Uses the official [Shinylive R exporter](https://posit-dev.github.io/r-shinylive/).
