# Run from the repository root: Rscript scripts/export_shinylive.R
if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop('Install the exporter first: install.packages("shinylive")')
}
if (!file.exists("shinylive/app.R")) stop("Run this script from the repository root.")
shinylive::export("shinylive", "_site", template_params = list(title = "AJURI Dashboard · Shinylive"))
file.create("_site/.nojekyll")
