# Run from the repository root with: Rscript shinylive_app/build.R
# Only application source and data are exported; build files stay local.
local({
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script_arg) != 1L) {
    stop("Run this script with Rscript shinylive_app/build.R")
  }
  app_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop('Install the exporter first: install.packages("shinylive")')
  }

  # Staging also prevents a rebuild from including its own generated site/ folder.
  staging_dir <- tempfile("drinkr-shinylive-")
  dir.create(staging_dir)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
  app_files <- c("server.R", "ui.R", "helper_functions.R", "drink_info.csv")
  copied <- file.copy(file.path(app_dir, app_files), staging_dir)
  if (!all(copied)) {
    stop("Could not copy all application files into the export staging directory.")
  }

  shinylive::export(
    staging_dir,
    file.path(app_dir, "site"),
    wasm_packages = TRUE,
    assets_version = "0.10.12",
    quiet = FALSE,
    template_params = list(title = "drinkR: Blood Alcohol Concentration")
  )
  # Publish the exported files directly on GitHub Pages, without Jekyll processing.
  writeLines(character(), file.path(app_dir, "site", ".nojekyll"))
})
