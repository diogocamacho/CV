#!/usr/bin/env Rscript
# render_cv.R
# Usage from CLI:
#   Rscript render_cv.R                        # renders DMC_executive.Rmd
#   Rscript render_cv.R DMC_az_cvrm.Rmd        # renders a specific template
#   Rscript render_cv.R DMC_az_cvrm.Rmd output # renders to ./output/ folder

args <- commandArgs(trailingOnly = TRUE)

input_file  <- if (length(args) >= 1) args[1] else "DMC_executive.Rmd"
output_dir  <- if (length(args) >= 2) args[2] else "."

if (!file.exists(input_file)) {
  cat(sprintf("Error: '%s' not found.\n", input_file))
  cat("Available templates:\n")
  cat(paste(" ", list.files(".", pattern = "\\.Rmd$"), collapse = "\n"), "\n")
  quit(status = 1)
}

cat(sprintf("Rendering: %s\n", input_file))
cat(sprintf("Output to: %s\n", output_dir))

rmarkdown::render(
  input       = input_file,
  output_dir  = output_dir,
  quiet       = FALSE
)

output_pdf <- file.path(output_dir, sub("\\.Rmd$", ".pdf", basename(input_file)))
cat(sprintf("\nDone: %s\n", output_pdf))
