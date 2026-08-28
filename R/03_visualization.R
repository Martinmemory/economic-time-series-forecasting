#!/usr/bin/env Rscript

# 03_visualization.R
# Copy the README hero figure from the analysis output.
# All charts are generated in R/02_analysis.R; this script only selects
# the main-result image.

source("R/_project_root.R")
root <- find_project_root()
setwd(root)

src <- file.path(root, "figures", "additional", "q1_a_log_electricity.png")
dst <- file.path(root, "figures", "main-result.png")
stopifnot(file.exists(src))
ok <- file.copy(src, dst, overwrite = TRUE)
stopifnot(isTRUE(ok))
message("Hero figure copied to figures/main-result.png")
