#!/usr/bin/env Rscript
source("R/01_data_prep.R")
source("R/02_analysis.R")
source("R/03_visualization.R")
capture.output(sessionInfo(), file = "sessionInfo.txt")
message("All scripts completed.")
