#!/usr/bin/env Rscript

# 01_data_prep.R
# Validate the three course-provided series and write a coverage summary.
# No values are altered.

source("R/_project_root.R")
root <- find_project_root()
setwd(root)

elec <- read.csv(file.path(root, "data", "elec.csv"), stringsAsFactors = FALSE)
wti <- read.csv(file.path(root, "data", "wti_oil_price.csv"), stringsAsFactors = FALSE)
disney <- read.csv(file.path(root, "data", "disney_stock_price.csv"), stringsAsFactors = FALSE)

stopifnot(identical(names(elec), c("date", "elec")))
stopifnot(identical(names(wti)[1:2], c("date", "oil_price")) || "oil_price" %in% names(wti))
stopifnot("disney_stock" %in% names(disney))
stopifnot(nrow(elec) == 468L)
stopifnot(nrow(wti) == 466L)
stopifnot(nrow(disney) == 4498L)
stopifnot(!anyNA(elec$elec), !anyNA(wti$oil_price), !anyNA(disney$disney_stock))

coverage <- data.frame(
  series = c("U.S. electricity retail sales", "WTI crude oil price", "Disney closing price"),
  file = c("elec.csv", "wti_oil_price.csv", "disney_stock_price.csv"),
  observations = c(nrow(elec), nrow(wti), nrow(disney)),
  first_date = c(as.character(elec$date[1]), as.character(wti$date[1]), as.character(disney$date[1])),
  last_date = c(
    as.character(elec$date[nrow(elec)]),
    as.character(wti$date[nrow(wti)]),
    as.character(disney$date[nrow(disney)])
  ),
  stringsAsFactors = FALSE
)
dir.create(file.path(root, "outputs"), showWarnings = FALSE)
write.csv(coverage, file.path(root, "outputs", "data_coverage.csv"), row.names = FALSE)
print(coverage)
message("Data validation completed.")
