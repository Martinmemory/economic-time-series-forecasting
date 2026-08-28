# Resolve the repository root whether the script is sourced or run with Rscript.

find_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  candidates <- character(0)

  if (length(file_arg) >= 1L) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    script_path <- gsub("~\\+~", " ", script_path)
    candidates <- c(
      candidates,
      normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
    )
  }

  ofile <- NULL
  if (sys.nframe() > 0L) {
    ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  }
  if (!is.null(ofile) && nzchar(ofile)) {
    candidates <- c(
      candidates,
      normalizePath(file.path(dirname(ofile), ".."), mustWork = FALSE)
    )
  }

  candidates <- c(candidates, normalizePath(getwd(), mustWork = FALSE))

  for (cand in unique(candidates)) {
    if (isTRUE(file.exists(file.path(cand, "README.md"))) &&
        isTRUE(dir.exists(file.path(cand, "R")))) {
      return(normalizePath(cand))
    }
  }

  stop("Run this script from the repository root or with Rscript R/<script>.R")
}
