# ---------------------------------------------------------------------------
# 00-packages.R — packages the dashboard needs
#
# Install them once with:
#   install.packages(c("shiny", "bslib", "DT", "dplyr", "readr", "stringr",
#                      "yaml", "gh", "gitcreds", "httr2", "xml2", "usethis", "rstudioapi"))
# ---------------------------------------------------------------------------

needed_packages <- c("shiny", "bslib", "DT", "dplyr", "readr", "stringr", "yaml", "gh", "gitcreds", "httr2", "xml2")

check_packages <- function() {
  miss <- needed_packages[!vapply(needed_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop(
    "Please install these R packages first, then run the app again:\n  install.packages(c(",
    paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
  invisible(TRUE)
}
