#' Loader
#'
#' @export
load_pkgs <- function(
    pkgs = character(),
    core =  core_pkgs
){
  to_load <- c(pkgs, core)
  lapply(to_load, library, character.only = TRUE)
  invisible(to_load)
}

core_pkgs <- c("data.table", "tibble", "tidyr", "readr","readxl", "purrr", "dplyr", "dtplyr", "dbplyr", "stringr", "forcats", "lubridate", "hms", "broom", "tidyselect", "glue", "rlang", "vctrs")


