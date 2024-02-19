#' Split Path and get position
#'
#' @export
#'
path_split_i <- function(x, i){
  x_split <- fs::path_split(x)
  purrr::map(x_split, ~ .x[i])
}
