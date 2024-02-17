#' Title
#'
#' @name strings
NULL
#'
#' @rdname strings
#' @export
str_remove_or <- function(string, ...){
  patterns = rlang::list2(...) |> unlist()
  if(vctrs::vec_size(patterns) > 1){
    pattern <- paste(patterns, collapse = "|")
  }
  stringr::str_remove_all(string, patterns)
}
