#' Test for NULLs
#'
#' @name test_empty
NULL
#'
#' @rdname test_empty
#' @export
#'
is_empty <- rlang::is_empty
#'
#' @rdname test_empty
#' @export
#'
are_empty <- function(x){
  sapply(x,is_empty)
}
#'
#' @rdname test_empty
#' @export
#'
not_empty <- function(x){
  !sapply(x,is_empty)
}
#'
#' @rdname test_empty
#' @export
#'
all_empty <- function(x){
  all(sapply(x,is_empty))
}
#'
#' @rdname test_empty
#' @export
#'
not_all_empty <- function(x){
  !all(sapply(x,is_empty))
}
#'
#' @rdname test_empty
#' @export
#'
`%|0|%` <- function(x, y){
  if(is_empty(x)){
    y
  } else {
    x
  }
}
#'
#' @rdname test_empty
#' @export
#'
if_empty <- function(x, y){
  purrr::map2(x, y, ~ .x %|0|% .y)
}
