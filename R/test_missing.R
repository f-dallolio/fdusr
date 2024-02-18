#' Test for NULLs
#'
#' @name test_missing
NULL
#'
#' @rdname test_missing
#' @export
#'
is_missing <- function(x){
  is_na(x) || is_empty(x)
}
#'
#' @rdname test_missing
#' @export
#'
are_missing <- function(x){
  sapply(x,is_missing)
}
#'
#' @rdname test_missing
#' @export
#'
not_missing <- function(x){
  !sapply(x,is_missing)
}
#'
#' @rdname test_missing
#' @export
#'
all_missing <- function(x){
  all(sapply(x,is_missing))
}
#'
#' @rdname test_missing
#' @export
#'
not_all_missing <- function(x){
  !all(sapply(x,is_missing))
}
#'
#' @rdname test_missing
#' @export
#'
if_missing <- function(x, y = x){
  x[are_missing(x)] <- y[are_missing(x)]
  x
}
