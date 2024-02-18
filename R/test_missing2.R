
#' Test for NULLs
#'
#' @name test_missing2
NULL
#'
#' @rdname test_missing2
#' @export
#'
is_missing2 <- function(x){
  is_na(x) || is_empty2(x)
}
#'
#' @rdname test_missing2
#' @export
#'
are_missing2 <- function(x){
  sapply(x,is_missing2)
}
#'
#' @rdname test_missing2
#' @export
#'
not_missing2 <- function(x){
  !sapply(x,is_missing2)
}
#'
#' @rdname test_missing2
#' @export
#'
all_missing2 <- function(x){
  all(sapply(x,is_missing2))
}
#'
#' @rdname test_missing2
#' @export
#'
not_all_missing2 <- function(x){
  !all(sapply(x,is_missing2))
}
#'
#' @rdname test_missing2
#' @export
#'
if_missing2 <- function(x, y = x){
  x[are_missing2(x)] <- y[are_missing2(x)]
  x
}
