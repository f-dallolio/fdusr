#' Test for NULLs
#'
#' @name test_null
NULL
#'
#' @rdname test_null
#' @export
#'
is_null <- is.null
#'
#' @rdname test_null
#' @export
#'
is_null <- function(x){
  stopifnot(length(x) == 1)
  is_null(x)
}
#'
#' @rdname test_null
#' @export
#'
are_null <- function(x){
  sapply(x,is_null)
}
#'
#' @rdname test_null
#' @export
#'
not_null <- function(x){
  !sapply(x,is_null)
}
#'
#' @rdname test_null
#' @export
#'
all_null <- function(x){
  all(sapply(x,is_null))
}
#'
#' @rdname test_null
#' @export
#'
not_all_null <- function(x){
  !all(sapply(x,is_null))
}
#'
#' @rdname test_null
#' @export
#'
`%||%` <- rlang::`%||%`
#'
#' @rdname test_null
#' @export
#'
if_null <- function(x, y = x){
  mapply(`%||%`, x, y)
}
