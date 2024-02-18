#' Test for NULLs
#'
#' @name test_empty2
NULL
#'
#' @rdname test_empty2
#' @export
#'
is_empty2 <- function(x){
  is_empty(x) || x == ""
}
#'
#' @rdname test_empty2
#' @export
#'
is_scalar_empty2 <- function(x){
  stopifnot(length(x) == 1)
  is_empty2(x)
}
#'
#' @rdname test_empty2
#' @export
#'
are_empty2 <- function(x){
  sapply(x,is_scalar_empty2)
}
#'
#' @rdname test_empty2
#' @export
#'
not_empty2 <- function(x){
  !sapply(x,is_scalar_empty2)
}
#'
#' @rdname test_empty2
#' @export
#'
all_empty2 <- function(x){
  all(sapply(x,is_scalar_empty2))
}
#'
#' @rdname test_empty2
#' @export
#'
not_all_empty2 <- function(x){
  !all(sapply(x,is_scalar_empty2))
}
#'
#' @rdname test_empty2
#' @export
#'
`%|0|%` <- function(x, y){
  if(is_scalar_empty2(x)){
    y
  } else {
    x
  }
}
#'
#' @rdname test_empty2
#' @export
#'
if_empty2 <- function(x, y = x){
  mapply(`%|0|%`, x, y)
}
