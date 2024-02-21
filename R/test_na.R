#' Test for NULLs
#'
#' @name test_na
NULL
#'
#' @rdname test_na
#' @export
#'
is_na <- rlang::is_na
#'
#' @rdname test_na
#' @export
#'
is_na <- function(x){
  stopifnot(length(x) == 1)
  is_na(x)
}
#'
#' @rdname test_na
#' @export
#'
are_na <- function(x){
  sapply(x,is_na)
}
#'
#' @rdname test_na
#' @export
#'
not_na <- function(x){
  !sapply(x,is_na)
}
#'
#' @rdname test_na
#' @export
#'
all_na <- function(x){
  all(sapply(x,is_na))
}
#'
#' @rdname test_na
#' @export
#'
not_all_na <- function(x){
  !all(sapply(x,is_na))
}
#'
#' @rdname test_na
#' @export
#'
`%na%` <- function(x, y){
  if(is_na(x)){
    y
  } else {
    x
  }
}
#'
#' @rdname test_na
#' @export
#'
if_na <- function(x, y = x){
  mapply(`%na%`, x, y)
}
