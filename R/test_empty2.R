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
are_empty2 <- function(x){
  sapply(x,is_empty2)
}
#'
#' @rdname test_empty2
#' @export
#'
not_empty2 <- function(x){
  !sapply(x,is_empty2)
}
#'
#' @rdname test_empty2
#' @export
#'
all_empty2 <- function(x){
  all(sapply(x,is_empty2))
}
#'
#' @rdname test_empty2
#' @export
#'
not_all_empty2 <- function(x){
  !all(sapply(x,is_empty2))
}
#'
#' @rdname test_empty2
#' @export
#'
`%|x|%` <- function(x, y){
  if(is_empty2(x)){
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
  purrr::map2(x, y, ~ .x %|0|% .y)
}
