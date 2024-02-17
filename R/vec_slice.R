#' Title
#'
#' @name slicing
NULL
#'
#' @rdname slicing
#' @export
#'
vec_slice_pos <- function(x, i,..., rev = FALSE){
  if(rev) i <- sort(vctrs::vec_size(x) + 1 - abs(i))
  vctrs::vec_slice(x, i, ...)
}
#'
#' @rdname slicing
#' @export
#'
vec_slice_head <- function(x, n){
  vctrs::vec_slice(x, seq_len(n))
}
#'
#' @rdname slicing
#' @export
#'
vec_slice_tail <- function(x, n){
  rev(vctrs::vec_slice(rev(x), seq_len(n)))
}
