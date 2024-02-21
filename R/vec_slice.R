#' Title
#'
#' @name slicing
NULL
#'
#' @rdname slicing
#' @export
#'
# vec_slice_pos <- function(x, i,..., rev = FALSE){
  # if(rev) i <- sort(vctrs::vec_size(x) + 1 - abs(i))
#   vctrs::vec_slice(x, i, ...)
# }
vec_slice_pos <- function(x, i, ..., .sort = TRUE){
  n <- vctrs::vec_size(x)
  p1 <- sign(i) > 0
  p0 <- !p1
  i[p0] <- n + 1 + i[p0]
  if(.sort){
    i <- sort(i)
  }
  x[i]
}
#'
#' @rdname slicing
#' @export
#'
vslice <-  vec_slice_pos
#'
#' @rdname slicing
#' @export
#
lslice <- function(x, i) purrr::map(x, ~ vslice(.x, i))
#'
#' @rdname slicing
#' @export
#'
vec_slice_head <- function(x, n){
  x[seq_len(n)]
}
#'
#' @rdname slicing
#' @export
#'
vec_slice_tail <- function(x, n){
  i <- sort(vec_size(x) + 1 - seq_len(n))
  x[i]
}

