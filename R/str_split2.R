#' String Splitting Variations
#'
#' @name str_split2
NULL
#'
#' @rdname str_split2
#' @export
str_split2 <- function(string, pattern, n, simplify = TRUE){
  out <- stringr::str_split(string, pattern, n, simplify = FALSE)
  if(simplify && length(out) == 1){
    return(unlist(out))
  }
  out
}
#'
#' @rdname str_split2
#' @export
str_split_i2 <- function(string, pattern, i, simplify = TRUE){
  out <- stringr::str_split(string, pattern) |>
    lapply(iextract, i)
  if(simplify && length(out) == 1){
    return(unlist(out))
  }
  out
}
