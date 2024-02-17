#' NumChar
#'
#' @name numchar
NULL
#'
#' @rdname numchar
#' @export
is_numchar <- function(x){
  !is.na(as.numeric(x)) |>
    suppressWarnings()
}
#' @rdname numchar
#' @export
as_numchar <- function(x){
  stopifnot(is.character(x))
  if( !is.na(as.numeric(x)) ){
    return(as_numeric(x))
  }
  x |> suppressWarnings()
}
