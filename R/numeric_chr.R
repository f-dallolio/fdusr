#' Character vector as numeric
#'
#' @name numeric_char
NULL
#'
#' @rdname numeric_char
#' @export
is_numeric_chr <- function(x){
  !is.na(as.numeric(x)) |>
    suppressWarnings()
}
#'
#' @rdname numeric_char
#' @export
as_numeric_chr <- function(x){
  if( !all(is_numeric_chr(x)) ){
    return(x)
  }
  as.numeric(x)
}
