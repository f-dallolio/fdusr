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
as_numeric_chr <- function(x, if_false = c("string", "none", "error")){
  if_false <- match.arg(arg = if_false)
  if(if_false == "error"){
    stopifnot("x cannot be coerced to numeric" = is_numeric_chr(x))
  } else if (if_false == 'string') {
    if( is_numeric_chr(x) ){
      return(as.numeric(x))
    } else {
      return(x)
    }
  }
  NULL
}
