#' Check input class
#'
#' @export
check_class <- function(x){ # = list("call", "function", "formula", "character")){
  x <- rlang::enexpr(x)
  out <- class(x)
  if( out == "call" ){
    return(out)
  }
  class(eval(x))
}