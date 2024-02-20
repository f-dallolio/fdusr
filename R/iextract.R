#' Extract by index
#'
#' @export
#'
iextract <- function(x, i){
  n <- length(x)
  if( any(sign(i) == -1) ){
    i = sort(n + 1 -abs(i))
  }
  x[i]
}
