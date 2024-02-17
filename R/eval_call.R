#' Call and eval
#'
#' @export
#'
eval_call <- function(x, ...){
  if(length(x) == 1){
    return(compose2(eval, rlang::call2)(x))
  }
  sapply(x, compose2(eval, rlang::call2))
}
