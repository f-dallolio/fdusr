#' Variants of `partial` and `compose`
#'
#' @param x ...
#' @param as_expr indicates the need for enexpr0
#' @param fn_out returns the full orignal function body.
#'
#' @name partial_compose
NULL
#'
#' @rdname partial_compose
#' @export
#'
partial2 <- function(x, as_expr = TRUE, fn_out = TRUE ){
  if(as_expr){ xx <- rlang::enexpr(x) } else {  xx <- x }
  if( class(xx) == "name" ) xx <- rlang::call2(as.character((xx)))

  xx_name <- rlang::call_name(xx)
  xx_ns <- rlang::call_ns(xx)
  xx_call <- rlang::call2(xx_name)
  if( is.null(xx_ns) ){
    xx_fn <- base::get(xx_name)
  } else {
    xx_fn <- utils::getFromNamespace(xx_name, xx_ns)
  }

  new_call <- rlang::call_match(xx, xx_fn, defaults = TRUE)
  new_args <- rlang::call_args(new_call)
  new_ns <- base::getNamespaceName(environment(xx_fn))
  rlang::fn_fmls(xx_fn) <- new_args

  if(fn_out){
    out <- xx_fn
  } else {
    new_body <- rlang::call2(
      xx_name,
      rlang::splice(rlang::fn_fmls_syms(xx_fn)),
      .ns = new_ns)
    out <- rlang::new_function(
      new_args,
      new_body)
  }
  out
}
#'
#' @rdname partial_compose
#' @export
#'
compose2 <- function(...){
  x <- rlang::enexprs(...)
  x_list <- lapply(x, partial2)
  do.call(purrr::compose, x_list)
}
#'
#' @rdname partial_compose
#' @export
#'
pfn <- partial2
#'
#' @rdname partial_compose
#' @export
#'
cfn <- compose2
#'
#' @rdname partial_compose
#' @export
#'
make_fn <- function(x){
  xx <- rlang::enexpr(x)
  partial2( xx, as_expr = FALSE, fn_out = FALSE )
}
