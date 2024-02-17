#' New on `do.call`.
#'
#' @export
#'
do_call <- function (what, args = list(), quote = FALSE, envir = parent.frame())
{
  base::do.call(what = what, args = args, quote = quote, envir = envir)
}
