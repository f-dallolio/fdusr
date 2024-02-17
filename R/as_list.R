#' Vaiations of `as.list` and `do.call`.
#'
#' @name lists
NULL
#'
#' @rdname lists
#' @export
#'
as_list <- function (x, all.names = TRUE)
{
    base::as.list(x = x, all.names = all.names)
}
#'
#' @rdname lists
#' @export
#'
as_list2 <- function (x, ...)
{
  nms <- names(x)
  rlang::set_names(rlang::list2(rlang::splice(x), ...), nms)
}

