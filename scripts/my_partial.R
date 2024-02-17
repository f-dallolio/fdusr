# my_partial <- function(x){
#   xx <- rlang::enexpr(x)
#   if( class(xx) == "name" ) return(x)
#   xx_name <- rlang::call_name(xx)
#   xx_ns <- rlang::call_ns(xx)
#   xx_call <- rlang::call2(xx_name)
#   if( is.null(xx_ns) ){
#     xx_fn <- base::get(xx_name)
#   } else {
#     xx_fn <- utils::getFromNamespace(xx_name, xx_ns)
#   }
#   new_call <- rlang::call_match(xx, xx_fn, defaults = TRUE)
#   new_args <- rlang::call_args(new_call)
#   new_ns <- base::getNamespaceName(environment(xx_fn))
#   rlang::fn_fmls(xx_fn) <- new_args
#   # xx_fn
#   rlang::new_function(new_args, rlang::call2(xx_name, rlang::splice(rlang::fn_fmls_syms(xx_fn)), .ns = new_ns))
# }

my_partial <- function(x, as_expr = FALSE, fn_out = TRUE ){
  if(as_expr){
    xx <- rlang::enexpr(x)
  } else {
    xx <- x
  }
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
    out <- rlang::new_function(new_args, rlang::call2(xx_name, rlang::splice(rlang::fn_fmls_syms(xx_fn)), .ns = new_ns))
  }
  out
}

create_new_fun <- function(x){
  xx <- rlang::enexpr(x)
  my_partial( xx, as_expr = FALSE, fn_out = FALSE )
}

my_compose <- function(...){
  x <- rlang::enexprs(...)
  x_list <- lapply(x, my_partial)
  do.call(purrr::compose, x_list)
}

usethis::use_r(do_call)


fn_name <- "do_call"
new_fn = my_partial(do.call(args = list()), as_expr = T, fn_out = F)
new_fn2 <- rlang::new_function(rlang::fn_fmls(new_fn), rlang::fn_body(new_fn))

xx <- deparse(new_fn2)
xx[1] <- paste0(fn_name, " <- ", xx[1])
xx
xx |> write("data/test.txt")

use_r2 <- function(fn, path = NULL, overwrite = TRUE, open = rlang::is_interactive()) {
  qfn <- rlang::enquo(fn)
  name <- rlang:::quo_text(qfn)
  new_fn = my_partial(do.call(args = list()), as_expr = T, fn_out = F)
  new_fn2 <- rlang::new_function(rlang::fn_fmls(new_fn), rlang::fn_body(new_fn))
  out_fn <- deparse(new_fn2)
  out_fn[1] <- paste0(name, " <- ", out_fn[1])
  path <- paste0("R/", name,".R")
  dpath <- fs::as_fs_path(path) |> fs::path_dir()
  if( !fs::dir_exists(dpath) ){
    fs::dir_create(dpath)
  }
  if(!overwrite){
    paste0(
      "\n\n ### ",
      format.Date(Sys.time()),
      " ---- \n\n"
    ) |> write(path, append = TRUE)
    write(out_fn, path, append = TRUE)
  } else {
    write(out_fn, path, append = FALSE)
  }
  usethis:::edit_file(path, open = open)
  invisible(TRUE)
}
