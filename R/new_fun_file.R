#' Variant of `usethis::use_r`.
#'
#' @export
#'
new_fun_file <- function(fn, path = NULL, overwrite = TRUE, open = rlang::is_interactive()) {
  qfn0 <- rlang::enquos(fn, .named = TRUE)
  qfn <-  qfn0[[1]]
  name <- names(qfn0)
  new_fn = get(name)
  new_fn2 <- rlang::new_function(rlang::fn_fmls(new_fn), rlang::fn_body(new_fn))
  out_fn <- deparse(new_fn2)
  out_fn[1] <- paste0(name, " <- ", out_fn[1])
  if(is.null(path)){
    path <- paste0("R/", name,".R")
  }
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
