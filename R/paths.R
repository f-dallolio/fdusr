path_pos_i <- function(x, i){
  x <- sapply(x, fs::as_fs_path)
  paths <- x |> fs::path_split()
  if(i < 0){
    out <- purrr::map(paths, ~ fdusr::vec_slice_pos(.x, i, rev = TRUE))
  } else {
    out <- purrr::map(paths, ~ fdusr::vec_slice_pos(.x, i))
  }
  out[are_empty(out)] <- ""
  unlist(out) |>
    rlang::set_names(x)
}
