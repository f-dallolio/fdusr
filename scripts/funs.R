fdusr::load_pkgs(c("arrow", "fs", "snakecase"))

pf <- purrr::partial
cf <- purrr::compose

fn_fmls(as.list)$all.names <- TRUE

is_numchar <- function(x){
  !is.na(as.numeric(x)) |>
    suppressWarnings()
}

is_numchar(c('10', 'abba'))

eval_call <- function(x, ...){
  if(length(x) == 1){
    return(cf(eval, rlang::call2)(x))
  }
  sapply(x, cf(eval, rlang::call2))
}

make_fields <- function(x, ...){
  stopifnot(is.character(x))
    fn <- function(x) {
      .call <- str2lang(x)
      if(is.call(.call)) { return(eval(.call)) }
      eval_call(x)
    }
    sapply(x, fn)
}

as_schema <- function(x, ...){
  schema(make_fields(x,...))
}


vec_slice_pos <- function(x, i,..., rev = FALSE){
  if(rev) i <- sort(vec_size(x) + 1 - abs(i))
  vec_slice(x, i, ...)
}
vec_slice_head <- function(x, n){
  vec_slice(x, seq_len(n))
}
vec_slice_tail <- function(x, n){
  rev(vec_slice(rev(x), seq_len(n)))
}


str_remove_all <- function(string, pattern){
  if(vec_size(pattern) > 1){
    pattern <- paste(pattern, collapse = "|")
  }
  stringr::str_remove_all(string, pattern)
}

my_partial <- function(x){
  x <- enexpr(x)
  if(!is_call(x)){
    return(eval(x))
  }
  .fn <- sym(call_name(x))
  .args <- call_args(x)
  call2("partial", .fn, splice(.args)) |>
    eval()
}

my_partial2 <- function(obj){
  return(enexpr(obj))
  p_is_fun <- possibly(\(x) is_function(x), otherwise = FALSE)
  fun_flag <- p_is_fun(obj)
  if( fun_flag ){
    return(obj)
  } else (
    obj <- enexpr(obj)
  )
  p_is_call <- possibly(\(x) is_call(x), otherwise = FALSE)
  call_flag <- p_is_call(obj)
  stopifnot(call_flag)

  fn <- call_fn(obj)
  fmls <- fn_fmls(fn)
  new_fmls <- call_args(obj)
  if(length(new_fmls) > 1){
    fmls_id <- match(names(new_fmls), names(fmls))
    fn_fmls(fn)[[fmls_id]] <- new_fmls
  }
  fn
}

my_compose <- function( ... ){
  # x <- enexprs(...)
  # call_id <- x |> map_vec(is_call)
  # x <- modify(x, my_partial)
  compose(splice(x))
}

my_compose(mean)

my_partial(do.call(args = list()))

prt <- my_pmean()prt <- my_pmean()prt <- my_partial
cmp <- my_compose




split_adintel_path <- function(path, ...){

  fn <- function(x){
    as_fs_path(x)|>
      path_ext_remove() |>
      path_split() |>
      list_c() |>
      vec_slice_tail(n = 3) |>
      to_snake_case() |>
      set_names(c("year", "tbl_class", "tbl_type")) |>
      as_tibble_row() |>
      mutate(
        path = x,
        year = if_else(year == "master_files", NA, as.numeric(year)),
        tbl_class = tbl_class |>
          str_replace_all(pattern = "market_breaks", replacement = "impressions"),
        tbl_type = tbl_type |>
          str_remove_all(pattern = c("imp_", "ue_")) |>
          str_replace_all("network_tv", "national_tv") |>
          str_replace_all("spot_tv", "local_tv"),
        tbl = case_when(
          tbl_class == "occurrences" ~ paste("occ", tbl_type,  sep = "__"),
          tbl_class == "impressions" ~ paste("imp", tbl_type,  sep = "__"),
          tbl_class == "universe_estimates" ~ paste("ue", tbl_type,  sep = "__"),
          .default = paste("ref", tbl_type,  sep = "__"),
      ),
             .before = 1) |>
      filter(is.na(as.numeric(tbl_class))) |>
      suppressWarnings()
  }
  map(path, fn) |>
    list_rbind()
}


new_ad_path_vctr <- function(x = character()){
  x <- split_adintel_path(x) |> df_list()
  new_rcrd(fields =  x, class = "adintel_path")
}

format.adintel_path <- function(x, ...){
  paths <- field(x, "path") |> as.character()
  pcomm <- path_common(paths) |> as.character()
  str_replace_all(paths, pcomm, "... ")
}
obj_print_footer.adintel_path <- function(x, ...) {
  paths <- field(x, "path")# |> as.character()
  pcomm <- path_common(paths)# |> as.character()
  cat("... = ", pcomm, "\n", sep = "")
}

dir <- "/mnt/sata_data_1/adintel/ADINTEL_DATA_2010/"
.path <- dir_ls(.dir, recurse = T, type = "file")
