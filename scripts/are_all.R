
are_null <- function(x){
  sapply(x,is.null)
}
not_null <- function(x){
  !sapply(x,is.null)
}
all_null <- function(x){
  all(sapply(x,is.null))
}
not_all_null <- function(x){
  !all(sapply(x,is.null))
}


are_na <- function(x){
  sapply(x,is.na)
}
not_na <- function(x){
  !sapply(x,is.na)
}
all_na <- function(x){
  all(sapply(x,is.na))
}
not_all_na <- function(x){
  !all(sapply(x,is.na))
}


is_empty <- function(x) length(x) == 0 || x == ""

are_empty <- function(x){
  sapply(x, is_empty)
}
not_empty <- function(x){
  !are_empty(x)
}
which_empty <- function(x){
  which(x, are_empty)
}
which_not_empty <- function(x){
  which(x, not_empty)
}
all_empty <- function(x){
  all(sapply(x,is_empty))
}
not_all_empty <- function(x){
  !all_empty(x)
}

are_na <- function(x){
  sapply(x,is.na)
}
not_na <- function(x){
  !sapply(x,is.na)
}
all_na <- function(x){
  all(sapply(x,is.na))
}
not_all_na <- function(x){
  !all(sapply(x,is.na))
}

are_scalar <- function(x){
  sapply(x, length) ==  1L
}
not_scalar <- function(x){
  sapply(x, length) >  1L
}
all_scalar <- function(x){
  all(sapply(x, length) ==  1L)
}
not_all_scalar <- function(x){
  !all(sapply(x, length) ==  1L)
}


is_numeric_chr <- function(x){
  stopifnot(rlang::is_scalar_atomic(x) && is.character(x))
  suppressWarnings(
    !is.na(as.numeric(x))
  )
}

are_numeric_chr <- function(x){
  sapply(x, is_numeric_chr)
}
not_numeric_chr <- function(x){
  !sapply(x, is_numeric_chr)
}
all_numeric_chr <- function(x){
  all(sapply(x, is_numeric_chr))
}
not_all_numeric_chr <- function(x){
  !all_numeric_chr(x)
}
