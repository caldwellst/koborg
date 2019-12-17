#' Check if attributes identical for calculation borg questions
identical_calc_attr <- function(x, y) {
  identical(borg_attr(x), borg_attr(y))
  identical(borg_calc(x), borg_calc(y))
}

#' Check calculation attribute
calc_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) && size == 1))) {
    msg <- paste0("calculation must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_binary_sep_error")
  }
}

attr_calc_err <- function(x) {
  calc_error(borg_calc(x))
}
