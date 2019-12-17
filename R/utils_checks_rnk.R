#' Check if attributes identical for rank questions
identical_rnk_attr <- function(x, y) {
  all(
    identical(borg_attr(x), borg_attr(y)),
    identical(borg_slct_attr(x), borg_slct_attr(y)),
    identical(borg_rnk_attr(x), borg_rnk_attr(y))
  )
}

#' Check additional attributes for borg rank classes
attr_rnk_err <- function(x) {
  q_name_error(borg_q_name(x))
  rnk_pos_sep_error(borg_bin_sep(x))
}

#' Check binary separator attribute for borg select multiple classes
rnk_pos_sep_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) && size == 1))) {
    msg <- paste0("pos_sep must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_pos_sep_error")
  } else if (!vec_in(x, c("/", "."))) {
    msg <- paste0("pos_sep must be one of '/' or '.', not '", x, "'.")
    abort(msg, .subclass = "borg_pos_sep_val_error")
  }
}
