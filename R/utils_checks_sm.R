#' Check if attributes identical for select multiple questions
identical_sm_attr <- function(x, y) {
  all(
    identical(borg_attr(x), borg_attr(y)),
    identical(borg_slct_attr(x), borg_slct_attr(y)),
    identical(borg_sm_attr(x), borg_sm_attr(y))
  )
}

#' Check additional attributes for borg select multiple classes
attr_sm_err <- function(x) {
  q_name_error(borg_q_name(x))
  sm_bin_sep_error(borg_bin_sep(x))
}

#' Check binary separator attribute for borg select multiple classes
sm_bin_sep_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) && size == 1))) {
    msg <- paste0("binary_sep must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_binary_sep_error")
  } else if (!vec_in(x, c("/", "."))) {
    msg <- paste0("binary_sep must be one of '/' or '.', not '", x, "'.")
    abort(msg, .subclass = "borg_binary_sep_val_error")
  }
}

#' Check if select multiple values in choices
sm_chc_check <- function(x, choice_names, type = "list") {
  if (!all(are_na(choice_names))) {
    if (type == "char") {
      x <- str_split(x, " ")
    }
    check <- map_lgl(x, ~ all(. %in% choice_names))
    if (!all(check)) {
      msg <- paste0("Values are not in choice_names.")
      abort(msg, .subclass = "borg_sm_chc_error")
    }
  }
}
