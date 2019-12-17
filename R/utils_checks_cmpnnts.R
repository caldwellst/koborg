#' Check select multiple binary or rank position attributes
attr_cmpnnt_err <- function(x) {
  choice_name_error(borg_ch_name(x))
  choice_label_error(borg_ch_label(x))
  q_name_error(borg_q_name(x))
}

choice_label_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("choice_label must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_ch_lbl_error")
  }
}

choice_name_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("choice_name must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_ch_name_error")
  }
}

q_name_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("q_name must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_q_name_error")
  }
}
