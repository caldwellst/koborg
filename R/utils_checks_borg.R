#' Check if attributes identical for generic borg questions
identical_borg_attr <- function(x, y) {
  identical(borg_attr(x), borg_attr(y))
}

#' Check attribute errors for attributes appearing in all borg classes
attr_err <- function(x) {
  rlvnt_error(borg_rlvnt(x))
  lbl_error(borg_lbl(x))
  cnstrnt_error(borg_cnstrnt(x))
}

#' Check relevant attribute for borg classes
rlvnt_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("relevant must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_rlvnt_error")
  }
}

#' Check label attribute for borg classes
lbl_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("label must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_lbl_error")
  }
}

#' Check constraint attribute for borg classes
cnstrnt_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("constraint must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_cnstrnt_error")
  }
}
