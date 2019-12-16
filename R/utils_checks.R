#' Check if attributes identical for select multiple questions
identical_sm_attr <- function(x, y) {
  all(
    identical(borg_attr(x), borg_attr(y)),
    identical(borg_slct_attr(x), borg_slct_attr(y)),
    identical(borg_sm_attr(x), borg_sm_attr(y))
  )
}

#' Check if attributes identical for select one questions
identical_so_attr <- function(x, y) {
  all(
    identical(borg_attr(x), borg_attr(y)),
    identical(borg_slct_attr(x), borg_slct_attr(y))
  )
}

#' Check if attributes identical for calculation borg questions
identical_calc_attr <- function(x, y) {
  identical(borg_attr(x), borg_attr(y))
  identical(borg_calc(x), borg_calc(y))
}

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


#' Check select question choice names and labels
attr_slct_err <- function(x) {
  choice_names <- borg_ch_nms(x)
  ch_nms_error(choice_names)
  ch_lbls_error(borg_ch_lbls(x), choice_names)
}

#' Check choice names attribute for borg classes
ch_nms_error <- function(x) {
  if (!((length(x) == 1 && is.na(x)) | is.character(x))) {
    msg <- paste0("choice_names must be a character vector or NA, not ", class(x), ".")
    abort(msg, .subclass = "borg_ch_nms_error")
  }
}

#' Check choice labels attribute for borg classes
ch_lbls_error <- function(x, names) {
  if (is.character(x)) {
    if (all(are_na(names)) | (length(names) != length(x))) {
      msg <- paste0("choice_names is NA or not the same length as choice_labels")
      abort(msg, .subclass = "borg_ch_lbls_no_nms_error")
    }
  } else if (!(length(x) == 1 && is.na(x))) {
    msg <- paste0("choice_labels must be a character vector or NA, not ", class(choice_labels), ".")
    abort(msg, .subclass = "borg_ch_lbls_error")
  }
}

#' Check additional attributes for borg select multiple classes
attr_sm_err <- function(x) {
  sm_bin_type_error(borg_bin_type(x))
  sm_bin_sep_error(borg_bin_sep(x))
}

#' Check binary type attribute for borg select multiple classes
sm_bin_type_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) & size == 1))) {
    msg <- paste0("binary_type must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_bin_type_error")
  } else if (!vec_in(x, c("logical", "numeric"))) {
    msg <- paste0("binary_type character value must be one of 'logical' or 'numeric', not '", x, "'.")
    abort(msg, .subclass = "borg_bin_type_val_error")
  }
}

#' Check binary separator attribute for borg select multiple classes
sm_bin_sep_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.character(x) && size == 1))) {
    msg <- paste0("binary_type must be a character vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_binary_sep_error")
  } else if (!vec_in(x, c("/", "."))) {
    msg <- paste0("binary_type must be one of '/' or '.', not '", x, "'.")
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
      msg <- paste0("values are not in choice_names.")
      abort(msg, .subclass = "borg_sm_chc_error")
    }
  }
}

#' Check if select one value in choices
so_chc_check <- function(x, choice_names) {
  if (!all(are_na(choice_names))) {
    if (!(x %in% choice_names)) {
      msg <- paste0(x, " not in choice_names.")
      abort(msg, .subclass = "borg_so_chc_error")
    }
  }
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
