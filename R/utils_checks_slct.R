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
