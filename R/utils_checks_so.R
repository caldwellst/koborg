#' Check if attributes identical for select one questions
identical_so_attr <- function(x, y) {
  all(
    identical(borg_attr(x), borg_attr(y)),
    identical(borg_slct_attr(x), borg_slct_attr(y))
  )
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
