borg_rlvnt <- function(x) {
  attr(x, "relevant")
}

borg_ch_nms <- function(x) {
  attr(x, "choice_names")
}

borg_ch_lbls <- function(x) {
  attr(x, "choice_labels")
}

borg_lbl <- function(x) {
  attr(x, "label")
}

borg_cnstrnt <- function(x) {
  attr(x, "constraint")
}

borg_bin_type <- function(x) {
  attr(x, "binary_type")
}

borg_bin_sep <- function(x) {
  attr(x, "binary_sep")
}

borg_calc <- function(x) {
  attr(x, "calculation")
}

borg_attr <- function(x) {
  c(
    borg_rlvnt(x),
    borg_lbl(x),
    borg_cnstrnt(x)
  )
}

borg_slct_attr <- function(x) {
  c(
    borg_ch_nms(x),
    borg_ch_lbls(x)
  )
}

borg_sm_attr <- function(x) {
  c(
    borg_bin_type(x),
    borg_bin_sep(x)
  )
}
