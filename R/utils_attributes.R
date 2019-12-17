borg_rlvnt <- function(x) {
  attr(x, "relevant")
}

borg_ch_nms <- function(x) {
  attr(x, "choice_names")
}

borg_ch_lbls <- function(x) {
  attr(x, "choice_labels")
}

borg_ch_name <- function(x) {
  attr(x, "choice_name")
}

borg_ch_label <- function(x) {
  attr(x, "choice_label")
}

borg_q_name <- function(x) {
  attr(x, "q_name")
}

borg_lbl <- function(x) {
  attr(x, "label")
}

borg_cnstrnt <- function(x) {
  attr(x, "constraint")
}

borg_bin_sep <- function(x) {
  attr(x, "binary_sep")
}

borg_pos_sep <- function(x) {
  attr(x, "position_sep")
}

borg_calc <- function(x) {
  attr(x, "calculation")
}

borg_max_rank <- function(x) {
  attr(x, "max_rank")
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
    borg_bin_sep(x)
  )
}

borg_rnk_attr <- function(x) {
  c(
    borg_pos_sep(x),
    borg_max_rank(x)
  )
}


borg_sm_bin_attr <- function(x) {
  c(
    borg_ch_name(x),
    borg_ch_label(x),
    borg_q_name
  )
}
