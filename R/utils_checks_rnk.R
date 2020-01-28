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
  rnk_pos_sep_error(borg_pos_sep(x))
  rnk_max_rank_error(borg_max_rank(x))
}

#' Check position separator attribute for borg rank position classes
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

#' Check max rank attribute for borg position classes
rnk_max_rank_error <- function(x) {
  size <- vec_size(x)
  if (!(is.na(x) | (is.numeric(x) && size == 1))) {
    msg <- paste0("max_rank must be a numeric vector of length 1 or NA, not ", class(x), "of size ", size, ".")
    abort(msg, .subclass = "borg_max_rank_error")
  } else if (x != ceiling(x)) {
    msg <- paste0("max_rank must be an integer value, not ", x, ".")
    abort(msg, .subclass = "borg_max_rank_dbl_error")
  }
}

#' Check that position vector doesn't taken on value higher than max_rank
rnk_pos_rank_check <- function(x, max_rank) {
  if (!all(x <= max_rank) & !is.na(max_rank)) {
    msg <- paste0("rank_position cannot have value higher than max_rank, ", max_rank, ".")
    abort(msg, .subclass = "borg_max_rank_val_error")
  }
}
