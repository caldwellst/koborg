# CONSTRUCTORS FOR SELECT MULTIPLE CLASS

#' Low level rank position constructor
new_rank_position <- function(x = integer(),
                              relevant = NA,
                              choice_name = NA,
                              choice_label = NA,
                              q_name = NA,
                              label = NA,
                              constraint = NA,
                              max_rank = NA,
                              position_sep = "/") {
  vec_assert(x, integer())
  rnk_pos_rank_check(x, max_rank)

  new_vctr(x,
           relevant = relevant,
           choice_name = choice_name,
           choice_label = choice_label,
           q_name = q_name,
           label = label,
           constraint = constraint,
           position_sep = position_sep,
           max_rank = max_rank,
           class = "borg_rank_position")
}

#' Rank position constructor
#'
#' `rank_position()` constructs a rank position vector, which is an integer vector where each row
#' indicates the rank of that particular choice option. This output is not provided by KoBo outputs
#' by default.
#'
#' @importFrom stringr str_split
#'
#' @export
rank_position <- function(x = integer(),
                          relevant = NA,
                          choice_name = NA,
                          choice_label = NA,
                          q_name = NA,
                          label = NA,
                          constraint = NA,
                          max_rank = NA,
                          position_sep = "/") {

  x <- vec_cast(x, integer())
  validate_rank_pos(
    new_rank_position(
      x,
      relevant,
      choice_name,
      choice_label,
      q_name,
      label,
      constraint,
      max_rank,
      position_sep
    )
  )
}

#' Validation function for both select multiple classes
validate_rank_pos <- function(x) {
  attr_err(x)
  attr_cmpnnt_err(x)
  attr_rnk_err(x)
  x
}

# FORMATTING FOR PRINTING

#' @importFrom stringr str_count str_c
format.borg_rank_position <- function(x, ...) vec_data(x)

#' Full abbreviation in tibbles
vec_ptype_full.borg_rank_position <- function(x, ...) {
  "borg_rank_position"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_rank_position <- function(x, ...) {
  "rnk_bnry"
}

# COERCIONS

#' Boiler plate for coercion for coercion of rank positions
#'
#' @method vec_ptype2 borg_rank_position
#' @export
#' @export vec_ptype2.borg_rank_position
vec_ptype2.borg_rank_position <- function(x, y, ...) UseMethod("vec_ptype2.borg_rank_position", y)

#' @method vec_ptype2.borg_rank_position default
#' @export
vec_ptype2.borg_rank_position.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_rank_position borg_rank_position
#' @export
vec_ptype2.borg_rank_position.borg_rank_position <- function(x, y, ...) {
  if (identical_sm_attr(x, y)) {
    new_rank_position()
  } else {
    integer()
  }
}

# COERCION TO DOUBLE
#' @method vec_ptype2.borg_rank_position double
#' @export
vec_ptype2.borg_rank_position.double <- function(x, y, ...) {
  new_rank_position(relevant = borg_rlvnt(x),
                    choice_name = borg_ch_name(x),
                    choice_label = borg_ch_label(x),
                    q_name = borg_q_name(x),
                    label = borg_lbl(x),
                    constraint = borg_cnstrnt(x),
                    max_rank = borg_max_rank(x),
                    position_sep = borg_pos_sep(x))
}

#' @method vec_ptype2.double borg_rank_position
#' @export
vec_ptype2.double.borg_rank_position <- function(x, y, ...) {
  new_rank_position(relevant = borg_rlvnt(y),
                    choice_name = borg_ch_name(y),
                    choice_label = borg_ch_label(y),
                    q_name = borg_q_name(y),
                    label = borg_lbl(y),
                    constraint = borg_cnstrnt(y),
                    max_rank = borg_max_rank(y),
                    position_sep = borg_pos_sep(y))
}


# COERCION TO INTEGER
#' @method vec_ptype2.borg_rank_position integer
#' @export
vec_ptype2.borg_rank_position.integer <- function(x, y, ...) {
  new_rank_position(relevant = borg_rlvnt(x),
                    choice_name = borg_ch_name(x),
                    choice_label = borg_ch_label(x),
                    q_name = borg_q_name(x),
                    label = borg_lbl(x),
                    constraint = borg_cnstrnt(x),
                    max_rank = borg_max_rank(x),
                    position_sep = borg_pos_sep(x))
}

#' @method vec_ptype2.integer borg_rank_position
#' @export
vec_ptype2.integer.borg_rank_position <- function(x, y, ...) {
  new_rank_position(relevant = borg_rlvnt(y),
                    choice_name = borg_ch_name(y),
                    choice_label = borg_ch_label(y),
                    q_name = borg_q_name(y),
                    label = borg_lbl(y),
                    constraint = borg_cnstrnt(y),
                    max_rank = borg_max_rank(y),
                    position_sep = borg_pos_sep(y))
}


# CASTING

#' Boiler plate for casting rank position
#'
#' @method vec_cast borg_rank_position
#' @export
#' @export vec_cast.borg_rank_position
vec_cast.borg_rank_position <- function(x, to, ...) UseMethod("vec_cast.borg_rank_position")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_rank_position default
#' @export
vec_cast.borg_rank_position.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_rank_position to borg_rank_position
#'
#' @method vec_cast.borg_rank_position borg_rank_position
#' @export
vec_cast.borg_rank_position.borg_rank_position <- function(x, to, ...) {
  rank_position(vec_data(x),
                relevant = borg_rlvnt(to),
                choice_name = borg_ch_name(to),
                choice_label = borg_ch_label(to),
                label = borg_lbl(to),
                q_name = borg_q_name(to),
                constraint = borg_cnstrnt(to),
                max_rank = borg_max_rank(to),
                position_sep = borg_pos_sep(to))
}

# CASTING BETWEEN DOUBLE

#' Casting borg_rank_position to double
#'
#' @method vec_cast.double borg_rank_position
#' @export
vec_cast.double.borg_rank_position <- function(x, to, ...) as.double(vec_data(x))

#' Casting double to borg_rank_positionacter
#'
#' @method vec_cast.borg_rank_position double
#' @export
vec_cast.borg_rank_position.double <- function(x, to, ...) {
  rank_position(x,
                relevant = borg_rlvnt(to),
                choice_name = borg_ch_name(to),
                choice_label = borg_ch_label(to),
                q_name = borg_q_name(to),
                label = borg_lbl(to),
                constraint = borg_cnstrnt(to),
                max_rank = borg_max_rank(to),
                position_sep = borg_pos_sep(to))
}

# CASTING BETWEEN INTEGER

#' Casting borg_rank_position to integer
#'
#' @method vec_cast.integer borg_rank_position
#' @export
vec_cast.integer.borg_rank_position <- function(x, to, ...) as.integer(vec_data(x))

#' Casting integer to borg_rank_positionacter
#'
#' @method vec_cast.borg_rank_position integer
#' @export
vec_cast.borg_rank_position.integer <- function(x, to, ...) {
  rank_position(x,
                relevant = borg_rlvnt(to),
                choice_name = borg_ch_name(to),
                choice_label = borg_ch_label(to),
                q_name = borg_q_name(to),
                label = borg_lbl(to),
                constraint = borg_cnstrnt(to),
                max_rank = borg_max_rank(to),
                position_sep = borg_pos_sep(to))
}


# HELPER FUNCTIONS FOR CASTING

# RANK POSITION HELPERS

#' Cast to `borg_rank_position`
#'
#' Cast `x` to a `borg_rank_position` vector
#'
#' @param x An object to coerce to `borg_rank_position`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-rank-position
NULL

#' @rdname cast-rank-position
#' @export
as_rank_position <- function(x, ...) {
  UseMethod("as_rank_position")
}

#' @rdname cast-rank-position
#' @export
as_rank_position.borg_rank_position <- function(x, ...) x

#' @rdname cast-rank-position
#' @export
as_rank_position.numeric <- function(x,
                                     relevant = NA,
                                     choice_name = NA,
                                     choice_label = NA,
                                     q_name = NA,
                                     label = NA,
                                     constraint = NA,
                                     max_rank = NA,
                                     position_sep = "/",
                                     ...) {
  vec_cast(x, to = rank_position(relevant = relevant,
                                 choice_name = choice_name,
                                 choice_label = choice_label,
                                 q_name = q_name,
                                 label = label,
                                 constraint = constraint,
                                 max_rank = max_rank,
                                 position_sep = position_sep))
}

#' @rdname cast-rank-position
#' @export
as_rank_position.double <- function(x,
                                    relevant = NA,
                                    choice_name = NA,
                                    choice_label = NA,
                                    q_name = NA,
                                    label = NA,
                                    constraint = NA,
                                    max_rank = NA,
                                    position_sep = "/",
                                    ...) {
  vec_cast(x, to = rank_position(relevant = relevant,
                                 choice_name = choice_name,
                                 choice_label = choice_label,
                                 q_name = q_name,
                                 label = label,
                                 constraint = constraint,
                                 max_rank = max_rank,
                                 position_sep = position_sep))
}

#' @rdname cast-rank-position
#' @export
as_rank_position.integer <- function(x,
                                     relevant = NA,
                                     choice_name = NA,
                                     choice_label = NA,
                                     q_name = NA,
                                     label = NA,
                                     constraint = NA,
                                     max_rank = NA,
                                     position_sep = "/",
                                     ...) {
  vec_cast(x, to = rank_position(relevant = relevant,
                                 choice_name = choice_name,
                                 choice_label = choice_label,
                                 q_name = q_name,
                                 label = label,
                                 constraint = constraint,
                                 max_rank = max_rank,
                                 position_sep = position_sep))
}

## DEFINING ARITHMETIC

vec_arith.borg_rank_position <- function(op, x, y, ...) {
  UseMethod("vec_arith.borg_rank_position", y)
}
vec_arith.borg_rank_position.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

vec_arith.borg_rank_position.borg_rank_position <- function(op, x, y, ...) {
  if (identical_borg_attr(x, y)) {
    vec_restore(vec_arith_base(op, x, y), x)
  } else {
    vec_arith_base(op, x, y)
  }
}

vec_arith.numeric.borg_rank_position <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_rank_position.numeric <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.logical.borg_rank_position <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_rank_position.logical <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_rank_position.borg_number <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_number.borg_rank_position <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_rank_position.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = vec_restore(ifelse(x == T, F, T), x),
         `+` = x,
         `!` = vec_restore(ifelse(x == T, F, T), x),
         stop_incompatible_op(op, x, y))
}

vec_math.borg_rank_position <- function(.fn, .x, ...) vec_math_base(.fn, .x, ...)
