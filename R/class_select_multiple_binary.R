# CONSTRUCTORS FOR SELECT MULTIPLE CLASS

#' Low level select multiple binary constructor
new_sm_binary <- function(x = logical(),
                          relevant = NA,
                          choice_name = NA,
                          choice_label = NA,
                          q_name = NA,
                          label = NA,
                          constraint = NA,
                          binary_sep = "/") {
  vec_assert(x, logical())
  new_vctr(x,
           relevant = relevant,
           choice_name = choice_name,
           choice_label = choice_label,
           q_name = q_name,
           label = label,
           constraint = constraint,
           binary_sep = binary_sep,
           class = "borg_sm_binary")
}

#' Select multiple binary constructor
#'
#' `sm_binary()` constructs a select multiple binary vector, which is a logical vector where each row
#' indicates if that particular choice option was selected or not.
#'
#' @importFrom stringr str_split
#'
#' @export
sm_binary <- function(x = logical(),
                      relevant = NA,
                      choice_name = NA,
                      choice_label = NA,
                      q_name = NA,
                      label = NA,
                      constraint = NA,
                      binary_sep = "/") {

  x <- vec_cast(x, logical())
  validate_sm(
    new_sm_binary(
      x,
      relevant,
      choice_name,
      choice_label,
      q_name,
      label,
      constraint,
      binary_sep
    )
  )
}

#' Validation function for both select multiple classes
validate_sm <- function(x) {
  attr_err(x)
  attr_sm_bin_err(x)
  attr_sm_err(x)
  x
}

# FORMATTING FOR PRINTING

#' @importFrom stringr str_count str_c
format.borg_sm_binary <- function(x, ...) vec_data(x)

#' Full abbreviation in tibbles
vec_ptype_full.borg_sm_binary <- function(x, ...) {
  "borg_sm_binary"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_sm_binary <- function(x, ...) {
  "sm_binary"
}

# COERCIONS

#' Boiler plate for coercion for coercion of select multiple binarys
#'
#' @method vec_ptype2 borg_sm_binary
#' @export
#' @export vec_ptype2.borg_sm_binary
vec_ptype2.borg_sm_binary <- function(x, y, ...) UseMethod("vec_ptype2.borg_sm_binary", y)

#' @method vec_ptype2.borg_sm_binary default
#' @export
vec_ptype2.borg_sm_binary.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_sm_binary borg_sm_binary
#' @export
vec_ptype2.borg_sm_binary.borg_sm_binary <- function(x, y, ...) {
  if (identical_sm_attr(x, y)) {
    new_sm_binary()
  } else {
    logical()
  }
}

# COERCION TO LOGICAL
#' @method vec_ptype2.borg_sm_binary logical
#' @export
vec_ptype2.borg_sm_binary.logical <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(x),
                choice_name = borg_ch_name(x),
                choice_label = borg_ch_label(x),
                q_name = borg_q_name(x),
                label = borg_lbl(x),
                constraint = borg_cnstrnt(x),
                binary_sep = borg_bin_sep(x))
}

#' @method vec_ptype2.logical borg_sm_binary
#' @export
vec_ptype2.logical.borg_sm_binary <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(y),
                choice_name = borg_ch_name(y),
                choice_label = borg_ch_label(y),
                q_name = borg_q_name(y),
                label = borg_lbl(y),
                constraint = borg_cnstrnt(y),
                binary_sep = borg_bin_sep(y))
}

# COERCION TO DOUBLE
#' @method vec_ptype2.borg_sm_binary double
#' @export
vec_ptype2.borg_sm_binary.double <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(x),
                choice_name = borg_ch_name(x),
                choice_label = borg_ch_label(x),
                q_name = borg_q_name(x),
                label = borg_lbl(x),
                constraint = borg_cnstrnt(x),
                binary_sep = borg_bin_sep(x))
}

#' @method vec_ptype2.double borg_sm_binary
#' @export
vec_ptype2.double.borg_sm_binary <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(y),
                choice_name = borg_ch_name(y),
                choice_label = borg_ch_label(y),
                q_name = borg_q_name(y),
                label = borg_lbl(y),
                constraint = borg_cnstrnt(y),
                binary_sep = borg_bin_sep(y))
}


# COERCION TO INTEGER
#' @method vec_ptype2.borg_sm_binary integer
#' @export
vec_ptype2.borg_sm_binary.integer <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(x),
                choice_name = borg_ch_name(x),
                choice_label = borg_ch_label(x),
                q_name = borg_q_name(x),
                label = borg_lbl(x),
                constraint = borg_cnstrnt(x),
                binary_sep = borg_bin_sep(x))
}

#' @method vec_ptype2.integer borg_sm_binary
#' @export
vec_ptype2.integer.borg_sm_binary <- function(x, y, ...) {
  new_sm_binary(relevant = borg_rlvnt(y),
                choice_name = borg_ch_name(y),
                choice_label = borg_ch_label(y),
                q_name = borg_q_name(y),
                label = borg_lbl(y),
                constraint = borg_cnstrnt(y),
                binary_sep = borg_bin_sep(y))
}


# CASTING

#' Boiler plate for casting select multiple binary
#'
#' @method vec_cast borg_sm_binary
#' @export
#' @export vec_cast.borg_sm_binary
vec_cast.borg_sm_binary <- function(x, to, ...) UseMethod("vec_cast.borg_sm_binary")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_sm_binary default
#' @export
vec_cast.borg_sm_binary.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_sm_binary to borg_sm_binary
#'
#' @method vec_cast.borg_sm_binary borg_sm_binary
#' @export
vec_cast.borg_sm_binary.borg_sm_binary <- function(x, to, ...) {
  sm_binary(x,
            relevant = borg_rlvnt(to),
            choice_name = borg_ch_name(to),
            choice_label = borg_ch_label(to),
            label = borg_lbl(to),
            q_name = borg_q_name(to),
            constraint = borg_cnstrnt(to),
            binary_sep = borg_bin_sep(to))
}

# CASTING BETWEEN LOGICAL

#' Casting borg_sm_binary to logical
#'
#' @method vec_cast.logical borg_sm_binary
#' @export
vec_cast.logical.borg_sm_binary <- function(x, to, ...) vec_data(x)

#' Casting logical to borg_sm_binaryacter
#'
#' @method vec_cast.borg_sm_binary logical
#' @export
vec_cast.borg_sm_binary.logical <- function(x, to, ...) {
  sm_binary(x,
            relevant = borg_rlvnt(to),
            choice_name = borg_ch_name(to),
            choice_label = borg_ch_label(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            binary_sep = borg_bin_sep(to))
}

# CASTING BETWEEN DOUBLE

#' Casting borg_sm_binary to double
#'
#' @method vec_cast.double borg_sm_binary
#' @export
vec_cast.double.borg_sm_binary <- function(x, to, ...) as.double(vec_data(x))

#' Casting double to borg_sm_binaryacter
#'
#' @method vec_cast.borg_sm_binary double
#' @export
vec_cast.borg_sm_binary.double <- function(x, to, ...) {
  sm_binary(x,
            relevant = borg_rlvnt(to),
            choice_name = borg_ch_name(to),
            choice_label = borg_ch_label(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            binary_sep = borg_bin_sep(to))
}

# CASTING BETWEEN INTEGER

#' Casting borg_sm_binary to integer
#'
#' @method vec_cast.integer borg_sm_binary
#' @export
vec_cast.integer.borg_sm_binary <- function(x, to, ...) as.integer(vec_data(x))

#' Casting integer to borg_sm_binaryacter
#'
#' @method vec_cast.borg_sm_binary integer
#' @export
vec_cast.borg_sm_binary.integer <- function(x, to, ...) {
  sm_binary(x,
            relevant = borg_rlvnt(to),
            choice_name = borg_ch_name(to),
            choice_label = borg_ch_label(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            binary_sep = borg_bin_sep(to))
}


# HELPER FUNCTIONS FOR CASTING

# SELECT multiple binary HELPERS

#' Cast to `borg_sm_binary`
#'
#' Cast `x` to a `borg_sm_binary` vector
#'
#' @param x An object to coerce to `borg_sm_binary`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-sm-binary
NULL

#' @rdname cast-sm-binary
#' @export
as_sm_binary <- function(x, ...) {
  UseMethod("as_sm_binary")
}

#' @rdname cast-sm-binary
#' @export
as_sm_binary.borg_sm_binary <- function(x, ...) x

#' @rdname cast-sm-binary
#' @export
as_sm_binary.logical <- function(x,
                                 relevant = NA,
                                 choice_name = NA,
                                 choice_label = NA,
                                 q_name = NA,
                                 label = NA,
                                 constraint = NA,
                                 binary_sep = "/",
                                 ...) {
  vec_cast(x, to = sm_binary(relevant = relevant,
                             choice_name = choice_name,
                             choice_label = choice_label,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             binary = binary_sep))
}

#' @rdname cast-sm-binary
#' @export
as_sm_binary.numeric <- function(x,
                                 relevant = NA,
                                 choice_name = NA,
                                 choice_label = NA,
                                 q_name = NA,
                                 label = NA,
                                 constraint = NA,
                                 binary_sep = "/",
                                 ...) {
  vec_cast(x, to = sm_binary(relevant = relevant,
                             choice_name = choice_name,
                             choice_label = choice_label,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             binary = binary_sep))
}

#' @rdname cast-sm-binary
#' @export
as_sm_binary.double <- function(x,
                                relevant = NA,
                                choice_name = NA,
                                choice_label = NA,
                                q_name = NA,
                                label = NA,
                                constraint = NA,
                                binary_sep = "/",
                                ...) {
  vec_cast(x, to = sm_binary(relevant = relevant,
                             choice_name = choice_name,
                             choice_label = choice_label,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             binary = binary_sep))
}

#' @rdname cast-sm-binary
#' @export
as_sm_binary.integer <- function(x,
                                 relevant = NA,
                                 choice_name = NA,
                                 choice_label = NA,
                                 q_name = NA,
                                 label = NA,
                                 constraint = NA,
                                 binary_sep = "/",
                                 ...) {
  vec_cast(x, to = sm_binary(relevant = relevant,
                             choice_name = choice_name,
                             choice_label = choice_label,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             binary = binary_sep))
}

## DEFINING ARITHMETIC

vec_arith.borg_sm_binary <- function(op, x, y, ...) {
  UseMethod("vec_arith.borg_sm_binary", y)
}
vec_arith.borg_sm_binary.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

vec_arith.borg_sm_binary.borg_sm_binary <- function(op, x, y, ...) {
  if (identical_borg_attr(x, y)) {
    vec_restore(vec_arith_base(op, x, y), x)
  } else {
    vec_arith_base(op, x, y)
  }
}

vec_arith.numeric.borg_sm_binary <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_sm_binary.numeric <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.logical.borg_sm_binary <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_sm_binary.logical <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}


vec_arith.borg_sm_binary.borg_number <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_number.borg_sm_binary <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_sm_binary.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = vec_restore(ifelse(x == T, F, T), x),
         `+` = x,
         `!` = vec_restore(ifelse(x == T, F, T), x),
         stop_incompatible_op(op, x, y))
}

vec_math.borg_sm_binary <- function(.fn, .x, ...) vec_math_base(.fn, .x, ...)
