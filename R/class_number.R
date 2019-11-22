# CONSTRUCTORS FOR NUMBER CLASS

#' Low level number constructor
new_number <- function(x = double(),
                       relevant = NA,
                       label = NA,
                       constraint = NA) {
  vec_assert(x, double())
  new_vctr(x,
           relevant = relevant,
           label = label,
           constraint = constraint,
           class = "borg_number")
}

#' Number constructor
#'
#' `number()` constructs a number vector.
#'
#' @export
number <- function(x = double(),
                   relevant = NA,
                   label = NA,
                   constraint = NA) {
  x <- vec_cast(x, double())
  validate_number(
    new_number(
      x,
      relevant,
      label,
      constraint)
  )
}

#' Validator for number class
validate_number <- function(x) {
  attr_err(borg_rlvnt(x),
           borg_lbl(x),
           borg_cnstrnt(x))
  x
}

# FORMATTING FOR PRINTING

format.borg_number <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_number <- function(x, ...) {
  "number"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_number <- function(x, ...) {
  "nmbr"
}

# COERCIONS

#' Boiler plate for coercion for coercion of select one characters
#'
#' @method vec_ptype2 borg_number
#' @export
#' @export vec_ptype2.borg_number
vec_ptype2.borg_number <- function(x, y, ...) UseMethod("vec_ptype2.borg_number", y)

#' @method vec_ptype2.borg_number default
#' @export
vec_ptype2.borg_number.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_number borg_number
#' @export
vec_ptype2.borg_number.borg_number <- function(x, y, ...) {
  new_number(relevant = borg_rlvnt(x),
             constraint = borg_cnstrnt(x),
             label = borg_lbl(x))
}

# COERCION TO INTEGER
#' @method vec_ptype2.borg_number integer
#' @export
vec_ptype2.borg_number.integer <- function(x, y, ...) {
  new_number(relevant = borg_rlvnt(x),
             label = borg_lbl(x),
             constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.integer borg_number
#' @export
vec_ptype2.integer.borg_number <- function(x, y, ...) {
  new_number(relevant = borg_rlvnt(y),
             label = borg_lbl(y),
             constraint = borg_cnstrnt(y))
}

# COERCION TO DOUBLE

#' @method vec_ptype2.borg_number double
#' @export
vec_ptype2.borg_number.double <- function(x, y, ...) {
  new_number(relevant = borg_rlvnt(x),
             label = borg_lbl(x),
             constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.double borg_number
#' @export
vec_ptype2.double.borg_number <- function(x, y, ...) {
  new_number(relevant = borg_rlvnt(y),
             label = borg_lbl(y),
             constraint = borg_cnstrnt(y))
}

# CASTING

#' Boiler plate for casting number
#'
#' @method vec_cast borg_number
#' @export
#' @export vec_cast.borg_number
vec_cast.borg_number <- function(x, to, ...) UseMethod("vec_cast.borg_number")

#' @method vec_cast.borg_number default
#' @export
vec_cast.borg_number.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting number to number
#'
#' @method vec_cast.borg_number borg_number
#' @export
vec_cast.borg_number.borg_number <- function(x, to, ...) {
  number(x,
         relevant = borg_rlvnt(to),
         label = borg_lbl(to),
         constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN INTEGER

#' Casting number to integer
#'
#' @method vec_cast.integer borg_number
#' @export
vec_cast.integer.borg_number <- function(x, to, ...) vec_data(x)

#' Casting integer to number
#'
#' @method vec_cast.borg_number integer
#' @export
vec_cast.borg_number.integer <- function(x, to, ...) {
  number(x,
         relevant = borg_rlvnt(to),
         label = borg_lbl(to),
         constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN DOUBLE

#' Casting number to double
#'
#' @method vec_cast.double borg_number
#' @export
vec_cast.double.borg_number <- function(x, to, ...) vec_data(x)

#' Casting double to number
#'
#' @method vec_cast.borg_number double
#' @export
vec_cast.borg_number.double <- function(x, to, ...) {
  number(x,
         relevant = borg_rlvnt(to),
         label = borg_lbl(to),
         constraint = borg_cnstrnt(to))
}

# HELPER FUNCTIONS FOR CASTING

#' Cast to `number`
#'
#' Cast `x` to a `number` vector
#'
#' @param x An object to coerce to `number`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-borg-number
NULL

#' @rdname cast-borg-number
#' @export
as_number <- function(x, ...) {
  UseMethod("as_number")
}

#' @rdname cast-borg-number
#' @export
as_number.borg_number <- function(x, ...) x

#' @rdname cast-borg-number
#' @export
as_number.integer <- function(x,
                              relevant = NA,
                              label = NA,
                              constraint = NA,
                              ...) {
  vec_cast(x, to = number(relevant = relevant,
                          label = label,
                          constraint = constraint))
}

#' @rdname cast-borg-number
#' @export
as_number.double <- function(x,
                             relevant = NA,
                             label = NA,
                             constraint = NA,
                             ...) {
  vec_cast(x, to = number(relevant = relevant,
                          label = label,
                          constraint = constraint))
}

## DEFINING ARITHMETIC

vec_arith.borg_number <- function(op, x, y, ...) {
  UseMethod("vec_arith.borg_number", y)
}
vec_arith.borg_number.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

vec_arith.borg_number.borg_number <- function(op, x, y, ...) {
  if (identical_borg_attr(x, y)) {
    new_number(vec_arith_base(op, x, y),
               relevant = borg_rlvnt(x),
               constraint = borg_cnstrnt(x),
               label = borg_lbl(x))
  } else {
    vec_arith_base(op, x, y)
  }

}

vec_arith.numeric.borg_number <- function(op, x, y, ...) {
  new_number(vec_arith_base(op, x, y),
             relevant = borg_rlvnt(y),
             constraint = borg_cnstrnt(y),
             label = borg_lbl(y))
}

vec_arith.borg_number.numeric <- function(op, x, y, ...) {
  new_number(vec_arith_base(op, x, y),
             relevant = borg_rlvnt(x),
             constraint = borg_cnstrnt(x),
             label = borg_lbl(x))
}

vec_math.borg_number <- function(.fn, .x, ...) vec_math_base(.fn, .x, ...)
