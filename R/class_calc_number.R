# CONSTRUCTORS FOR CALCULATION NUMBER CLASS

#' Low level calc number constructor
new_calc_number <- function(x = double(),
                            relevant = NA,
                            label = NA,
                            constraint = NA,
                            calculation = NA) {
  vec_assert(x, double())
  new_vctr(x,
           relevant = relevant,
           label = label,
           constraint = constraint,
           calculation = calculation,
           class = "borg_calc_number")
}

#' Calc number constructor
#'
#' `calc_number()` constructs a calc_number vector.
#'
#' @export
calc_number <- function(x = double(),
                   relevant = NA,
                   label = NA,
                   constraint = NA,
                   calculation = NA) {
  x <- vec_cast(x, double())
  validate_calc_number(
    new_calc_number(
      x,
      relevant,
      label,
      constraint,
      calculation)
  )
}

#' Validator for calc number class
validate_calc_number <- function(x) {
  attr_err(x)
  attr_calc_err(x)
  x
}

# FORMATTING FOR PRINTING

format.borg_calc_number <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_calc_number <- function(x, ...) {
  "borg_calc_number"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_calc_number <- function(x, ...) {
  "calc_n"
}

# COERCIONS

#' Boiler plate for coercion for coercion of calc number
#'
#' @method vec_ptype2 borg_calc_number
#' @export
#' @export vec_ptype2.borg_calc_number
vec_ptype2.borg_calc_number <- function(x, y, ...) UseMethod("vec_ptype2.borg_calc_number", y)

#' @method vec_ptype2.borg_calc_number default
#' @export
vec_ptype2.borg_calc_number.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_calc_number borg_calc_number
#' @export
vec_ptype2.borg_calc_number.borg_calc_number <- function(x, y, ...) {
  if (identical_calc_attr(x, y)) {
    x
  } else {
    double()
  }
}

# COERCION TO INTEGER
#' @method vec_ptype2.borg_calc_number integer
#' @export
vec_ptype2.borg_calc_number.integer <- function(x, y, ...) {
  new_calc_number(relevant = borg_rlvnt(x),
                  label = borg_lbl(x),
                  constraint = borg_cnstrnt(x),
                  calculation = borg_calc(x))
}

#' @method vec_ptype2.integer borg_calc_number
#' @export
vec_ptype2.integer.borg_calc_number <- function(x, y, ...) {
  new_calc_number(relevant = borg_rlvnt(y),
                  label = borg_lbl(y),
                  constraint = borg_cnstrnt(y),
                  calculation = borg_calc(y))
}

# COERCION TO DOUBLE

#' @method vec_ptype2.borg_calc_number double
#' @export
vec_ptype2.borg_calc_number.double <- function(x, y, ...) {
  new_calc_number(relevant = borg_rlvnt(x),
                  label = borg_lbl(x),
                  constraint = borg_cnstrnt(x),
                  calculation = borg_calc(x))
}

#' @method vec_ptype2.double borg_calc_number
#' @export
vec_ptype2.double.borg_calc_number <- function(x, y, ...) {
  new_calc_number(relevant = borg_rlvnt(y),
                  label = borg_lbl(y),
                  constraint = borg_cnstrnt(y),
                  calculation = borg_calc(y))
}

# CASTING

#' Boiler plate for casting calc number
#'
#' @method vec_cast borg_calc_number
#' @export
#' @export vec_cast.borg_calc_number
vec_cast.borg_calc_number <- function(x, to, ...) UseMethod("vec_cast.borg_calc_number")

#' @method vec_cast.borg_calc_number default
#' @export
vec_cast.borg_calc_number.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting calc number to calc number
#'
#' @method vec_cast.borg_calc_number borg_calc_number
#' @export
vec_cast.borg_calc_number.borg_calc_number <- function(x, to, ...) {
  calc_number(x,
              relevant = borg_rlvnt(to),
              label = borg_lbl(to),
              constraint = borg_cnstrnt(to),
              calculation = borg_calc(to))
}

# CASTING BETWEEN INTEGER

#' Casting calc number to integer
#'
#' @method vec_cast.integer borg_calc_number
#' @export
vec_cast.integer.borg_calc_number <- function(x, to, ...) vec_data(x)

#' Casting integer to calc number
#'
#' @method vec_cast.borg_calc_number integer
#' @export
vec_cast.borg_calc_number.integer <- function(x, to, ...) {
  calc_number(x,
              relevant = borg_rlvnt(to),
              label = borg_lbl(to),
              constraint = borg_cnstrnt(to),
              calculation = borg_calc(to))
}

# CASTING BETWEEN DOUBLE

#' Casting calc number to double
#'
#' @method vec_cast.double borg_calc_number
#' @export
vec_cast.double.borg_calc_number <- function(x, to, ...) vec_data(x)

#' Casting double to calc number
#'
#' @method vec_cast.borg_calc_number double
#' @export
vec_cast.borg_calc_number.double <- function(x, to, ...) {
  calc_number(x,
              relevant = borg_rlvnt(to),
              label = borg_lbl(to),
              constraint = borg_cnstrnt(to),
              calculation = borg_calc(to))
}

# CASTING BETWEEN CHARACTER

#' Casting calc number to character
#'
#' @method vec_cast.character borg_calc_number
#' @export
vec_cast.character.borg_calc_number <- function(x, to, ...) as.character(vec_data(x))

#' Casting character to calc number
#'
#' @method vec_cast.borg_calc_number character
#' @export
vec_cast.borg_calc_number.character <- function(x, to, ...) {
  calc_number(x,
              relevant = borg_rlvnt(to),
              label = borg_lbl(to),
              constraint = borg_cnstrnt(to),
              calculation = borg_calc(to))
}


# HELPER FUNCTIONS FOR CASTING

#' Cast to `calc_number`
#'
#' Cast `x` to a `calc_number` vector
#'
#' @param x An object to coerce to `calc_number`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-borg-calc-number
NULL

#' @rdname cast-borg-calc-number
#' @export
as_calc_number <- function(x, ...) {
  UseMethod("as_calc_number")
}

#' @rdname cast-borg-calc-number
#' @export
as_calc_number.borg_calc_number <- function(x, ...) x

#' @rdname cast-borg-calc-number
#' @export
as_calc_number.integer <- function(x,
                                   relevant = NA,
                                   label = NA,
                                   constraint = NA,
                                   calculation = NA,
                                   ...) {
  vec_cast(x, to = calc_number(relevant = relevant,
                               label = label,
                               constraint = constraint,
                               calculation = calculation))
}

#' @rdname cast-borg-calc-number
#' @export
as_calc_number.double <- function(x,
                                  relevant = NA,
                                  label = NA,
                                  constraint = NA,
                                  calculation = NA,
                                  ...) {
  vec_cast(x, to = calc_number(relevant = relevant,
                               label = label,
                               constraint = constraint,
                               calculation = calculation))
}

#' @rdname cast-borg-calc-number
#' @export
as_calc_number.character <- function(x,
                                     relevant = NA,
                                     label = NA,
                                     constraint = NA,
                                     calculation = NA,
                                     ...) {
  vec_cast(x, to = calc_number(relevant = relevant,
                               label = label,
                               constraint = constraint,
                               calculation = calculation))
}

## DEFINING ARITHMETIC

vec_arith.borg_calc_number <- function(op, x, y, ...) {
  UseMethod("vec_arith.borg_calc_number", y)
}
vec_arith.borg_calc_number.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

vec_arith.borg_calc_number.borg_calc_number <- function(op, x, y, ...) {
  if (identical_borg_attr(x, y)) {
    new_calc_number(vec_arith_base(op, x, y),
                    relevant = borg_rlvnt(x),
                    constraint = borg_cnstrnt(x),
                    label = borg_lbl(x),
                    calculation = borg_calc(x))
  } else {
    vec_arith_base(op, x, y)
  }

}

vec_arith.numeric.borg_calc_number <- function(op, x, y, ...) {
  new_calc_number(vec_arith_base(op, x, y),
                  relevant = borg_rlvnt(y),
                  constraint = borg_cnstrnt(y),
                  label = borg_lbl(y),
                  calculation = borg_calc(y))
}

vec_arith.borg_calc_number.numeric <- function(op, x, y, ...) {
  new_calc_number(vec_arith_base(op, x, y),
                  relevant = borg_rlvnt(x),
                  constraint = borg_cnstrnt(x),
                  label = borg_lbl(x),
                  calculation = borg_calc(x))
}

vec_arith.borg_calc_number.borg_number <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_arith.borg_number.borg_calc_number <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

vec_math.borg_calc_number <- function(.fn, .x, ...) vec_math_base(.fn, .x, ...)
