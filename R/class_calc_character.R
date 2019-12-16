# CONSTRUCTORS FOR CALC CHARACTER CLASS

#' Low level calc character constructor
new_calc_character <- function(x = character(),
                               relevant = NA,
                               label = NA,
                               constraint = NA,
                               calculation = NA) {
  vec_assert(x, character())
  new_vctr(x,
           relevant = relevant,
           label = label,
           constraint = constraint,
           calculation = calculation,
           class = "borg_calc_character")
}

#' Calculation character constructor
#'
#' `calc_character()` constructs a calc character vector.
#'
#' @export
calc_character <- function(x = character(),
                           relevant = NA,
                           label = NA,
                           constraint = NA,
                           calculation = NA) {
  x <- vec_cast(x, character())
  validate_calc_character(
    new_calc_character(
      x,
      relevant,
      label,
      constraint,
      calculation)
  )
}

#' Validator for calc character class
validate_calc_character <- function(x) {
  attr_err(x)
  attr_calc_err(x)
  x
}

# FORMATTING FOR PRINTING

format.borg_calc_character <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_calc_character <- function(x, ...) {
  "borg_calc_character"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_calc_character <- function(x, ...) {
  "calc_c"
}

# COERCIONS

#' Boiler plate for coercion for coercion of calc characters
#'
#' @method vec_ptype2 borg_calc_character
#' @export
#' @export vec_ptype2.borg_calc_character
vec_ptype2.borg_calc_character <- function(x, y, ...) UseMethod("vec_ptype2.borg_calc_character", y)

#' @method vec_ptype2.borg_calc_character default
#' @export
vec_ptype2.borg_calc_character.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_calc_character borg_calc_character
#' @export
vec_ptype2.borg_calc_character.borg_calc_character <- function(x, y, ...) {
  if (identical_calc_attr(x, y)) {
    new_calc_character()
  } else {
    character()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_calc_character character
#' @export
vec_ptype2.borg_calc_character.character <- function(x, y, ...) {
  new_calc_character(relevant = borg_rlvnt(x),
                     label = borg_lbl(x),
                     constraint = borg_cnstrnt(x),
                     calculation = borg_calc(x))
}

#' @method vec_ptype2.character borg_calc_character
#' @export
vec_ptype2.character.borg_calc_character <- function(x, y, ...) {
  new_calc_character(relevant = borg_rlvnt(y),
                     label = borg_lbl(y),
                     constraint = borg_cnstrnt(y),
                     calculation = borg_calc(y))
}

# CASTING

#' Boiler plate for casting calc character
#'
#' @method vec_cast borg_calc_character
#' @export
#' @export vec_cast.borg_calc_character
vec_cast.borg_calc_character <- function(x, to, ...) UseMethod("vec_cast.borg_calc_character")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_calc_character default
#' @export
vec_cast.borg_calc_character.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_calc_character to borg_calc_character
#'
#' @method vec_cast.borg_calc_character borg_calc_character
#' @export
vec_cast.borg_calc_character.borg_calc_character <- function(x, to, ...) {
  if (identical_calc_attr(x, to)) {
    x
  } else {
    character()
  }
}

# CASTING BETWEEN CHARACTER

#' Casting borg_calc_character to character
#'
#' @method vec_cast.character borg_calc_character
#' @export
vec_cast.character.borg_calc_character <- function(x, to, ...) vec_data(x)

#' Casting character to borg_calc_characteracter
#'
#' @method vec_cast.borg_calc_character character
#' @export
vec_cast.borg_calc_character.character <- function(x, to, ...) {
  calc_character(x,
                 relevant = borg_rlvnt(to),
                 label = borg_lbl(to),
                 constraint = borg_cnstrnt(to),
                 calculation = borg_calc(to))
}

# HELPER FUNCTIONS FOR CASTING

#' Cast to `borg_calc_character`
#'
#' Cast `x` to a `borg_calc_character` vector
#'
#' @param x An object to coerce to `borg_calc_character`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-calc-character
NULL

#' @rdname cast-calc-character
#' @export
as_calc_character <- function(x, ...) {
  UseMethod("as_calc_character")
}

#' @rdname cast-calc-character
#' @export
as_calc_character.borg_calc_character <- function(x, ...) x

#' @rdname cast-calc-character
#' @export
as_calc_character.character <- function(x,
                                        relevant = NA,
                                        label = NA,
                                        constraint = NA,
                                        calculation = NA,
                                        ...) {
  vec_cast(x, to = calc_character(relevant = relevant,
                                  label = label,
                                  constraint = constraint,
                                  calculation = calculation))
}
