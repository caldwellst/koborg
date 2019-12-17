# CONSTRUCTORS FOR text CLASS

#' Low level text constructor
new_text <- function(x = character(),
                     relevant = NA,
                     label = NA,
                     constraint = NA) {
  vec_assert(x, character())
  new_vctr(x,
           relevant = relevant,
           label = label,
           constraint = constraint,
           class = "borg_text")
}

#' Text constructor
#'
#' `text()` constructs a text vector.
#'
#' @export
text <- function(x = character(),
                 relevant = NA,
                 label = NA,
                 constraint = NA) {
  x <- vec_cast(x, character())
  validate_text(
    new_text(
      x,
      relevant,
      label,
      constraint)
  )
}

#' Validator for text class
validate_text <- function(x) {
  attr_err(x)
  x
}

# FORMATTING FOR PRINTING

format.borg_text <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_text <- function(x, ...) {
  "borg_text"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_text <- function(x, ...) {
  "txt"
}

# COERCIONS

#' Boiler plate for coercion for coercion of texts
#'
#' @method vec_ptype2 borg_text
#' @export
#' @export vec_ptype2.borg_text
vec_ptype2.borg_text <- function(x, y, ...) UseMethod("vec_ptype2.borg_text", y)

#' @method vec_ptype2.borg_text default
#' @export
vec_ptype2.borg_text.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_text borg_text
#' @export
vec_ptype2.borg_text.borg_text <- function(x, y, ...) {
  if (identical_calc_attr(x, y)) {
    new_text()
  } else {
    character()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_text character
#' @export
vec_ptype2.borg_text.character <- function(x, y, ...) {
  new_text(relevant = borg_rlvnt(x),
           label = borg_lbl(x),
           constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.character borg_text
#' @export
vec_ptype2.character.borg_text <- function(x, y, ...) {
  new_text(relevant = borg_rlvnt(y),
           label = borg_lbl(y),
           constraint = borg_cnstrnt(y))
}

# CASTING

#' Boiler plate for casting text
#'
#' @method vec_cast borg_text
#' @export
#' @export vec_cast.borg_text
vec_cast.borg_text <- function(x, to, ...) UseMethod("vec_cast.borg_text")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_text default
#' @export
vec_cast.borg_text.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_text to borg_text
#'
#' @method vec_cast.borg_text borg_text
#' @export
vec_cast.borg_text.borg_text <- function(x, to, ...) {
  text(vec_data(x),
       relevant = borg_rlvnt(to),
       label = borg_lbl(to),
       constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN CHARACTER

#' Casting borg_text to character
#'
#' @method vec_cast.character borg_text
#' @export
vec_cast.character.borg_text <- function(x, to, ...) vec_data(x)

#' Casting character to borg_textacter
#'
#' @method vec_cast.borg_text character
#' @export
vec_cast.borg_text.character <- function(x, to, ...) {
  text(x,
       relevant = borg_rlvnt(to),
       label = borg_lbl(to),
       constraint = borg_cnstrnt(to))
}

# HELPER FUNCTIONS FOR CASTING

#' Cast to `borg_text`
#'
#' Cast `x` to a `borg_text` vector
#'
#' @param x An object to coerce to `borg_text`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-text
NULL

#' @rdname cast-text
#' @export
as_text <- function(x, ...) {
  UseMethod("as_text")
}

#' @rdname cast-text
#' @export
as_text.borg_text <- function(x, ...) x

#' @rdname cast-text
#' @export
as_text.character <- function(x,
                              relevant = NA,
                              label = NA,
                              constraint = NA,
                              ...) {
  vec_cast(x, to = text(relevant = relevant,
                        label = label,
                        constraint = constraint,
                        calculation = calculation))
}
