# CONSTRUCTORS FOR SELECT MULTIPLE HELPER CLASS
# CHARACTER VECTOR THAT DISALLOWS SPACES

#' Low level text constructor
#' @importFrom stringr str_split
new_char_helper <- function(x = character()) {
  vec_assert(x, character())
  if (length(x) > 0) {
    x <- unlist(str_split(x, " "))
  }
  new_vctr(x, class = "char_helper")
}

char_helper <- function(x = character()) {
  x <- vec_cast(x, character())
  new_char_helper(x)
}

# FORMATTING FOR PRINTING
format.char_helper <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.char_helper <- function(x, ...) {
  "char_helper"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.char_helper <- function(x, ...) {
  "chr_hlp"
}

# COERCIONS

#' Boiler plate for coercion for coercion of texts
#'
#' @method vec_ptype2 char_helper
#' @export
#' @export vec_ptype2.char_helper
vec_ptype2.char_helper <- function(x, y, ...) UseMethod("vec_ptype2.char_helper", y)

#' @method vec_ptype2.char_helper default
#' @export
vec_ptype2.char_helper.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.char_helper char_helper
#' @export
vec_ptype2.char_helper.char_helper <- function(x, y, ...) char_helper()

# COERCION TO CHARACTER
#' @method vec_ptype2.char_helper character
#' @export
vec_ptype2.char_helper.character <- function(x, y, ...) char_helper()

#' @method vec_ptype2.character char_helper
#' @export
vec_ptype2.character.char_helper <- function(x, y, ...) char_helper()

# CASTING

#' Boiler plate for casting text
#'
#' @method vec_cast char_helper
#' @export
#' @export vec_cast.char_helper
vec_cast.char_helper <- function(x, to, ...) UseMethod("vec_cast.char_helper")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.char_helper default
#' @export
vec_cast.char_helper.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting char_helper to char_helper
#'
#' @method vec_cast.char_helper char_helper
#' @export
vec_cast.char_helper.char_helper <- function(x, to, ...) new_char_helper(vec_data(x))

# CASTING BETWEEN CHARACTER

#' Casting char_helper to character
#'
#' @method vec_cast.character char_helper
#' @export
vec_cast.character.char_helper <- function(x, to, ...) vec_data(x)

#' Casting character to char_helperacter
#'
#' @method vec_cast.char_helper character
#' @export
vec_cast.char_helper.character <- function(x, to, ...) char_helper(x)
