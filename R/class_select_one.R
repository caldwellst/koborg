# CONSTRUCTORS FOR SELECT ONE CLASS

#' Low level select one constructor
new_slct_one <- function(x = character(),
                        relevant = NA,
                        choice_names = NA,
                        choice_labels = NA,
                        label = NA,
                        constraint = NA) {
  vec_assert(x, character())
  so_chc_check(x, choice_names)
  new_vctr(x,
           relevant = relevant,
           choice_names = choice_names,
           choice_labels = choice_labels,
           label = label,
           constraint = constraint,
           class = "borg_slct_one")
}

#' Select one character constructor
#'
#' `slct_one()` constructs a select one vector. Can be constructed from a character vector itself.
#'
#' @export
slct_one <- function(x = character(),
                     relevant = NA,
                     choice_names = NA,
                     choice_labels = NA,
                     label = NA,
                     constraint = NA) {
  x <- vec_cast(x, character())
  validate_slct_one(
    new_slct_one(
      x,
      relevant,
      choice_names,
      choice_labels,
      label,
      constraint)
  )
}

#' Validator for select one class
validate_slct_one <- function(x) {
  attr_err(x)
  attr_slct_err(x)
  x
}

# FORMATTING FOR PRINTING

format.borg_slct_one <- function(x, ...) vec_data(x)

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_slct_one <- function(x, ...) {
  "borg_slct_one"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_slct_one <- function(x, ...) {
  "slct_one"
}

# COERCIONS

#' Boiler plate for coercion for coercion of select one characters
#'
#' @method vec_ptype2 borg_slct_one
#' @export
#' @export vec_ptype2.borg_slct_one
vec_ptype2.borg_slct_one <- function(x, y, ...) UseMethod("vec_ptype2.borg_slct_one", y)

#' @method vec_ptype2.borg_slct_one default
#' @export
vec_ptype2.borg_slct_one.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_slct_one borg_slct_one
#' @export
vec_ptype2.borg_slct_one.borg_slct_one <- function(x, y, ...) {
  if (identical_so_attr(x, y)) {
    new_slct_one()
  } else {
    character()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_slct_one character
#' @export
vec_ptype2.borg_slct_one.character <- function(x, y, ...) {
  new_slct_one(relevant = borg_rlvnt(x),
               choice_names = borg_ch_nms(x),
               choice_labels = borg_ch_lbls(x),
               label = borg_lbl(x),
               constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.character borg_slct_one
#' @export
vec_ptype2.character.borg_slct_one <- function(x, y, ...) {
  new_slct_one(relevant = borg_rlvnt(y),
               choice_names = borg_ch_nms(y),
               choice_labels = borg_ch_lbls(y),
               label = borg_lbl(y),
               constraint = borg_cnstrnt(y))
}

# CASTING

#' Boiler plate for casting select one
#'
#' @method vec_cast borg_slct_one
#' @export
#' @export vec_cast.borg_slct_one
vec_cast.borg_slct_one <- function(x, to, ...) UseMethod("vec_cast.borg_slct_one")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_slct_one default
#' @export
vec_cast.borg_slct_one.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_slct_one to borg_slct_one
#'
#' @method vec_cast.borg_slct_one borg_slct_one
#' @export
vec_cast.borg_slct_one.borg_slct_one <- function(x, to, ...) {
  slct_one(x,
           relevant = borg_rlvnt(to),
           choice_names = borg_ch_nms(to),
           choice_labels = borg_ch_lbls(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN CHARACTER

#' Casting borg_slct_one to character
#'
#' @method vec_cast.character borg_slct_one
#' @export
vec_cast.character.borg_slct_one <- function(x, to, ...) vec_data(x)

#' Casting character to borg_slct_oneacter
#'
#' @method vec_cast.borg_slct_one character
#' @export
vec_cast.borg_slct_one.character <- function(x, to, ...) {
  slct_one(x,
           relevant = borg_rlvnt(to),
           choice_names = borg_ch_nms(to),
           choice_labels = borg_ch_lbls(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}

# HELPER FUNCTIONS FOR CASTING

#' Cast to `borg_slct_one`
#'
#' Cast `x` to a `borg_slct_one` vector
#'
#' @param x An object to coerce to `borg_slct_one`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-slct-one
NULL

#' @rdname cast-slct-one
#' @export
as_slct_one <- function(x, ...) {
  UseMethod("as_slct_one")
}

#' @rdname cast-slct-one
#' @export
as_slct_one.borg_slct_one <- function(x, ...) x

#' @rdname cast-slct-one
#' @export
as_slct_one.character <- function(x,
                                  relevant = NA,
                                  choice_names = NA,
                                  choice_labels = NA,
                                  label = NA,
                                  constraint = NA,
                                  ...) {
  vec_cast(x, to = slct_one(relevant = relevant,
                            choice_names = choice_names,
                            choice_labels = choice_labels,
                            label = label,
                            constraint = constraint))
}
