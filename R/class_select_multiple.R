# CONSTRUCTORS FOR SELECT MULTIPLE CLASS

#' Low level select multiple character constructor
new_sm_char <- function(x = character(),
                        relevant = NA,
                        choice_names = NA,
                        choice_labels = NA,
                        label = NA,
                        constraint = NA,
                        binary_type = "logical",
                        binary_sep = "/") {
  vec_assert(x, character())
  attr_err(relevant,
           label,
           constraint)
  attr_slct_err(choice_names, choice_labels)
  attr_sm_err(binary_type, binary_sep)
  sm_chc_check(x, choice_names, type = "char")
  new_vctr(x,
           relevant = relevant,
           choice_names = choice_names,
           choice_labels = choice_labels,
           label = label,
           constraint = constraint,
           binary_type = binary_type,
           binary_sep = binary_sep,
           class = "borg_sm_char")
}


#' Select multiple character constructor
#'
#' `sm_char()` constructs a select multiple character vector, each value in the vecotr corresponds to the selected
#' options for that survey row in string format. Can be constructed from a character vector itself.
#'
#' @importFrom stringr str_split
#'
#' @export
sm_char <- function(x = character(),
                    relevant = NA,
                    choice_names = NA,
                    choice_labels = NA,
                    label = NA,
                    constraint = NA,
                    binary_type = "logical",
                    binary_sep = "/") {

  new_sm_char(x,
              relevant,
              choice_names,
              choice_labels,
              label,
              constraint,
              binary_type,
              binary_sep)
}

#' Low level select multiple list constructor
new_sm_list <- function(x = list(),
                        relevant = NA,
                        choice_names = NA,
                        choice_labels = NA,
                        label = NA,
                        constraint = NA,
                        binary_type = "logical",
                        binary_sep = "/") {
  vec_assert(x, list())
  attr_err(relevant,
           label,
           constraint)
  attr_slct_err(choice_names, choice_labels)
  attr_sm_err(binary_type, binary_sep)
  sm_chc_check(x, choice_names)
  new_list_of(x,
              ptype = character(),
              relevant = relevant,
              choice_names = choice_names,
              choice_labels = choice_labels,
              label = label,
              constraint = constraint,
              binary_type = binary_type,
              binary_sep = binary_sep,
              class = "borg_sm_list")
}


#' Select multiple list constructor
#'
#' `sm_list()` constructs a select multiple list object, where each entry in the list corresponds to the selected
#' options for that survey row. Can be constructed from an existing list of character vectors or a character vector
#' itself that will be turned into a list by splitting on spaces in the string.
#'
#' @importFrom stringr str_split
#'
#' @export
sm_list <- function(x = list(),
                    relevant = NA,
                    choice_names = NA,
                    choice_labels = NA,
                    label = NA,
                    constraint = NA,
                    binary_type = "logical",
                    binary_sep = "/") {

  if (vec_is(x, character())) {
    x <- str_split(x, " ")
  }

  new_sm_list(x,
              relevant,
              choice_names,
              choice_labels,
              label,
              constraint,
              binary_type,
              binary_sep)
}

# FORMATTING FOR PRINTING

#' @importFrom stringr str_count str_c
format.borg_sm_char <- function(x, ...) {
  str_c(str_count(x, " ") + 1, " chosen")
}

#' @importFrom stringr str_c
format.borg_sm_list <- function(x, ...) {
  format_one <- function(x) {
    if (!all(is.na(x))) {
      paste0(length(x), " chosen")
    } else {
      NA
    }
  }
  map_chr(x, format_one)
}

obj_print_data.borg_sm_list <- function(x, ...) {
  if (length(x) == 0)
    return()
  print(format(x), quote = FALSE)
}

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_sm_char <- function(x, ...) {
  "borg_sm_char"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_sm_char <- function(x, ...) {
  "sm_char"
}

#' Full abbreviation in tibbles
vec_ptype_full.borg_sm_list <- function(x, ...) {
  "borg_sm_list"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_sm_list <- function(x, ...) {
  "sm_list"
}

# COERCIONS

#' Boiler plate for coercion for coercion of select multiple characters
#'
#' @method vec_ptype2 borg_sm_char
#' @export
#' @export vec_ptype2.borg_sm_char
vec_ptype2.borg_sm_char <- function(x, y, ...) UseMethod("vec_ptype2.borg_sm_char", y)

#' @method vec_ptype2.borg_sm_char default
#' @export
vec_ptype2.borg_sm_char.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' Boiler plate for coercion of select multiple lists
#'
#' @method vec_ptype2 borg_sm_list
#' @export
#' @export vec_ptype2.borg_sm_list
vec_ptype2.borg_sm_list <- function(x, y, ...) UseMethod("vec_ptype2.borg_sm_list", y)

#' @method vec_ptype2.borg_sm_list default
#' @export
vec_ptype2.borg_sm_list.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION
#' @method vec_ptype2.borg_sm_list borg_sm_list
#' @export
vec_ptype2.borg_sm_list.borg_sm_list <- function(x, y, ...) {
  if (identical_sm_attr(x, y)) {
    new_sm_list()
  } else {
    list()
  }
}

#' @method vec_ptype2.borg_sm_char borg_sm_char
#' @export
vec_ptype2.borg_sm_char.borg_sm_char <- function(x, y, ...) {
  if (identical_sm_attr(x, y)) {
    new_sm_char()
  } else {
    character()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_sm_char character
#' @export
vec_ptype2.borg_sm_char.character <- function(x, y, ...) {
  new_sm_char(relevant = borg_rlvnt(x),
              choice_names = borg_ch_nms(x),
              choice_labels = borg_ch_lbls(x),
              label = borg_lbl(x),
              constraint = borg_cnstrnt(x),
              binary_type = borg_bin_type(x),
              binary_sep = borg_bin_sep(x))
}

#' @method vec_ptype2.character borg_sm_char
#' @export
vec_ptype2.character.borg_sm_char <- function(x, y, ...) {
  new_sm_char(relevant = borg_rlvnt(y),
              choice_names = borg_ch_nms(y),
              choice_labels = borg_ch_lbls(y),
              label = borg_lbl(y),
              constraint = borg_cnstrnt(y),
              binary_type = borg_bin_type(y),
              binary_sep = borg_bin_sep(y))
}


#' @method vec_ptype2.character borg_sm_list
#' @export
vec_ptype2.character.borg_sm_list <- function(x, y, ...) {
  sm_list(relevant = borg_rlvnt(y),
          choice_names = borg_ch_nms(y),
          choice_labels = borg_ch_lbls(y),
          label = borg_lbl(y),
          constraint = borg_cnstrnt(y),
          binary_type = borg_bin_type(y),
          binary_sep = borg_bin_sep(y))
}

#' @method vec_ptype2.borg_sm_list character
#' @export
vec_ptype2.borg_sm_list.character <- function(x, y, ...) {
  sm_list(relevant = borg_rlvnt(x),
          choice_names = borg_ch_nms(x),
          choice_labels = borg_ch_lbls(x),
          label = borg_lbl(x),
          constraint = borg_cnstrnt(x),
          binary_type = borg_bin_type(x),
          binary_sep = borg_bin_sep(x))
}

# CONVERSION BETWEEN EACH OTHER

#' @method vec_ptype2.borg_sm_char borg_sm_list
#' @export
vec_ptype2.borg_sm_char.borg_sm_list <- function(x, y, ...) {
  if(identical_sm_attr(x, y)) {
    new_sm_char(relevant = borg_rlvnt(x),
                choice_names = borg_ch_nms(x),
                choice_labels = borg_ch_lbls(x),
                label = borg_lbl(x),
                constraint = borg_cnstrnt(x),
                binary_type = borg_bin_type(x),
                binary_sep = borg_bin_sep(x))
  } else {
    character()
  }
}

#' @method vec_ptype2.borg_sm_list borg_sm_char
#' @export
vec_ptype2.borg_sm_list.borg_sm_char <- function(x, y, ...) {
  if(identical_sm_attr(x, y)) {
    new_sm_char(relevant = borg_rlvnt(y),
                choice_names = borg_ch_nms(y),
                choice_labels = borg_ch_lbls(y),
                label = borg_lbl(y),
                constraint = borg_cnstrnt(y),
                binary_type = borg_bin_type(y),
                binary_sep = borg_bin_sep(y))
  } else {
    character()
  }
}

# CASTING

#' Boiler plate for casting select multiple character
#'
#' @method vec_cast borg_sm_char
#' @export
#' @export vec_cast.borg_sm_char
vec_cast.borg_sm_char <- function(x, to, ...) UseMethod("vec_cast.borg_sm_char")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_sm_char default
#' @export
vec_cast.borg_sm_char.default <- function(x, to, ...) vec_default_cast(x, to)

#' Boiler plate for casting select multiple list
#'
#' @method vec_cast borg_sm_list
#' @export
#' @export vec_cast.borg_sm_list
vec_cast.borg_sm_list <- function(x, to, ...) UseMethod("vec_cast.borg_sm_list")

#' @method vec_cast.borg_sm_list default
#' @export
vec_cast.borg_sm_list.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' @method vec_cast.borg_sm_list borg_sm_list
#' @export
vec_cast.borg_sm_list.borg_sm_list <- function(x, to, ...) {
  if (identical_sm_attr(x, to)) {
    x
  } else {
    list()
  }
}

#' Casting borg_sm_char to borg_sm_char
#'
#' @method vec_cast.borg_sm_char borg_sm_char
#' @export
vec_cast.borg_sm_char.borg_sm_char <- function(x, to, ...) {
  if (identical_sm_attr(x, to)) {
    x
  } else {
    character()
  }
}

# CASTING BETWEEN CHARACTER

#' Casting borg_sm_char to character
#'
#' @method vec_cast.character borg_sm_char
#' @export
vec_cast.character.borg_sm_char <- function(x, to, ...) vec_data(x)

#' Casting character to borg_sm_character
#'
#' @method vec_cast.borg_sm_char character
#' @export
vec_cast.borg_sm_char.character <- function(x, to, ...) {
  sm_char(x,
          relevant = borg_rlvnt(to),
          choice_names = borg_ch_nms(to),
          choice_labels = borg_ch_lbls(to),
          label = borg_lbl(to),
          constraint = borg_cnstrnt(to),
          binary_type = borg_bin_type(to),
          binary_sep = borg_bin_sep(to))
}

#' @method vec_cast.borg_sm_list character
#' @export
vec_cast.borg_sm_list.character <- function(x, to, ...) {
  sm_list(x,
          relevant = borg_rlvnt(to),
          choice_names = borg_ch_nms(to),
          choice_labels = borg_ch_lbls(to),
          label = borg_lbl(to),
          constraint = borg_cnstrnt(to),
          binary_type = borg_bin_type(to),
          binary_sep = borg_bin_sep(to))
}

#' @importFrom stringr str_c
#' @method vec_cast.character borg_sm_list
#' @export
vec_cast.character.borg_sm_list <- function(x, to, ...) map_chr(x, str_c, collapse = " ")

# CASTING BETWEEN THEMSELVES

#' Casting borg_sm_char to borg_sm_list
#'
#' @importFrom stringr str_split
#'
#' @method vec_cast.borg_sm_list borg_sm_char
#' @export
vec_cast.borg_sm_list.borg_sm_char <- function(x, to, ...) {
  sm_list(str_split(x, " "),
          relevant = borg_rlvnt(to),
          choice_names = borg_ch_nms(to),
          choice_labels = borg_ch_lbls(to),
          label = borg_lbl(to),
          constraint = borg_cnstrnt(to),
          binary_type = borg_bin_type(to))
}

#' Casting borg_sm_list to borg_sm_char
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#'
#' @method vec_cast.borg_sm_char borg_sm_list
#' @export
vec_cast.borg_sm_char.borg_sm_list <- function(x, to, ...) {
  sm_char(map_chr(x, str_c, collapse = " "),
          relevant = borg_rlvnt(to),
          choice_names = borg_ch_nms(to),
          choice_labels = borg_ch_lbls(to),
          label = borg_lbl(to),
          constraint = borg_cnstrnt(to),
          binary_type = borg_bin_type(to),
          binary_sep = borg_bin_sep(to))
}

# HELPER FUNCTIONS FOR CASTING

# SELECT MULTIPLE CHARACTER HELPERS

#' Cast to `borg_sm_char`
#'
#' Cast `x` to a `borg_sm_char` vector
#'
#' @param x An object to coerce to `borg_sm_char`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-sm-char
NULL

#' @rdname cast-sm-char
#' @export
as_sm_char <- function(x, ...) {
  UseMethod("as_sm_char")
}

#' @rdname cast-sm-char
#' @export
as_sm_char.borg_sm_char <- function(x, ...) x

#' @rdname cast-sm-char
#' @export
as_sm_char.character <- function(x,
                                 relevant = NA,
                                 choice_names = NA,
                                 choice_labels = NA,
                                 label = NA,
                                 constraint = NA,
                                 binary_type = "logical",
                                 binary_sep = "/",
                                 ...) {
  vec_cast(x, to = sm_char(relevant = relevant,
                           choice_names = choice_names,
                           choice_labels = choice_labels,
                           label = label,
                           constraint = constraint,
                           binary_type = binary_type,
                           binary_sep = binary_sep))
}

#' @rdname cast-sm-char
#' @export
as_sm_char.borg_sm_list <- function(x, ...) {
  vec_cast(x, to = sm_char(relevant = borg_rlvnt(x),
                           choice_names = borg_ch_nms(x),
                           choice_labels = borg_ch_lbls(x),
                           label = borg_lbl(x),
                           constraint = borg_cnstrnt(x),
                           binary_type = borg_bin_type(x),
                           binary_sep = borg_bin_sep(x)))
}

# SELECT MULTIPLE LIST HELPERS

#' Cast to `borg_sm_list`
#'
#' Cast `x` to a `borg_sm_list` vector
#'
#' @param x An object to coerce to `borg_sm_list`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-sm-list
NULL

#' @rdname cast-sm-list
#' @export
as_sm_list <- function(x, ...) {
  UseMethod("as_sm_list")
}

#' @rdname cast-sm-list
#' @export
as_sm_list.borg_sm_list <- function(x, ...) x

#' @rdname cast-sm-char
#' @export
as_sm_list.character <- function(x,
                                 relevant = NA,
                                 choice_names = NA,
                                 choice_labels = NA,
                                 label = NA,
                                 constraint = NA,
                                 binary_type = "logical",
                                 binary_sep = "/",
                                 ...) {
  vec_cast(x, to = sm_char(relevant = relevant,
                           choice_names = choice_names,
                           choice_labels = choice_labels,
                           label = label,
                           constraint = constraint,
                           binary_type = binary_type,
                           binary_sep = binary_sep))
}

#' @rdname cast-sm-char
#' @export
as_sm_list.borg_sm_char <- function(x, ...) {
  vec_cast(x, to = sm_list(relevant = borg_rlvnt(x),
                           choice_names = borg_ch_nms(x),
                           choice_labels = borg_ch_lbls(x),
                           label = borg_lbl(x),
                           constraint = borg_cnstrnt(x),
                           binary_type = borg_bin_type(x),
                           binary_sep = borg_bin_sep(x)))
}

#' @export
as.character.borg_sm_list <- function(x, ...) {
  vec_cast(x, character())
}
