# CONSTRUCTORS FOR RANK CLASS

#' Low level rank character constructor
new_rank_char <- function(x = character(),
                          relevant = NA,
                          choice_names = NA,
                          choice_labels = NA,
                          q_name = NA,
                          label = NA,
                          constraint = NA,
                          max_rank = na_length(choice_names),
                          position_sep = "/") {
  vec_assert(x, character())
  sm_chc_check(x, choice_names, type = "char")
  new_vctr(x,
           relevant = relevant,
           choice_names = choice_names,
           choice_labels = choice_labels,
           q_name = q_name,
           label = label,
           constraint = constraint,
           max_rank = max_rank,
           position_sep = position_sep,
           class = "borg_rank_char")
}

#' Get length or return NA if NA
na_length <- function(x) {
  if(all(is.na(x))) NA else length(x[!is.na(x)])
}

#' Rank character constructor
#'
#' `rank_char()` constructs a rank character vector, each value in the vector corresponds to the ordered
#' options for that survey row in string format. Can be constructed from a character vector itself.
#'
#' @importFrom stringr str_split
#'
#' @export
rank_char <- function(x = character(),
                      relevant = NA,
                      choice_names = NA,
                      choice_labels = NA,
                      q_name = NA,
                      label = NA,
                      constraint = NA,
                      max_rank = na_length(choice_names),
                      position_sep = "/") {

  validate_rank(
    new_rank_char(
      x,
      relevant,
      choice_names,
      choice_labels,
      q_name,
      label,
      constraint,
      max_rank,
      position_sep
    )
  )
}

#' Low level rank list constructor
new_rank_list <- function(x = list(),
                          relevant = NA,
                          choice_names = NA,
                          choice_labels = NA,
                          q_name = NA,
                          label = NA,
                          constraint = NA,
                          max_rank = na_length(choice_names),
                          position_sep = "/") {
  vec_assert(x, list())
  sm_chc_check(x, choice_names)
  new_list_of(x,
              ptype = character(),
              relevant = relevant,
              choice_names = choice_names,
              choice_labels = choice_labels,
              q_name = q_name,
              label = label,
              constraint = constraint,
              max_rank = max_rank,
              position_sep = position_sep,
              class = "borg_rank_list")
}


#' Rank list constructor
#'
#' `rank_list()` constructs a rank list object, where each entry in the list corresponds to the ordered
#' option for that survey row. Can be constructed from an existing list of character vectors or a character vector
#' itself that will be turned into a list by splitting on spaces in the string.
#'
#' @importFrom stringr str_split
#'
#' @export
rank_list <- function(x = list(),
                      relevant = NA,
                      choice_names = NA,
                      choice_labels = NA,
                      q_name = NA,
                      label = NA,
                      constraint = NA,
                      max_rank = na_length(choice_names),
                      position_sep = "/") {

  if (vec_is(x, character())) {
    x <- lapply(x, function(x) vec_cast(x, char_helper()))
  }

  validate_rank(
    new_rank_list(
      x,
      relevant,
      choice_names,
      choice_labels,
      q_name,
      label,
      constraint,
      max_rank,
      position_sep
    )
  )
}

#' Validation function for both rank classes
validate_rank <- function(x) {
  attr_err(x)
  attr_slct_err(x)
  attr_rnk_err(x)
  x
}

# FORMATTING FOR PRINTING

#' @importFrom stringr str_split str_c
format.borg_rank_char <- function(x, ...) {
  x <- str_split(x, " ")
  str_c("1 of ", map_dbl(x, length), ": ", map_chr(x, ~ .[1]))
}

#' @importFrom stringr str_c
format.borg_rank_list <- function(x, ...) {
  format_one <- function(x) {
    if (!all(is.na(x))) {
      paste0("1 of ", length(x), ": ", x[1])
    } else {
      NA
    }
  }
  map_chr(x, format_one)
}

obj_print_data.borg_rank_list <- function(x, ...) {
  if (length(x) == 0)
    return()
  print(format(x), quote = FALSE)
}

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_rank_char <- function(x, ...) {
  "borg_rank_char"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_rank_char <- function(x, ...) {
  "rnk_chr"
}

#' Full abbreviation in tibbles
vec_ptype_full.borg_rank_list <- function(x, ...) {
  "borg_rank_list"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_rank_list <- function(x, ...) {
  "rnk_lst"
}

# COERCIONS

#' Boiler plate for coercion for coercion of rank characters
#'
#' @method vec_ptype2 borg_rank_char
#' @export
#' @export vec_ptype2.borg_rank_char
vec_ptype2.borg_rank_char <- function(x, y, ...) UseMethod("vec_ptype2.borg_rank_char", y)

#' @method vec_ptype2.borg_rank_char default
#' @export
vec_ptype2.borg_rank_char.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' Boiler plate for coercion of rank lists
#'
#' @method vec_ptype2 borg_rank_list
#' @export
#' @export vec_ptype2.borg_rank_list
vec_ptype2.borg_rank_list <- function(x, y, ...) UseMethod("vec_ptype2.borg_rank_list", y)

#' @method vec_ptype2.borg_rank_list default
#' @export
vec_ptype2.borg_rank_list.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION
#' @method vec_ptype2.borg_rank_list borg_rank_list
#' @export
vec_ptype2.borg_rank_list.borg_rank_list <- function(x, y, ...) {
  if (identical_rnk_attr(x, y)) {
    x
  } else {
    list()
  }
}

#' @method vec_ptype2.borg_rank_char borg_rank_char
#' @export
vec_ptype2.borg_rank_char.borg_rank_char <- function(x, y, ...) {
  if (identical_rnk_attr(x, y)) {
    x
  } else {
    character()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_rank_char character
#' @export
vec_ptype2.borg_rank_char.character <- function(x, y, ...) x

#' @method vec_ptype2.character borg_rank_char
#' @export
vec_ptype2.character.borg_rank_char <- function(x, y, ...) y

#' @method vec_ptype2.character borg_rank_list
#' @export
vec_ptype2.character.borg_rank_list <- function(x, y, ...) y

#' @method vec_ptype2.borg_rank_list character
#' @export
vec_ptype2.borg_rank_list.character <- function(x, y, ...) x

# COERSION BETWEEN LISTS

#' @method vec_ptype2.list borg_rank_list
#' @export
vec_ptype2.list.borg_rank_list <- function(x, y, ...) y

#' @method vec_ptype2.borg_rank_list list
#' @export
vec_ptype2.borg_rank_list.list <- function(x, y, ...) x

# COERSION BETWEEN EACH OTHER

#' @method vec_ptype2.borg_rank_char borg_rank_list
#' @export
vec_ptype2.borg_rank_char.borg_rank_list <- function(x, y, ...) {
  if(identical_rnk_attr(x, y)) {
    new_rank_char(relevant = borg_rlvnt(x),
                  choice_names = borg_ch_nms(x),
                  choice_labels = borg_ch_lbls(x),
                  q_name = borg_q_name(x),
                  label = borg_lbl(x),
                  constraint = borg_cnstrnt(x),
                  max_rank = borg_max_rank(x),
                  position_sep = borg_pos_sep(x))
  } else {
    character()
  }
}

#' @method vec_ptype2.borg_rank_list borg_rank_char
#' @export
vec_ptype2.borg_rank_list.borg_rank_char <- function(x, y, ...) {
  if(identical_rnk_attr(x, y)) {
    y
  } else {
    character()
  }
}

# CASTING

#' Boiler plate for casting rank character
#'
#' @method vec_cast borg_rank_char
#' @export
#' @export vec_cast.borg_rank_char
vec_cast.borg_rank_char <- function(x, to, ...) UseMethod("vec_cast.borg_rank_char")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_rank_char default
#' @export
vec_cast.borg_rank_char.default <- function(x, to, ...) vec_default_cast(x, to)

#' Boiler plate for casting rank list
#'
#' @method vec_cast borg_rank_list
#' @export
#' @export vec_cast.borg_rank_list
vec_cast.borg_rank_list <- function(x, to, ...) UseMethod("vec_cast.borg_rank_list")

#' @method vec_cast.borg_rank_list default
#' @export
vec_cast.borg_rank_list.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' @method vec_cast.borg_rank_list borg_rank_list
#' @export
vec_cast.borg_rank_list.borg_rank_list <- function(x, to, ...) {
  rank_list(vec_data(x),
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

#' Casting borg_rank_char to borg_rank_char
#'
#' @method vec_cast.borg_rank_char borg_rank_char
#' @export
vec_cast.borg_rank_char.borg_rank_char <- function(x, to, ...) {
  rank_char(vec_data(x),
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

# CASTING BETWEEN CHARACTER

#' Casting borg_rank_char to character
#'
#' @method vec_cast.character borg_rank_char
#' @export
vec_cast.character.borg_rank_char <- function(x, to, ...) vec_data(x)

#' Casting character to borg_rank_character
#'
#' @method vec_cast.borg_rank_char character
#' @export
vec_cast.borg_rank_char.character <- function(x, to, ...) {
  rank_char(x,
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

#' @method vec_cast.borg_rank_list character
#' @export
vec_cast.borg_rank_list.character <- function(x, to, ...) {
  rank_list(x,
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

# CASTING BETWEEN LISTS

#' @method vec_cast.list borg_rank_list
#' @export
vec_cast.list.borg_rank_list <- function(x, to, ...) vec_data(x)

#' @method vec_cast.borg_rank_list list
#' @export
vec_cast.borg_rank_list.list <- function(x, to, ...) {
  rank_list(x,
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

#' @importFrom stringr str_c
#' @method vec_cast.character borg_rank_list
#' @export
vec_cast.character.borg_rank_list <- function(x, to, ...) map_chr(x, str_c, collapse = " ")


# CASTING BETWEEN THEMSELVES

#' Casting borg_rank_char to borg_rank_list
#'
#' @importFrom stringr str_split
#'
#' @method vec_cast.borg_rank_list borg_rank_char
#' @export
vec_cast.borg_rank_list.borg_rank_char <- function(x, to, ...) {
  rank_list(str_split(x, " "),
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

#' Casting borg_rank_list to borg_rank_char
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#'
#' @method vec_cast.borg_rank_char borg_rank_list
#' @export
vec_cast.borg_rank_char.borg_rank_list <- function(x, to, ...) {
  rank_char(map_chr(x, str_c, collapse = " "),
            relevant = borg_rlvnt(to),
            choice_names = borg_ch_nms(to),
            choice_labels = borg_ch_lbls(to),
            q_name = borg_q_name(to),
            label = borg_lbl(to),
            constraint = borg_cnstrnt(to),
            max_rank = borg_max_rank(to),
            position_sep = borg_pos_sep(to))
}

# HELPER FUNCTIONS FOR CASTING

# RANK CHARACTER HELPERS

#' Cast to `borg_rank_char`
#'
#' Cast `x` to a `borg_rank_char` vector
#'
#' @param x An object to coerce to `borg_rank_char`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-rank-char
NULL

#' @rdname cast-rank-char
#' @export
as_rank_char <- function(x, ...) {
  UseMethod("as_rank_char")
}

#' @rdname cast-rank-char
#' @export
as_rank_char.borg_rank_char <- function(x, ...) x

#' @rdname cast-rank-char
#' @export
as_rank_char.character <- function(x,
                                   relevant = NA,
                                   choice_names = NA,
                                   choice_labels = NA,
                                   q_name = NA,
                                   label = NA,
                                   constraint = NA,
                                   max_rank = na_length(choice_names),
                                   position_sep = "/",
                                   ...) {
  vec_cast(x, to = rank_char(relevant = relevant,
                             choice_names = choice_names,
                             choice_labels = choice_labels,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             max_rank = max_rank,
                             position_sep = position_sep))
}

#' @rdname cast-rank-char
#' @export
as_rank_char.borg_rank_list <- function(x, ...) {
  vec_cast(x, to = rank_char(relevant = borg_rlvnt(x),
                             choice_names = borg_ch_nms(x),
                             choice_labels = borg_ch_lbls(x),
                             q_name = borg_q_name(x),
                             label = borg_lbl(x),
                             constraint = borg_cnstrnt(x),
                             max_rank = borg_max_rank(x),
                             position_sep = borg_pos_sep(x)))
}

# RANK LIST HELPERS

#' Cast to `borg_rank_list`
#'
#' Cast `x` to a `borg_rank_list` vector
#'
#' @param x An object to coerce to `borg_rank_list`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-rank-list
NULL

#' @rdname cast-rank-list
#' @export
as_rank_list <- function(x, ...) {
  UseMethod("as_rank_list")
}

#' @rdname cast-rank-list
#' @export
as_rank_list.borg_rank_list <- function(x, ...) x

#' @rdname cast-rank-char
#' @export
as_rank_list.character <- function(x,
                                   relevant = NA,
                                   choice_names = NA,
                                   choice_labels = NA,
                                   q_name = NA,
                                   label = NA,
                                   constraint = NA,
                                   max_rank = na_length(choice_names),
                                   position_sep = "/",
                                   ...) {
  vec_cast(x, to = rank_char(relevant = relevant,
                             choice_names = choice_names,
                             choice_labels = choice_labels,
                             q_name = q_name,
                             label = label,
                             constraint = constraint,
                             max_rank = max_rank,
                             position_sep = position_sep))
}

#' @rdname cast-rank-char
#' @export
as_rank_list.borg_rank_char <- function(x, ...) {
  vec_cast(x, to = rank_list(relevant = borg_rlvnt(x),
                             choice_names = borg_ch_nms(x),
                             choice_labels = borg_ch_lbls(x),
                             q_name = borg_q_name(x),
                             label = borg_lbl(x),
                             constraint = borg_cnstrnt(x),
                             max_rank = borg_max_rank(x),
                             position_sep = borg_pos_sep(x)))
}

#' @export
as.character.borg_rank_list <- function(x, ...) {
  vec_cast(x, character())
}
