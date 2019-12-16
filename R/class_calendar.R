# CONSTRUCTORS FOR CALENDAR CLASS

#' Low level calendar constructor
new_calendar <- function(x = new_date(),
                         relevant = NA,
                         label = NA,
                         constraint = NA) {
  vec_assert(x, new_date())
  new_vctr(x,
           relevant = relevant,
           label = label,
           constraint = constraint,
           class = "borg_calendar")
}

#' Calendar constructor
#'
#' `calendar()` constructs a calendar vector.
#'
#' @importFrom stringr str_detect
#'
#' @export
calendar <- function(x = new_date(),
                     relevant = NA,
                     label = NA,
                     constraint = NA,
                     source = "excel") {
  x <- vec_cast(x, new_date())
  validate_calendar(
    new_calendar(
      x,
      relevant,
      label,
      constraint)
  )
}

#' Validator for calendar class
validate_calendar <- function(x) {
  attr_err(x)
  x
}

# FORMATTING FOR PRINTING
format.borg_calendar <- function(x, ...) {
  x <- vec_cast(x, new_date())
  format.Date(x)
}

# VECTOR NAMES AND ABBREVIATIONS

#' Full abbreviation in tibbles
vec_ptype_full.borg_calendar <- function(x, ...) {
  "borg_calendar"
}

#' Partial abbreviation in tibbles
vec_ptype_abbr.borg_calendar <- function(x, ...) {
  "clndr"
}

# COERCIONS

#' Boiler plate for coercion for coercion of calendars
#'
#' @method vec_ptype2 borg_calendar
#' @export
#' @export vec_ptype2.borg_calendar
vec_ptype2.borg_calendar <- function(x, y, ...) UseMethod("vec_ptype2.borg_calendar", y)

#' @method vec_ptype2.borg_calendar default
#' @export
vec_ptype2.borg_calendar.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# SELF COERCION

#' @method vec_ptype2.borg_calendar borg_calendar
#' @export
vec_ptype2.borg_calendar.borg_calendar <- function(x, y, ...) {
  if (identical_calc_attr(x, y)) {
    new_calendar()
  } else {
    new_date()
  }
}

# COERCION TO CHARACTER
#' @method vec_ptype2.borg_calendar character
#' @export
vec_ptype2.borg_calendar.character <- function(x, y, ...) {
  new_calendar(relevant = borg_rlvnt(x),
               label = borg_lbl(x),
               constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.character borg_calendar
#' @export
vec_ptype2.character.borg_calendar <- function(x, y, ...) {
  new_calendar(relevant = borg_rlvnt(y),
               label = borg_lbl(y),
               constraint = borg_cnstrnt(y))
}

# COERCION TO DATE
#' @method vec_ptype2.borg_calendar Date
#' @export
vec_ptype2.borg_calendar.Date <- function(x, y, ...) {
  new_calendar(relevant = borg_rlvnt(x),
               label = borg_lbl(x),
               constraint = borg_cnstrnt(x))
}

#' @method vec_ptype2.Date borg_calendar
#' @export
vec_ptype2.Date.borg_calendar <- function(x, y, ...) {
  new_calendar(relevant = borg_rlvnt(y),
               label = borg_lbl(y),
               constraint = borg_cnstrnt(y))
}

# CASTING

#' Boiler plate for casting calendar
#'
#' @method vec_cast borg_calendar
#' @export
#' @export vec_cast.borg_calendar
vec_cast.borg_calendar <- function(x, to, ...) UseMethod("vec_cast.borg_calendar")

#' @importFrom vctrs vec_cast vec_default_cast
#'
#' @method vec_cast.borg_calendar default
#' @export
vec_cast.borg_calendar.default <- function(x, to, ...) vec_default_cast(x, to)

# CASTING TO SELF

#' Casting borg_calendar to borg_calendar
#'
#' @method vec_cast.borg_calendar borg_calendar
#' @export
vec_cast.borg_calendar.borg_calendar <- function(x, to, ...) {
  if (identical_borg_attr(x, to)) {
    x
  } else {
    character()
  }
}

# CASTING BETWEEN CHARACTER

#' Casting borg_calendar to character
#'
#' @method vec_cast.character borg_calendar
#' @export
vec_cast.character.borg_calendar <- function(x, to, ...) {
  format(x)
}

#' Casting character to borg_calendaracter
#'
#' @method vec_cast.borg_calendar character
#' @export
vec_cast.borg_calendar.character <- function(x, to, ...) {
  calendar(x,
           relevant = borg_rlvnt(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN DATE

#' Casting borg_calendar to Date
#'
#' @method vec_cast.Date borg_calendar
#' @export
vec_cast.Date.borg_calendar <- function(x, to, ...) new_date(x)

#' Casting Date to borg_calendaracter
#'
#' @method vec_cast.borg_calendar Date
#' @export
vec_cast.borg_calendar.Date <- function(x, to, ...) {
  calendar(x,
           relevant = borg_rlvnt(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}

# CASTING BETWEEN POSIXct

#' Casting borg_calendar to POSIXct
#'
#' @method vec_cast.POSIXct borg_calendar
#' @export
vec_cast.POSIXct.borg_calendar <- function(x, to, ...) as.POSIXct(new_date(x), ...)

#' Casting POSIXct to borg_calendaracter
#'
#' @method vec_cast.borg_calendar POSIXct
#' @importFrom lubridate as_date
#' @export
vec_cast.borg_calendar.POSIXct <- function(x, to, ...) {
  calendar(as_date(x),
           relevant = borg_rlvnt(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}


# CASTING BETWEEN POSIXlt

#' Casting borg_calendar to POSIXlt
#'
#' @method vec_cast.POSIXlt borg_calendar
#' @export
vec_cast.POSIXlt.borg_calendar <- function(x, to, ...) as.POSIXlt(new_date(x), ...)

#' Casting POSIXlt to borg_calendaracter
#'
#' @method vec_cast.borg_calendar POSIXlt
#' @imporFrom lubridate as_date
#' @export
vec_cast.borg_calendar.POSIXlt <- function(x, to, ...) {
  calendar(as_date(x),
           relevant = borg_rlvnt(to),
           label = borg_lbl(to),
           constraint = borg_cnstrnt(to))
}

# HELPER FUNCTIONS FOR CASTING

#' Cast to `borg_calendar`
#'
#' Cast `x` to a `borg_calendar` vector
#'
#' @param x An object to coerce to `borg_calendar`.
#' @param ... Arguments passed on to further methods.
#'
#' @name cast-calendar
NULL

#' @rdname cast-calendar
#' @export
as_calendar <- function(x, ...) {
  UseMethod("as_calendar")
}

#' @rdname cast-calendar
#' @export
as_calendar.borg_calendar <- function(x, ...) x

#' @rdname cast-calendar
#' @export
as_calendar.character <- function(x,
                                  relevant = NA,
                                  label = NA,
                                  constraint = NA,
                                  ...) {
  vec_cast(x, to = calendar(relevant = relevant,
                            label = label,
                            constraint = constraint))
}

#' @export
as.POSIXct.borg_calendar <- function(x, tz = "", ...) {
  as.POSIXct(new_date(x), tz = tz, ...)
}

#' @export
as.Date.borg_calendar <- function(x, ...) {
  new_date(x)
}
#' @export
as.POSIXlt.borg_calendar <- function(x, tz = "", ...) {
  as.POSIXlt(new_date(x), tz = tz, ...)
}


## DEFINING ARITHMETIC

vec_arith.borg_calendar <- function(op, x, y, ...) {
  UseMethod("vec_arith.borg_calendar", y)
}
vec_arith.borg_calendar.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

vec_arith.borg_calendar.borg_calendar <- function(op, x, y, ...) {
  switch(op, `-` = difftime(x, y, units = "days"),
         stop_incompatible_op(op, x, y))
}

vec_arith.numeric.borg_calendar <- function(op, x, y, ...) {
  switch(op, `+` = vec_restore(vec_arith_base(op, x, y), y),
         stop_incompatible_op(op, x, y))
}

vec_arith.borg_calendar.numeric <- function(op, x, y, ...) {
  switch(op,
         `+` = vec_restore(vec_arith_base(op, x, y), x),
         `-` = vec_restore(vec_arith_base(op, x, y), x),
         stop_incompatible_op(op, x, y))
}
