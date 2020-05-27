# CONSTRUCTORS FOR KIBBLE CLASS

#' @importFrom tibble tibble new_tibble
#' @importFrom assertthat assert_that
new_kibble <- function(df = tibble(),
                       survey = NA,
                       choices = NA) {
  assert_that(is.data.frame(df))
  new_tibble(df,
             survey = survey,
             choices = choices,
             nrow = nrow(df),
             class = "kibble")
}

#' Kibble constructor
#'
#' `kibble()` constructs a kibble data frame.
#'
#' @export
kibble <- function(df = tibble(),
                   survey = NA,
                   choices = NA) {
  new_kibble(df,
             survey = survey,
             choices = choices)
}
