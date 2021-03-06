% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_rank.R
\name{rank_list}
\alias{rank_list}
\title{Rank list constructor}
\usage{
rank_list(
  x = list(),
  relevant = NA,
  choice_names = NA,
  choice_labels = NA,
  q_name = NA,
  label = NA,
  constraint = NA,
  max_rank = na_length(choice_names),
  position_sep = "/"
)
}
\description{
`rank_list()` constructs a rank list object, where each entry in the list corresponds to the ordered
option for that survey row. Can be constructed from an existing list of character vectors or a character vector
itself that will be turned into a list by splitting on spaces in the string.
}
