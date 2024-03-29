% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{missingness_patterns}
\alias{missingness_patterns}
\title{missingness patterns}
\usage{
missingness_patterns(
  df,
  min_freq = ifelse(relative, 1/nrow(df), 1),
  long_pattern = FALSE,
  print_legend = ifelse(long_pattern, FALSE, TRUE),
  show_culprit = TRUE,
  relative = FALSE,
  omit_complete = TRUE
)
}
\arguments{
\item{df}{dataset}

\item{min_freq}{show only patterns that occur at least this often. Defaults to 1 observation.}

\item{long_pattern}{by default (FALSE) only shows column indices for space and legibility reasons.}

\item{print_legend}{prints a legend for the column indices, defaults to FALSE if long_pattern is set}

\item{show_culprit}{defaults to TRUE. In case a missingness pattern boils down to one variable, it will be shown here.}

\item{relative}{defaults to FALSE. If true, percentages are shown (relative to total before excluding minimum frequency).}

\item{omit_complete}{defaults to TRUE. Columns that don't have any missings are excluded.}
}
\description{
this function shows how common possible missingness patterns are. Emulates misschk in stata.
\enumerate{
\item excludes any variables that don't have any missings, so as not to clutter output. Disable using omit_complete
\item sorts variables by number of missings, so that the usual suspects show up at the front.
\item displays number of missings accounted for by each pattern
}
}
\examples{
data(ChickWeight)
ChickWeight[1:2,c('weight','Chick')] = NA
ChickWeight[3:5,'Diet'] = NA
names(ChickWeight); nrow(ChickWeight)
missingness_patterns(ChickWeight)
}
