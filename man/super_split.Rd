% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\name{super_split}
\alias{super_split}
\title{Split a dataframe into a list of (lists of ...) dataframes}
\usage{
super_split(.data, ...)
}
\arguments{
\item{.data}{a dataframe}

\item{...}{(unquoted) names of columns to split by}
}
\value{
a list of dataframes when splitting by a single variable, a list of
lists of dataframes when splitting by 2 variables, and so on.
}
\description{
This function is a streamlined, recursive version of
\code{\link[base:split]{split()}}.
}
\examples{
# some kind of 2 by 2 design
df <- data.frame(
  x =    c(1, 2, 3, 4, 5, 6, 7, 8),
  time = c(1, 1, 2, 2, 1, 1, 2, 2),
  group = c("a", "a", "a", "a", "b", "b", "b", "b")
)

super_split(df, group)

super_split(df, time)

# split by group and then split each of those by time
super_split(df, group, time)
}
