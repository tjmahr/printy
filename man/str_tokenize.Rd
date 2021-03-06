% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringr-like.R
\name{str_tokenize}
\alias{str_tokenize}
\title{Break a string to individual (character) tokens}
\usage{
str_tokenize(string, pattern = NULL)
}
\arguments{
\item{string}{a character vector of strings to break}

\item{pattern}{pattern to use for splitting. Defaults to \code{NULL} so that
strings are split into individual characters.}
}
\value{
a single character vector of the tokens
}
\description{
The usual job of this function is to break a string into a vector of
individual characters, but it can break strings using other separators.
}
\examples{
str_tokenize(c("abc", "de"))
str_tokenize(c("abc de fg"), " ")
}
