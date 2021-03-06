% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc.R
\name{cc}
\alias{cc}
\title{Combine comma separated strings}
\usage{
cc(x, simplify)
}
\arguments{
\item{x}{Vector of comma separated character strings}

\item{simplify}{Logical indicating whether to return a character vector
(the default) when the length of x is one. This argument does nothing
if the length of x is greater than 1.}
}
\value{
If length of x is 1 then a character vector otherwise a list of
  character vectors.
}
\description{
Split strings by comma into character vector(s)
}
\examples{

## comma separated alphabet
abcs <- paste(letters, collapse = ",")

## split single string
cc(abcs)

## return as list
cc(abcs, simplify = FALSE)

## select columns
mtcars[, cc("cyl,mpg,wt,gear")]

## character vector with multiple strings
x <- c("v1,v2,v3", "y1,y2,y5")

## convert strings into list of [split] character vectors
cc(x)

}
