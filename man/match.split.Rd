% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{match.split}
\alias{match.split}
\title{Match most similar pairs for trial generation in a subset of names}
\usage{
match.split(s, ...)
}
\arguments{
\item{s}{The split of the names. Only the first two groups will be used,
if the split contains more than two groups.}

\item{...}{Arguments passed on to \code{names.dist}
\describe{
  \item{x}{The object containing the names}
}}
}
\description{
This function matches pairs of names based on similarity to create
a set of trials. Names are matched, such that each name is only used
once and the sum of the distances over all names in total is minimized.
}
\details{
Normally the principal components are used for matching names, but
other ratings can also be added and emphasized to create sets
of names which are more similar along a special set of ratings.

Note: the split and the matching can be performed in one step
using the method \code{\link{match.partition}}.
}
\examples{

# Create a split based on Sex
s <- partition.names(Sex)

# Match pairs of male and female names
m <- match.split( s )
m

# Emphasize on competence and intelligence (weighted 10 times)
m <- match.split(s, Competence=10, Intelligence=10)


}
