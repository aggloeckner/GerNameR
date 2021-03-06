% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{match.partition}
\alias{match.partition}
\title{Match most similar pairs for trial generation in a subset of names}
\usage{
match.partition(split, discard = 0, subset = filter.names(), ...)
}
\arguments{
\item{split}{The name of the rating on which the split
should be performed}

\item{discard}{The percentage of names too close to the median
which should be discarded (default: 0, i.e. keep
all names)}

\item{subset}{An optional subset on which the split should be
done. If this is left out, the split will
be created on all names.}

\item{...}{Arguments passed on to \code{match.split}
\describe{
  \item{s}{The split of the names. Only the first two groups will be used,
if the split contains more than two groups.}
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

Note: This function is a shortcut to create the split using
\code{\link{partition.names}} and match the pairs using
\code{\link{match.split}} in a single step.
}
\examples{

# Just match names split on Sex rating
m <- match.partition(Sex)
m

# Match names, but discard some which are ambigous in terms of Sex
m <- match.partition(Sex, discard = 0.2)
m

# First filter unfamiliar and foreign names
s <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)
m <- match.partition(Sex, discard = 0.2, subset=s)
m

# Emphasize on competence and intelligence (weighted 10 times)
m <- match.partition(Sex, discard = 0.2, subset=s, Competence=10, Intelligence=10)
m

}
