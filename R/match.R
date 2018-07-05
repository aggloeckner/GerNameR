# Copyright (C) 2018 Tillmann Nett for FernUni Hagen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
###############################################################################

#' Match most similar pairs for trial generation in a subset of names
#'
#' This function matches pairs of names based on similarity to create
#' a set of trials. Names are matched, such that each name is only used
#' once and the sum of the distances over all names in total is minimized.
#'
#' Normally the principal components are used for matching names, but
#' other ratings can also be added and emphasized to create sets
#' of names which are more similar along a special set of ratings.
#'
#' Note: the split and the matching can be performed in one step
#' using the method \code{\link{match.partition}}.
#'
#' @inheritDotParams names.dist
#'
#' @param s The split of the names. Only the first two groups will be used,
#'          if the split contains more than two groups.
#'
#' @importFrom rlang enquo !!
#' @importFrom clue solve_LSAP
#'
#' @examples
#'
#' # Create a split based on Sex
#' s <- partition.names(Sex)
#'
#' # Match pairs of male and female names
#' m <- match.split( s )
#' m
#'
#' # Emphasize on competence and intelligence (weighted 10 times)
#' m <- match.split(s, Competence=10, Intelligence=10)
#'
#'
#' @export
match.split <- function(s, ...) {
  dists <- names.dist( s, ... )
  if( ncol(dists) < nrow(dists) ){
    rotated <- T
    dists <- t( dists )
  } else {
    rotated <- F
  }
  asgn <- clue::solve_LSAP( dists )

  if(rotated) {
    res <- make.split( g1 = s[1, as.numeric(asgn) ], g2 = s[ 2 ])
    res$dist <- sapply(seq_along(res$g1), function(i) dists[as.character(res$g2[i]),as.character(res$g1[i])])
  } else {
    res <- make.split( g1 = s[ 1 ], g2 = s[2, as.numeric(asgn) ])
    res$dist <- sapply(seq_along(res$g1), function(i) dists[as.character(res$g1[i]),as.character(res$g2[i])])
  }
  ord <- order(res$dist)
  res$g1 <- res$g1[ord]
  res$g2 <- res$g2[ord]
  res$dist <- res$dist[ord]

  class(res) <- c("names.pairs",class( res ))
  res
}

#' Match most similar pairs for trial generation in a subset of names
#'
#' This function matches pairs of names based on similarity to create
#' a set of trials. Names are matched, such that each name is only used
#' once and the sum of the distances over all names in total is minimized.
#'
#' Normally the principal components are used for matching names, but
#' other ratings can also be added and emphasized to create sets
#' of names which are more similar along a special set of ratings.
#'
#' Note: This function takes the two groups of names as separate arguments,
#  each of which must contain a subset of the total names.
#'
#' @inheritDotParams match.split
#'
#' @param n1 First subset of the names used in pairing
#' @param n2 Second subset of the names used in pairing
#'
#' @examples
#'
#' # Split the names according to intelligence
#' s <- partition.names(Intelligence)
#'
#' # Further split the names randomly
#' low <- partition.names.random(subset=s[1], prop=c(1,1,1,1))
#' high <- partition.names.random(subset=s[1], prop=c(1,1,1,1))
#'
#' # Create sets with matching high/low intelligence names
#' low_low <- match.pairs(low[1], low[2])
#' low_high <- match.pairs(low[3], high[1])
#' high_low <- match.pairs(high[2], low[4])
#' high_high <- match.pairs(high[3], high[4])
#'
#' @export
match.pairs <- function(n1, n2, ...) {
  s <- make.split( n1, n2 )
  match.split( s )
}

#' Match most similar pairs for trial generation in a subset of names
#'
#' This function matches pairs of names based on similarity to create
#' a set of trials. Names are matched, such that each name is only used
#' once and the sum of the distances over all names in total is minimized.
#'
#' Normally the principal components are used for matching names, but
#' other ratings can also be added and emphasized to create sets
#' of names which are more similar along a special set of ratings.
#'
#' Note: This function is a shortcut to create the split using
#' \code{\link{partition.names}} and match the pairs using
#' \code{\link{match.split}} in a single step.
#'
#' @inheritParams partition.names
#' @inheritDotParams match.split
#'
#' @examples
#'
#' # Just match names split on Sex rating
#' m <- match.pairs(Sex)
#' m
#'
#' # Match names, but discard some which are ambigous in terms of Sex
#' m <- match.pairs(Sex, discard = 0.2)
#' m
#'
#' # First filter unfamiliar and foreign names
#' s <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)
#' m <- match.pairs(Sex, discard = 0.2, subset=s)
#' m
#'
#' # Emphasize on competence and intelligence (weighted 10 times)
#' m <- match.pairs(Sex, discard = 0.2, subset=s, Competence=10, Intelligence=10)
#' m
match.partition <- function(split, discard=0, subset=filter.names(), ...) {
  split.q <- rlang::enquo( split )
  s <- partition.names( !!split.q, discard, subset )
  match.split( s, ...)
}

as.data.frame.names.pairs <- function(x, ...) {
  data.frame( name1 = as.character(x$g1),
              name2 = as.character(x$g2),
              distance = x$dist, stringsAsFactors = F, ... )
}

#' @export
print.names.pairs <- function(x, ...) {
  print( as.data.frame( x ), ... )
}

#' Create a group of names all similar to each other
#'
#' Creates a group of names with n members from two groups, such that all names
#' are as similar to each other as possible. Names are not only similar across groups
#' but also to each other in the group.
#'
#' Uses a genetic algorithm to find the best names, so different results may be
#' found each run.
#'
#' To seed the genetic algorithm with well suited sets, first random sets of names
#' are created. To increase the search space, more names are selected than will later
#' be retrieved for each of the partitions. The number of additional names, which
#' are included is controlled by a ga.param value.
#'
#' @inheritParams partition.names
#' @inheritDotParams names.dist
#'
#' @param n            Number of names from each group to select
#' @param ga.params    Parameters to genetic algorithm
#'                     \describe{
#'                        \item{penalty.factor}{How strongly to penalize candidate solution,
#'                         which do not have the right number of names. Increase, if the wrong
#'                         number of names is returned repeatedly}
#'                        \item{init.factor}{How many more names to include initialy to increase
#'                         the search space.}
#'                        \item{maxiter}{Maximal number of iterations. See \code{\link[GA]{ga}} for details.}
#'                        \item{run}{Number of consecutive generations without improvement. See \code{\link[GA]{ga}}
#'                         for details.}
#'                        \item{popSize}{Initial population size. See \code{\link[GA]{ga}} for details.}
#'                        \item{pmutation}{Mutation probability. See \code{\link[GA]{ga}} for details.}
#'                        \item{pcrossover}{Crossover probability. See \code{\link[GA]{ga}} for details.}
#'                        \item{elitism}{Number of best candidates to keep each iteration.
#'                         See \code{\link[GA]{ga}} for details.}
#'                        \item{parallel}{Should the GA run in parallel? See \code{\link[GA]{ga}} for details.}
#'                        \item{monitor}{Monitor function. See \code{\link[GA]{ga}} for details.}
#'                     }
#'
#'
#' @importFrom GA ga gabin_uCrossover
#' @importFrom stats runif
#'
#' @export
match.groups <- function(split, n, discard=0, subset=filter.names(),
                         ga.params = list(), ...) {

  gap <- list( penalty.factor=1.5, init.factor=5, maxiter = 1000,
               run = 200, popSize = 200, pmutation=0.2, pcrossover = 0.8,
               elitism = 10, parallel=F, monitor = function(obj) {})

  # Get the passed parameters
  namsp <- names(gap)
  gap[(namp <- names(ga.params))] <- ga.params
  if(length(noNms <- namp[!namp %in% namsp])) {
    warning("unknown genetic algorithm parameter: ", paste(noNms, collapse = ", "))
  }


  split.q <- rlang::enquo( split )
  groups <- partition.names( !!split.q, discard, subset )
  allnames <- filter.names()
  allnames <- allnames[c(as.numeric(groups[1]),as.numeric(groups[2]))]
  dists <- names.dist(allnames, ...)

  # Maximal distance, so we can penalize too large and too small solutions correctly
  max.dist <- max(dists)
  penalty  <- max.dist * gap$penalty.factor

  # Set up information for GA
  # Number of bits is decided based on elements in both groups
  n.g1 <- length(groups[1])
  n.g2 <- length(groups[2])
  nbits <- n.g1 + n.g2

  # give the algorithm some head start by including possible candidates which
  # have the right number of names in each group
  suggestions <- matrix(0, nrow=gap$popSize, ncol=nbits)
  for(i in seq(gap$popSize)) {
    x.g1 <- ceiling(stats::runif(gap$init.factor*n, min=0,    max=n.g1))
    x.g2 <- ceiling(stats::runif(gap$init.factor*n, min=n.g1, max=n.g1+n.g2))
    suggestions[i, c(x.g1,x.g2)] <- 1
  }

  # Number of bits for genetic algorithm
  # Cost function for optimization using GA Package
  fitness <- function(x){
    msk <- x == 1
    n.sol <- sum(x)
    # First part of fitness: Overall distance of the selected names
    if(n.sol >= 1) {
      d <- dists[msk,msk]
      rv <- sum(d)/(n.sol*(n.sol-1))
    } else {
      rv <- 0
    }
    # Second part of fitness: Penalty for too large or too small solutions
    n.s.g1 <- sum(x[seq(n.g1)])
    rv <- rv + abs(n-n.s.g1) * penalty
    n.s.g2 <- sum(x[seq(from=n.g1+1,to=n.g1+n.g2)])
    rv <- rv + abs(n-n.s.g2) * penalty
    -rv
  }

  s <- GA::ga( type = "binary", fitness = fitness, nBits = nbits, maxiter = gap$maxiter,
               run = gap$run, popSize = gap$popSize, suggestions = suggestions,
               crossover = GA::gabin_uCrossover, pmutation = gap$pmutation,
               pcrossover = gap$pcrossover, elitism = gap$elitism, monitor = gap$monitor,
               parallel = gap$parallel )
  allnames[s@solution==1]
}
