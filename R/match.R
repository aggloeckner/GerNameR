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
#' @inheritParams partition.names
#' @inheritDotParams names.dist
#'
#' @importFrom rlang enquo !!
#' @importFrom clue solve_LSAP
#'
#' @export
match.pairs <- function(split, discard=0, subset=filter.names(), ...) {
  split.q <- rlang::enquo( split )
  groups <- partition.names( rlang::`!!`(split.q), discard, subset )
  dists <- names.dist( groups, ... )
  if( ncol(dists) < nrow(dists) ){
    rotated <- T
    dists <- t( dists )
  } else {
    rotated <- F
  }
  asgn <- clue::solve_LSAP( dists )

  if(rotated) {
    res <- list( g1 = groups[1, as.numeric(asgn) ], g2 = groups[ 2 ])
    res$dist <- sapply(seq_along(res$g1), function(i) dists[as.character(res$g2[i]),as.character(res$g1[i])])
  } else {
    res <- list( g1 = groups[ 1 ], g2 = groups[2, as.numeric(asgn) ])
    res$dist <- sapply(seq_along(res$g1), function(i) dists[as.character(res$g1[i]),as.character(res$g2[i])])
  }
  ord <- order(res$dist)
  res$g1 <- res$g1[ord]
  res$g2 <- res$g2[ord]
  res$dist <- res$dist[ord]

  class(res) <- c("names.pairs","names.split")
  res
}

#' @export
print.names.pairs <- function(x, ...) {
  rv <- data.frame( name1 = as.character(x$g1),
                    name2 = as.character(x$g2),
                    distance = x$dist )
  print( rv )
}

#'
#'
#'
#' @export
match.groups <- function(split, n, discard=0, subset=filter.names(), penalty.factor=1.5, init.factor=5, ...) {
  split.q <- rlang::enquo( split )
  groups <- partition.names( rlang::`!!`(split.q), discard, subset )
  allnames <- filter.names()
  allnames <- allnames[c(as.numeric(groups[1]),as.numeric(groups[2]))]
  dists <- names.dist(allnames, ...)

  # Maximal distance, so we can penalize too large and too small solutions correctly
  max.dist <- max(dists)
  penalty  <- max.dist * penalty.factor

  # Set up information for GA
  # Number of bits is decided based on elements in both groups
  n.g1 <- length(groups[1])
  n.g2 <- length(groups[2])
  nbits <- n.g1 + n.g2

  # give the algorithm some head start by including possible candidates which
  # have the right number of names in each group
  suggestions <- matrix(0,nrow=200,ncol=nbits)
  for(i in seq(200)) {
    x.g1 <- ceiling(runif(init.factor*n, min=0,    max=n.g1))
    x.g2 <- ceiling(runif(init.factor*n, min=n.g1, max=n.g1+n.g2))
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
    rv <- rv + abs(n-n.s.g1) * penalty.factor
    n.s.g2 <- sum(x[seq(from=n.g1+1,to=n.g1+n.g2)])
    rv <- rv + abs(n-n.s.g2) * penalty.factor
    -rv
  }

  s <- GA::ga( type = "binary", fitness = fitness, nBits = nbits, maxiter = 1000,
               run = 200, popSize = 200, suggestions = suggestions,
               crossover = GA::gabin_uCrossover, pmutation = 0.2,
               monitor = function(obj) {})
  allnames[s@solution==1]
}
