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

#' Split a set of names into two subsets based on a rating
#'
#' Creates a median split based on a given rating. In addition a
#' percentage of names to close to the median can be discarded.
#' Also allows further splitting already selected subsets of names.
#'
#' @param split       The name of the rating on which the split
#'                    should be performed
#'
#' @param discard     The percentage of names too close to the median
#'                    which should be discarded (default: 0, i.e. keep
#'                    all names)
#'
#' @param subset      An optional subset on which the split should be
#'                    done. If this is left out, the split will
#'                    be created on all names.
#'
#' @return An S3 object of class "names.split". The individual
#'         groups can be retrieved using the [] with the first index
#'         1 for the first group or 2 for the second group.
#'
#' @examples
#'
#' # Split all names along the rating "Sex"
#' s <- partition.names(Sex)
#'
#' # Female names
#' s[1]
#' # Male names
#' s[2]
#'
#' # Same as before, but remove ambigous names (20%)
#' s <- partition.names(Sex, discard=0.2)
#'
#' # Female names
#' s[1]
#' # Male names
#' s[2]
#'
#' # First filter on Competence, then split according to Sex
#' n <- filter.names(Competence >= 0.5)
#' s <- partition.names(Sex, discard=0.2, subset=n)
#'
#' # Female names
#' s[1]
#' # Male names
#' s[2]
#'
#' @importFrom rlang enquo
#' @importFrom rlang !!
#'
#' @export
partition.names <- function(split, discard=0, subset = filter.names()) {

  # We want to make the split work, both if we are given
  # a name, as well as when we are given a character
  split.q <- rlang::enquo( split )

  if( 0 > discard | discard >= 1 ) {
    stop( "Cannot discard ", discard*100, "% of data" )
  }

  # Cutoff values for both groups
  # Centered around the median, with ambigous elements removed
  trgt.high <- 0.5 + discard/2
  trgt.low  <- 0.5 - discard/2

  g1 <- filter.names( !!split.q < trgt.low )
  g2 <- filter.names( !!split.q > trgt.high )

  rv <- list(low = g1 & subset, high = g2 & subset)
  class(rv) <- "names.split"
  rv
}

#' Split a set of names into random subsets
#'
#' Creates a random split of a subset or all naes. Names can be split
#' into subsets of different size based on the proportion of the names
#' in each subset after the split.
#'
#' @param subset      An optional subset on which the split should be
#'                    done. If this is left out, the split will
#'                    be created on all names.
#'
#' @param prop        Proportion of the names in each subset (defaults to two groups
#'                    of equal size). Proportions are given as a vector with the
#'                    proportion in each group. Proportions are automatically
#'                    normalized.
#'
#' @return An S3 object of class "names.split". The individual
#'         groups can be retrieved using the operator [].
#'
#' @examples
#'
#' # Two random groups of equal sizes from all names
#' s <- partition.names.random()
#'
#' # Three groups of different proportions (first group twice as large)
#' s <- partition.names.random( prop = c(2,1,1) )
#'
#' # First filter on Competence, then split randomly
#' n <- filter.names(Competence >= 0.5)
#' s <- partition.names.random(subset=n)
#'
#' @export
partition.names.random <- function(subset = filter.names(), prop = c(0.5,0.5)) {
  idx <- as.numeric( subset )
  idx <- sample(idx)

  c <- seq_along( idx )

  # Normalize proportions
  prop <- (prop / sum(prop)) * length(idx)

  # upper and lower index for each group
  upper <- ceiling( cumsum( prop ) )
  lower <- c(0, head( upper, -1) )

  msks <- lapply(seq_along(upper), function(i) (lower[i] < c) & (c <= upper[i]))
  rv <- lapply(msks, function(msk) make.names.selection( sort( idx[msk] ) ) )
  class(rv) <- "names.split"
  rv
}

#' @export
`[.names.split` <- function(x, i, j, ...) {
  # No second index, just get the corresponding group
  if( missing(j) ) {
    return( x[[i]] )
  }

  x[[i]][ j ]
}
