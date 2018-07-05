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

# A selection is a subset of the names, which is stored as an
# index into the complete name set for simplicity.
#
# Since indexes into sets can take different form in R, e.g. a vector
# of logicals which is True for the selecte positions or a vector
# of numericals with the index numbers, we use different representations
# depending on context and convert between them as needed.
# In some cases, this may cause us to loose information, such as duplicates
# or ordering structure, but the operations themselves should usually clearly
# show that this information is lost (e.g. using & on a numeric representation
# looses duplicates, as there is no easy way to keep this information).


make.names.selection <- function(v) {
  rv <- v
  class(rv) <- c("names.selection", class(rv))
  rv
}

#' Filter a subset from the names based on percentiles
#'
#' This method filters a set of names from the complete
#' set. The set is filtered based on predicates, which
#' are evaluated in relation to the percentiles of the
#' given variable (see examples). Multiple predicates
#' are first all evaluated on the complete dataset and
#' then combined (i.e. the percentiles are not
#' re-evaluated in the filtered dataset).
#'
#' @param ... Predicates on the dataset using non-standard evaluation
#'
#' @examples
#'
#' # Retrieve all names without any further reduction
#' filter.names()
#'
#' # Retrieve all names with Intelligence ratings above the median
#' filter.names(Intelligence >= 0.5)
#'
#' # Retrieve all names, which are not too uncommon (Familiarity at least
#' # in the 20th percentile) and for which Intelligence is above the
#' # median
#' filter.names(Familiarity >= 0.2, Intelligence >= 0.5)
#'
#' @importFrom rlang quos eval_tidy
#'
#' @export
filter.names <- function(...) {
  qs   <- rlang::quos( ... )
  if(length(qs) == 0) {
    rv <- rep(T, nrow( names.mean.pc) )
  } else {
    # Create a matrix of percentiles for all attributes
    percs <- seq(0,1,length.out = nrow(names.mean.pc) )
    names.percs <- apply( names.mean.pc[,columns.ratings], MARGIN = 2, function(clm) percs[rank(clm, ties.method="min")])

    msks <- lapply(qs, rlang::eval_tidy, data = as.data.frame( names.percs ) )
    msks <- do.call( cbind, msks )
    rv <- apply(msks, MARGIN=1, all)
  }

  make.names.selection( rv )
}

#' @S3method as.logical names.selection
as.logical.names.selection <- function(x, ...) {
  UseMethod("as.logical.names.selection")
}

#' @S3method as.logical.names.selection logical
as.logical.names.selection.logical <- function(x, ...) {
  # just strip off class type
  rv <- x
  class(rv) <- "logical"
  rv
}

#' @S3method as.logical.names.selection numeric
as.logical.names.selection.numeric <- function(x, ...) {
  rv <- rep(F, nrow(names.mean.pc))
  rv[x] <- T
  rv
}

#' @S3method as.double names.selection
as.double.names.selection <- function(x, ...) {
  UseMethod("as.double.names.selection")
}

#' @S3method as.double.names.selection logical
as.double.names.selection.logical <- function(x, ...) {
  rv <- as.double( which( x ) )
  rv
}

#' @S3method as.double.names.selection numeric
as.double.names.selection.numeric <- function(x, ...) {
  # Just strip of class type
  rv <- x
  class(rv) <- "numeric"
  rv
}

#' @export
as.character.names.selection <- function(x, ...) {
  # We can select here independently of the actual representations
  # so we do not need any conversion
  names.mean.pc[x, "name"]
}

#' @export
as.data.frame.names.selection <- function(x, ...) {
  data.frame( name = as.character(x), stringsAsFactors = F, ... )
}

#' @export
print.names.selection <- function(x, ... ) {
  print( as.data.frame( x ), ... )
}

#' @export
`&.names.selection` <- function(x,y) {
  # To intersect two selections, we must ignore the ordering
  # and multiples, so we always have to convert to logical
  # first
  rv <- as.logical( x ) & as.logical( y )
  class(rv) <- c("names.selection",class(rv))
  rv
}

#' @export
`[.names.selection` <- function(x, i, ...) {
  # If we select, we may always have multiples
  # so we must always return a numeric index
  #
  # Convert to numeric index first, so we know
  # that we can just select correctly

  x.numeric <- as.numeric( x )
  # Select the correct elements
  rv <- x.numeric[ i ]
  class(rv) <- c("names.selection", class(rv))
  rv
}

#' @S3method length names.selection
length.names.selection <- function(x) {
  UseMethod("length.names.selection")
}

#' @S3method length.names.selection logical
length.names.selection.logical <- function(x) {
  sum(x)
}

#' @S3method length.names.selection numeric
length.names.selection.numeric <- function(x) {
  length( as.numeric( x ) )
}
