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

#' Calculate distance between sets of names
#'
#' @param x The object containing the names
#' @param ... Further weighting of attributes (see examples)
#'
#' @export
names.dist <- function(x, ...) {
  UseMethod( "names.dist", x )
}

#' Calculate the distance between all pairs of names in a set
#'
#' @importFrom rlang quos eval_tidy
#' @importFrom stats dist
#'
#' @inheritParams names.dist
#'
#' @export
names.dist.names.selection <- function(x, ...) {
  qs <- rlang::quos( ... )
  # TODO: make it possible to ignore overall similarity and just select specific ratings
  m1 <- as.matrix( names.mean.pc[x, columns.princomp] )
  # TODO: Split name set into set of PCs and Ratings, so we cannot accidentally weigth a PC here
  m2 <- lapply(names(qs), function(n) names.mean.pc[x, n] * rlang::eval_tidy(qs[[n]]))
  m <- do.call(cbind, c(m2,list(m1)))
  rv <- stats::dist( m )
  rv <- as.matrix( rv )
  rownames(rv) <- as.character( x )
  colnames(rv) <- as.character( x )
  rv
}

#' Calculate the distance between all names from two groups of names
#'
#' @inheritParams names.dist
#'
#' @importFrom rlang quos eval_tidy
#' @importFrom pdist pdist
#'
#' @export
names.dist.names.split <- function(x, ...) {
  qs <- rlang::quos( ... )

  # First use the principal components for matching (overall similarity of names)
  m1 <- as.matrix( names.mean.pc[,columns.princomp] )
  m2 <- lapply(names(qs), function(n) names.mean.pc[,n] * rlang::eval_tidy(qs[[n]]))

  m <- do.call(cbind, c(m2,list(m1)))
  # Pairwise distance matrix
  rv <- pdist::pdist(m, indices.A = as.numeric( x[ 1 ] ), indices.B = as.numeric( x[ 2 ] ) )
  rv <- as.matrix( rv )
  rownames( rv ) <- as.character( x[ 1 ] )
  colnames( rv ) <- as.character( x[ 2 ] )
  rv
}
