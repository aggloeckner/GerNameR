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

#' Show the ratings for a set of names
#' 
#' @param ...       The ratings that should be retrieved
#' @param subset    The subset for which the ratings should be retrieved
#' 
#' @examples
#' 
#' s <- filter.names(Intelligence > 0.75)
#' 
#' names.ratings(subset=s, Intelligence, Competence)
#' 
#' @export
names.ratings <- function(..., subset = filter.names()) {
  qs <- rlang::ensyms( ... )
  if(length(qs) == 0) {
    return( numeric() )
  }
  ratings <- lapply(qs, rlang::eval_tidy, data=names.mean.pc)
  ratings <- lapply(ratings, as.matrix)
  ratings <- do.call(cbind, ratings)
  colnames(ratings) <- lapply(qs, as.character)
  ratings <- ratings[subset,,drop=F]
  rownames(ratings) <-  as.character(subset)
  ratings
}