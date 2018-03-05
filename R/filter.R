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

filter.names <- function(lower=list(), upper=list(), type=c("names","index","mask")) {

  type <- match.arg(type)
  
  if( length(lower) > 0) {
    lower.trgt <- sapply(names(lower), function(cl) quantile(names.mean.pc[,cl], lower[[cl]], names = F) )
    lower.keep <- apply(X=names.mean.pc[,names(lower),drop=F] >= lower.trgt, MARGIN = 1, FUN=all)
  } else {
    lower.keep <- T
  }
  
  if( length(upper) > 0 ){
    upper.trgt <- sapply(names(upper), function(cl) quantile(names.mean.pc[,cl], 1-upper[[cl]], names = F) )
    upper.keep <- apply(X=names.mean.pc[,names(upper),drop=F] <= upper.trgt, MARGIN = 1, FUN=all)
  } else {
    upper.keep <- T
  }
  switch(type,
         names = names.mean.pc[lower.keep & upper.keep, "name"],
         index = which( lower.keep & upper.keep ),
         mask  = lower.keep & upper.keep )
}