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

columns.princomp <- c("PC1","PC2","PC3","PC4","PC5")
columns.ratings  <- c( "Education", "Age", "Attractiveness", "Intelligence", "Religiousness", "Familiarity",
                       "Sex", "Nationality", "Modern", "Old", "Ageless", "Competence", "Warmth")

match.names <- function(split, attributes = list(),  discard=0, filter=list(lower=list(), upper=list()), include.pc = F) {
  trgt.values <- names.mean.pc[,split]
  # find the target value to perform the split on
  split.trgt <- median( trgt.values )
  
  # Masks for group1 and group2 after splitting
  g1 <- trgt.values < split.trgt
  g2 <- trgt.values > split.trgt
  
  # Also remove "discard" many elements, unless discard <=0 
  if(discard > 0){
    g1 <- trgt.values < quantile(trgt.values[g1],probs=1-discard, names = F)
    g2 <- trgt.values > quantile(trgt.values[g2],probs=  discard, names = F)
  }
  
  flt.msk <- filter.names(lower=filter$lower, upper=filter$upper, type="mask")
  g1 <- g1 & flt.msk
  g2 <- g2 & flt.msk
  idx.g1 <- which(g1)
  idx.g2 <- which(g2)
  
  # First use the principal components for matching (overall similarity of names)
  m <- as.matrix( names.mean.pc[,columns.princomp] )
  
  # Additional attributes (weighted)
  if( length(attributes) > 0 ){
    m <- cbind(m,t(t(names.mean.pc[,names(attributes)]) * unlist(attributes)))
  }
  
  # Pairwise distance matrix
  dists <- pdist::pdist(m, indices.A = idx.g1, indices.B = idx.g2 )
  dists <- as.matrix( dists )
  if( ncol(dists) < nrow(dists) ){
    rotated <- T
    dists <- t( dists )
  } else {
    rotated <- F
  }
  
  asgn <- clue::solve_LSAP(dists)
  
  if(rotated) {
    idx.g1.mtch <- idx.g1[asgn[seq_along(idx.g2)]]
    idx.g2.mtch <- idx.g2
  } else {
    idx.g1.mtch <- idx.g1
    idx.g2.mtch <- idx.g2[asgn[seq_along(idx.g1)]]
  }
  res <- data.frame(name1 = names.mean.pc[idx.g1.mtch, "name"],
                    name2 = names.mean.pc[idx.g2.mtch, "name"])
  
  res$dist <- sapply(seq(nrow(res)), function(i) dists[i,asgn[i]])
  
  res[,columns.ratings] <- (names.mean.pc[idx.g1.mtch,columns.ratings] + names.mean.pc[idx.g2.mtch,columns.ratings])/2
  if(include.pc) {
    res[,columns.princomp] <- (names.mean.pc[idx.g1.mtch,columns.princomp] + names.mean.pc[idx.g2.mtch,columns.princomp])/2
  }
  res[,paste0("diff.",columns.ratings)] <- abs( (names.mean.pc[idx.g1.mtch,columns.ratings] - names.mean.pc[idx.g2.mtch,columns.ratings]) )
  res[order(res$dist),]
}

match.groups <- function(split, n, attributes = list(),  discard=0, filter=list(lower=list(), upper=list()), include.pc = F) {
  # If we just have to create one group of pairs, we can just match names immediately
  if(n <= 1) {
    return( match.names(split, attributes, discard, filter, include.pc) )
  }
  # Otherwise we will group up further
  mtch <- match.groups( split, n-1, attributes, discard, filter, include.pc = T)
  m <- as.matrix( mtch[,columns.princomp])
  # Additional attributes (weighted)
  if( length(attributes) > 0 ){
    m <- cbind(m,t(t(mtch[,names(attributes)]) * unlist(attributes)))
  }
  
  dists <- dist( m )
  dists <- as.matrix( dists )
  if( ncol(dists) < nrow(dists) ){
    rotated <- T
    dists <- t( dists )
  } else {
    rotated <- F
  }
  
  asgn <- clue::solve_LSAP(dists)
  asgn
}
