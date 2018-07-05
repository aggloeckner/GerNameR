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

context("Class Types")

###############################################

test_that("Selections have the correct classes", {

	s0 <- filter.names()
	s1 <- filter.names(Intelligence >= 0.5)
	s2 <- filter.names(Intelligence >= 0.5, Familiarity >= 0.5)

	# Basic types
	expect_is(s0, "names.selection")
	expect_is(s1, "names.selection")
	expect_is(s2, "names.selection")

  # With operators
  so1 <- s0 & s1
  so2 <- s0[seq(1,100)]

  expect_is(so1, "names.selection")
  expect_is(so2, "names.selection")

  # Conversions
  expect_is(as.numeric(s0), "numeric")
  expect_is(as.numeric(s1), "numeric")
  expect_is(as.numeric(s2), "numeric")
  expect_is(as.numeric(so1), "numeric")
  expect_is(as.numeric(so2), "numeric")


  expect_is(as.logical(s0), "logical")
  expect_is(as.logical(s1), "logical")
  expect_is(as.logical(s2), "logical")
  expect_is(as.logical(so1), "logical")
  expect_is(as.logical(so2), "logical")

  expect_is(as.data.frame(s0), "data.frame")
  expect_is(as.data.frame(s1), "data.frame")
  expect_is(as.data.frame(s2), "data.frame")
  expect_is(as.data.frame(so1), "data.frame")
  expect_is(as.data.frame(so2), "data.frame")

})

test_that("Partitions have the correct classes", {
  ##### Setup

  n1 <- filter.names(Competence >= 0.5)
  n2 <- filter.names(Competence >= 0.5, Warmth >= 0.5)
  n3 <- n2[seq(1,100)]

  ##### Attribute Splits

  s0 <- partition.names(Sex)
  s1 <- partition.names(Sex, discard = 0.2)

  expect_is(s0, "names.split")
  expect_is(s1, "names.split")
  expect_is(s0[1], "names.selection")
  expect_is(s1[1], "names.selection")
  expect_is(s0[2], "names.selection")
  expect_is(s1[2], "names.selection")


  s2 <- partition.names(Sex, subset = n1)
  s3 <- partition.names(Sex, discard = 0.2, subset = n1)

  expect_is(s2, "names.split")
  expect_is(s3, "names.split")
  expect_is(s2[1], "names.selection")
  expect_is(s3[1], "names.selection")
  expect_is(s2[2], "names.selection")
  expect_is(s3[2], "names.selection")


  s4 <- partition.names(Sex, subset = n2)
  s5 <- partition.names(Sex, discard = 0.2, subset = n2)

  expect_is(s4, "names.split")
  expect_is(s5, "names.split")
  expect_is(s4[1], "names.selection")
  expect_is(s5[1], "names.selection")
  expect_is(s4[2], "names.selection")
  expect_is(s5[2], "names.selection")


  s6 <- partition.names(Sex, subset = n3)
  s7 <- partition.names(Sex, discard = 0.2, subset = n3)

  expect_is(s6, "names.split")
  expect_is(s7, "names.split")
  expect_is(s6[1], "names.selection")
  expect_is(s7[1], "names.selection")
  expect_is(s6[2], "names.selection")
  expect_is(s7[2], "names.selection")

  ##### Random splits

  s8 <- partition.names.random()
  s9 <- partition.names.random(subset = n1)
  s10 <- partition.names.random(subset = n2)
  s11 <- partition.names.random(subset = n3)

  expect_is(s8, "names.split")
  expect_is(s9, "names.split")
  expect_is(s10, "names.split")
  expect_is(s11, "names.split")
  expect_is(s8[1], "names.selection")
  expect_is(s9[1], "names.selection")
  expect_is(s10[1], "names.selection")
  expect_is(s11[1], "names.selection")
  expect_is(s8[2], "names.selection")
  expect_is(s9[2], "names.selection")
  expect_is(s10[2], "names.selection")
  expect_is(s11[2], "names.selection")


  s12 <- partition.names.random(prop = c(1,2))
  s13 <- partition.names.random(subset = n1, prop = c(1,2))
  s14 <- partition.names.random(subset = n2, prop = c(1,2))
  s15 <- partition.names.random(subset = n3, prop = c(1,2))

  expect_is(s12, "names.split")
  expect_is(s13, "names.split")
  expect_is(s14, "names.split")
  expect_is(s15, "names.split")
  expect_is(s12[1], "names.selection")
  expect_is(s13[1], "names.selection")
  expect_is(s14[1], "names.selection")
  expect_is(s15[1], "names.selection")
  expect_is(s12[2], "names.selection")
  expect_is(s13[2], "names.selection")
  expect_is(s14[2], "names.selection")
  expect_is(s15[2], "names.selection")


  s16 <- partition.names.random(prop = c(1,2,3))
  s17 <- partition.names.random(subset = n1, prop = c(1,2,3))
  s18 <- partition.names.random(subset = n2, prop = c(1,2,3))
  s19 <- partition.names.random(subset = n3, prop = c(1,2,3))

  expect_is(s16, "names.split")
  expect_is(s17, "names.split")
  expect_is(s18, "names.split")
  expect_is(s19, "names.split")
  expect_is(s16[1], "names.selection")
  expect_is(s17[1], "names.selection")
  expect_is(s18[1], "names.selection")
  expect_is(s19[1], "names.selection")
  expect_is(s16[2], "names.selection")
  expect_is(s17[2], "names.selection")
  expect_is(s18[2], "names.selection")
  expect_is(s19[2], "names.selection")
  expect_is(s16[3], "names.selection")
  expect_is(s17[3], "names.selection")
  expect_is(s18[3], "names.selection")
  expect_is(s19[3], "names.selection")
})

test_that("Matchings have the correct classes", {
  s <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)

  m1 <- match.pairs(Sex)

  expect_is(m1, "names.pairs")
  expect_is(m1, "names.split")
  expect_is(m1[1], "names.selection")
  expect_is(m1[2], "names.selection")
  # Conversion to data.frame
  expect_is(as.data.frame(m1), "data.frame")


  m2 <- match.pairs(Sex, discard = 0.2)

  expect_is(m2, "names.pairs")
  expect_is(m2, "names.split")
  expect_is(m2[1], "names.selection")
  expect_is(m2[2], "names.selection")
  # Conversion to data.frame
  expect_is(as.data.frame(m2), "data.frame")


  m3 <- match.pairs(Sex, discard = 0.2, subset=s)

  expect_is(m3, "names.pairs")
  expect_is(m3, "names.split")
  expect_is(m3[1], "names.selection")
  expect_is(m3[2], "names.selection")
  # Conversion to data.frame
  expect_is(as.data.frame(m3), "data.frame")


  m4 <- match.pairs(Sex, discard = 0.2, subset=s, Competence=10, Intelligence=10)

  expect_is(m4, "names.pairs")
  expect_is(m4, "names.split")
  expect_is(m4[1], "names.selection")
  expect_is(m4[2], "names.selection")
  # Conversion to data.frame
  expect_is(as.data.frame(m4), "data.frame")

  ##### Group matching

  g1 <- match.groups(Sex, n=2, discard = 0.2, ga.params = list(run=10, maxiter = 100, popSize = 20))

  expect_is(g1, "names.selection")


  g2 <- match.groups(Sex, n=2, discard = 0.2, subset = s, ga.params = list(run=10, maxiter = 100, popSize = 20))

  expect_is(g2, "names.selection")
})
