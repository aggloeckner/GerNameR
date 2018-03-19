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

})

test_that("Partitions have the correct classes", {
  s0 <- partition.names(Sex)
  s1 <- partition.names(Sex, discard = 0.2)

  n1 <- filter.names(Competence >= 0.5)
  n2 <- filter.names(Competence >= 0.5, Warmth >= 0.5)

  s2 <- partition.names(Sex, subset = n1)
  s3 <- partition.names(Sex, discard = 0.2, subset = n1)

  s4 <- partition.names(Sex, subset = n2)
  s5 <- partition.names(Sex, discard = 0.2, subset = n2)

  n3 <- n2[seq(1,100)]

  s6 <- partition.names(Sex, subset = n3)
  s7 <- partition.names(Sex, discard = 0.2, subset = n3)

  expect_is(s0, "names.split")
  expect_is(s1, "names.split")
  expect_is(s2, "names.split")
  expect_is(s3, "names.split")
  expect_is(s4, "names.split")
  expect_is(s5, "names.split")
  expect_is(s6, "names.split")
  expect_is(s7, "names.split")

  expect_is(s0[1], "names.selection")
  expect_is(s1[1], "names.selection")
  expect_is(s2[1], "names.selection")
  expect_is(s3[1], "names.selection")
  expect_is(s4[1], "names.selection")
  expect_is(s5[1], "names.selection")
  expect_is(s6[1], "names.selection")
  expect_is(s7[1], "names.selection")

  expect_is(s0[2], "names.selection")
  expect_is(s1[2], "names.selection")
  expect_is(s2[2], "names.selection")
  expect_is(s3[2], "names.selection")
  expect_is(s4[2], "names.selection")
  expect_is(s5[2], "names.selection")
  expect_is(s6[2], "names.selection")
  expect_is(s7[2], "names.selection")

})

test_that("Matchings have the correct classes", {
  s <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)

  m1 <- match.pairs(Sex)
  m2 <- match.pairs(Sex, discard = 0.2)
  m3 <- match.pairs(Sex, discard = 0.2, subset=s)
  m4 <- match.pairs(Sex, discard = 0.2, subset=s, Competence=10, Intelligence=10)

  expect_is(m1, "names.pairs")
  expect_is(m2, "names.pairs")
  expect_is(m3, "names.pairs")
  expect_is(m4, "names.pairs")

  expect_is(m1, "names.split")
  expect_is(m2, "names.split")
  expect_is(m3, "names.split")
  expect_is(m4, "names.split")

  expect_is(m1[1], "names.selection")
  expect_is(m2[1], "names.selection")
  expect_is(m3[1], "names.selection")
  expect_is(m4[1], "names.selection")

  expect_is(m1[2], "names.selection")
  expect_is(m2[2], "names.selection")
  expect_is(m3[2], "names.selection")
  expect_is(m4[2], "names.selection")

  ### Group matching
  g1 <- match.groups(Sex, n=2, discard = 0.2, ga.params = list(run=10, maxiter = 100, popSize = 20))
  g2 <- match.groups(Sex, n=2, discard = 0.2, subset = s, ga.params = list(run=10, maxiter = 100, popSize = 20))

  expect_is(g1, "names.selection")
  expect_is(g2, "names.selection")
})
