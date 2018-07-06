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

context("Partition")

###############################################

test_that("Partitions have correct sizes", {
  ##### Setup

  n1 <- filter.names()
  n2 <- filter.names(Competence >= 0.5)

  ##### Attribute Splits

  s1 <- partition.names(Sex)
  s2 <- partition.names(Sex, subset = n1)
  s3 <- partition.names(Sex, subset = n2)

  # No items are lost or added
  expect_equal( length(s1[1]) + length(s1[2]), length(n1))
  expect_equal( length(s2[1]) + length(s2[2]), length(n1))
  expect_equal( length(s3[1]) + length(s3[2]), length(n2))

  s4 <- partition.names(Sex, discard=0.2)
  s5 <- partition.names(Sex, discard=0.2, subset = n1)
  s6 <- partition.names(Sex, discard=0.2, subset = n2)

  # No items are added
  expect_true( (length(s4[1]) + length(s4[2])) <= length(n1))
  expect_true( (length(s5[1]) + length(s5[2])) <= length(n1))
  expect_true( (length(s6[1]) + length(s6[2])) <= length(n2))

  #### Random Splits

  s7 <- partition.names.random()
  s8 <- partition.names.random(subset = n1)
  s9 <- partition.names.random(subset = n2)

  # all items are kept, none are added
  expect_equal( length(s7[1]) + length(s7[2]), length(n1))
  expect_equal( length(s8[1]) + length(s8[2]), length(n1))
  expect_equal( length(s9[1]) + length(s9[2]), length(n2))
  # Correct proportions
  expect_true(abs(length(s7[1]) - length(s7[2])) <= 1)
  expect_true(abs(length(s8[1]) - length(s8[2])) <= 1)
  expect_true(abs(length(s9[1]) - length(s9[2])) <= 1)


  s10 <- partition.names.random(prop = c(1,2))
  s11 <- partition.names.random(subset = n1, prop = c(1,2))
  s12 <- partition.names.random(subset = n2, prop = c(1,2))

  # all items are kept, none are added
  expect_equal( length(s10[1]) + length(s10[2]), length(n1))
  expect_equal( length(s11[1]) + length(s11[2]), length(n1))
  expect_equal( length(s12[1]) + length(s12[2]), length(n2))
  # Correct proportions
  expect_true(abs(length(s10[1]) - length(s10[2])/2) <= 1)
  expect_true(abs(length(s11[1]) - length(s11[2])/2) <= 1)
  expect_true(abs(length(s12[1]) - length(s12[2])/2) <= 1)


  s13 <- partition.names.random(prop = c(1,2,4))
  s14 <- partition.names.random(subset = n1, prop = c(1,2,4))
  s15 <- partition.names.random(subset = n2, prop = c(1,2,4))

  # all items are kept, none are added
  expect_equal( length(s13[1]) + length(s13[2]) + length(s13[3]), length(n1))
  expect_equal( length(s14[1]) + length(s14[2]) + length(s14[3]), length(n1))
  expect_equal( length(s15[1]) + length(s15[2]) + length(s15[3]), length(n2))
  # Correct proportions
  expect_true(abs(length(s13[1]) - length(s13[2])/2) <= 1)
  expect_true(abs(length(s14[1]) - length(s14[2])/2) <= 1)
  expect_true(abs(length(s15[1]) - length(s15[2])/2) <= 1)
  expect_true(abs(length(s13[1]) - length(s13[3])/4) <= 1)
  expect_true(abs(length(s14[1]) - length(s14[3])/4) <= 1)
  expect_true(abs(length(s15[1]) - length(s15[3])/4) <= 1)
  expect_true(abs(length(s13[2]) - length(s13[3])/2) <= 1)
  expect_true(abs(length(s14[2]) - length(s14[3])/2) <= 1)
  expect_true(abs(length(s15[2]) - length(s15[3])/2) <= 1)

})

test_that("Partitions are disjoint", {
  ##### Setup

  n1 <- filter.names()
  n2 <- filter.names(Competence >= 0.5)

  ##### Attribute Splits

  s1 <- partition.names(Sex)
  s2 <- partition.names(Sex, subset = n1)
  s3 <- partition.names(Sex, subset = n2)

  expect_length(s1[1] & s1[2], 0)
  expect_length(s2[1] & s2[2], 0)
  expect_length(s3[1] & s3[2], 0)


  s4 <- partition.names(Sex, discard=0.2)
  s5 <- partition.names(Sex, discard=0.2, subset = n1)
  s6 <- partition.names(Sex, discard=0.2, subset = n2)

  expect_length(s4[1] & s4[2], 0)
  expect_length(s5[1] & s5[2], 0)
  expect_length(s6[1] & s6[2], 0)

  #### Random Splits

  s7 <- partition.names.random()
  s8 <- partition.names.random(subset = n1)
  s9 <- partition.names.random(subset = n2)

  expect_length(s7[1] & s7[2], 0)
  expect_length(s8[1] & s8[2], 0)
  expect_length(s9[1] & s9[2], 0)

  s10 <- partition.names.random(prop = c(1,2))
  s11 <- partition.names.random(subset = n1, prop = c(1,2))
  s12 <- partition.names.random(subset = n2, prop = c(1,2))

  expect_length(s10[1] & s10[2], 0)
  expect_length(s11[1] & s11[2], 0)
  expect_length(s12[1] & s12[2], 0)

  s13 <- partition.names.random(prop = c(1,2,4))
  s14 <- partition.names.random(subset = n1, prop = c(1,2,4))
  s15 <- partition.names.random(subset = n2, prop = c(1,2,4))

  expect_length(s13[1] & s13[2], 0)
  expect_length(s14[1] & s14[2], 0)
  expect_length(s15[1] & s15[2], 0)

  expect_length(s13[1] & s13[3], 0)
  expect_length(s14[1] & s14[3], 0)
  expect_length(s15[1] & s15[3], 0)

  expect_length(s13[2] & s13[3], 0)
  expect_length(s14[2] & s14[3], 0)
  expect_length(s15[2] & s15[3], 0)

})

test_that("Partitions split correctly along the ratings", {
  ##### Setup

  n1 <- filter.names()
  n2 <- filter.names(Competence >= 0.5)

  s1 <- partition.names(Attractiveness)
  s2 <- partition.names(Attractiveness, discard = 0.2)

  expect_true( max(ratings(subset=s1[1], Attractiveness)) < min(ratings(subset=s1[2], Attractiveness)) )
  expect_true( max(ratings(subset=s2[1], Attractiveness)) < min(ratings(subset=s2[2], Attractiveness)) )


  s3 <- partition.names(Attractiveness, subset = n1)
  s4 <- partition.names(Attractiveness, subset = n1, discard = 0.2)

  expect_true( max(ratings(subset=s3[1], Attractiveness)) < min(ratings(subset=s3[2], Attractiveness)) )
  expect_true( max(ratings(subset=s4[1], Attractiveness)) < min(ratings(subset=s4[2], Attractiveness)) )


  s5 <- partition.names(Attractiveness, subset = n2)
  s6 <- partition.names(Attractiveness, subset = n2, discard = 0.2)

  expect_true( max(ratings(subset=s5[1], Attractiveness)) < min(ratings(subset=s5[2], Attractiveness)) )
  expect_true( max(ratings(subset=s6[1], Attractiveness)) < min(ratings(subset=s6[2], Attractiveness)) )
})

test_that("Partitions discard correctly", {
  ##### Setup

  n1 <- filter.names()
  n2 <- filter.names(Competence >= 0.5)

  s1 <- partition.names(Attractiveness)
  s2 <- partition.names(Attractiveness, discard = 0.2)

  expect_true( max(ratings(subset=s1[1], Attractiveness)) >= max(ratings(subset=s2[1], Attractiveness)) )
  expect_true( min(ratings(subset=s1[1], Attractiveness)) <= min(ratings(subset=s2[2], Attractiveness)) )


  s3 <- partition.names(Attractiveness, subset = n1)
  s4 <- partition.names(Attractiveness, subset = n1, discard = 0.2)

  expect_true( max(ratings(subset=s3[1], Attractiveness)) >= max(ratings(subset=s4[1], Attractiveness)) )
  expect_true( min(ratings(subset=s3[1], Attractiveness)) <= min(ratings(subset=s4[2], Attractiveness)) )


  s5 <- partition.names(Attractiveness, subset = n2)
  s6 <- partition.names(Attractiveness, subset = n2, discard = 0.2)

  expect_true( max(ratings(subset=s5[1], Attractiveness)) >= max(ratings(subset=s6[1], Attractiveness)) )
  expect_true( min(ratings(subset=s5[1], Attractiveness)) <= min(ratings(subset=s6[2], Attractiveness)) )


  expect_true( max(ratings(subset=s1[1], Attractiveness)) >= max(ratings(subset=s5[1], Attractiveness)) )
  expect_true( min(ratings(subset=s1[1], Attractiveness)) <= min(ratings(subset=s5[2], Attractiveness)) )

  expect_true( max(ratings(subset=s2[1], Attractiveness)) >= max(ratings(subset=s6[1], Attractiveness)) )
  expect_true( min(ratings(subset=s2[1], Attractiveness)) <= min(ratings(subset=s6[2], Attractiveness)) )
})

