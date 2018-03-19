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

context("Matching")

###############################################

test_that("Matches have correct size", {
  s0 <- filter.names()
  s1 <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)

  m1 <- match.pairs(Sex)
  m2 <- match.pairs(Sex, subset=s0)
  m3 <- match.pairs(Sex, subset=s1)
  m4 <- match.pairs(Sex, discard = 0.2)
  m5 <- match.pairs(Sex, discard = 0.2, subset=s0)
  m6 <- match.pairs(Sex, discard = 0.2, subset=s1)
  m7 <- match.pairs(Sex, discard = 0.2, subset=s0, Competence=10, Intelligence=10)
  m8 <- match.pairs(Sex, discard = 0.2, subset=s1, Competence=10, Intelligence=10)

  # We should not have more than original dataset
  expect_true( (length(m1[1]) + length(m1[2])) <= length(s0))
  expect_true( (length(m2[1]) + length(m2[2])) <= length(s0))
  expect_true( (length(m3[1]) + length(m3[2])) <= length(s1))
  expect_true( (length(m4[1]) + length(m4[2])) <= length(s0))
  expect_true( (length(m5[1]) + length(m5[2])) <= length(s0))
  expect_true( (length(m6[1]) + length(m6[2])) <= length(s1))
  expect_true( (length(m7[1]) + length(m7[2])) <= length(s0))
  expect_true( (length(m8[1]) + length(m8[2])) <= length(s1))

  # Both sets should match in length
  expect_equal(length(m1[1]), length(m1[2]))
  expect_equal(length(m2[1]), length(m2[2]))
  expect_equal(length(m3[1]), length(m3[2]))
  expect_equal(length(m4[1]), length(m4[2]))
  expect_equal(length(m5[1]), length(m5[2]))
  expect_equal(length(m6[1]), length(m6[2]))
  expect_equal(length(m7[1]), length(m7[2]))
  expect_equal(length(m8[1]), length(m8[2]))
})
