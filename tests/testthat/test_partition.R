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

test_that("Partition have correct sizes", {
  n1 <- filter.names()
  s1 <- partition.names(Sex)
  expect_equal( length(s1[1]) + length(s1[2]), length(n1))

  s2 <- partition.names(Sex, subset = n1)
  expect_equal( length(s2[1]) + length(s2[2]), length(n1))

  n2 <- filter.names(Competence >= 0.5)
  s3 <- partition.names(Sex, subset = n2)
  expect_equal( length(s3[1]) + length(s3[2]), length(n2))

  s4 <- partition.names(Sex, discard=0.2)
  expect_true( (length(s4[1]) + length(s4[2])) <= length(n1))

  s5 <- partition.names(Sex, discard=0.2, subset = n1)
  expect_true( (length(s5[1]) + length(s5[2])) <= length(n1))

  s6 <- partition.names(Sex, discard=0.2, subset = n2)
  expect_true( (length(s6[1]) + length(s6[2])) <= length(n2))

})
