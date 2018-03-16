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

test_that("Correct classes are returned", {

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
