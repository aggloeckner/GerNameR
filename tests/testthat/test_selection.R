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

context("Selection")

###############################################

test_that("Selections have the correct number of elements", {
  idx1 <- seq(1,100)
  idx2 <- c(idx1,idx1)
  idx3 <- seq(1,200,by=2)
  idx4 <- c(idx3,idx3)

	s0 <- filter.names()
	# Without multiples
	so1 <- s0[idx1]
	# With multiples
	so2 <- s0[idx2]
	# Without multiples after first selecting multiples
	so3 <- so2[idx3]
	# With multiples after first selecting multiples
	so4 <- so2[idx4]

	expect_equal( length(so1), length(idx1) )
	expect_equal( length(so2), length(idx2) )
	expect_equal( length(so3), length(idx3) )
	expect_equal( length(so4), length(idx4) )



})
