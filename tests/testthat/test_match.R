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

  ##### Setup

  s0 <- filter.names()
  s1 <- filter.names(Familiarity >= 0.5, Nationality >= 0.5)
  sp1 <- partition.names(Sex)
  sp2 <- partition.names(Sex, discard = 0.2)
  sp3 <- partition.names(Sex, discard = 0.2, subset = s1)
  sp4 <- partition.names.random()
  sp5 <- partition.names.random(subset = s1)
  sp6 <- partition.names.random(prop=c(1,2))
  sp7 <- partition.names.random(subset = s1, prop=c(1,2))
  sp8 <- partition.names.random(prop=c(1,2,4))

  ##### Match with split based on attribute

  m1 <- match.partition(Sex)

  # We should not have more than original dataset
  expect_true( (length(m1[1]) + length(m1[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m1[1]), length(m1[2]))


  m2 <- match.partition(Sex, subset=s0)

  # We should not have more than original dataset
  expect_true( (length(m2[1]) + length(m2[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m2[1]), length(m2[2]))

  m3 <- match.partition(Sex, subset=s1)

  # We should not have more than original dataset
  expect_true( (length(m3[1]) + length(m3[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m3[1]), length(m3[2]))


  m4 <- match.partition(Sex, discard = 0.2)

  # We should not have more than original dataset
  expect_true( (length(m4[1]) + length(m4[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m4[1]), length(m4[2]))

  m5 <- match.partition(Sex, discard = 0.2, subset=s0)

  # We should not have more than original dataset
  expect_true( (length(m5[1]) + length(m5[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m5[1]), length(m5[2]))

  m6 <- match.partition(Sex, discard = 0.2, subset=s1)

  # We should not have more than original dataset
  expect_true( (length(m6[1]) + length(m6[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m6[1]), length(m6[2]))

  m7 <- match.partition(Sex, discard = 0.2, subset=s0, Competence=10, Intelligence=10)

  # We should not have more than original dataset
  expect_true( (length(m7[1]) + length(m7[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m7[1]), length(m7[2]))


  m8 <- match.partition(Sex, discard = 0.2, subset=s1, Competence=10, Intelligence=10)

  # We should not have more than original dataset
  expect_true( (length(m8[1]) + length(m8[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m8[1]), length(m8[2]))

  ##### Match based on predefined split

  m9 <- match.split(sp1)
  m10 <- match.split(sp1, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m9[1]) + length(m9[2])) <= length(s0))
  expect_true( (length(m10[1]) + length(m10[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m9[1]), length(m9[2]))
  expect_equal(length(m10[1]), length(m10[2]))
  # We should match as many names as possible
  expect_equal(min(length(m9[1]),length(m9[2])),   min(length(sp1[1]),length(sp1[2])) )
  expect_equal(min(length(m10[1]),length(m10[2])), min(length(sp1[1]),length(sp1[2])) )


  m11 <- match.split(sp2)
  m12 <- match.split(sp2, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m11[1]) + length(m11[2])) <= length(s0))
  expect_true( (length(m12[1]) + length(m12[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m11[1]), length(m11[2]))
  expect_equal(length(m12[1]), length(m12[2]))
  # We should match as many names as possible
  expect_equal(min(length(m11[1]),length(m11[2])), min(length(sp2[1]),length(sp2[2])) )
  expect_equal(min(length(m12[1]),length(m12[2])), min(length(sp2[1]),length(sp2[2])) )


  m13 <- match.split(sp3)
  m14 <- match.split(sp3, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m13[1]) + length(m13[2])) <= length(s1))
  expect_true( (length(m14[1]) + length(m14[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m13[1]), length(m13[2]))
  expect_equal(length(m14[1]), length(m14[2]))
  # We should match as many names as possible
  expect_equal(min(length(m13[1]),length(m13[2])), min(length(sp3[1]),length(sp3[2])) )
  expect_equal(min(length(m14[1]),length(m14[2])), min(length(sp3[1]),length(sp3[2])) )


  m15 <- match.split(sp4)
  m16 <- match.split(sp4, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m15[1]) + length(m15[2])) <= length(s0))
  expect_true( (length(m16[1]) + length(m16[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m15[1]), length(m15[2]))
  expect_equal(length(m16[1]), length(m16[2]))
  # We should match as many names as possible
  expect_equal(min(length(m15[1]),length(m15[2])), min(length(sp4[1]),length(sp4[2])) )
  expect_equal(min(length(m16[1]),length(m16[2])), min(length(sp4[1]),length(sp4[2])) )

  m17 <- match.split(sp5)
  m18 <- match.split(sp5, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m17[1]) + length(m17[2])) <= length(s1))
  expect_true( (length(m18[1]) + length(m18[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m17[1]), length(m17[2]))
  expect_equal(length(m18[1]), length(m18[2]))
  # We should match as many names as possible
  expect_equal(min(length(m17[1]),length(m17[2])), min(length(sp5[1]),length(sp5[2])) )
  expect_equal(min(length(m18[1]),length(m18[2])), min(length(sp5[1]),length(sp5[2])) )

  m19 <- match.split(sp6)
  m20 <- match.split(sp6, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m19[1]) + length(m19[2])) <= length(s0))
  expect_true( (length(m20[1]) + length(m20[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m19[1]), length(m19[2]))
  expect_equal(length(m20[1]), length(m20[2]))
  # We should match as many names as possible
  expect_equal(min(length(m19[1]),length(m19[2])), min(length(sp6[1]),length(sp6[2])) )
  expect_equal(min(length(m20[1]),length(m20[2])), min(length(sp6[1]),length(sp6[2])) )


  m21 <- match.split(sp7)
  m22 <- match.split(sp7, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m21[1]) + length(m21[2])) <= length(s1))
  expect_true( (length(m22[1]) + length(m22[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m21[1]), length(m21[2]))
  expect_equal(length(m22[1]), length(m22[2]))
  # We should match as many names as possible
  expect_equal(min(length(m21[1]),length(m21[2])), min(length(sp7[1]),length(sp7[2])) )
  expect_equal(min(length(m22[1]),length(m22[2])), min(length(sp7[1]),length(sp7[2])) )


  m23 <- match.split(sp8)
  m24 <- match.split(sp8, Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m23[1]) + length(m23[2])) <= length(s0))
  expect_true( (length(m24[1]) + length(m24[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m23[1]), length(m23[2]))
  expect_equal(length(m24[1]), length(m24[2]))
  # We should match as many names as possible
  expect_equal(min(length(m23[1]),length(m23[2])), min(length(sp8[1]),length(sp8[2])) )
  expect_equal(min(length(m24[1]),length(m24[2])), min(length(sp8[1]),length(sp8[2])) )

  ##### Match based on two subsets

  m25 <- match.pairs(sp1[1],sp1[2])
  m26 <- match.pairs(sp1[1],sp1[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m25[1]) + length(m25[2])) <= length(s0))
  expect_true( (length(m26[1]) + length(m26[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m25[1]), length(m25[2]))
  expect_equal(length(m26[1]), length(m26[2]))
  # We should match as many names as possible
  expect_equal(min(length(m25[1]),length(m25[2])), min(length(sp1[1]),length(sp1[2])) )
  expect_equal(min(length(m26[1]),length(m26[2])), min(length(sp1[1]),length(sp1[2])) )


  m27 <- match.pairs(sp2[1],sp2[2])
  m28 <- match.pairs(sp2[1],sp2[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m27[1]) + length(m27[2])) <= length(s0))
  expect_true( (length(m28[1]) + length(m28[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m27[1]), length(m27[2]))
  expect_equal(length(m28[1]), length(m28[2]))
  # We should match as many names as possible
  expect_equal(min(length(m27[1]),length(m27[2])), min(length(sp2[1]),length(sp2[2])) )
  expect_equal(min(length(m28[1]),length(m28[2])), min(length(sp2[1]),length(sp2[2])) )


  m29 <- match.pairs(sp3[1],sp3[2])
  m30 <- match.pairs(sp3[1],sp3[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m29[1]) + length(m29[2])) <= length(s1))
  expect_true( (length(m30[1]) + length(m30[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m29[1]), length(m29[2]))
  expect_equal(length(m30[1]), length(m30[2]))
  # We should match as many names as possible
  expect_equal(min(length(m29[1]),length(m29[2])), min(length(sp3[1]),length(sp3[2])) )
  expect_equal(min(length(m30[1]),length(m30[2])), min(length(sp3[1]),length(sp3[2])) )


  m31 <- match.pairs(sp4[1],sp4[2])
  m32 <- match.pairs(sp4[1],sp4[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m31[1]) + length(m31[2])) <= length(s0))
  expect_true( (length(m32[1]) + length(m32[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m31[1]), length(m31[2]))
  expect_equal(length(m32[1]), length(m32[2]))
  # We should match as many names as possible
  expect_equal(min(length(m31[1]),length(m31[2])), min(length(sp4[1]),length(sp4[2])) )
  expect_equal(min(length(m32[1]),length(m32[2])), min(length(sp4[1]),length(sp4[2])) )

  m33 <- match.pairs(sp5[1],sp5[2])
  m34 <- match.pairs(sp5[1],sp5[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m33[1]) + length(m33[2])) <= length(s1))
  expect_true( (length(m34[1]) + length(m34[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m33[1]), length(m33[2]))
  expect_equal(length(m34[1]), length(m34[2]))
  # We should match as many names as possible
  expect_equal(min(length(m33[1]),length(m33[2])), min(length(sp5[1]),length(sp5[2])) )
  expect_equal(min(length(m34[1]),length(m34[2])), min(length(sp5[1]),length(sp5[2])) )

  m35 <- match.pairs(sp6[1],sp6[2])
  m36 <- match.pairs(sp6[1],sp6[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m35[1]) + length(m35[2])) <= length(s0))
  expect_true( (length(m36[1]) + length(m36[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m35[1]), length(m35[2]))
  expect_equal(length(m36[1]), length(m36[2]))
  # We should match as many names as possible
  expect_equal(min(length(m35[1]),length(m35[2])), min(length(sp6[1]),length(sp6[2])) )
  expect_equal(min(length(m36[1]),length(m36[2])), min(length(sp6[1]),length(sp6[2])) )


  m37 <- match.pairs(sp7[1],sp7[2])
  m38 <- match.pairs(sp7[1],sp7[2], Competence=10, Intelligence=10)
  # We should not have more than original dataset
  expect_true( (length(m37[1]) + length(m37[2])) <= length(s1))
  expect_true( (length(m38[1]) + length(m38[2])) <= length(s1))
  # Both sets should match in length
  expect_equal(length(m37[1]), length(m37[2]))
  expect_equal(length(m38[1]), length(m38[2]))
  # We should match as many names as possible
  expect_equal(min(length(m37[1]),length(m37[2])), min(length(sp7[1]),length(sp7[2])) )
  expect_equal(min(length(m38[1]),length(m38[2])), min(length(sp7[1]),length(sp7[2])) )


  m39 <- match.pairs(sp8[1],sp8[2])
  m40 <- match.pairs(sp8[1],sp8[3])
  m41 <- match.pairs(sp8[2],sp8[3])
  # We should not have more than original dataset
  expect_true( (length(m39[1]) + length(m39[2])) <= length(s0))
  expect_true( (length(m40[1]) + length(m40[2])) <= length(s0))
  expect_true( (length(m41[1]) + length(m41[2])) <= length(s0))
  # Both sets should match in length
  expect_equal(length(m39[1]), length(m39[2]))
  expect_equal(length(m40[1]), length(m40[2]))
  expect_equal(length(m41[1]), length(m41[2]))
  # We should match as many names as possible
  expect_equal(min(length(m39[1]),length(m39[2])), min(length(sp8[1]),length(sp8[2])) )
  expect_equal(min(length(m40[1]),length(m40[2])), min(length(sp8[1]),length(sp8[3])) )
  expect_equal(min(length(m41[1]),length(m41[2])), min(length(sp8[2]),length(sp8[3])) )
})
