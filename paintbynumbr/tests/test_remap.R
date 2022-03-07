context("remap")
library(colorspace) # necessary?

test_that("remap function returns expected values", {
  expect_equal(remap(seq(1:100), output_start = 0, output_stop = 1), seq(from = 0, to = 1, by = 0.01))
})
#> Test passed 🎊
