library(testthat)
library(fars)

test_that("make_filename function", expect_equal(make_filename(2013), "accident_2013.csv.bz2"))
