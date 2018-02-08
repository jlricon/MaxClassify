
library(magrittr)
library(data.table)
library(testthat)
context("MaxClassify")


test_that("We can get most common words",{
  expect_equal(get_common_words_single(c("this is a test test","this also happens to be a test","tests are this"),minDocFreq = 2),
               c("a" ,   "test" ,"this"))
})


