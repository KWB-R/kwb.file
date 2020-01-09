#library(testthat)

test_that("to_subdir_matrix() works", {

  f <- kwb.file:::to_subdir_matrix
  
  expect_error(f())
  
  paths <- c("a", "a/b", "a/b/c")
  
  expect_identical(dim(f(paths)), c(3L, 3L))
  expect_identical(f(paths, method = 1), f(paths, method = 2))
  expect_length(f(paths, result_type = "list"), 3L)
})
