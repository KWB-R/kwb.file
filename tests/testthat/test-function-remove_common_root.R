test_that("remove_common_root() works", {

  f <- kwb.file::remove_common_root
  
  expect_error(f())
  expect_identical(f("a"), structure("a", root = ""))
  expect_identical(f(list("a")), structure(list("a"), root = ""))
  expect_identical(
    f(c("a/b", "a/c", "a/d/e")), 
    structure(c("b", "c", "d/e"), root = "a")
  )
  expect_identical(
    f(list(c("a", "b"), c("a", "c"), c("a", "d", "e"))), 
    structure(list(c("b"), c("c"), c("d", "e")), root = "a")
  )
})
