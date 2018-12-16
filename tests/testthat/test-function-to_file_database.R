test_that("to_file_database() works", {

  expect_error(to_file_database(), "missing")
  
  paths <- paste0("a/b/c/", LETTERS[1:10])
  
  db <- to_file_database(paths, remove_common_base = FALSE)
  
  expect_identical(names(db), c("files", "folders"))
  expect_identical(names(db$files), c("file_id", "file_name", "folder_id"))
  expect_identical(names(db$folders), c("folder_id", "folder_path"))
  
  expect_identical(nrow(db$files), length(paths))
  expect_identical(nrow(db$folders), 1L)
  expect_identical(db$files$file_name, basename(paths))
  expect_identical(db$folders$folder_path, "a/b/c")
  
})

