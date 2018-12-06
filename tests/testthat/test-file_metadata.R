context("test-file_metadata")

test_that("Reading file metadata works", {

  paths <- kwb.file::dir_full(system.file(package = "kwb.file"))
  
  temp_dir <- tempdir()
  
  kwb.file:::write_file_info_to_yaml_file(
    source_paths = paths, 
    target_files = basename(paths), 
    target_dir = temp_dir
  )
  
  yaml_file <- file.path(temp_dir, "FILES.yaml")
  
  expect_true(file.exists(yaml_file))
  
  file_metadata_df <- read_file_metadata(yaml_file)
  file_metadata_ls <- read_file_metadata(yaml_file, out_class = "list")
  
  expect_is(file_metadata_df, "data.frame")
  expect_is(file_metadata_ls, "list")
  expect_true(! is.data.frame(file_metadata_ls))
  
  elements <- c("file_id", "original_name", "original_folder")
  
  # Exactly these columns in file_metadata_df
  expect_identical(names(file_metadata_df), elements)
  
  # Exactly these elements in all elements of file_metadata_ls
  expect_identical(names(file_metadata_ls[[1]]), elements)
  expect_true(kwb.utils::allAreIdentical(lapply(file_metadata_ls, names)))
  
  expect_identical(basename(paths), file_metadata_df$original_name)
  expect_identical(basename(dirname(paths)), file_metadata_df$original_folder)
})
