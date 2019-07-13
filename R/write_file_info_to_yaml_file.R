# write_file_info_to_yaml_file -------------------------------------------------
write_file_info_to_yaml_file <- function(source_paths, target_files, target_dir)
{
  file_info <- file_paths_to_file_info(file_paths = source_paths, target_files)
  
  writeLines(file_info_to_yaml(file_info), file.path(target_dir, "FILES.yaml"))
}

# file_paths_to_file_info ------------------------------------------------------
file_paths_to_file_info <- function(file_paths, target_files)
{
  relative_paths <- remove_common_root(file_paths, n_keep = 2)
                     
  kwb.utils::noFactorDataFrame(
    file_id = target_files,
    original_name = basename(relative_paths),
    original_folder = dirname(relative_paths)
  )
}

# file_info_to_yaml ------------------------------------------------------------
file_info_to_yaml <- function(file_info)
{
  c("files:", unlist(lapply(seq_len(nrow(file_info)), function(i) {
    
    record <- file_info[i, ]
    prefixes <- rep_len("  ", length(record))
    prefixes[1] <- "- "
    sprintf("%s %s: %s", prefixes, names(record), as.list(record))
  })))
}
