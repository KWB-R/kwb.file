# to_file_database -------------------------------------------------------------

#' Create Two Table Relational Database From Paths
#' 
#' From a vector of given file paths, this function generates short and unique 
#' identifiers for files and folders. The assignements between identifiers and 
#' original paths are stored in two data frames, \code{files} and \code{folders}
#' that are returned.
#' 
#' @param files vector of file paths
#' @param remove_common_base if \code{TRUE} (default) the common root of all
#'   \code{files} is removed before creating the database
#' @return list of two data frames, \code{files} and \code{folders}
#' @export
to_file_database <- function(files, remove_common_base = TRUE)
{
  folder_table <- to_folder_table(dirname(files), remove_common_base)

  lookup_table <- folder_table

  if (isTRUE(remove_common_base)) {
    base_dir <- kwb.utils::getAttribute(folder_table, "base_dir")

    lookup_table$folder_path <- file.path(base_dir, lookup_table$folder_path)
  }

  # Remove trailing slashes
  lookup_table$folder_path <- gsub("/$", "", lookup_table$folder_path)

  list(
    files = to_file_table(files, lookup_table),
    folders = folder_table
  )
}

# to_folder_table -------------------------------------------------------------
to_folder_table <- function(folder_paths, remove_common_base = TRUE)
{
  folders <- sort(unique(folder_paths))

  folder_table <- kwb.utils::noFactorDataFrame(
    folder_id = kwb.utils::createIdAlong(folders),
    folder_path = folders
  )

  if (isTRUE(remove_common_base)) {
    
    subdirs <- kwb.fakin:::splitPaths(folder_table$folder_path)

    subdirs <- kwb.fakin::removeCommonRoot(subdirs)

    folder_table$folder_path <- paste_path_parts(subdirs)

    attr(folder_table, "base_dir") <- kwb.utils::getAttribute(subdirs, "root")
  }

  folder_table
}

# paste_path_parts -------------------------------------------------------------
paste_path_parts <- function(path_parts)
{
  stopifnot(is.list(path_parts), all(sapply(path_parts, is.character)))
  
  sapply(path_parts, function(p) {
    
    if (length(p) > 0) {
      
      do.call(file.path, as.list(p))
      
    } else {
      
      ""
    }
  })
}

# to_file_table ----------------------------------------------------------------
to_file_table <- function(files, lookup_table)
{
  kwb.utils::noFactorDataFrame(
    file_id = sprintf("file_%02X", seq_along(files)),
    file_name = basename(files),
    folder_id = sapply(
      dirname(files), kwb.utils::tableLookup,
      x = lookup_table[, 2:1]
    )
  )
}
