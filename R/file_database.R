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
#' @examples 
#' paths <- c(
#'   "very_long/very_ugly_path/even with spaces.doc",
#'   "very_long/very_ugly_path/even with spaces.docx"
#' )
#' 
#' to_file_database(paths)
#' to_file_database(paths, remove_common_base = FALSE)
#' 
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

# add_file_info ----------------------------------------------------------------

#' Add File Information From File Database
#' 
#' @param data data frame with column \code{file_id} containing file identifiers
#'   and with an attribute \code{file_db} containing a "file database" as
#'   created by \code{to_file_database}
#' @return data frame \code{data} with additional columns \code{folder_path}
#'   and \code{file_name}
#' @export
#' @examples 
#' # Define some paths
#' paths <- c(
#'   "/very/long/path/very_long_file_name_1",
#'   "/very/long/path/very_long_file_name_2",
#'   "/very/long/path/very_long_file_name_3"
#' )
#' 
#' # Create a "file database" from the paths
#' file_db <- kwb.file::to_file_database(paths, remove_common_base = FALSE)
#' 
#' # Create a data frame that relates some information to the files.
#' # Use the file identifier instead of the full name to keep the data clean
#' (df <- kwb.utils::noFactorDataFrame(
#'   file_id = file_db$files$file_id, 
#'   value = seq_along(paths)
#' ))
#' 
#' # Store the file database in the attribute "file_db"
#' df <- structure(df, file_db = file_db)
#' 
#' # Restore the full file paths
#' add_file_info(df)
#' 
add_file_info <- function(data)
{
  `%>%` <- dplyr::`%>%`
  
  file_db <- kwb.utils::getAttribute(data, "file_db")
  
  data %>% 
    dplyr::left_join(file_db$files, by = "file_id") %>%
    dplyr::left_join(file_db$folders, by = "folder_id") %>%
    kwb.utils::removeColumns("folder_id") %>%
    kwb.utils::moveColumnsToFront(c("file_id", "folder_path", "file_name"))
}
