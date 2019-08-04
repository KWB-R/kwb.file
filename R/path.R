# split_into_dir_and_file ------------------------------------------------------

#' "Split Full Paths into Directory Path and Filename"
#' 
#' @param paths vector of character representing full file paths
#' @return data frame with columns \code{directory} and \code{file}
#' @importFrom kwb.utils lastElement noFactorDataFrame
#' @export
#' @examples 
#' split_into_dir_and_file(c("path/to/file-1", "path/to/file-2"))
split_into_dir_and_file <- function(paths)
{
  parts <- split_paths(paths, dbg = FALSE)
  
  get_dirname <- function(x) paste(x[- length(x)], collapse = "/")
  
  kwb.utils::noFactorDataFrame(
    directory = sapply(parts, get_dirname), 
    file = sapply(parts, kwb.utils::lastElement)
  )
}

# split_paths ------------------------------------------------------------------

#' Split Full Paths at Slashes into Parts
#' 
#' @param paths vector of character representing full file paths
#' @param dbg if \code{TRUE} (default), a debug message is shown
#' @param use_fs whether or not to simply use \code{\link[fs]{path_split}}. 
#'   Defaults to \code{FALSE}
#' @importFrom kwb.utils catAndRun
#' @export
#' @examples
#' segments <- split_paths(c("path/to/file-1", "path/to/file-2"))
#' segments
split_paths <- function(paths, dbg = TRUE, use_fs = FALSE)
{
  if (use_fs) {
    return(fs::path_split(paths))
  }
  
  # Which paths start with "//server"?
  server_indices <- grep("^//[^/]", paths)
  
  # Remove // from server paths
  if (any(server_indices)) {
    server_paths <- paths[server_indices]
    paths[server_indices] <- substr(server_paths, 3, nchar(server_paths))
  }
  
  parts <- kwb.utils::catAndRun(
    "Splitting paths", dbg = dbg, strsplit(paths, "/")
  )
  
  # Add // back to server paths
  if (any(server_indices)) {
    parts[server_indices] <- lapply(parts[server_indices], function(x) {
      `[<-`(x, 1, paste0("//", x[1]))
    })
  }

  parts  
}

# get_max_path_depth -----------------------------------------------------------
get_max_path_depth <- function(parts = split_paths(paths), paths = NULL)
{
  ifelse(length(parts) == 0, 0L, max(lengths(parts)))
}
