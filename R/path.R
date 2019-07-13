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
#' @importFrom kwb.utils catAndRun
#' @export
#' @examples
#' segments <- split_paths(c("path/to/file-1", "path/to/file-2"))
#' segments
split_paths <- function(paths, dbg = TRUE)
{
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

# remove_common_root -----------------------------------------------------------

#' Remove the Common Root Parts
#'
#' @param x list of vectors of character as returned by
#'   \code{\link[base]{strsplit}} or a vector of character.
#' @param n_keep number of common path segments to keep (so that the path
#'   tree keeps its root)
#' @param dbg if \code{TRUE} debug messages are shown
#'
#' @export
#' @examples
#' # Split paths at the slashes
#' absparts <- strsplit(c("a/b/c", "a/b/d", "a/b/e/f/g", "a/b/hi"), "/")
#'
#' # Remove the common parts of the paths
#' relparts <- remove_common_root(absparts)
#' relparts
#'
#' # The extracted root is returned in attribute "root"
#' attr(relparts, "root")
#'
remove_common_root <- function(x, n_keep = 0, dbg = TRUE)
{
  if (! (was_list <- is.list(x))) {
    x <- split_paths(as.character(x), dbg = dbg)
  }
  
  root <- ""
  
  n_common <- get_number_of_common_start_segments(x)
  
  if ((n_remove <- n_common - n_keep) > 0) {
    
    # Determine the root path
    root <- kwb.utils::collapsed(x[[1]][1:n_remove], "/")
    
    # Remove the first n_common parts of each list entry
    text <- paste("Removing the first", n_remove, "path segments")
    
    kwb.utils::catAndRun(text, dbg = dbg, {
      x <- lapply(x, function(segments) {
        if (length(segments) > n_remove) segments[- (1:n_remove)] else ""
      })
    })
  }
  
  # If the input was not a list, convert the list back to a vector of character
  if (! was_list) {
    x <- kwb.utils::catAndRun("Putting path segments together", dbg = dbg, {
      sapply(x, function(xx) do.call(paste, c(as.list(xx), sep = "/")))
    })
  }
  
  # Set attribute "root"
  structure(x, root = root)
}

# get_number_of_common_start_segments ------------------------------------------
get_number_of_common_start_segments <- function(list_of_segments)
{
  # Define helper function
  get_segment <- function(depth) {
    
    result <- sapply(list_of_segments, "[", depth)
    result[is.na(result)] <- ""
    result
  }

  tree_height <- get_max_path_depth(parts = list_of_segments)

  i <- 1
  
  while (i < tree_height && kwb.utils::allAreEqual(get_segment(i))) {
    
    i <- i + 1
  }
  
  i - 1
}

# get_max_path_depth -----------------------------------------------------------
get_max_path_depth <- function(parts = split_paths(paths), paths = NULL)
{
  ifelse(length(parts) == 0, 0L, max(lengths(parts)))
}
