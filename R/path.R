# split_paths ------------------------------------------------------------------
split_paths <- function(paths, dbg = TRUE)
{
  kwb.utils::catAndRun("Splitting paths", dbg = dbg, strsplit(paths, "/"))
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
  
  n_common <- get_common_start_segments(x)
  
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
    
    kwb.utils::catAndRun("Putting path segments together", dbg = dbg, {
      
      x <- sapply(x, function(xx) do.call(paste, c(as.list(xx), sep = "/")))
    })
  }
  
  # Set attribute "root"
  structure(x, root = root)
}

# get_common_start_segments ----------------------------------------------------
get_common_start_segments <- function(list_of_segments)
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

# get_max_path_depth ---------------------------------------------------------------------
get_max_path_depth <- function(parts = split_paths(paths), paths = NULL)
{
  if (length(parts) == 0) {
    
    0L
    
  } else {
    
    max(kwb.utils::getElementLengths(parts))
  }
}
