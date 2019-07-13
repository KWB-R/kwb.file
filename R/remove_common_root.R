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
  
  i <- 1L
  max_i <- max(lengths(x))
  
  while (i <= max_i && kwb.utils::allAreEqual(sapply(x, "[", i))) i <- i + 1
  
  n_remove <- i - 1 - n_keep
  
  if (n_remove > 0) {
    
    kwb.utils::catAndRun(
      sprintf("Removing the first %d path segments", n_remove), dbg = dbg, {
        
        # Determine the root path
        root <- kwb.utils::collapsed(x[[1]][1:n_remove], "/")
        
        # Remove the first n_common parts of each list entry
        x <- lapply(x, function(xx) {
          if (length(xx) > n_remove) xx[-(1:n_remove)] else character()
        })
      }
    )
    
  } else {
    
    root <- ""
  }
  
  # If the input was not a list, convert the list back to a vector of character
  if (! was_list) {
    x <- kwb.utils::catAndRun("Putting path segments together", dbg = dbg, {
      sapply(x, function(xx) if (length(xx)) {
        do.call(paste, c(as.list(xx), sep = "/"))
      } else {
        ""
      })
    })
  }
  
  # Set attribute "root"
  structure(x, root = root)
}
