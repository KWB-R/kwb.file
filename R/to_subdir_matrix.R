# to_subdir_matrix -------------------------------------------------------------

#' Convert a Vector of Paths to a Matrix of Subfolders
#'
#' @param paths vector of path strings
#' @param fill.value value used to fill empty cells of the result matrix
#' @param result_type one of \code{c("matrix", "data.frame", "list")}, 
#'   specifying the type of object to be returned. Result type "list" is only
#'   implemented for \code{method = 2}.
#' @param dbg if \code{TRUE} debug messages are shown
#' @param method integer specifying the implementation method. One of 1, 2.
#' @return matrix or data frame, depending on \code{result_type}
#' 
#' @importFrom kwb.utils asNoFactorDataFrame enlargeVector
#' 
#' @export
#'
#' @examples
#' folder_matrix <- kwb.file::to_subdir_matrix(c("a1/b1/c1", "a1/b2", "a2"))
#'
#' folder_matrix
#'
#' dim(folder_matrix)
#'
#' folder_matrix[folder_matrix[, 1] == "a1", ]
#'
to_subdir_matrix <- function(
  paths, fill.value = "", result_type = "matrix", dbg = FALSE, method = 2
)
{
  stopifnot(result_type %in% c("matrix", "data.frame", "list"))
  stopifnot(result_type != "list" || method == 2)
  stopifnot(method %in% 1:2)
  
  if (! is.list(paths)) {
    paths <- split_paths(paths, dbg = dbg)
  }

  if (! is.list(paths) || ! all(sapply(paths, mode) == "character")) {

    stop(
      "to_subdir_matrix(): paths must be a list of character vectors ",
      "or a vector of character.", call. = FALSE
    )
  }

  if (method == 1) {

    # Get the maximum path depth
    max_depth <- get_max_path_depth(paths)
    
    # Extend all list entries to a length of <max_depth>, filling with <fill.with>
    paths <- lapply(paths, kwb.utils::enlargeVector, max_depth, fill.value)
    
    # Create a matrix of subdirectories
    result <- matrix(unlist(paths), nrow = length(paths), byrow = TRUE)
    
  } else if (method == 2) {
    
    path_depths <- lengths(paths)
    
    max_depth <- max(path_depths)

    original_rows <- split(seq_along(paths), path_depths)
    
    subdirs_in_depth <- lapply(original_rows, function(indices) {
      depth <- path_depths[indices[1]]
      m <- matrix(unlist(paths[indices]), byrow = TRUE, ncol = depth)
      if (result_type == "list") m else cbind(m,  matrix(
        fill.value, ncol = (max_depth - depth), nrow = length(indices)
      ))
    })
    
    # Return the list of matrices if requested
    if (result_type == "list") {
      return(structure(subdirs_in_depth, original_rows = original_rows))
    }
    
    subdirs <- do.call(rbind, subdirs_in_depth)

    result <- subdirs[order(unlist(original_rows)), ]
  }

  # Return the result as an object of the requested type
  if (result_type == "matrix") {
    return(result)
  } 
  
  if (result_type == "data.frame") {
    return(kwb.utils::asNoFactorDataFrame(result))
  }
}
