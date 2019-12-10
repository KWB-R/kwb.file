# to_subdir_matrix -------------------------------------------------------------

#' Convert a Vector of Paths to a Matrix of Subfolders
#'
#' @param paths vector of path strings
#' @param fill.value value used to fill empty cells of the result matrix
#' @param result_type one of \code{c("matrix", "data.frame")}, specifying the
#'   type of object to be returned
#' @param dbg if \code{TRUE} debug messages are shown
#' @param method integer specifying the implementation method. One of 1, 2, 3.
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
  paths, fill.value = "", result_type = "matrix", dbg = FALSE, method = 3
)
{
  stopifnot(result_type %in% c("matrix", "data.frame"))

  if (! is.list(paths)) {

    paths <- split_paths(paths, dbg = dbg)
  }

  if (! is.list(paths) || ! all(sapply(paths, mode) == "character")) {

    stop(
      "to_subdir_matrix(): paths must be a list of character vectors ",
      "or a vector of character.", call. = FALSE
    )
  }

  stopifnot(method %in% 1:3)
  
  if (method == 1) {

    # Get the maximum path depth
    max_depth <- get_max_path_depth(paths)
    
    # Extend all list entries to a length of <max_depth>, filling with <fill.with>
    paths <- lapply(paths, kwb.utils::enlargeVector, max_depth, fill.value)
    
    # Create a matrix of subdirectories
    result <- matrix(unlist(paths), nrow = length(paths), byrow = TRUE)
    
  } else {
    
    path_depths <- lengths(paths)
    
    max_depth <- max(path_depths)
    
    if (method == 2) {
      
      subdirs <- do.call(rbind, lapply(split(paths, path_depths), function(x) {
        depth <- length(x[[1]])
        m <- matrix(unlist(x), byrow = TRUE, ncol = depth)
        n_fill <- (max_depth - depth) * length(x)
        matrix(c(m, rep_len(fill.value, n_fill)), ncol = max_depth)
      }))
      
      row_order <- order(unlist(split(seq_along(paths), path_depths)))
      
    } else if (method == 3) {
      
      index_list <- split(seq_along(paths), path_depths)
      
      subdirs <- do.call(rbind, lapply(index_list, function(indices) {
        depth <- path_depths[indices[1]]
        cbind(
          matrix(unlist(paths[indices]), byrow = TRUE, ncol = depth), 
          matrix(fill.value, ncol = (max_depth - depth), nrow = length(indices))
        )
      }))
      
      row_order <- order(unlist(index_list))
    }
    
    result <- subdirs[row_order, ]
  }

  # Return the result as an object of the requested type
  if (result_type == "matrix") {

    result

  } else if (result_type == "data.frame") {

    kwb.utils::asNoFactorDataFrame(result)

  } else {

    stop("result_type not supported: ", result_type, call. = FALSE)
  }
}
