# to_subdir_matrix_2 -----------------------------------------------------------
to_subdir_matrix_2 <- function(paths, method = 2)
{
  parts <- split_paths(paths, dbg = FALSE)
  
  path_depths <- lengths(parts)

  max_depth <- max(path_depths)

  if (method == 1) {
    
    subdirs <- do.call(rbind, lapply(split(parts, path_depths), function(x) {
      depth <- length(x[[1]])
      m <- matrix(unlist(x), byrow = TRUE, ncol = depth)
      n_fill <- (max_depth - depth) * length(x)
      matrix(c(m, rep_len(NA, n_fill)), ncol = max_depth)
    }))
    
    row_order <- order(unlist(split(seq_along(paths), path_depths)))
    
  } else if (method == 2) {

    index_list <- split(seq_along(parts), path_depths)
    
    subdirs <- do.call(rbind, lapply(index_list, function(indices) {
      depth <- path_depths[indices[1]]
      cbind(
        matrix(unlist(parts[indices]), byrow = TRUE, ncol = depth), 
        matrix(NA, ncol = (max_depth - depth), nrow = length(indices))
      )
    }))
    
    row_order <- order(unlist(index_list))
  }
  
  subdirs[row_order, ]
}
