# to_subdir_matrix_2 -----------------------------------------------------------
to_subdir_matrix_2 <- function(paths)
{
  parts <- split_paths(paths)

  path_depths <- lengths(parts)
  
  max_depth <- max(path_depths)

  matrices <- lapply(split(parts, path_depths), function(x) {
    depth <- length(x[[1]])
    m <- matrix(unlist(x), byrow = TRUE, ncol = depth)
    n_fill <- (max_depth - depth) * length(x)
    matrix(c(m, rep_len(NA, n_fill)), ncol = max_depth)
  })

  subdirs <- do.call(rbind, matrices)

  row_order <- order(unlist(split(seq_along(paths), path_depths)))

  subdirs[row_order, ]
}
