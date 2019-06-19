# split_into_root_folder_file_extension ----------------------------------------

#' Split Full Paths into Root, Folder, File and Extension
#'
#' @param paths vector of character representing full file paths
#' @param n_root_parts number of first path segments considered as "root"
#' @return data frame with columns \code{root}, \code{folder}, \code{file},
#'   \code{extension}, \code{depth}
#' @export
#' @examples 
#' paths <- c(
#'   "//always/the/same/root/project-1/intro.doc",
#'   "//always/the/same/root/project-1/logo.png",
#'   "//always/the/same/root/project-2/intro.txt",
#'   "//always/the/same/root/project-2/planning/file-1.doc",
#'   "//always/the/same/root/project-2/result/report.pdf"
#' )
#' 
#' split_into_root_folder_file_extension(paths)
#' split_into_root_folder_file_extension(paths, n_root_parts = 6)
#' split_into_root_folder_file_extension(paths, n_root_parts = 7)
#' 
split_into_root_folder_file_extension <- function(paths, n_root_parts = 0)
{
  parts <- split_paths(paths)
  
  result <- do.call(rbind, lapply(parts, function(x) {
    paste_path <- function(indices) paste(x[indices], collapse = "/")
    first_indices <- seq_len(n_root_parts)
    n_parts <- length(x)
    file <- x[n_parts]
    c(
      root = paste_path(first_indices),
      folder = paste_path(- c(first_indices, n_parts)),
      file = file,
      extension = kwb.utils::fileExtension(file)
    )
  }))
  
  kwb.utils::setColumns(
    kwb.utils::asNoFactorDataFrame(result), 
    depth = lengths(parts) - n_root_parts, 
    dbg = FALSE
  )
}
