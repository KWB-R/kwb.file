# dir_full ---------------------------------------------------------------------
#
#' Helper function to return full paths
#' 
#' This function provides a shortcut to \code{dir(..., full.names = TRUE)}
#' 
#' @param \dots arguments passed to \code{\link{dir}}
#' 
#' @export
#' 
#' @examples 
#' dir_full(system.file(package = "kwb.file"))
#' 
dir_full <- function(...)
{
  dir(..., full.names = TRUE)
}

# dir_full_recursive_xml -------------------------------------------------------

#' Get Full Paths to all XML files Below a Root Folder
#' 
#' @param root path to root folder
#' 
#' @return vector of character
#' 
#' @export
#' 
dir_full_recursive_xml <- function(root) 
{
  dir_full(root, "[.]xml$", recursive = TRUE, ignore.case = TRUE)
}
