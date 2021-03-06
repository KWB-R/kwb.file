# copy_files_to_target_dir -----------------------------------------------------

#' Copy Files to Flat Structure
#' 
#' Calls \code{file.copy} under the hood but gives a message about the indices
#' and paths of the files that could not be copied.
#' 
#' @param from_paths paths to the files to be copied
#' @param target_dir path to the target directory
#' @param target_files relative paths to the target files, relative to
#'   \code{target_dir}
#' @export
#' @importFrom kwb.utils stringList collapsed
#' @importFrom utils head
#' @examples 
#' root <- system.file(package = "kwb.file")
#' 
#' relative_paths <- dir(root, recursive = TRUE)
#' 
#' # The original files are in root or in different subfolders
#' relative_paths
#' 
#' # Create a temporary target folder
#' target_dir <- kwb.utils::createDirectory(file.path(tempdir(), "target"))
#' 
#' # Copy all files into one target folder without subfolders
#' from_paths <- file.path(root, relative_paths)
#' to_paths <- basename(from_paths)
#' 
#' # Make sure that the target file names contain no duplicates, otherwise
#' # an error is raised
#' to_paths <- kwb.utils::makeUnique(to_paths, warn = FALSE)
#' 
#' # Copy the files
#' copy_files_to_target_dir(from_paths, target_dir, to_paths)
#' 
#' # Look at the result
#' dir(target_dir, recursive = TRUE)
#' 
copy_files_to_target_dir <- function(from_paths, target_dir, target_files)
{
  to_paths <- file.path(target_dir, target_files)

  is_duplicated <- duplicated(target_files)
  
  if (sum(is_duplicated)) {
    
    stop(
      call. = FALSE, "The path(s) to the target file(s) must not contain ", 
      "duplicates but they do:\n  ", 
      kwb.utils::stringList(utils::head(target_files[is_duplicated]))
    )
  }
  
  success <- file.copy(from = from_paths, to = to_paths)
  
  if (! all(success)) {
    
    message(
      sprintf(
        "\n*** Could not copy these files (indices %s):\n\n- ", 
        kwb.utils::collapsed(which(! success), ",")
      ),
      kwb.utils::collapsed(from_paths[! success], "\n- ")
    )
  }
}

# get_download_dir -------------------------------------------------------------

#' Get Default Download Directory
#' 
#' @return assumed default download directory on the user's computer (vector of
#'   character of length one)
#'   
#' @export
#' @importFrom kwb.utils selectElements
#' @examples
#' dir_full(get_download_dir())
#' 
get_download_dir <- function()
{
  download_dirs <- list(windows = "~/../Downloads", unix = "~/Downloads")
  
  kwb.utils::selectElements(download_dirs, .Platform$OS.type)
}

# to_simple_names --------------------------------------------------------------

#' Convert Long File Paths to Simple Paths
#' 
#' @param paths vector of character containing file paths
#' @param method \code{method = 1}: file names generated match the pattern
#'   \code{file_<xx>} with \code{<xx>} being an integer number of two digits.
#'   \code{method = 2}: file names generated match the pattern \code{file_<sha>}
#'   with \code{<sha>} being the first \code{sha1_digits} digits of the sha1
#'   hash (see e.g. \url{http://www.sha1-online.com/}) of the base names of the
#'   \code{paths}. By default, the base name is the file name (without folder
#'   path) without extension. The base names can be determined individually by
#'   providing a function in \code{get_base}
#' @param get_base function taking a vector of character as input and returning
#'   a vector of character as output. If not \code{NULL}, this function will be
#'   used to determine the base paths from the \code{paths} when \code{method =
#'   2} was specified.
#' @param sha1_digits number of digits used when \code{method = 2} is to be
#'   applied
#' @return vector of character as long as \code{paths}
#' @export
#' @importFrom digest sha1
#' @importFrom kwb.utils fileExtension removeExtension
#' @examples
#' paths <- c("v1_ugly_name_1.doc",  "v1_very_ugly_name.xml",
#'            "v2_ugly_name_1.docx", "v2_very_ugly_name.xmlx")
#'            
#' to_simple_names(paths, method = 1L)
#' writeLines(sort(to_simple_names(paths, method = 2L)))
#' 
#' # All sha1 are different because all base names (file name without extension
#' # by default) are different. If you want to give the same sha1 to files that 
#' # correspond to each other but have a different extension, set the function 
#' # that extracts the "base name" of the file:
#'
#' get_base <- function(x) kwb.utils::removeExtension(gsub("^v\\d+_", "", x))
#' 
#' writeLines(sort(to_simple_names(paths, method = 2L, get_base = get_base)))
#' 
#' # Now the file names that have the same base name (neglecting the prefix 
#' # v1_ or v2_) get the same sha1 and thus appear as groups in the sorted 
#' # file list
#' 
to_simple_names <- function(
  paths, method = 1L, get_base = NULL, sha1_digits = 4
)
{
  extensions <- kwb.utils::fileExtension(paths)
  
  simple_names <- if (method == 1L) {
    
    sprintf("file_%02d.%s", seq_along(paths), extensions)
    
  } else if (method == 2L) {
    
    if (is.null(get_base)) {
      
      get_base <- function(path) kwb.utils::removeExtension(basename(path))
    }
    
    sha1 <- sapply(get_base(paths), digest::sha1)
    
    sprintf("file_%s.%s", substr(sha1, 1, sha1_digits), extensions)
    
  } else {
    
    stop(call. = FALSE, "No such method: ", method)
  }
  
  stopifnot(! any(duplicated(simple_names)))
  
  simple_names
}
