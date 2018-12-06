# read_file_metadata -----------------------------------------------------------

#' Read File Metadata from YAML-File
#' 
#' @param yaml_file path to YAML-File containing file metadata (as saved with
#'   \code{kwb.file:::write_file_info_to_yaml_file})
#' @param file_encoding passed to argument \code{fileEncoding} of
#'   \code{\link[yaml]{read_yaml}}
#' @param out_class one of "data.frame", "list"
#' @return depending on \code{out_class}, either a data frame with the following
#'   columns or a list with the following elements is returned: 
#'   \describe{
#'     \item{file_id}{clean file name given to the original file for simpler
#'       access}, 
#'     \item{original_name}{original file name given by data provider},
#'     \item{original_folder}{original path to folder in which file was
#'       provided.}.
#'   }
read_file_metadata <- function(
  yaml_file, file_encoding = "UTF-8", out_class = c("data.frame", "list")[1]
)
{
  stopifnot(out_class %in% c("list", "data.frame"))
  
  file_info <- yaml::read_yaml(yaml_file, fileEncoding = file_encoding)
  
  root_node <- kwb.utils::selectElements(file_info, "files")
  
  if (out_class == "data.frame") {
    
    kwb.utils::rbindAll(lapply(root_node, kwb.utils::asNoFactorDataFrame))
    
  } else {
    
    file_info
  }
}
