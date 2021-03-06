% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_into_root_folder_file_extension.R
\name{split_into_root_folder_file_extension}
\alias{split_into_root_folder_file_extension}
\title{Split Full Paths into Root, Folder, File and Extension}
\usage{
split_into_root_folder_file_extension(paths, n_root_parts = 0)
}
\arguments{
\item{paths}{vector of character representing full file paths}

\item{n_root_parts}{number of first path segments considered as "root"}
}
\value{
data frame with columns \code{root}, \code{folder}, \code{file},
  \code{extension}, \code{depth}
}
\description{
Split Full Paths into Root, Folder, File and Extension
}
\examples{
paths <- c(
  "//always/the/same/root/project-1/intro.doc",
  "//always/the/same/root/project-1/logo.png",
  "//always/the/same/root/project-2/intro.txt",
  "//always/the/same/root/project-2/planning/file-1.doc",
  "//always/the/same/root/project-2/result/report.pdf"
)

split_into_root_folder_file_extension(paths)
split_into_root_folder_file_extension(paths, n_root_parts = 6)
split_into_root_folder_file_extension(paths, n_root_parts = 7)

}
