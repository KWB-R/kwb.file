# devtools::install_github("KWB-R/kwb.orcid")
#
# remotes::install_github("KWB-R/kwb.pkgbuild", build = TRUE, build_opts = c(
#   "--no-resave-data", "--no-manual"
# ))

package <- "kwb.file"

# Set the path to your new package (folder e.g. created with New Project... in
# RStudio, cloning the corresponding GitHub repo)

package_dir <- kwb.utils::safePath(
  kwb.utils::get_homedir(), "Documents/github-repos", package
)

# Create a default package structure
usethis::create_package(path = package_dir, open = FALSE)

# Delete the original DESCRIPTION file
fs::file_delete(path = file.path(package_dir, "DESCRIPTION"))

author <- list(
  name = "Hauke Sonnenberg",
  orcid = kwb.orcid::get_kwb_orcids()["Hauke Sonnenberg"],
  url = "https://github.com/hsonne"
)

description <- list(
  name = package,
  title = "Functions Related to File and Path Operations",
  desc  = paste(
    "This package provides helper functions that have been developed during",
    "different research projects at KWB. The functions are dealing with file",
    "operations and handling file and folder paths. Let's see what we have in",
    "different scripts and other packages and better fits here..."
  )
)

setwd(package_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.1.0.9000",
  stage = "experimental"
)

# And now, let's do the first commit and upload everything to GitHub
# (manually)...
