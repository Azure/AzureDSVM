# install packages

installPkgs <- function(list_of_pkgs, lib=.libPaths()) {
  if (!(lib %in% .libPaths())) {
    dir.create(lib, showWarnings=FALSE)
    .libPaths(c(lib, .libPaths()))
  }
  
  new_packages <- list_of_pkgs[!(list_of_pkgs %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) {
    sapply(new_packages, install.packages, lib=lib)
  }
}
