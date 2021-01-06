#Migrate all libraries
lib_loc <- "C:/R/win-library/3.5"
to_install <- unname(installed.packages(lib.loc = lib_loc)[, "Package"])
to_install
remove.packages(to_install, lib="C:/R/win-library/3.5")
install.packages(pkgs = to_install, lib="C:/R/R-4.0.2/library")

#Set new path
.libPaths("C:/R/win-library")