#If on Ubuntu may need to 
#sudo apt-get install libgdal-dev
#sudo apt-get upgrade libpng-dev

#If using Microsoft R Open then need to run this too. 
ifelse(!nzchar(Sys.getenv("TCL_LIBRARY")), Sys.setenv(TCL_LIBRARY="/usr/lib64/microsoft-r/3.3/lib64/R/share/tcl8.6/"), NA)

library(devtools)
devtools::install_github(repo="nz-mbie/mbie-r-package-public", subdir = "pkg",force=T)
devtools::install_github(repo="nz-mbie/mbiemaps-public", subdir = "pkg",force=T)


