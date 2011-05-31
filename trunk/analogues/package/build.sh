#!/bin/sh
##
##
## Must be run from the parent of the package directory (no options
## to change target of check or tarball!?!)
## source: http://stackoverflow.com/questions/4380849/roxygen-package-building-and-use-rd2-true

PACKAGE=$1
VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

R --no-restore --slave <<EOR
  library(roxygen)
  roxygenize(package.dir="${PACKAGE}",
             roxygen.dir="${PACKAGE}",
             use.Rd2=TRUE,
             overwrite=TRUE,
             copy.package=FALSE,
             unlink.target=FALSE)
EOR

R CMD build ${PACKAGE}
# R CMD check ${PACKAGE}_${VERSION}.tar.gz
R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz

## Build the windows package
R CMD INSTALL -l localRlib ${PACKAGE}_${VERSION}.tar.gz
cd localRlib
zip -r $PACKAGE $PACKAGE
cd ..
mv localRlib/$PACKAGE.zip .

