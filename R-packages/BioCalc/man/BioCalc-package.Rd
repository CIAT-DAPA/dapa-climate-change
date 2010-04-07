\name{BioCalc-package}

\alias{BioCalc-package}

\docType{package}

\title{Bioclimatic variable calculation}

\description{
 Calculates the whole set, or any needed 
}

\details{
\tabular{ll}{
Package: \tab BioCalc\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2009-12-26\cr
License: \tab GPL2.0\cr
LazyLoad: \tab yes\cr
}
Main functions are:
BioCalc(folder, type='')
LoadMonthlyFiles(folder, varbl='', type'')
p1Calc(rlist, filename, format='')

Among others (pXXCalc, XX = 1 to 19) that can be viewed by typing help(function) in the R console.
}

\author{
Julian Ramirez

Maintainer: Julian Ramirez <dawnpatrolmustaine@gmail.com>
}
\references{Busby J.R. 1991. BIOCLIM: a bioclimatic analysis and prediction system. In C.R. Margules, M.P. Austin, eds. Nature conservation: cost effective biological surveys and data analysis, pp. 64,68. Canberra, Australia, Commonwealth Scientific and Industrial Research Organisation, CSIRO.}

\keyword{ bioclimatics }

\examples{
# Folder where your data is stored (in ascii format, for example)
# dir <- "C://MyMonthlyData//"
#
# Load monthly precipitation to calculate P12 (total annual rainfall)
# preclist <- LoadMonthlyFiles(dir, ext='.asc', varbl='prec', format='ascii')
#
# Now calculate P12
# outfile <- paste(dir, "//P12.asc", sep="")
# p12 <- p12Calc(preclist, outfile, format='ascii')
# 
# Or just calculate the whole set
# bioclim <- BioCalc(dir, ext='.asc', format='ascii')
#
}
