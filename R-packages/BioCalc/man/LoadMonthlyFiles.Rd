\name{LoadMonthlyFiles}

\alias{LoadMonthlyFiles}

\title{Load Monthly Rasters}

\description{
 Load a set of 12 raster files for a specified variable
}

\usage{
 LoadMonthlyFiles(folder, ext='', varbl='', format='')
}

\arguments{
  \item{folder}{The folder in which your input files are stored}
  \item{ext}{The file extension of your input raster files}
  \item{varbl}{The name of your variable: tmin, tmax, tmean, prec}
  \item{format}{The format of your input files. Can be raster, ascii, BIL, or any supported by the raster package}
}

\details{
The function only works with prefixes tmin, tmax, tmean, and prec. If your files have any other prefix, please change accordingly.
In the case of tmean, if the file does not exist, the program automatically calculates it as the average of tmin and tmax.
This function is based upon several features of the raster package (available at sourceforge.net)
}

\value{
  A list of rasters from which a stack can be created.
}

\author{Julian Ramirez, dawnpatrolmustaine@gmail.com}

\seealso{ \code{\link[BioCalc]{BioCalc}} }

\examples{
# Folder where your data is stored (in ascii format, for example)
# dir <- "C://MyMonthlyData//"
#
# Suppose you want to load precipitation (prec) grids
# rlist <- LoadMonthlyFiles(dir, ext='.asc', varbl='prec', format='ascii')
# rlist
}

\keyword{bioclimatics}
