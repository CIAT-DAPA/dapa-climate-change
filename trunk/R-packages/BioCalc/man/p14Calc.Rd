\name{p14Calc}

\alias{p14Calc}

\title{P14. Precipitation of driest month}

\description{
 Calculates the minimum monthly rainfall based upon monthly rainfall data
}

\usage{
 p14Calc(rlist, outfile, format='')
}

\arguments{
  \item{rlist}{A list of rasters from 1 to 12 corresponding to precipitation}
  \item{outfile}{Name of the output file}
  \item{format}{The format of your input files. Can be GRD, ASCII, BIL...}
}

\details{
The list of rasters can be generated via the LoadMonthlyFiles function of this package.
This function is based upon several features of the raster package (available at sourceforge.net). Application of the functions
take very long since they are cell-by-cell calculations (when not enough space in memory is available). So, any efficient contributions
would be very useful.
}

\value{
  A RasterLayer object with the specified bioclimatic variable, plus the output file in the input format
}

\author{Julian Ramirez, dawnpatrolmustaine@gmail.com}

\references{Busby J.R. 1991. BIOCLIM: a bioclimatic analysis and prediction system. In C.R. Margules, M.P. Austin, eds. Nature conservation: cost effective biological surveys and data analysis, pp. 64,68. Canberra, Australia, Commonwealth Scientific and Industrial Research Organisation, CSIRO.}

\seealso{ \code{\link[BioCalc]{BioCalc}}, \code{\link[BioCalc]{LoadMonthlyFiles}} }

\examples{
# Folder where your data is stored (in ascii format, for example)
# dir <- "C://MyMonthlyData//"
#
# Load the files
# rasterlist <- LoadMonthlyFiles(dir, ext='.asc', varbl='prec', format='ascii')
#
# Now calculate the bioclimatic variable
# outputfile <- "C://MyOutput//P14.asc"
# p14 <- p14Calc(rasterlist, outputfile, format='ascii')
# 
}

\keyword{bioclimatics}
