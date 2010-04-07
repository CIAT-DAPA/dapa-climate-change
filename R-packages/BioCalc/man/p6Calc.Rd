\name{p6Calc}

\alias{p6Calc}

\title{P6. Minimum temperature of coldest month}

\description{
 Calculates the minimum temperature of the coldest month
}

\usage{
 p6Calc(rlist, outfile, format='')
}

\arguments{
  \item{rlist}{A list of rasters from 1 to 12 corresponding to minimum temperatures}
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
  A RasterLayer object with minimum yearly temperature, plus the output file in the input format
}

\author{Julian Ramirez, dawnpatrolmustaine@gmail.com}

\references{Busby J.R. 1991. BIOCLIM: a bioclimatic analysis and prediction system. In C.R. Margules, M.P. Austin, eds. Nature conservation: cost effective biological surveys and data analysis, pp. 64, 68. Canberra, Australia, Commonwealth Scientific and Industrial Research Organisation (CSIRO).}

\seealso{ \code{\link[BioCalc]{BioCalc}}, \code{\link[BioCalc]{LoadMonthlyFiles}} }

\examples{
# Folder where your data is stored (in ascii format, for example)
# dir <- "C://MyMonthlyData//"
#
# Load the files
# rasterlist <- LoadMonthlyFiles(dir, ext='.asc', varbl='tmin', format='ascii')
#
# Now calculate the bioclimatic variable
# outputfile <- "C://MyOutput//P6.asc"
# p6 <- p6Calc(rasterlist, outputfile, format='ascii')
# 
}

\keyword{bioclimatics}
