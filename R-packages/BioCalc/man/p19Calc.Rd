\name{p19Calc}

\alias{p19Calc}

\title{P19. Precipitation of warmest quarter}

\description{
 Calculates the total precipitation of the coldest three-month period based upon monthly mean temperature and rainfall data
}

\usage{
 p19Calc(rlist, rlist2, outfile, format='')
}

\arguments{
  \item{rlist}{A list of rasters from 1 to 12 corresponding to precipitation}
  \item{rlist2}{A list of rasters from 1 to 12 corresponding to mean temperatures}
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
# rasterlist1 <- LoadMonthlyFiles(dir, ext='.asc', varbl='prec', format='ascii')
# rasterlist2 <- LoadMonthlyFiles(dir, ext='.asc', varbl='tmean', format='ascii')
#
# Now calculate the bioclimatic variable
# outputfile <- "C://MyOutput//P19.asc"
# p19 <- p19Calc(rasterlist1, rasterlist2, outputfile, format='ascii')
# 
}

\keyword{bioclimatics}
