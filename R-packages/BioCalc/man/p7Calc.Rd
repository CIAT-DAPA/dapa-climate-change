\name{p7Calc}

\alias{p7Calc}

\title{P7. Annual temperature range}

\description{
 Calculates the isothermality (P5 - P6) based upon previously calculated bioclimatics
}

\usage{
 p7Calc(p5, p6, outfile, format='')
}

\arguments{
  \item{p5}{a RasterLayer object being P5: max. temp. warmest month}
  \item{p6}{a RasterLayer object being P6: min. temp. coldest month}
  \item{outfile}{Name of the output file}
  \item{format}{The format of your input files. Can be GRD, ASCII, BIL...}
}

\details{
This function is based upon several features of the raster package (available at sourceforge.net). Application of the functions
take very long since they are cell-by-cell calculations (when not enough space in memory is available). So, any efficient contributions
would be very useful.
}

\value{
  A RasterLayer object with the averaged values of the monthly input files, and the raster itself stored as a file.
}

\author{Julian Ramirez, dawnpatrolmustaine@gmail.com}

\references{Busby J.R. 1991. BIOCLIM: a bioclimatic analysis and prediction system. In C.R. Margules, M.P. Austin, eds. Nature conservation: cost effective biological surveys and data analysis, pp. 64, 68. Canberra, Australia, Commonwealth Scientific and Industrial Research Organisation (CSIRO).}

\seealso{ \code{\link[BioCalc]{BioCalc}}, \code{\link[BioCalc]{LoadMonthlyFiles}} }

\examples{
# Folder where your data is stored (in ascii format, for example)
#
# Load the files
# p5 <- raster("C://MyOutput//P5.asc")
# p6 <- raster("C://MyOutput//P6.asc")
#
# Now calculate the bioclimatic variable
# outputfile <- "C://MyOutput//P7.asc"
# p7 <- p7Calc(p5, p6, outputfile, format='ascii')
# 
}

\keyword{bioclimatics}
