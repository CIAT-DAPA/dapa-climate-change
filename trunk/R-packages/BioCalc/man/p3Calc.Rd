\name{p3Calc}

\alias{p3Calc}

\title{P3. Isothermality}

\description{
 Calculates the isothermality (P2/P7) based upon previously calculated bioclimatics
}

\usage{
 p3Calc(p2, p7, outfile, format='')
}

\arguments{
  \item{p2}{a RasterLayer object being P2: annual mean diurnal temperature range}
  \item{p7}{a RasterLayer object being P7: annual temperature range}
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
# p2 <- raster("C://MyOutput//P2.asc")
# p7 <- raster("C://MyOutput//P7.asc")
#
# Now calculate the bioclimatic variable
# outputfile <- "C://MyOutput//P3.asc"
# p3 <- p3Calc(p2, p7, outputfile, format='ascii')
# 
}

\keyword{bioclimatics}
