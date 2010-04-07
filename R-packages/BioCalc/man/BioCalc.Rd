\name{BioCalc}

\alias{BioCalc}

\title{Bioclimatic variable calculation}

\description{
 Calculates the complete set of 19 bioclimatics commonly used in niche modeling
}

\usage{
 BioCalc(folder, ext='', format='')
}

\arguments{
  \item{folder}{The folder in which your input monthly files are stored}
  \item{ext}{The file extension of your input monthly or whatever files}
  \item{format}{The format of your input files. Can be raster, ascii, BIL, or any supported in the package raster}
}

\details{
Your folder should contain at least three variables (tmin, tmax, prec) for each of the 12 months in order to perform the calculation.
This function is based upon several features of the raster package (available at sourceforge.net). Application of the functions
take very long since they are cell-by-cell calculations (when not enough space in memory is available). So, any efficient contributions
would be very useful.
}

\value{
  A stack with the 19 bioclimatic variables, and each of the variables stored in your input folder (in the same input format)
}

\author{Julian Ramirez, dawnpatrolmustaine@gmail.com}

\references{Busby J.R. 1991. BIOCLIM: a bioclimatic analysis and prediction system. In C.R. Margules, M.P. Austin, eds. Nature conservation: cost effective biological surveys and data analysis, pp. 64,68. Canberra, Australia, Commonwealth Scientific and Industrial Research Organisation, CSIRO.}

\seealso{ \code{\link[BioCalc]{p1Calc}}, \code{\link[BioCalc]{p2Calc}} }

\examples{
# Folder where your data is stored (in ascii format, for example)
# dir <- "C://MyMonthlyData//"
#
# Now just calculate the whole set
# bioclim <- BioCalc(dir, ext=".asc", format='ascii')
# 
}

\keyword{bioclimatics}
