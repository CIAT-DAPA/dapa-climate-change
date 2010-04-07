\name{pointDensity}

\alias{pointDensity}

\title{Point density}

\description{
 Calculates the density of points for a specified raster and a specified point distribution
}

\usage{
 pointDensity(msk, accfile, radius=3, type='', outfile='')
}

\arguments{
  \item{msk}{A mask indicating for which pixels the calculation will be done, can be a path pointing to a file, or a RasterLayer object}
  \item{accfile}{A .csv file with the list of accessions with coordinates}
  \item{radius}{The radius of the circular neighborhood where accessions are to be counted}
  \item{type}{The type of calculation to perform: simple, garea, parea}
  \item{outfile}{The name of the output raster}
}

\details{
The mask (msk) is a surface with 1s and NAs. Areas with 1s are those where the calculations will be done.

The list of accessions (acclist) is simply a comma separated values (csv) file in which the 1st and 2nd columns are numbers (useful only for identification 
of accessions), and the 3rd and 4th correspond to longitude (x) and latitude (y) values of each accession.

The radius should be in map units (if lat, lon, the it would be degrees). You need to choose this value appropriately for your point distribution.

The calculation type (type) is simple if you want to compute using Euclidean distances, and return a raster in which each pixel indicates the number 
of accessions in the specified neighborhood; garea uses the Euclidean distance and returns a raster in which each pixel indicates the number of accessions 
per unit area (squared map units); parea uses the GreatCircle distance and returns a raster in which each pixel indicates the number of accessions per
squared kilometer (km2).
}

\value{
  A RasterLayer object corresponding to the density of points
}

\author{Julian Ramirez, \email{dawnpatrolmustaine@gmail.com}}

\references{Not yet}

\seealso{ \code{\link[raster]{pointDistance}} }

\examples{
#
# Load input data
mkr <- raster(system.file("external/mask.grd", package="GapAnalysis")) #This is commonly the iSPAM surface
infile <- system.file("external/accessions.csv", package="GapAnalysis")
#
# Now compute point densities
pdr <- pointDensity(mkr, infile, radius=3, type='simple', outfile="pd_out.grd")
# plot(pdr)
}

\keyword{gaps}
