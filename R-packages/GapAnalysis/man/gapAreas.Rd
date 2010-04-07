\name{gapAreas}

\alias{gapAreas}

\title{Ecogeographic gap analysis}

\description{
 Calculates areas with defficient sampling at the geographic and environmental level
}

\usage{
 gapAreas(pointdens, gthresh=10, evdist, ethresh=10, outfile='')
}

\arguments{
  \item{pointdens}{A raster file or RasterLayer object with previously calculated point densities}
  \item{gthresh}{The threshold below which areas are considered a gap in respect to point densities}
  \item{evdist}{A raster file or RasterLayer object with previously calculated environmental distances}
  \item{ethresh}{The threshold above which areas are considered a gap in respect to environmental distances}
  \item{outfile}{The name of the output raster}
}

\details{
The threshold used in point densities (gthresh) is used to select areas where the sampling is considered to be poor. In other words, areas BELOW this 
threshold would be a geographic gap. In the other hand, the threshold used in environmental distances (ethresh) is used to select areas which are 
environmentally far from the core multidimensional environmental distribution of your accessions; in other words, areas ABOVE this threshold are 
considered as environmental gaps.
}

\value{
  A RasterLayer object with three values corresponding to sampling deficiencies: 0 for no gap areas, 1 for geographic gaps alone, 2 for environmental 
  gaps alone, and 3 indicates areas where both environmental and geographic gaps are found. Areas with falling in class 3 should be prioritized.
}

\author{Julian Ramirez, \email{dawnpatrolmustaine@gmail.com}}

\references{Not yet}

\seealso{ \code{\link[GapAnalysis]{pointDensity}}, \code{\link[GapAnalysis]{evDistance}} }

\examples{
# Load input data
mkr <- raster(system.file("external/mask.grd", package="GapAnalysis")) #This is commonly the iSPAM surface
infile <- system.file("external/accessions.csv", package="GapAnalysis")
#
# Now compute point densities
pdr <- pointDensity(mkr, infile, radius=3, type='simple', outfile="pd_out.grd")
# plot(pdr)
#
# Define your inputs
spms <- raster(system.file("external/mask.grd", package="GapAnalysis"))
bclist <- system.file(paste("external/bioclim/bio_", c(1:4, 6:19), ".grd", sep=""), package="GapAnalysis")
bcvars <- stack(bclist)
infile <- system.file("external/accessions_with_data.csv", package="GapAnalysis")
sfile <- system.file("external/env_matrix.csv", package="GapAnalysis")
#
# Now calculate environmental distances (Mahalanobis)
edr <- evDistance(spms, bcvars, ext='', infile, sfile, oper='min', nacc='', outfile="env_dist.grd")
#plot(edr)
#
# Finally compute the gappy areas
gpa <- gapAreas(pdr, gthresh=6, edr, ethresh=10, outfile="gap_areas.grd")
# 
}

\keyword{gaps}
