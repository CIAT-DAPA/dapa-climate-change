\name{GapAnalysis-package}

\alias{GapAnalysis-package}
\alias{GapAnalysis}

\docType{package}

\title{Ecogeographic gap analysis}

\description{
 Calculates the areas where further collections should be done by geographic and environmental distances
}

\details{
\tabular{ll}{
Package: \tab GapAnalysis\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2009-12-29\cr
License: \tab GPL2.0\cr
LazyLoad: \tab yes\cr
\cr
Main functions are:\cr
\cr
pointDensity(msk, accfile, radius=3, type='', outname='')\cr
evDistance(spamsurf, bioclimvars, ext='', acclist, smplfile, oper='min', nacc='', outfile='')\cr
gapAreas(pointdens, gthresh=10, evdist, ethresh=10, outfile='')\cr
}
}

\author{
Julian Ramirez

Maintainer: Julian Ramirez <dawnpatrolmustaine@gmail.com>
}

\references{No refs. yet}

\keyword{ gaps }

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
