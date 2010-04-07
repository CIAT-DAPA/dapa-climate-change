\name{evDistance}

\alias{evDistance}

\title{Environmental distance of accessions}

\description{
 Calculates the minimum Mahalanobis distance of each cell on a grid with respect to a set of accessions
}

\usage{
 evDistance(spamsurf, variables, ext='', acclist, smplfile, oper='min', nacc='', outfile='')
}

\arguments{
  \item{spamsurf}{Path to the file or RasterLayer object corresponding to the base mask, or areas where you want to compute values}
  \item{variables}{A RasterStack corresponding to the variables, or the folder in which your input enviromental envelope rasters are stored}
  \item{ext}{The extension of your environmental envelope rasters, can be empty or NULL when using a RasterStack}
  \item{acclist}{A .csv file with nrow equal to the number of accessions and ncol equal to the number of environmental layers plus 4, it should be idrow, idaccession, lon, lat, var1...varn.}
  \item{smplfile}{A .csv file corresponding to background sampling of your multi dimensional space. Random locations over your mask area or even your entire mask area. This file should contain the same number of columns as your acclist file. This is the base for the covariance matrix of the Mahalanobis distance function}
  \item{oper}{The type of operation to perform: min, max, mean, meanmin, meanmax}
  \item{nacc}{The number of accessions on which to perform the specified operation, only relevant for meanmin and meanmax, can be NULL for other oper values}
  \item{outfile}{The name of the output file. This is stored in R raster format: .grd}
}

\details{
The spam surface and the bioclimatic rasters need to have the same extent and cellsize. The biofolder argument should contain raster files in any of the supported formats (see writeFormats() of the raster package for further information on this. Argument ext needs to be specified in order to load the files.

The list of accessions (acclist) is simply a comma separated values (csv) file in which the 1st and 2nd columns are numbers (useful only for identification of accessions), the 3rd and 4th correspond to longitude (x) and latitude (y) values of each accession, while the other fields (5th to [nlayers+4]) correspond to each of the environmental envelope variables. The example provided here uses a bioclimatic envelope composed by 18 variables derived from monthly data. But you can use any layers you might desire. Just keep in mind that these 18 bioclimatic layers are highly correlated and the covariance matrix will tend to turn singular (not inversion possible), thus making the Mahalanobis distance computation impossible. For this set of variables, we found that P5 (max. temperature of warmest month) was causing the problem, and in the examples section we removed it.

The sample file (smplfile) corresponds to a comma separated values (csv) file over the space where you want to perform your calculation. Usually this file contains a random sample of the pixels in the spam surface (spamsurf). However, it can contain the all the pixels if you desire so. This file can be created by the function extractBackground(...) of this package. The file should have the same structure as the accessions file (acclist), same number of columns and number. These data are used to compute the covariance matrix for calculating the Mahalanobis distance.

The oper argument is used to determine the distance to be put in each pixel of the output raster. Each time the Mahalanobis distance of a pixel to the set of accessions is calculated, it returns a vector with length equal to the number of accessions, so this argument was created to defin which value of the vector will be used as pixel value; min will indicate the minimum Mahalanobis distance between the pixel and the set of accessions will be used; max will put the maximum distance; mean will calculate the average of the output vector; meanmin will calculate the average of the nearest nacc accessions; whilst meanmax will calculate the average of the furthest nacc accessions. This is often because one single accession can be an outlier of the distribution, so taking the average of a subset of accessions (either the nearest or furthest groups) will ensure you're not using an outlier. However, you can first perform an outlier test to test if your distribution has any outliers, and decide what operator to use in view of that.
}

\value{
  A RasterLayer object and file corresponding to the Mahalanobis distance of each pixel respect to the entire set of accessions
}

\author{Julian Ramirez, \email{dawnpatrolmustaine@gmail.com}; Jacob van Etten, \email{jacobvanetten@yahoo.com}}

\references{Not yet}

\seealso{ \code{\link[GapAnalysis]{gapAreas}}, \code{\link[GapAnalysis]{pointDensity}} }

\examples{
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
}

\keyword{gaps}
