\name{extractStoreValues}

\alias{extractStoreValues}

\title{Extract values of a set of points}

\description{
 Extracts the values of a set of point locations in a group of raster layers (RasterStack), stores the output values in a comma separated value (.csv) file, and returns a data frame as output.
}

\usage{
 extractStoreValues(variables, ext='', accfile, outfile='')
}

\arguments{
  \item{variables}{RasterStack object with your set of variables, or path pointing to the folder where your variables are stored. If a path is used you need to specify the ext argument}
  \item{ext}{The extension of your raster layers, can be empty when using a RasterStack object}
  \item{accfile}{A .csv file with nrow equal to the number of accessions and ncol equal to 4. idrow, idaccession, lon, lat}
  \item{outfile}{The name of the output file}
}

\details{
 This function allows to extract the values in order to further compute the environmental distance between accessions (evDistance function of this package). The function allows to extract the values for any set of accessions, and for any set of raster layers (grids), and store them into a .csv file. You can also perform this step manually using other the raster package (function xyValues(...)), and then store the data using the function write.csv(...) of the utils package. You can also use other GIS software if you consider the functions within R take longer than they should.
 
 The ext parameter does not need to be specified when you put a RasterStack in the variables argument. When reading rasters composed by two files, you need to specify in the ext parameter only one (the principal) raster layer file. In R raster layers (composed by .gri and .grd) it would be .grd, in BILs it would be .bil, etc...
 
 The accession list file is a .csv file with 4 columns, the first two ones are the row number and the accession ID, the third column must be the longitude (x), and the fourth column must be the latitude (y).
}

\value{
  A data frame with the same fields as the input accessions file and with the values corresponding to the sampled environmental layers in other columns of the data frame.
}

\author{Julian Ramirez, \email{dawnpatrolmustaine@gmail.com}}

\references{Not yet}

\seealso{ \code{\link[raster]{xyValues}}, \code{\link[GapAnalysis]{evDistance}} }

\examples{
# Create your RasterStack of variables
envl <- system.file(paste("external/bioclim/bio_", c(1:19), ".grd", sep=""), package="GapAnalysis")
envl <- stack(envl)
accf <- system.file("external/accessions.csv", package="GapAnalysis")

# Now create your output
evl <- extractStoreValues(envl, ext='', accf, outfile='acc_with_data.csv')
}

\keyword{gaps}
