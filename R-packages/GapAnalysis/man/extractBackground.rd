\name{extractBackground}

\alias{extractBackground}

\title{Extract background values}

\description{
 Extracts the values of a random set of point locations or for all pixels in a group of raster layers (RasterStack), stores the output values in a comma separated value (.csv) file, and returns a data frame as output.
}

\usage{
 extractBackground(msk, variables, ext='', n=100, outfile='')
}

\arguments{
  \item{msk}{A RasterLayer object or path pointing to a file indicating the geographic space where the samples are to be taken}
  \item{variables}{RasterStack object with your set of variables, or path pointing to the folder where your variables are stored. If a path is used you need to specify the ext argument}
  \item{ext}{The extension of your raster layers, can be empty when using a RasterStack object}
  \item{n}{The number of random locations for which data will be extracted, should be numeric and greater than zero, or 'all' to indicate all pixels are being sampled}
  \item{outfile}{The name of the output file}
}

\details{
 This function allows to extract the values in order to further compute the environmental distance between accessions (evDistance function of this package). It only extracts data for those cells having a value different to NA. The function allows to extract the values for a set of random pixels within a mask, and for any set of raster layers (grids), and store them into a .csv file. You can also perform this step manually using other the raster package (see functions xyValues(...), sampleRandom(...), and xyFromCell(...)), and then store the data using the function write.csv(...) of the utils package. You can also use other GIS software if you consider the functions within R take longer than they should.
 
 The ext parameter does not need to be specified when you put a RasterStack in the variables argument. When reading rasters composed by two files, you need to specify in the ext parameter only one (the principal) raster layer file. In R raster layers (composed by .gri and .grd) it would be .grd, in BILs it would be .bil, etc...
 
 The n parameter can be a number (from 1 to the number of cells), or 'all' if you wish to sample all the cells within the raster (this is suggested for rasters that can be easily loaded in memory. If you provide a value greater than the number of cells in your mask, the program will take, by default, all cells within the mask.
 
 The output of this function is intended to be used in the evDistance(...) function of this package.
}

\value{
 A data frame with the four basic fields (row id, cell id, longitude, latitude), and additional columns depending on the number of variables you are using.
}

\author{Julian Ramirez, \email{dawnpatrolmustaine@gmail.com}}

\references{Not yet}

\seealso{ \code{\link[raster]{xyValues}}, \code{\link[GapAnalysis]{evDistance}} }

\examples{
# Create your RasterStack of environmental variables
envl <- system.file(paste("external/bioclim/bio_", c(1:19), ".grd", sep=""), package="GapAnalysis")
envl <- stack(envl)
ofl <- "background.csv"
mask <- raster(system.file("external/mask.grd", package="GapAnalysis"))

# Now create your output
bkg <- extractBackground(mask, envl, ext='', n=100, outfile=ofl)
}

\keyword{gaps}
