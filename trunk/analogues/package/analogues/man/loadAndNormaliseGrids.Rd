\name{loadAndNormaliseGrids}
\alias{loadAndNormaliseGrids}
\title{Load a raster an normalises it...}
\usage{loadAndNormaliseGrids(x)}
\description{Load a raster an normalises it}
\details{This functions loads raster grids
and normlizes them to a mean of 0 and sd of 1}
\value{a RasterLayer}
\arguments{\item{path}{to a raster}}
\examples{r <- loadAndNormaliseGrids("weather.asc")}
