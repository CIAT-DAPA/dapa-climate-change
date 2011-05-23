\name{report}
\alias{report}
\title{Create a report...}
\usage{report(params=ccafs_params, r=list(), r_lab="", pdf_name="test_pdf")}
\description{Create a report}
\value{prints a report and save it as pdf}
\arguments{\item{params}{an object of the class AnalogueParameters}
\item{r}{a \code{list} of rasters}
\item{r_lab}{\code{vector} of labels corresponding to \code{r}}
\item{pdf_name}{path and name to output pdf}}
\examples{report(ccafs_params, list(results[[1]]), "Test", "test.pdf")}
