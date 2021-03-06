% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions_original.R
\name{fars_read}
\alias{fars_read}
\title{Read csv.bz2 file and create a data frame tbl.}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A csv.bz2 file that can be explicitly specify
or from a string "character" generated with the \code{make_filename} function of this package.}
}
\value{
This function returns the dataframe tbl from an input that is a csv.bz2 file.
}
\description{
This is a function that read a csv.bz2 file. You can
manually write the name of the csv.bz2 file in the filename parameter
and it is utilized in conjuntion with the \code{make_filename} function for files obtain
from the US National Highway Traffic Safety Administration's that have this
nomenclature "accident_%d.csv.bz2" where %d represent a year.
It will return a tibble object that have all these classes: 'tbl_df', 'tbl' and 'data.frame'.
The function return an error message if the file does not exist or if it is not find.
}
\examples{
\dontrun{
data <- fars_read(filename = "./accident_2013.csv.bz2")
file<-make_filename(year=2013)
data<-fars_read(filename = file)
}
}
