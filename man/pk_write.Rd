% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk_assembly.R
\name{pk_write}
\alias{pk_write}
\title{Write PK(PD) dataset to specified location}
\usage{
pk_write(df, file)
}
\arguments{
\item{df}{PK(PD) dataframe}

\item{file}{filepath}
}
\value{
NA
}
\description{
Dataset created by pk_build() or pk_combine() will be outputted as a .csv file with NONMEM-standard formatting.
}
