% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk_assembly.R
\name{cov_find}
\alias{cov_find}
\title{Find covariates of particular types}
\usage{
cov_find(df, cov, type)
}
\arguments{
\item{df}{PK(PD) dataset}

\item{cov}{covariate distribution}

\item{type}{covariate type}
}
\value{
vector of desired column names
}
\description{
Can filter for categorical, continuous, or other covariates.
Can filter for numeric or character type.
}
