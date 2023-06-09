% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk_assembly.R
\name{cov_apply}
\alias{cov_apply}
\title{Apply covariates to PK(PD) dataset}
\usage{
cov_apply(
  df,
  cov,
  id.by = "USUBJID",
  time.by = NA,
  direction = "downup",
  exp = F,
  ebe = F,
  cov.rnd = NA,
  na = -999,
  demo.map = T,
  keep.other = T
)
}
\arguments{
\item{df}{PK(PD) dataframe generated by pk_build}

\item{cov}{dataframe of covariates}

\item{id.by}{id variable to merge covariates}

\item{time.by}{time variable to merge covariates}

\item{direction}{fill direction for time-varying covariates}

\item{exp}{treats new covariates as exposure metrics when TRUE}

\item{ebe}{treats new covariates as empirical bayes estimates when TRUE}

\item{cov.rnd}{covariate rounding parameter}

\item{na}{value to replace NA numeric covariates}

\item{demo.map}{toggle pre-set numeric values for SEX, RACE, and ETHNIC demographic variables}

\item{keep.other}{filter to keep or remove other events, EVID = 2}
}
\value{
PK(PD) dataset with additional covariates
}
\description{
Add covariates to a dataset built by pk_build() or pk_combine()
Can add subject-level covariates (by any ID variable) or time-varying (by any time variable)
}
