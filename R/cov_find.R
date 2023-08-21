#' Find covariates of particular types
#'
#' Can filter for categorical, continuous, or other covariates.
#' Can filter for numeric or character type.
#'
#' @param df PK(PD) dataset
#' @param cov covariate distribution
#' @param type covariate type
#'
#' @return vector of desired column names
#'
#' @examples
#' ## Simple ex domain with 1 subject and 1 dose
#' ex <- data.frame(STUDYID = "ABC101",
#'                  USUBJID = "ABC101-001",
#'                  EXSTDTC = "2000-01-01 10:00:00",
#'                  EXSTDY = 1,
#'                  EXTPTNUM = 0,
#'                  EXDOSE = 100,
#'                  CMT = 1,
#'                  EXTRT = "ABC",
#'                  EXDOSU = "mg",
#'                  VISIT = "Day 1",
#'                  EXTPT = "Dose",
#'                  EXDOSFRQ = "Once",
#'                  EXROUTE = "Oral")
#'
#' ## Simple pc domain with 1 subject and 3 observations
#' pc <- data.frame(USUBJID = "ABC101-001",
#'                  PCDTC = c("2000-01-01 09:40:00",
#'                            "2000-01-01 10:29:00",
#'                            "2000-01-01 12:05:00"),
#'                  PCDY = 1,
#'                  PCTPTNUM = c(0, ##Units of hours
#'                               0.021,
#'                               0.083),
#'                  PCSTRESN = c(NA,
#'                               469,
#'                               870),
#'                  PCLLOQ = 25,
#'                  CMT = 2,
#'                  VISIT = "Day 1",
#'                  PCTPT = c("Pre-dose",
#'                            "30-min post-dose",
#'                            "2-hr post-dose"),
#'                  PCTEST = "ABC",
#'                  PCSTRESU = "ug/mL")
#'
#' ## Create with pk_build()
#' df <- pk_build(ex, pc)
#'
#' ## Simple dm domain for the corresponding study
#' dm <- data.frame(USUBJID = c("ABC101-001",
#'                              "ABC101-002",
#'                              "ABC101-003"),
#'                  AGE = c(45,
#'                          37,
#'                          73),
#'                  AGEU = "years",
#'                  SEX = c("Male",
#'                          "Female",
#'                          "Male"),
#'                  RACE = c("White",
#'                           "White",
#'                           "Black"),
#'                  ETHNIC = c("Not Hispanic/Latino",
#'                             "Not Hispanic/Latino",
#'                             "Not Hispanic/Latino"))
#'
#' ## Add covariates with cov_apply()
#' df1 <- cov_apply(df, dm)
#'
#' ## Find covariates with cov_find()
#' cov_find(df1, cov="categorical", type="numeric")
#' cov_find(df1, cov="categorical", type="character")
#' cov_find(df1, cov="continuous", type="numeric")
#' cov_find(df1, cov="units", type="character")
#'
#' @export
cov_find <- function(df, cov, type) {
  NSTUDY <- DOSEA <- PDOSEF <- TIMEU <- DTIM <- NULL

  df1 <- dplyr::select(df, NSTUDY, DOSEA:PDOSEF)
  df1 <- dplyr::select(df1, -DOSEA, -PDOSEF)

  df2 <- dplyr::select(df, NSTUDY, TIMEU:DTIM)
  df2 <- dplyr::select(df2, -TIMEU, -DTIM)

  if (cov=="categorical") {
    if (type=="numeric") {
      covs <- colnames(df1)[grepl("^N", colnames(df1)) | grepl("^T", colnames(df1))]
      covs <- covs[paste0(covs, "C") %in% colnames(df)]
      return(covs)
    }
    else if (type=="character") {
      covs <- colnames(df1)[grepl("^N", colnames(df1)) | grepl("^T", colnames(df1))]
      covs <- paste0(covs[paste0(covs, "C") %in% colnames(df)], "C")
      return(covs)
    }
    else {
      stop("type must be numeric or character")
    }
  }

  else if (cov=="continuous") {
    if (type=="numeric") {
      covs <- colnames(df1)[grepl("^B", colnames(df1)) | grepl("^T", colnames(df1))]
      covs <- covs[!paste0(covs, "C") %in% colnames(df)]
      return(covs)}
    else if (type=="character") {
      stop("continuous covariates must be numeric only")
    }
    else {
      stop("type must be numeric")
    }
  }

  else if (cov=="units") {
    if (type=="numeric") {
      stop("units' type must be character.")
    }
    else if (type=="character") {
      covs <- colnames(df2)[grepl("^B", colnames(df2)) | grepl("^T", colnames(df2))]
      covs <- covs[grepl("U$", covs)]
      return(covs)
    }
    else {
      stop("type must be character")
    }
  }

  else if (cov=="exposure") {
    if (type=="numeric") {
      covs <- colnames(df1)[grepl("^C", colnames(df1))]
      return(covs)
    }
    else {
      stop("type must be numeric")
    }
  }

  else if (cov=="empirical bayes estimate") {
    if (type=="numeric") {
      covs <- colnames(df1)[grepl("^I", colnames(df1))]
      return(covs)
    }
    else {
      stop("type must be numeric")
    }
  }

  else if (cov=="other") {
    if (type=="numeric") {
      covs <- colnames(df1)[!grepl("^N", colnames(df1)) & !grepl("^T", colnames(df1)) & !grepl("^B", colnames(df1)) & !grepl("^C", colnames(df1)) & !grepl("^I", colnames(df1))]
      return(covs)
    }
    else if (type=="character") {
      covs <- colnames(df2)[!grepl("^N", colnames(df2)) & !grepl("^T", colnames(df2)) & !grepl("^C", colnames(df2)) & !grepl("^I", colnames(df2))]
      return(covs)
    }
    else {
      stop("type must be numeric or character")
    }
  }

  else {
    stop("cov must be categorical, continuous, exposure, empirical bayes estiamte, or other")
  }
}
