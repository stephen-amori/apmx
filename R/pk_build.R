#' Create a NONMEM PK(PD) dataset
#'
#' Input a pre-processed ex and pc domain for combination into a NONMEM dataset.
#' Additional pd endpoints, subject-level covariates, and time-varying covariates can also be added.
#' Other parameters can customize some calculations and formatting.
#'
#' @param ex dose event dataframe
#' @param pc pc event dataframe
#' @param pd pd event dataframe
#' @param sl.cov subject-level covariate dataframe
#' @param tv.cov time-varying covariate dataframe
#' @param time.units units for time attributes
#' @param cycle.length cycle length in units of days
#' @param na value for missing numeric items
#' @param time.rnd time attribute rounding parameter
#' @param amt.rnd amount attribute rounding parameter
#' @param dv.rnd dependent variable attribute rounding parameter
#' @param cov.rnd covariate attribute rounding parameter
#' @param impute imputation method
#' @param BDV baseline pd attribute
#' @param DDV change from baseline pd attribute
#' @param PDV percent change from baseline pd attribute
#' @param sparse threshold for sparse sampling
#' @param demo.map toggle pre-set numeric values for SEX, RACE, and ETHNIC demographic variables
#' @param tv.cov.fill time-varying covariate fill direction
#' @param keep.other filter to keep or remove other events, EVID = 2
#'
#' @return PK(PD) dataset
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
#' @export
#'
pk_build <- function(ex, pc=NA, pd=NA, sl.cov=NA, tv.cov=NA,
                     time.units="days", cycle.length=NA, na=-999,
                     time.rnd=NULL, amt.rnd=NULL, dv.rnd=NULL, cov.rnd=NULL,
                     impute=NA, BDV=F, DDV=F, PDV=F, sparse=3,
                     demo.map = T, tv.cov.fill = "downup", keep.other=T) {

  USUBJID <- NDAY <- TPT <- ADDL <- II <- IMPEX <- ODV <- IMPDV <- DTIM <- NULL
  CMT <- EVID <- NTFD <- LDOSE1 <- TIMEU <- LDOSE2 <- NDOSE1 <- NDOSE2 <- NULL
  FDOSE <- tII <- ATFD <- NTLD <- ATLD <- PCATFD <- PCTPT <- PCDTIM <- NULL
  IMPDTIM <- EXATFD <- EXNTFD <- DUR <- AMT <- BLQ <- DVID <- DVIDC <- NULL
  DOSEA <- NROUTEC <- NROUTE <- NFRQ <- NFRQC <- DOSENUM <- NSTUDY <- NULL
  NSTUDYC <- PDOS <- DUPF <- AMTF <- MAX <- SPARSEF <- SDF <- NOEXF <- NULL
  NTLC <- RATE <- LDV <- C <- SUBJID <- ID <- MDV <- LLOQ <- LINE <- NULL
  VISIT <- TPTC <- DOMAIN <- DVIDU <- VERSN <- BUILD <- DNTFD <- TIMEF <- NULL

  func.version <- "0.3.0"

  ###EX QC###
  cdisc.cols.ex <- data.frame("COLUMN" = c("USUBJID", "DTIM", "NDAY",
                                           "TPT", "AMT", "STUDY", "VISIT",
                                           "TPTC", "DVID", "DVIDU", "ROUTE", "FRQ"),
                              "CDISC" = c("USUBJID$", "EXSTDTC$|EXDTC$|ASTDTM$|ADT$", "EXSTDY$",
                                          "EXTPTNUM$", "EXDOSE$|AVAL$", "STUDYID$", "VISIT$",
                                          "EXTPT$", "EXTRT$", "EXDOSU$", "EXROUTE$", "EXDOSFRQ$"))

  if(!is.data.frame(ex)) {
    stop("ex must be a data frame of dose events.")
  }

  for (i in 1:length(colnames(ex))) {
    if (!colnames(ex)[i] %in% cdisc.cols.ex$COLUMN) {
      for (j in 1:nrow(cdisc.cols.ex)) {
        if (grepl(cdisc.cols.ex$CDISC[j], colnames(ex)[i])) {
          colnames(ex)[i] <- cdisc.cols.ex$COLUMN[j]
        }
      }
    }
  }

  req.cols <- c("USUBJID", "DTIM", "NDAY", "TPT", "AMT", "VISIT", "CMT", "TPTC", "DVID", "DVIDU", "ROUTE", "FRQ")

  for (i in req.cols) {
    if (!any(i %in% colnames(ex))) {
      stop(paste0("Column ", i, " is missing from the ex dataset."))}

    if (i %in% c("NDAY", "TPT", "AMT", "CMT") & !is.numeric(unlist(ex[, i]))) {
      stop(paste0("Column ", i, " in ex is not numeric type."))}

    if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "ROUTE", "FRQ", "DVIDU") & !is.character(unlist(ex[, i]))) {
      stop(paste0("Column ", i, " in ex is not character type."))}

    if(i %in% c("USUBJID", "CMT") & TRUE %in% is.na(ex[, i])) {
      stop(paste0(i, " missing in ex for at least 1 row."))}
  }

  ex <- dplyr::arrange(ex, USUBJID, NDAY, TPT)

  ex.nonmem <- c()
  ex.col.c <- c()
  ex.col.n <- c("DOSENUM", "DOSEA")
  for (i in 1:length(colnames(ex))) {
    name = colnames(ex)[i]
    if (name %in% c("STUDY", "ROUTE", "FRQ")) {
      if(!is.character(unlist(ex[,name]))) {
        stop(paste(name, "in ex must be character type."))
      }
      ex.col.c <- c(ex.col.c, paste0("N", name, "C"))
      ex.col.n <- c(ex.col.n, paste0("N", name))
      ex[, paste0("N", name)] <- match(unlist(ex[, name]), sort(unique(unlist(ex[, name]))))
      if(length(unique(ex[, name]))==2) {
        ex[, paste0("N", name)] <- ex[, paste0("N", name)]-1
      }
      colnames(ex)[colnames(ex)==name] <- paste0("N", name, "C")
    }
    else if (!any(name %in% req.cols)) {
      if(name %in% c("ADDL", "II", "SS", "DUR")) {
        ex.nonmem <- c(ex.nonmem, name)
      }
      else if(is.character(unlist(ex[, name])) & (!name %in% c("DVIDU", "DOMAIN", "STUDY"))) {
        ex.col.c <- c(ex.col.c, name)
      }
      else if (!name %in% c("DVIDU", "DUR", "CMT", "DOMAIN", "STUDY")) {
        ex.col.n <- c(ex.col.n, name)
      }
    }
  }

  if ("ADDL" %in% colnames(ex) & !("II" %in% colnames(ex))) {
    stop("If ex contains ADDL, it must contain II")
  }

  if ("II" %in% colnames(ex) & !("ADDL" %in% colnames(ex))) {
    stop("If ex contains II, it must contain ADDL")
  }

  if (!"ADDL" %in% colnames(ex)) {
    ex <- dplyr::mutate(ex, ADDL = NA)
  }

  if (!"II" %in% colnames(ex)) {
    ex <- dplyr::mutate(ex, II = NA)
  }

  if (!"IMPEX" %in% colnames(ex)) {
    ex <- dplyr::mutate(ex, IMPEX = 0)
  }

  ex$DOSENUM <- NA
  for (i in 1:nrow(ex)) {
    if (i==1) {
      ex$DOSENUM[i] <- 1
    }
    else if (ex$USUBJID[i]!=ex$USUBJID[i-1]) {
      ex$DOSENUM[i] <- 1
    }
    else {
      ex$DOSENUM[i] <- 1 + ex$DOSENUM[i-1] + ifelse(is.na(ex$ADDL[i]), 0, ex$ADDL[i])
    }
  }

  if(!any("EVID" %in% colnames(ex))) {
    ex <- dplyr::mutate(as.data.frame(ex), EVID = 1)
  } #add event ID for dose events

  if(!any("DOMAIN" %in% colnames(ex))) {
    ex <- dplyr::mutate(as.data.frame(ex), DOMAIN = "EX")
  }

  if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", ex$DTIM[!is.na(ex$DTIM)])) {
    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", ex$DTIM[!is.na(ex$DTIM)])) {
      stop("DTIM in ex is not ISO 8601 format.")
    }
  }

  ex <- dplyr::mutate(ex, DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                                  grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                                  grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                                  grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

  if(0 %in% ex[, "NDAY"]) {
    stop("NDAY in ex has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
  }

  if(FALSE %in% is.na(unlist(ex[is.na(ex$ADDL), "II"]))) {
    stop("At least one row in ex has a documented II when ADDL is NA.")
  }
  if(FALSE %in% is.na(unlist(ex[is.na(ex$II), "ADDL"]))) {
    stop("At least one row in ex has a documented ADDL when II is NA.")
  }

  usubjid <- unique(ex$USUBJID)

  ###PC QC###
  pc.col.c <- c()
  pc.col.n <- c()

  if (!is.data.frame(pc) & !is.data.frame(pd)) {
    stop("Please enter a pc or pd domain.")
  }

  if (is.data.frame(pc)) {
    cdisc.cols.pc <- data.frame("COLUMN" = c("USUBJID", "DTIM", "NDAY",
                                             "TPT", "ODV", "LLOQ", "STUDY", "VISIT",
                                             "TPTC", "DVID", "DVIDU"),
                                "CDISC" = c("USUBJID$", "PCSTDTC$|PCDTC$|ASTDTM$|ADT$", "PCDY$",
                                            "PCTPTNUM$", "PCSTRESN$|AVAL$", "PCLLOQ$", "STUDYID$", "VISIT$",
                                            "PCTPT$", "PCTEST$|PCTESTCD$", "PCSTRESU$|PCORRESU$"))

    for (i in 1:length(colnames(pc))) {
      if (!colnames(pc)[i] %in% cdisc.cols.pc$COLUMN) {
        for (j in 1:nrow(cdisc.cols.pc)) {
          if (grepl(cdisc.cols.pc$CDISC[j], colnames(pc)[i])) {
            colnames(pc)[i] <- cdisc.cols.pc$COLUMN[j]
          }
        }
      }
    }

    req.cols <- c("USUBJID", "DTIM", "NDAY", "TPT", "ODV", "LLOQ", "CMT", "VISIT", "TPTC", "DVID", "DVIDU")
    for (i in req.cols) {
      if (!any(i %in% colnames(pc))) {
        stop(paste0("Column ", i, " is missing from the pc dataset."))
      }

      if (i %in% c("NDAY", "TPT", "ODV", "CMT") & !is.numeric(unlist(pc[, i]))) {
        stop(paste0("Column ", i, " in pc is not numeric type."))
      }

      if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "DVIDU") & !is.character(unlist(pc[, i]))) {
        stop(paste0("Column ", i, " in pc is not character type."))
      }

      if(i %in% c("USUBJID", "CMT", "DVID") & TRUE %in% is.na(pc[, i])) {
        stop(paste0(i, " missing in pc for at least 1 row."))
      }
    }

    for (i in colnames(pc)) {
      if (!any(i %in% req.cols)) {
        if(is.character(unlist(pc[, i])) & !i %in% c("DOMAIN")) {
          pc.col.c <- c(pc.col.c, i)
        }
        else if (!i %in% c("DOMAIN")) {
          pc.col.n <- c(pc.col.n, i)
        }
      }
    }

    if(!any("LDV" %in% colnames(pc))) {
      pc <- dplyr::mutate(as.data.frame(pc), LDV = log(ODV))
    }

    if(!any("EVID" %in% colnames(pc))) {
      pc <- dplyr::mutate(as.data.frame(pc), EVID = 0)
    }

    if(!any("DOMAIN" %in% colnames(pc))) {
      pc <- dplyr::mutate(as.data.frame(pc), DOMAIN = "PC")
    }

    check <- dplyr::filter(pc, ODV<=0)
    if(nrow(check) > 0) {
      stop("At least one dependent variable in PC is less than or equal to 0.")
    }

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", pc$DTIM[!is.na(pc$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", pc$DTIM[!is.na(pc$DTIM)])) {
        stop("DTIM in pc is not ISO 8601 format for at least one row.")
      }
    }

    pc <- dplyr::mutate(pc, DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

    pc <- dplyr::mutate(pc, IMPDV = ifelse(!"IMPDV" %in% colnames(pc), 0, IMPDV))

    if(0 %in% pc[, "NDAY"]) {
      stop("NDAY in pc has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
    }

    usubjid <- unique(c(usubjid, pc$USUBJID))
    dvids <- unique(pc$DVID)
  }

  ###PD QC###
  pd.col.n <- c()
  pd.col.c <- c()

  if (is.data.frame(pd)) {
    req.cols <- c("USUBJID", "DTIM", "NDAY", "TPT", "ODV", "LLOQ", "CMT", "VISIT", "TPTC", "DVID", "DVIDU")
    for (i in req.cols) {
      if (!any(i %in% colnames(pd))) {
        stop(paste0("Column ", i, " is missing from the pd dataset."))
      }

      if (i %in% c("NDAY", "TPT", "ODV", "CMT") & !is.numeric(unlist(pd[, i]))) {
        stop(paste0("Column ", i, " in pd is not numeric type."))
      }

      if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "DVIDU") & !is.character(unlist(pd[, i]))) {
        stop(paste0("Column ", i, " in pd is not character type."))
      }

      if(i %in% c("USUBJID", "CMT", "DVID") & TRUE %in% is.na(pd[, i])) {
        stop(paste0(i, " missing in pd for at least 1 row."))
      }
    }

    for (i in colnames(pd)) {
      if (!any(i %in% req.cols)) {
        if(is.character(unlist(pd[, i])) & i!="DOMAIN") {
          pd.col.c <- c(pd.col.c, i)
        }
        else if (i!="DOMAIN") {
          pd.col.n <- c(pd.col.n, i)
        }
      }
    }

    if(!any("LDV" %in% colnames(pd))) {
      pd <- dplyr::mutate(as.data.frame(pd), LDV = log(ODV))
    }

    if(!any("EVID" %in% colnames(pd))) {
      pd <- dplyr::mutate(as.data.frame(pd), EVID = 0)
    }

    if(!any("DOMAIN" %in% colnames(pd))) {
      pd <- dplyr::mutate(as.data.frame(pd), DOMAIN = "PD")
    }

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", pd$DTIM[!is.na(pd$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", pd$DTIM[!is.na(pd$DTIM)])) {
        stop("DTIM in pd is not ISO 8601 format.")
      }
    }

    pd <- dplyr::mutate(pd, DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                                    grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

    if(0 %in% pd[, "NDAY"]) {
      stop("NDAY in pd has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
    }

    usubjid <- unique(c(usubjid, pd$USUBJID))
    dvids <- unique(c(dvids, pd$DVID))

    pd <- dplyr::mutate(pd, IMPDV = ifelse(!"IMPDV" %in% colnames(pd), 0, IMPDV))
  }

  ###SL.COV QC###
  if(is.list(sl.cov) & !is.data.frame(sl.cov)) {
    for (i in 1:length(sl.cov)) {
      if (!"USUBJID" %in% colnames(sl.cov[[i]])) {
        stop(paste0("USUBJID not including in all sl.cov dataframes."))
      }
    }

    sl.cov <- suppressMessages(purrr::reduce(sl.cov, dplyr::full_join, ))
  }

  if (is.data.frame(sl.cov)) {
    req.cols <- c("USUBJID")
    for (i in req.cols) {
      if (!any(i %in% colnames(sl.cov))) {
        stop(paste0("Column ", i, " is missing from the sl.cov dataset."))
      }

      if (i %in% c("USUBJID") & !is.character(unlist(sl.cov[, i]))) {
        stop(paste0("Column ", i, " in sl.cov is not character type."))
      }

      if(i %in% c("USUBJID") & TRUE %in% is.na(sl.cov[, i])) {
        stop(paste0(i, " missing in sl.cov for at least 1 row."))
      }
    }

    if (!"STUDY" %in% colnames(sl.cov)) {
      if(!"NSTUDYC" %in% colnames(ex)) {
        stop("STUDY column must be included in ex or sl.cov.")
      }
    }

    if(nrow(sl.cov)!=length(unique(sl.cov$USUBJID))) {
      stop("sl.cov has duplicate USUBJID rows.")
    }

    missing <- c()
    for (i in unique(usubjid)) {
      if (!i %in% sl.cov$USUBJID) {
        missing <- c(missing, i)
      }
    }

    if (length(missing)>=1) {
      warning(paste0("The following USUBJID(s) have PKPD events but are not in sl.cov: ", paste0(unique(missing), collapse = ", ")))
    }
  }

  if(!is.data.frame(sl.cov)) {
    if (is.na(sl.cov)) {
      if(!"NSTUDYC" %in% colnames(ex)) {
        stop("STUDY column must be included in ex or sl.cov.")
      }
    }
  }

  ###TV.COV QC###
  if(is.list(tv.cov) & !is.data.frame(tv.cov)) {
    tv.cov <- purrr::reduce(tv.cov, dplyr::bind_rows)
  }

  if (is.data.frame(tv.cov)) {
    req.cols <- c("USUBJID", "DTIM")
    for (i in req.cols) {
      if (!any(i %in% colnames(tv.cov))) {
        stop(paste0("Column ", i, " is missing from the tv.cov dataset."))
      }

      if (i %in% c("USUBJID") & !is.character(unlist(tv.cov[, i]))) {
        stop(paste0("Column ", i, " in tv.cov is not character type."))
      }

      if(i %in% c("USUBJID", "DTIM") & TRUE %in% is.na(tv.cov[, i])) {
        stop(paste0(i, " missing in tv.cov for at least 1 row."))
      }
    }

    covs <- colnames(tv.cov)[!(colnames(tv.cov) %in% c("USUBJID", "DTIM"))]

    tv.cov <- as.data.frame(tv.cov)
    tv.cov <- dplyr::arrange(tv.cov, USUBJID, DTIM)
    tv.cov <- dplyr::group_by(tv.cov, USUBJID, DTIM)
    tv.cov <- tidyr::fill(tv.cov, tidyselect::all_of(covs), .direction="downup")
    tv.cov <- dplyr::ungroup(tv.cov)
    tv.cov <- dplyr::distinct(tv.cov)
    tv.cov <- dplyr::mutate(tv.cov, EVID = 2)
    tv.cov <- dplyr::mutate(tv.cov, DOMAIN = "TVCOV")

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
        if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
          stop("DTIM in tv.cov is not ISO 8601 format.")
        }
      }
    }

    tv.cov <- dplyr::mutate(tv.cov, DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M"),
                                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d")))

    check <- dplyr::mutate(tv.cov, Check = paste0(USUBJID, DTIM))

    if(nrow(tv.cov)!=length(unique(check$Check))) {
      stop("tv.cov has duplicate USUBJID-DTIM rows.")
    }

    missing <- c()
    for (i in unique(usubjid)) {
      if (!i %in% tv.cov$USUBJID) {
        missing <- c(missing, i)
      }
    }

    if (length(missing)>=1) {
      warning(paste0("The following USUBJID(s) have PKPD events but are not in tv.cov: ", paste0(unique(missing), collapse = ", ")))
    }
  }

  ###OTHER QC###
  if (!any(time.units %in% c("days", "hours"))) {
    stop("time.units parameter must be in days or hours.")
  }

  if(!is.null(time.rnd)) {
    if(is.numeric(time.rnd)) {
      if(time.rnd%%1!=0) {
        stop("time.rnd parameter must an integer (the number of rounded decimal points).")
      }
    }
    else {
      stop("time.rnd parameter must be an integer (the number of rounded decimal points).")
    }
  }

  if(!is.null(amt.rnd)) {
    if(is.numeric(amt.rnd)) {
      if(amt.rnd%%1!=0) {
        stop("amt.rnd parameter must an integer (the number of rounded decimal points).")
      }
    }
    else {
      stop("amt.rnd parameter must be an integer (the number of rounded decimal points).")
    }
  }

  if(!is.null(dv.rnd)) {
    if(is.numeric(dv.rnd)) {
      if(dv.rnd%%1!=0) {
        stop("dv.rnd parameter must an integer (the number of rounded decimal points).")
      }
    }
    else {
      stop("dv.rnd parameter must be an integer (the number of rounded decimal points).")
    }
  }

  if(!is.null(cov.rnd)) {
    if(is.numeric(cov.rnd)) {
      if(cov.rnd%%1!=0) {
        stop("cov.rnd parameter must an integer (the number of rounded decimal points).")
      }
    }
    else {
      stop("cov.rnd parameter must be an integer (the number of rounded decimal points).")
    }
  }

  if (BDV==F & DDV==T & !any("BDV" %in% colnames(pd))) {
    stop("BDV parameter must be TRUE or BDV column must be included in pd to create DDV.")
  }

  if (BDV==F & PDV==T & !any("BDV" %in% colnames(pd))) {
    stop("BDV parameter must be TRUE or BDV column must be included in pd to create PDV.")
  }

  if (DDV==F & PDV==T & !any("BDV" %in% colnames(pd)) & !any("DDV" %in% colnames(pd))) {
    stop("DDV parameter must be TRUE or BDV & DDV columns must be included in pd to create PDV.")
  }

  if (!any(tv.cov.fill %in% c("down", "downup", "up", "updown"))) {
    stop("tv.cov.fill parameter must be a tidy direction (down, up, downup, updown).")
  }

  if (!is.numeric(sparse)) {
    stop("sparse parameter must be numeric to set the threshold for sparse flag.")
  }

  if (sparse<=0) {
    stop("sparse parameter must be greater than 0.")
  }

  if (!is.na(cycle.length)) {
    if (!is.numeric(cycle.length)) {
      stop("cycle.length parameter must be numeric or NA.")
    }
    if (cycle.length<=0) {
      stop("cycle.length parameter must be greater than 0.")
    }
  }

  if(!is.numeric(na)) {
    stop("na parameter must be numeric.")
  }

  if(!is.na(impute)) {
    if (!(impute %in% c(1, 2))) {
      stop("impute parameter must be method 1, method 2, or NA.")
    }
  }

  if(is.na(demo.map)) {
    stop("demo.map parameter must be TRUE or FALSE.")
  }

  if(!is.logical(demo.map)) {
    stop("demo.map parameter must be TRUE or FALSE.")
  }

  if(!is.logical(keep.other)) {
    stop("keep.other parameter must be TRUE or FALSE.")
  }

  ###BIND EVENTS TOGETHER###
  df <- ex

  if (is.data.frame(pc)) {
    df <- dplyr::bind_rows(df, pc)
    df <- dplyr::arrange(df, USUBJID, NDAY, TPT, CMT, -EVID)
  }

  if(is.data.frame(pd)) {
    df <- dplyr::bind_rows(df, pd)
    df <- dplyr::arrange(df, USUBJID, NDAY, TPT, CMT, -EVID)
  }

  ###ACTUAL + NOMINAL TIME CALCULATIONS###
  df <- dplyr::mutate(df,
                      TIMEU = time.units,
                      NTFD = dplyr::case_when(NDAY==na | TPT==na ~ -999,
                                              TIMEU=="days" & NDAY>=1 ~ NDAY-1 + TPT,
                                              TIMEU=="days" & NDAY<0 ~ NDAY+TPT,
                                              TIMEU=="hours" & NDAY>=1 ~ 24*(NDAY-1) + TPT,
                                              TIMEU=="hours" & NDAY<0 ~ 24*(NDAY) + TPT),
                      NDOSE1 = ifelse(EVID %in% c(1, 4), NTFD, NA),
                      NDOSE2 = ifelse(EVID %in% c(1, 4), NTFD+ADDL*II, NA),
                      LDOSE1 = ifelse(EVID %in% c(1, 4), as.character(DTIM), NA),
                      LDOSE1 = ifelse(EVID %in% c(1, 4) & is.na(LDOSE1), "1900-01-01 00:00:00", LDOSE1),
                      LDOSE2 = ifelse(EVID %in% c(1, 4),
                                      ifelse(TIMEU=="days", as.character(DTIM+ADDL*II*3600*24), as.character(DTIM+ADDL*II*3600)),
                                      NA),
                      LDOSE2 = ifelse(EVID %in% c(1, 4) & is.na(LDOSE2), "1900-01-01 00:00:00", LDOSE2),
                      tII = ifelse(EVID %in% c(1, 4),
                                   ifelse(is.na(II), 0, II),
                                   NA))
    df <- dplyr::arrange(df, USUBJID, EVID)
    df <- dplyr::group_by(df, USUBJID, EVID)
    df <- dplyr::mutate(df, FDOSE = ifelse(EVID==1 & dplyr::row_number()==1, as.character(DTIM), NA))
    df <- dplyr::ungroup(df)
    df <- dplyr::group_by(df, USUBJID)
    df <- tidyr::fill(df, NDOSE1, NDOSE2, .direction="downup")
    df <- dplyr::ungroup(df)
    df <- dplyr::arrange(df, USUBJID, DTIM, NDAY, TPT, CMT, -EVID)
    df <- dplyr::group_by(df, USUBJID)
    df <- tidyr::fill(df, LDOSE1, LDOSE2, FDOSE, tII, .direction="downup")
    df <- dplyr::ungroup(df)
    df <- dplyr::mutate(df,
                        LDOSE1 = ifelse(LDOSE1=="1900-01-01 00:00:00", NA, LDOSE1),
                        LDOSE2 = ifelse(LDOSE2=="1900-01-01 00:00:00", NA, LDOSE2),
                        FDOSE = ifelse(nchar(FDOSE)==10, paste(FDOSE, "00:00:00"), FDOSE),
                        FDOSE = as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                        LDOSE1 = ifelse(nchar(LDOSE1)==10, paste(LDOSE1, "00:00:00"), LDOSE1),
                        LDOSE1 = as.POSIXct(LDOSE1, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                        LDOSE2 = ifelse(nchar(LDOSE2)==10, paste(LDOSE2, "00:00:00"), LDOSE2),
                        LDOSE2 = as.POSIXct(LDOSE2, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                        ATFD = as.numeric(difftime(DTIM, FDOSE, units=time.units)),
                        ATLD = ifelse(is.na(ATFD), NA,
                                      dplyr::case_when(DTIM >= LDOSE2 ~ as.numeric(difftime(DTIM, LDOSE2, units=time.units)),
                                                       is.na(tII) ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)),
                                                       tII==0 ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)),
                                                       ATFD<=0 ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)),
                                                       TRUE ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)) %% tII)))
    df <- dplyr::mutate(df,
                        NTLD = dplyr::case_when(NTFD==-999 ~ na,
                                                NTFD >= NDOSE2 ~ NTFD-NDOSE2,
                                                is.na(tII) ~ NTFD-NDOSE1,
                                                tII==0 ~ NTFD-NDOSE1,
                                                ATFD<=0 ~ NTFD-NDOSE1,
                                                TRUE ~ (NTFD-NDOSE1) %% tII),
                        NTLD = ifelse(NTLD==-999, NA, NTLD),
                        NTFD = ifelse(NTFD==-999, NA, NTFD))
    df <- dplyr::select(df, -LDOSE1, -LDOSE2, -NDOSE1, -NDOSE2, -tII)

  ###IMPUTATION METHODS###
  if (!is.na(impute)) {
    if (impute==1) {
      df <- dplyr::mutate(df,
                          IMPEX = ifelse(is.na(ATFD) & !is.na(NTFD) & EVID==1, 1, IMPEX),
                          IMPDV = ifelse(is.na(ATFD) & !is.na(NTFD) & EVID==0, 1, IMPDV),
                          ATLD = ifelse(is.na(ATFD), NTLD, ATLD),
                          ATFD = ifelse(is.na(ATFD), NTFD, ATFD))
    }

    if (impute==2) {
      df <- dplyr::arrange(df, USUBJID, NTFD, EVID)
      df <- dplyr::group_by(df, USUBJID, NDAY)
      df <- dplyr::mutate(df,
                          PCATFD = ifelse(EVID==0, ATFD, NA),
                          PCTPT = ifelse(EVID==0 & is.na(ATFD) & is.na(FDOSE), TPT, NA),
                          PCTPT = ifelse(EVID==0 & !is.na(ATFD) & !is.na(FDOSE), TPT, PCTPT),
                          PCDTIM = ifelse(EVID==0, as.character(DTIM), NA))
      df <- tidyr::fill(df, PCATFD, PCTPT, PCDTIM, .direction="updown")
      df <- dplyr::ungroup(df)
      df <- dplyr::mutate(df,
                          IMPEX = dplyr::case_when(EVID==1 & is.na(DTIM) ~ 1,
                                                   EVID==1 & is.na(IMPEX) ~ 0,
                                                   TRUE ~ IMPEX),
                          PCDTIM = ifelse(nchar(PCDTIM)==10, paste(PCDTIM, "00:00:00"), PCDTIM),
                          PCDTIM = as.POSIXct(PCDTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                          IMPDTIM = ifelse(EVID==1 & is.na(DTIM), as.character(PCDTIM-PCTPT*ifelse(time.units=="days", 24*60*60, 60*60)), NA),
                          IMPFEX = ifelse(is.na(FDOSE), 1, 0),
                          FDOSE = ifelse(is.na(FDOSE) & EVID==1 & NTFD==0, as.character(IMPDTIM), as.character(FDOSE)))
      df <- dplyr::group_by(df, USUBJID)
      df <- tidyr::fill(df, FDOSE, .direction="downup")
      df <- dplyr::ungroup(df)
      df <- dplyr::mutate(df,
                          FDOSE = ifelse(nchar(FDOSE)==10, paste(FDOSE, "00:00:00"), FDOSE),
                          FDOSE = as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                          IMPDTIM = ifelse(nchar(IMPDTIM)==10, paste(IMPDTIM, "00:00:00"), IMPDTIM),
                          IMPDTIM = as.POSIXct(IMPDTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                          ATFD = dplyr::case_when(!is.na(IMPDTIM) & is.na(ATFD) ~ as.numeric(difftime(IMPDTIM, FDOSE, units=time.units)),
                                                  EVID==1 & is.na(ATFD) & is.na(PCATFD) & !is.na(FDOSE) ~ as.numeric(difftime(DTIM, FDOSE, units=time.units)),
                                                  EVID==1 & is.na(ATFD) & is.na(PCATFD) ~ NTFD,
                                                  EVID==1 & is.na(ATFD) ~ PCATFD-PCTPT,
                                                  TRUE ~ ATFD),
                          ATLD = ifelse(EVID==1 & is.na(ATLD), 0, ATLD))
      df <- dplyr::mutate(df,
                          EXATFD = ifelse(EVID==1, ATFD, NA),
                          EXNTFD = ifelse(EVID==1, NTFD, NA),
                          IMPDTIM = as.character(IMPDTIM))
      df <- dplyr::group_by(df, USUBJID)
      df <- tidyr::fill(df, EXATFD, EXNTFD, IMPDTIM, IMPEX, .direction="downup")
      df <- dplyr::mutate(df,
                          IMPDV = ifelse(EVID==0 & is.na(DTIM), 1, IMPDV),
                          IMPDTIM = ifelse(nchar(IMPDTIM)==10, paste(IMPDTIM, "00:00:00"), IMPDTIM),
                          IMPDTIM = as.POSIXct(IMPDTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                          ATFD = dplyr::case_when(EVID==0 & is.na(ATFD) & !is.na(DTIM) ~ as.numeric(difftime(DTIM, IMPDTIM, units=time.units)),
                                                  EVID==0 & is.na(ATFD) & is.na(EXATFD) ~ NTFD,
                                                  EVID==0 & is.na(ATFD) ~ EXATFD-EXNTFD+NTFD,
                                                  TRUE ~ ATFD),
                          ATLD = dplyr::case_when(EVID==0 & is.na(ATLD) & is.na(EXATFD) ~ NTLD,
                                                  EVID==0 & is.na(ATLD) ~ ATFD-EXATFD,
                                                  EVID==0 & IMPEX==1 ~ ATFD-EXATFD,
                                                  TRUE ~ ATLD),
                          NTLD = dplyr::case_when(IMPDV==1 | IMPFEX==1 | IMPEX==1 ~ NTFD-EXNTFD,
                                                  TRUE ~ NTLD))
      df <- dplyr::ungroup(df)
      df <- dplyr::select(df, -PCATFD, -PCTPT, -EXATFD, -EXNTFD, -IMPDTIM)
      df <- dplyr::arrange(df, USUBJID, ATFD, EVID, CMT)
    }
  }

  ###DOSE AND OBSERVATION CALCULATIONS###
  if ("DUR" %in% colnames(ex)) {
    if (!"RATE" %in% colnames(ex)) {
      df <- dplyr::mutate(df, RATE = ifelse(!is.na(DUR), AMT/DUR, NA))
    }
    ex.nonmem <- c("RATE", ex.nonmem)
  }

  df <- dplyr::mutate(df,
                      DOSEA = ifelse(EVID==1, AMT, NA),
                      BLQ = dplyr::case_when(EVID==0 & is.na(ODV) & ATFD<=0 ~ 1, #pre-dose BLQ
                                             EVID==0 & is.na(ODV) ~ 2, #post-dose BLQ
                                             EVID==0 ~ 0, #NO BLQ
                                             TRUE ~ -99),
                      BLQ = ifelse(BLQ==-99, NA, BLQ), #NA for dose or other events
                      MDV = ifelse(is.na(ODV), 1, 0),
                      NTLC = dplyr::case_when(is.na(cycle.length) ~ NTFD,
                                              is.numeric(cycle.length) & NTFD<0 ~ NTFD,
                                              is.numeric(cycle.length) & time.units=="hours" ~ NTFD %% cycle.length*24,
                                              is.numeric(cycle.length) & time.units=="days" ~ NTFD %% cycle.length),
                      DVIDC = DVID,
                      DVID = match(DVIDC, dvids),
                      DVID = ifelse(EVID %in% c(1, 4), NA, DVID))
  df <- dplyr::group_by(df, USUBJID, NDAY)
  df <- tidyr::fill(df, IMPEX, .direction="downup") #apply to pre-dose records
  df <- dplyr::group_by(df, USUBJID)
  df <- tidyr::fill(df, DOSEA, IMPEX, NROUTEC, NROUTE, NFRQ, NFRQC, DOSENUM, .direction="downup") #apply to all records
  df <- dplyr::ungroup(df)

  if ("NSTUDY" %in% colnames(ex)) {
    df <- dplyr::group_by(df, USUBJID)
    df <- tidyr::fill(df, NSTUDY, NSTUDYC, .direction="downup")
    df <- dplyr::ungroup(df)
  }

  ###PD PROCESSING###
  pd.dvs <- c()
  if (is.data.frame(pd)) {

    if(BDV==T | "BDV" %in% colnames(pd)) {
      pd.dvs <- c(pd.dvs, "BDV")
    }
    if(DDV==T | "DDV" %in% colnames(pd)) {
      pd.dvs <- c(pd.dvs, "DDV")
    }
    if(PDV==T | "PDV" %in% colnames(pd)) {
      pd.dvs <- c(pd.dvs, "PDV")
    }

    if(BDV==T & !any("BDV" %in% colnames(pd))) {
      df <- dplyr::arrange(df, USUBJID, DVIDC, ATFD)
      df <- dplyr::mutate(df,
                          PDOS = ifelse(ATFD<=0 & EVID==0, 1, 0))
      df <- dplyr::group_by(df, USUBJID, DVIDC, PDOS)
      df <- dplyr::mutate(df,
                          BDV = ifelse(DVIDC %in% pd$DVID & PDOS==1, dplyr::last(ODV), NA))
      df <- dplyr::group_by(df, USUBJID, DVIDC)
      df <- tidyr::fill(df, BDV, .direction="downup")
      df <- dplyr::ungroup(df)
      df <- dplyr::arrange(df, USUBJID, ATFD, CMT, -EVID)

      for (i in unique(pd$DVID)) {
        check <- dplyr::filter(df, DVIDC==i)
        check <- dplyr::filter(check, is.na(BDV))
        check <- dplyr::filter(check, !is.na(ATFD))

        if (nrow(check)>0) {
          warning(paste0("The following USUBJID(s) do not have a baseline ", i, " observation at or prior to first dose (BDV, DDV, PDV not calculated): ", paste0(sort(unique(check$USUBJID)), collapse = ", ")))
        }
      }
    }

    if(DDV==TRUE & !any("DDV" %in% colnames(pd))) {
      df <- dplyr::mutate(df, DDV = ODV-BDV)
    }

    if(PDV==TRUE & !any("PDV" %in% colnames(pd))) {
      df <- dplyr::mutate(df, PDV = 100*DDV/BDV)
    }
  }

  ###PRE-PROCESS COVARIATES###
  s.cat.cov.c <- c() #vector to contain subject-level character categorical covariates
  s.cont.cov <- c() #vector to contain subject-level continuous covariates
  s.cat.cov.n <- c() #vector to contain subject-level numeric categorical covariates
  stud.col.n <- c("NSTUDY") #vector for study column
  stud.col.c <- c("NSTUDYC") #vector for study column
  s.cont.cov.n <- c() # was missing
  s.cont.cov.units <- c()

  if (is.data.frame(sl.cov)==TRUE) {

    for (i in 1:length(colnames(sl.cov))) {
      name = colnames(sl.cov)[i]
      if (name=="USUBJID") {
        next
      }
      if (name=="STUDY") {
        if(!is.character(unlist(sl.cov[,name]))) {stop("STUDY in sl.cov must be character type.")
        }
      }
      if (name=="SEX" & demo.map==T) {
        sl.cov[, "NSEX"] <- NA
        sl.cov$NSEX[grepl("m|male", sl.cov$SEX, ignore.case = T)] <- 0
        sl.cov$NSEX[grepl("f|female", sl.cov$SEX, ignore.case = T)] <- 1
        sl.cov$NSEX[grepl("unk", sl.cov$SEX, ignore.case = T)] <- 2
        sl.cov$NSEX[grepl("other", sl.cov$SEX, ignore.case = T)] <- 3
        s.cat.cov.n <- c(s.cat.cov.n, "NSEX")
        s.cat.cov.c <- c(s.cat.cov.c, "NSEXC")
        colnames(sl.cov)[i] <- "NSEXC"
        if(length(sort(unique(sl.cov$NSEX)))!=length(sort(unique(sl.cov$NSEXC)))) {
          warning("At least one NSEXC failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (name=="RACE" & demo.map==T) {
        sl.cov[, "NRACE"] <- NA
        sl.cov$NRACE[grepl("white|caucasian", sl.cov$RACE, ignore.case = T)] <- 1
        sl.cov$NRACE[grepl("black|african|aa", sl.cov$RACE, ignore.case = T)] <- 2
        sl.cov$NRACE[grepl("asian", sl.cov$RACE, ignore.case = T) & !grepl("caucasian", sl.cov$RACE, ignore.case=T)] <- 3
        sl.cov$NRACE[grepl("alaskan|native", sl.cov$RACE, ignore.case = T)] <- 4
        sl.cov$NRACE[grepl("hawa|pacific|island", sl.cov$RACE, ignore.case = T)] <- 5
        sl.cov$NRACE[grepl("multiple|mul", sl.cov$RACE, ignore.case = T)] <- 6
        sl.cov$NRACE[grepl("other", sl.cov$RACE, ignore.case = T)] <- 7
        sl.cov$NRACE[grepl("unknown", sl.cov$RACE, ignore.case = T)] <- 8
        s.cat.cov.n <- c(s.cat.cov.n, "NRACE")
        s.cat.cov.c <- c(s.cat.cov.c, "NRACEC")
        colnames(sl.cov)[i] <- "NRACEC"
        if(length(sort(unique(sl.cov$NRACE)))!=length(sort(unique(sl.cov$NRACEC)))) {
          warning("At least one NRACE failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (name=="ETHNIC" & demo.map==T) {
        sl.cov[, "NETHNIC"] <- NA
        sl.cov$NETHNIC[grepl("not", sl.cov$ETHNIC, ignore.case = T)] <- 0
        sl.cov$NETHNIC[grepl("his", sl.cov$ETHNIC, ignore.case = T) & !grepl("not", sl.cov$ETHNIC, ignore.case=T)] <- 1
        sl.cov$NETHNIC[grepl("unk", sl.cov$ETHNIC, ignore.case = T)] <- 2
        sl.cov$NETHNIC[grepl("other", sl.cov$ETHNIC, ignore.case = T)] <- 3
        s.cat.cov.n <- c(s.cat.cov.n, "NETHNIC")
        s.cat.cov.c <- c(s.cat.cov.c, "NETHNICC")
        colnames(sl.cov)[i] <- "NETHNICC"
        if(length(sort(unique(sl.cov$NETHNIC)))!=length(sort(unique(sl.cov$NETHNICC)))) {
          warning("At least one NETHNIC failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (is.numeric(unlist(sl.cov[,name]))){
        index_of_unit_column <- grep(paste0(name, "U"), names(sl.cov))
        if (length(index_of_unit_column) != 1) {
          stop(paste("All numerical covariates in sl.cov need units."))
        }
        if(length(sort(unique(unlist(sl.cov[,paste0(name, "U")]))))>1) {
          stop(paste(name, "has more than one unit."))
        }
        baseline_name <- paste0("B", name)
        baseline_name_units <- paste0("B", name, "U")
        colnames(sl.cov)[i] <- baseline_name
        colnames(sl.cov)[index_of_unit_column] <- baseline_name_units
        s.cont.cov <- append(s.cont.cov, baseline_name)
        s.cont.cov.units <- append(s.cont.cov.units, baseline_name_units)
      }
      else if (substr(name, nchar(name), nchar(name))=="U" & gsub("U$", "", name) %in% colnames(sl.cov)) {
        next
      }
      else {
        s.cat.cov.c <- c(s.cat.cov.c, paste0("N", name, "C"))
        s.cat.cov.n <- c(s.cat.cov.n, paste0("N", name))
        sl.cov[, paste0("N", name)] <- match(unlist(sl.cov[,name]), sort(unique(unlist(sl.cov[,name]))))
        if(length(unique(sl.cov[, name]))==2) {
          sl.cov[, paste0("N", name)] <- sl.cov[, paste0("N", name)]-1
        }
        colnames(sl.cov)[i] <- paste0("N", name, "C")
      }
    }

    for (i in colnames(sl.cov)) {
      if (i %in% colnames(df) & i!="USUBJID") {
        stop(paste0(i, " column is duplicated in sl.cov and another dataset. Please include this column in one dataset only."))
      }
    }
  }

  t.cat.cov.c <- c() #vector to contain subject-level character categorical covariates
  t.cont.cov <- c() #vector to contain subject-level continuous covariates
  t.cat.cov.n <- c() #vector to contain subject-level numeric categorical covariates
  t.cont.cov.units <- c()

  if (is.data.frame(tv.cov)==TRUE) {

    for (i in 1:length(colnames(tv.cov))) {
      name = colnames(tv.cov)[i]
      if (name %in% c("USUBJID", "DTIM", "EVID", "DOMAIN")) {
        next
      }
      else if (name=="SEX" & demo.map==T) {
        tv.cov[, "TSEX"] <- NA
        tv.cov$TSEX[grepl("m|male", tv.cov$SEX, ignore.case = T)] <- 0
        tv.cov$TSEX[grepl("f|female", tv.cov$SEX, ignore.case = T)] <- 1
        tv.cov$TSEX[grepl("unk|not|miss", tv.cov$SEX, ignore.case = T)] <- 2
        tv.cov$TSEX[grepl("other", tv.cov$SEX, ignore.case = T)] <- 3
        t.cat.cov.n <- c(t.cat.cov.n, "TSEX")
        t.cat.cov.c <- c(t.cat.cov.c, "TSEXC")
        colnames(tv.cov)[i] <- "TSEXC"
        if(length(sort(unique(sl.cov$TSEX)))!=length(sort(unique(sl.cov$TSEXC)))) {
          warning("At least one TSEXC failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (name=="TRACE" & demo.map==T) {
        tv.cov[, "TRACE"] <- NA
        tv.cov$TRACE[grepl("white|caucasian", tv.cov$RACE, ignore.case = T)] <- 1
        tv.cov$TRACE[grepl("black|african|aa", tv.cov$RACE, ignore.case = T)] <- 2
        tv.cov$NRACE[grepl("asian", tv.cov$RACE, ignore.case = T) & !grepl("caucasian", tv.cov$RACE, ignore.case=T)] <- 3
        tv.cov$TRACE[grepl("alaskan|native", tv.cov$RACE, ignore.case = T)] <- 4
        tv.cov$TRACE[grepl("hawa|pacific|island", tv.cov$RACE, ignore.case = T)] <- 5
        tv.cov$TRACE[grepl("multiple|mul", tv.cov$RACE, ignore.case = T)] <- 6
        tv.cov$TRACE[grepl("other", tv.cov$RACE, ignore.case = T)] <- 7
        tv.cov$TRACE[grepl("unknown", tv.cov$RACE, ignore.case = T)] <- 8
        t.cat.cov.n <- c(t.cat.cov.n, "TRACE")
        t.cat.cov.c <- c(t.cat.cov.c, "TRACEC")
        colnames(sl.cov)[i] <- "TRACEC"
        if(length(sort(unique(sl.cov$TRACE)))!=length(sort(unique(sl.cov$TRACEC)))) {
          warning("At least one TRACE failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (name=="ETHNIC" & demo.map==T) {
        tv.cov[, "TETHNIC"] <- NA
        tv.cov$TETHNIC[grepl("not", tv.cov$ETHNIC, ignore.case = T)] <- 0
        tv.cov$TETHNIC[grepl("his", tv.cov$ETHNIC, ignore.case = T)] <- 1
        tv.cov$TETHNIC[grepl("unk", tv.cov$ETHNIC, ignore.case = T)] <- 2
        tv.cov$TETHNIC[grepl("other", tv.cov$ETHNIC, ignore.case = T)] <- 3
        t.cat.cov.n <- c(t.cat.cov.n, "TETHNIC")
        t.cat.cov.c <- c(t.cat.cov.c, "TETHNICC")
        colnames(sl.cov)[i] <- "TETHNICC"
        if(length(sort(unique(sl.cov$TETHNIC)))!=length(sort(unique(sl.cov$TETHNICC)))) {
          warning("At least one TETHNIC failed to map. Consider setting demo.map = FALSE.")
        }
      }
      else if (is.numeric(unlist(tv.cov[,name]))){
        index_of_unit_column <- grep(paste0(name, "U"), names(tv.cov))
        if (length(index_of_unit_column) != 1) {
          stop("All numerical covariates need units.")
        }
        if(length(sort(unique(unlist(tv.cov[,paste0(name, "U")]))))>1) {
          stop(paste(name, "has more than one unit."))
        }
        baseline_name <- paste0("T", name)
        baseline_name_units <- paste0("T", name, "U")
        colnames(tv.cov)[i] <- baseline_name
        colnames(tv.cov)[index_of_unit_column] <- baseline_name_units
        t.cont.cov <- append(t.cont.cov, baseline_name)
        t.cont.cov.units <- append(t.cont.cov.units, baseline_name_units)
      }
      else if (substr(name, nchar(name), nchar(name))=="U" & gsub("U$", "", name) %in% colnames(tv.cov)) {
        next
      }
      else {
        t.cat.cov.c <- c(t.cat.cov.c, paste0("T", name, "C"))
        t.cat.cov.n <- c(t.cat.cov.n, paste0("T", name))
        tv.cov[, paste0("T", name)] <- match(unlist(tv.cov[,name]), sort(unique(unlist(tv.cov[,name]))))
        if(length(unique(tv.cov[, name]))==2) {
          tv.cov[, paste0("T", name)] <- tv.cov[, paste0("T", name)]-1
        }
        colnames(tv.cov)[i] <- paste0("T", name, "C")
      }
    }

    for (i in colnames(tv.cov)) {
      if (i %in% colnames(df) & !i %in% c("USUBJID", "DTIM", "EVID", "DOMAIN")) {
        stop(paste0(i, " column is duplicated in tl.cov and another dataset. Please include this column in one dataset only."))
      }
    }
  }

  cat.cov.c <- c(s.cat.cov.c, t.cat.cov.c) #all character categorical variables
  cat.cov.n <- c(s.cat.cov.n, t.cat.cov.n) #all numeric categorical variables
  cont.cov <- c(s.cont.cov, t.cont.cov) #all continuous variables
  cont.cov.units <- c(s.cont.cov.units, t.cont.cov.units)

  ###JOIN SUBJECT-LEVEL COVARIATES###
  if(is.data.frame(sl.cov)==TRUE) {
    df <- dplyr::left_join(df, sl.cov, by="USUBJID")
  }

  ###JOIN TIME-VARYING COVARIATES###
  if(is.data.frame(tv.cov)==TRUE) {
    df <- dplyr::bind_rows(df, tv.cov) #add tv.cov
    df <- dplyr::arrange(df, USUBJID, DTIM) #get in time order
    df <- dplyr::group_by(df, USUBJID) #group
    df <- tidyr::fill(df, FDOSE, .direction="downup")
    df <- dplyr::ungroup(df)
    df <- dplyr::mutate(df,
                        ATFD = ifelse(EVID==2, as.numeric(difftime(DTIM, FDOSE, units=time.units)), ATFD))

    if (is.null(time.rnd)) {
      df <- dplyr::mutate(df, ATFD = ATFD-0.001)
    }

    else {
      df <- dplyr::mutate(df,
                          ATFD = dplyr::case_when(EVID==2 & time.rnd>=3 ~ ATFD-0.001,
                                                  EVID==2 & time.rnd==2 ~ ATFD-0.01,
                                                  EVID==2 & time.rnd==1 ~ ATFD-0.1,
                                                  TRUE ~ ATFD))
    }

    df <- dplyr::arrange(df, USUBJID, ATFD)
    df <- dplyr::group_by(df, USUBJID)

    if(length(t.cont.cov)>0) {df <- tidyr::fill(df, tidyselect::all_of(t.cont.cov), .direction=tv.cov.fill)} #fill variables in the list downup
    if(length(t.cat.cov.c)>0) {df <- tidyr::fill(df, tidyselect::all_of(t.cat.cov.c), .direction=tv.cov.fill)}
    if(length(t.cat.cov.n)>0) {df <- tidyr::fill(df, tidyselect::all_of(t.cat.cov.n), .direction=tv.cov.fill)}
    if(length(t.cont.cov.units)>0) {df <- tidyr::fill(df, tidyselect::all_of(t.cont.cov.units), .direction=tv.cov.fill)}

    df <- dplyr::ungroup(df)
  }

  ###FLAG ITEMS###
  dvid.dv <- sort(unique(dplyr::filter(df, EVID==0)$DVID))
  if(!is.na(impute)) {
    if (impute==2) {
      flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", dvid.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPFEX", "IMPDV")
    }
    if (impute==1) {
      flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", dvid.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPDV")
    }
  }
  if (is.na(impute)) {
    flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", dvid.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPDV")
  }

  #NODV_F
  for (i in dvid.dv) {
    usubjid <- data.frame(unique(dplyr::filter(df, DVID==i & EVID==0)$USUBJID),
                          0)
    colnames(usubjid) <- c("USUBJID", paste0("NODV", i, "F"))
    df <- dplyr::left_join(df, usubjid, by="USUBJID")
    df[is.na(df[, paste0("NODV", i, "F")]), paste0("NODV", i, "F")] <- 1
    df[df[, paste0("NODV", i, "F")]==1, "C"] <- "C"
  }

  #DUPF
  df <- dplyr::group_by(df, USUBJID, ATFD, EVID, CMT, DVID)
  df <- dplyr::mutate(df, DUPF = ifelse(dplyr::row_number()>=2 & !is.na(ATFD), 1, NA)) #flag duplicate records in same usubjid-atfd-evid-amt-odv-cmt
  df <- tidyr::fill(df, DUPF, .direction="up") #apply flag to all records in the group
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, DUPF = ifelse(is.na(DUPF), 0, DUPF)) #0 for all non-duplicate records

  #AMTF
  df <- dplyr::mutate(df, AMTF = ifelse(is.na(AMT) & EVID==1, 1, 0)) #missing AMT flag
  df <- dplyr::group_by(df, USUBJID, NDAY)
  df <- tidyr::fill(df, AMTF, .direction="downup")
  df <- dplyr::group_by(df, USUBJID)
  df <- tidyr::fill(df, AMTF, .direction="downup")
  df <- dplyr::ungroup(df)

  #NOEXF
  df <- dplyr::group_by(df, USUBJID)
  df <- dplyr::mutate(df, NOEXF = ifelse(!any(1 %in% EVID), 1, 0)) #no dose records for a given subject
  df <- dplyr::ungroup(df)

  #SPARSEF
  cmts <- c()
  if (is.data.frame(pc)) {
    cmts <- c(cmts, unique(pc$CMT))
  }
  if (is.data.frame(pd)) {
    cmts <- c(cmts, unique(pd$CMT))
  }

  if (!is.null(cmts)) {
    df <- dplyr::arrange(df, USUBJID, NDAY, EVID, CMT)
    df <- dplyr::group_by(df, USUBJID, NDAY, EVID, CMT)
    df <- dplyr::mutate(df, MAX = ifelse(EVID==0 & CMT==min(cmts), max(dplyr::row_number()), NA)) #number of observations on that day in the smallest observation compartment
    df <- dplyr::group_by(df, USUBJID, NDAY)
    df <- tidyr::fill(df, MAX, .direction="downup") #apply to all records that day
    df <- dplyr::mutate(df, SPARSEF = dplyr::case_when(any(1 %in% EVID) & any(0 %in% EVID) & MAX>=sparse ~ 0, #when the NDAY has at least one dose and at least one observation and enough observations to meet the threshold, 0
                                                       any(1 %in% EVID) ~ 1))
    df <- dplyr::group_by(df, USUBJID)
    df <- tidyr::fill(df, SPARSEF, .direction="downup")
    df <- dplyr::ungroup(df)
  }

  #PDOSEF, TIMEF, PLBOF
  df <- dplyr::mutate(df,
                      PDOSEF = ifelse(ATFD<0, 1, 0), #pre-dose flag
                      TIMEF = ifelse(is.na(ATFD), 1, 0), #missing ATFD flag
                      PLBOF = ifelse(DOSEA==0, 1, 0), #placebo flag
                      C = dplyr::case_when(PDOSEF==1 ~ "C",
                                           TIMEF==1 ~ "C",
                                           AMTF==1 ~ "C",
                                           DUPF==1 ~ "C"))

  #SDF & TREXF
  df <- dplyr::arrange(df, USUBJID, EVID)
  df <- dplyr::group_by(df, USUBJID, EVID)
  df <- dplyr::mutate(df,
                      MAX = ifelse(EVID %in% c(0,2), max(ATFD), NA), #last observation or other ATFD
                      SDF = dplyr::case_when(EVID==1 & max(dplyr::row_number())>1 ~ 0, #if dose event and more than one dose, 0
                                             EVID==1 & max(dplyr::row_number())==1 & ADDL>0 ~ 0, #if dose event and more than one dose, 0
                                             EVID==1 & max(dplyr::row_number())==1 ~ 1)) #one dose, 1
  df <- dplyr::ungroup(df)
  df <- dplyr::arrange(df, USUBJID, ATFD, CMT, -EVID)
  df <- dplyr::group_by(df, USUBJID)
  df <- tidyr::fill(df, MAX, SDF, .direction="downup")
  df <- dplyr::mutate(df,
                      TREXF = ifelse(EVID==1 & ATFD>MAX, 1, 0),
                      SDF = ifelse(NOEXF==1, NA, SDF))
  df <- dplyr::ungroup(df)

  ###FIX NA ITEMS###
  for (i in c("NTFD", "NTLC", "NTLD", ex.col.n, pc.col.n, pd.col.n, cat.cov.n, cont.cov)) {
    df[(is.na(df[, i])) & df$EVID!=2, i] <- na
  }

  ###ROUDING###
  if(is.numeric(time.rnd)) {
    df <- dplyr::mutate(df,
                        ATFD = round(ATFD, time.rnd),
                        ATLD = round(ATLD, time.rnd),
                        NTFD = round(NTFD, time.rnd),
                        NTLC = round(NTLC, time.rnd),
                        NTLD = round(NTLD, time.rnd),
                        TPT = round(TPT, time.rnd))

    if ("DUR" %in% ex.nonmem) {
      df <- dplyr::mutate(df, DUR = round(DUR, time.rnd))
    }
  }

  if(is.numeric(amt.rnd)) {
    df <- dplyr::mutate(df,
                        AMT = round(AMT, amt.rnd),
                        RATE = ifelse("RATE" %in% ex.nonmem, round(RATE, amt.rnd), NA))

    if ("RATE" %in% ex.nonmem) {
      df <- dplyr::mutate(df, RATE = round(RATE, time.rnd))
    }
  }

  if(is.numeric(dv.rnd)) {
    df <- dplyr::mutate(df,
                        ODV = round(ODV, dv.rnd),
                        LDV = round(LDV, dv.rnd))

    if("BDV" %in% colnames(df)) {
      df <- dplyr::mutate(df, BDV = round(BDV, dv.rnd))
    }

    if("DDV" %in% colnames(df)) {
      df <- dplyr::mutate(df, DDV = round(DDV, dv.rnd))
    }

    if("PDV" %in% colnames(df)) {
      df <- dplyr::mutate(df, PDV = round(PDV, dv.rnd))
    }
  }

  if(is.numeric(cov.rnd)) {
    cov.num <- round(df[, cont.cov], cov.rnd)
    df <- dplyr::select(df, -tidyselect::all_of(cont.cov))
    df <- dplyr::bind_cols(df, cov.num)
  }

  ###FINAL SORTING/ARRANGEMENT###
  df <- dplyr::arrange(df, USUBJID, ATFD, CMT, EVID)
  df <- dplyr::mutate(df,
                      ID = match(USUBJID, unique(df$USUBJID)),
                      TIMEU = time.units,
                      SUBJID = gsub("\\D+", "", USUBJID),
                      LINE = dplyr::row_number(),
                      DTIM = as.character(DTIM),
                      FDOSE = as.character(FDOSE),
                      VERSN = func.version,
                      BUILD = Sys.Date(),
                      NSTUDY = ifelse(NSTUDY==na, NA, NSTUDY))
  df <- dplyr::group_by(df, USUBJID)
  df <- tidyr::fill(df, NSTUDY, NSTUDYC, .direction="downup")
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, NSTUDY = ifelse(is.na(NSTUDY), na, NSTUDY))
  df <- dplyr::mutate_at(df,
                         .vars = c(ex.col.c, pc.col.c, pd.col.c, cat.cov.c),
                         .funs = function(x) toupper(x))

  df <- dplyr::select(df,
                      C, tidyselect::all_of(stud.col.n), SUBJID, ID, ATFD, ATLD, NTFD, NTLC, NTLD, NDAY, TPT,
                      EVID, MDV, CMT, DVID, AMT, tidyselect::all_of(ex.nonmem), ODV, LDV, tidyselect::all_of(pd.dvs),
                      BLQ, LLOQ, tidyselect::all_of(ex.col.n), tidyselect::all_of(pc.col.n), tidyselect::all_of(pd.col.n),
                      tidyselect::all_of(cat.cov.n), tidyselect::all_of(cont.cov), tidyselect::all_of(flags),
                      LINE, USUBJID, tidyselect::all_of(stud.col.c), VISIT, TPTC, DOMAIN, DVIDC, DVIDU, TIMEU,
                      tidyselect::all_of(ex.col.c), tidyselect::all_of(pc.col.c), tidyselect::all_of(pd.col.c),
                      tidyselect::all_of(cat.cov.c), tidyselect::all_of(s.cont.cov.units),
                      tidyselect::all_of(t.cont.cov.units), DTIM, FDOSE, VERSN, BUILD)

  ###FILTER OTHER EVENTS###
  if (keep.other==F) {
    df <- dplyr::filter(df, EVID!=2)
    df <- dplyr::mutate(df, LINE = dplyr::row_number())
  }

  ###WARNINGS###
  check <- dplyr::filter(df, !is.na(NTFD) & NTFD!=na)
  check <- dplyr::group_by(check, USUBJID)
  check <- dplyr::mutate(check, DNTFD = NTFD-dplyr::lag(NTFD))
  check <- dplyr::filter(check, DNTFD < 0)
  check <- dplyr::filter(check, !is.na(ATFD))

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one event that occurred out of protocol order (NTFD is not strictly increasing): ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- dplyr::filter(df, ATFD >= 0)
  check <- dplyr::filter(check, !is.na(ATFD))
  check <- dplyr::filter(check, NTLD < 0)
  check <- dplyr::filter(check, !is.na(NTLD))
  check <- dplyr::filter(check, NTLD != na)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one negative NTLD value after first dose: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- dplyr::filter(df, TIMEF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one event with missing ATFD: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- dplyr::filter(df, AMTF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one dose event with missing AMT: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- dplyr::filter(df, DUPF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one duplicate event: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- colnames(df)[nchar(colnames(df))>8]
  if (length(check)>=1) {
    warning(paste0("The following column name(s) are longer than 8 characters: ", paste0(check, collapse = ", ")))
  }

  covariates <- c(cat.cov.n, cont.cov)

  for (i in covariates) {
    if (grepl("^B", i)) {
      if (paste0("T", gsub("^B", "", i)) %in% colnames(df)) {
        check <- df[df$NTFD==0, c(i, paste0("T", gsub("^B", "", i)))]
        if (FALSE %in% c(check[,1]==check[,2])) {
          warning(paste(i, "and", paste0("T", gsub("^B", "", i)), "are not equivalent at first dose (baseline)."))
        }
      }
    }
    if (grepl("^N", i)) {
      if (paste0("T", gsub("^N", "", i)) %in% colnames(df)) {
        check <- df[df$NTFD==0, c(i, paste0("T", gsub("^N", "", i)))]
        if (FALSE %in% c(check[,1]==check[,2])) {
          warning(paste(i, "and", paste0("T", gsub("^N", "", i)), "are not equivalent at first dose (baseline)."))
        }
      }
    }
  }

  ###RETURN FINAL DATASET###
  return(df)
}
