#' Build a NONMEM PK(PD) dataset
#'
#' Input a pre-processed an ex and pc domain for combination into a NONMEM dataset.
#' Additional pd endpoints, subject-level covariates, and time-varying covariates can also be added.
#' Other parameters can customize some calculations and formatting
#'
#' @param ex dose event dataframe
#' @param pc pc event dataframe
#' @param pd pd event dataframe
#' @param sl.cov subject-level covarite dataframe
#' @param tv.cov time-varying covariate dataframe
#' @param time.units units for time attributes
#' @param cycle.length cycle length
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
#' @export
pk_build <- function(ex, pc=NA, pd=NA, sl.cov=NA, tv.cov=NA,
                     time.units="days", cycle.length=NA, na=-999,
                     time.rnd=F, amt.rnd=F, dv.rnd=F, cov.rnd=F,
                     impute=NA, BDV=F, DDV=F, PDV=F, sparse=3,
                     demo.map = T, tv.cov.fill = "downup", keep.other=T) {

  func.version <- "0.2.0"

  ###EX QC###
  cdisc.cols.ex <- data.frame("COLUMN" = c("USUBJID", "DTIM", "NDAY",
                                           "TPT", "AMT", "STUDY", "VISIT",
                                           "TPTC", "DVID", "DVIDU", "ROUTE", "FRQ"),
                              "CDISC" = c("USUBJID$", "EXSTDTC$|EXDTC$|ASTDTM$|ADT$", "EXSTDY$",
                                          "EXTPTNUM$", "EXDOSE$|AVAL$", "STUDYID$", "VISIT$",
                                          "EXTPT$", "EXTRT$", "EXDOSU$", "EXROUTE$", "EXDOSFRQ$"))

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
    if (!any(i %in% colnames(ex))) { #if a given column is missing
      stop(paste0("Column ", i, " is missing from the ex dataset."))} #inform the user which variable is missing from which dataset

    if (i %in% c("NDAY", "TPT", "AMT", "CMT") & !is.numeric(unlist(ex[, i]))) { #if a column is numeric
      stop(paste0("Column ", i, " in ex is not numeric type."))}

    if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "ROUTE", "FRQ", "DVIDU") & !is.character(unlist(ex[, i]))) { #if a column is character
      stop(paste0("Column ", i, " in ex is not character type."))}

    if(i %in% c("USUBJID", "CMT") & TRUE %in% is.na(ex[, i])) {
      stop(paste0(i, " missing in ex for at least 1 row."))}
  }

  ex <- ex %>%
    dplyr::arrange(USUBJID, NDAY, TPT)

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

  ex <- ex %>%
    dplyr::mutate(ADDL = ifelse(!"ADDL" %in% colnames(ex), NA, ADDL),
                  II = ifelse(!"II" %in% colnames(ex), NA, II),
                  IMPEX = ifelse(!"IMPEX" %in% colnames(ex), 0, IMPEX))

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
    ex <- as.data.frame(ex) %>%
      dplyr::mutate(EVID = 1)
  } #add event ID for dose events

  if(!any("DOMAIN" %in% colnames(ex))) {
    ex <- as.data.frame(ex) %>%
      dplyr::mutate(DOMAIN = "EX")
  }

  if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", ex$DTIM[!is.na(ex$DTIM)])) {
    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", ex$DTIM[!is.na(ex$DTIM)])) {
      stop("DTIM in ex is not ISO 8601 format.")
    }
  }

  ex <- ex %>%
    dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                          grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                          grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                          grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

  if(0 %in% ex[, "NDAY"]) {
    stop("NDAY in ex has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
  }

  if(FALSE %in% is.na(ex[is.na(ex$ADDL), "II"])) {
    stop("At least one row in ex has a documented II when ADDL is NA.")
  }
  if(FALSE %in% is.na(ex[is.na(ex$II), "ADDL"])) {
    stop("At least one row in ex has a documented ADDL when II is NA.")
  }

  usubjid <- unique(ex$USUBJID)

  ###PC QC###
  pc.col.c <- c()
  pc.col.n <- c()

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

      if (i %in% c("NDAY", "TPT", "ODV", "CMT") & !is.numeric(unlist(pc[, i]))) { #if a column is numeric
        stop(paste0("Column ", i, " in pc is not numeric type."))
      }

      if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "DVIDU") & !is.character(unlist(pc[, i]))) { #if a column is character
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

    if(!any("EVID" %in% colnames(pc))) {
      pc <- as.data.frame(pc) %>%
        dplyr::mutate(EVID = 0, #add EVID for observation events
                      LDV = log(ODV)) #natural log ODV
    }

    if(!any("DOMAIN" %in% colnames(pc))) {
      pc <- as.data.frame(pc) %>%
        dplyr::mutate(DOMAIN = "PC")
    }

    check <- pc %>% dplyr::filter(ODV<=0)
    if(nrow(check) > 0) {
      stop("At least one dependent variable in PC is less than or equal to 0.")
    }

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", pc$DTIM[!is.na(pc$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", pc$DTIM[!is.na(pc$DTIM)])) {
        stop("DTIM in pc is not ISO 8601 format for at least one row.")
      }
    }

    pc <- pc %>%
      dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

    pc <- pc %>%
      dplyr::mutate(IMPDV = ifelse(!"IMPDV" %in% colnames(pc), 0, IMPDV))

    if(0 %in% pc[, "NDAY"]) {
      stop("NDAY in pc has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
    }

    usubjid <- c(usubjid, unique(pc$USUBJID))
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

      if (i %in% c("NDAY", "TPT", "ODV", "CMT") & !is.numeric(unlist(pd[, i]))) { #if a column is numeric
        stop(paste0("Column ", i, " in pd is not numeric type."))
      }

      if (i %in% c("USUBJID", "VISIT", "TPTC", "DVID", "DVIDU") & !is.character(unlist(pd[, i]))) { #if a column is character
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

    if(!any("EVID" %in% colnames(pd))) {
      pd <- as.data.frame(pd) %>%
        dplyr::mutate(EVID = 0, #add EVID for observation events
                      LDV = ODV) #log-transformed ODV
    }

    if(!any("DOMAIN" %in% colnames(pd))) {
      pd <- as.data.frame(pd) %>%
        dplyr::mutate(DOMAIN = "PD")
    }

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", pd$DTIM[!is.na(pd$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", pd$DTIM[!is.na(pd$DTIM)])) {
        stop("DTIM in pd is not ISO 8601 format.")
      }
    }

    pd <- pd %>%
      dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M")))

    if(0 %in% pd[, "NDAY"]) {
      stop("NDAY in pd has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
    }

    usubjid <- c(usubjid, unique(pd$USUBJID))

    pd <- pd %>%
      dplyr::mutate(IMPDV = ifelse(!"IMPDV" %in% colnames(pd), 0, IMPDV))
  }

  ###SL.COV QC###
  if(is.list(sl.cov) & !is.data.frame(sl.cov)) {
    sl.cov <- suppressMessages(sl.cov %>% purrr::reduce(dplyr::full_join, ))
  }

  if (is.data.frame(sl.cov)) {
    req.cols <- c("USUBJID")
    for (i in req.cols) {
      if (!any(i %in% colnames(sl.cov))) {
        stop(paste0("Column ", i, " is missing from the sl.cov dataset."))
      }

      if (i %in% c("USUBJID") & !is.character(unlist(sl.cov[, i]))) { #if a column is character
        stop(paste0("Column ", i, " in sl.cov is not character type."))
      }

      if(i %in% c("USUBJID") & TRUE %in% is.na(sl.cov[, i])) {
        stop(paste0(i, " missing in sl.cov for at least 1 row."))
      }
    }

    if (!"STUDY" %in% colnames(sl.cov)) {
      if(!"NSTUDYC" %in% colnames(ex)) {
        stop(paste0("STUDY column must be included in ex or sl.cov."))
      }
    }

    if(nrow(sl.cov)!=length(unique(sl.cov$USUBJID))) { #more than 1 row per subject
      stop("sl.cov has duplicate USUBJID rows.")
    }

    for (i in colnames(sl.cov)[2:ncol(sl.cov)]) {
      if (i %in% c(colnames(ex), colnames(pc), colnames(pd))) {
        stop(paste0(i, " column is duplicated in sl.cov and another dataset. Please include this column in one dataset only."))
      }
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

  ###TV.COV QC###
  if(is.list(tv.cov) & !is.data.frame(tv.cov)) {
    tv.cov <- tv.cov %>%
      purrr::reduce(dplyr::bind_rows)
  }

  if (is.data.frame(tv.cov)) {
    req.cols <- c("USUBJID", "DTIM")
    for (i in req.cols) {
      if (!any(i %in% colnames(tv.cov))) {
        stop(paste0("Column ", i, " is missing from the tv.cov dataset."))
      }

      if (i %in% c("USUBJID") & !is.character(unlist(tv.cov[, i]))) { #if a column is character
        stop(paste0("Column ", i, " in tv.cov is not character type."))
      }

      if(i %in% c("USUBJID", "DTIM") & TRUE %in% is.na(tv.cov[, i])) {
        stop(paste0(i, " missing in tv.cov for at least 1 row."))
      }
    }

    covs <- colnames(tv.cov)[!(colnames(tv.cov) %in% c("USUBJID", "DTIM"))]

    tv.cov <- as.data.frame(tv.cov) %>%
      dplyr::arrange(USUBJID, DTIM) %>%
      dplyr::group_by(USUBJID, DTIM) %>%
      tidyr::fill(tidyselect::all_of(covs), .direction="downup") %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(EVID = 2,
                    DOMAIN = "TVCOV")

    if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
        if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", tv.cov$DTIM[!is.na(tv.cov$DTIM)])) {
          stop("DTIM in tv.cov is not ISO 8601 format.")
        }
      }
    }

    tv.cov <- tv.cov %>%
      dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M"),
                                            grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d")))

    for (i in colnames(tv.cov)) {
      if (i %in% c(colnames(ex), colnames(pc), colnames(pd)) & !i %in% c("USUBJID", "DTIM", "EVID", "DOMAIN")) {
        stop(paste0(i, " column is duplicated in tl.cov and another dataset. Please include this column in one dataset only."))
      }
    }

    check <- tv.cov %>%
      dplyr::mutate(Check = paste0(USUBJID, DTIM))

    if(nrow(tv.cov)!=length(unique(paste0(check$USUBJID, check$DTIM)))) {
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

  ###TIME.UNITS QC###
  if (!any(time.units %in% c("days", "hours"))) {
    stop("time.units parameter must be in days or hours.")
  }

  ###ROUNDING QC###
  if(time.rnd!=F & time.rnd%%1!=0) {
    stop("time.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
  }

  if(amt.rnd!=F & amt.rnd%%1!=0) {
    stop("amt.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
  }

  if(dv.rnd!=F & dv.rnd%%1!=0) {
    stop("dv.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
  }

  if(cov.rnd!=F & cov.rnd%%1!=0) {
    stop("cov.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
  }

  ###BDV/DDV/PDV QC###
  if (BDV==F & DDV==T & !any("BDV" %in% colnames(pd))) {
    stop("BDV parameter must be TRUE or BDV column must be included in pd to create DDV.")
  }

  if (BDV==F & PDV==T & !any("BDV" %in% colnames(pd))) {
    stop("BDV parameter must be TRUE or BDV column must be included in pd to create PDV.")
  }

  if (DDV==F & PDV==T & !any("BDV" %in% colnames(pd)) & !any("DDV" %in% colnames(pd))) {
    stop("DDV parameter must be TRUE or BDV & DDV columns must be included in pd to create PDV.")
  }

  ###OTHER ARGUMENT QC###
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

  if(!is.na(demo.map)) {
    if(!demo.map %in% c(T, F)) {
      stop("demo.map parameter must be TRUE or FALSE.")
    }
  }

  if(!is.logical(keep.other)) {
    stop("keep.other parameter must be TRUE or FALSE.")
  }

  ###BIND EVENTS TOGETHER###
  df <- ex

  if (is.data.frame(pc)) {
    df <- df %>%
      dplyr::bind_rows(pc) %>% #add dose and pc events
      dplyr::arrange(USUBJID, NDAY, TPT, CMT, -EVID)
  }

  if(is.data.frame(pd)) {
    df <- df %>%
      dplyr::bind_rows(pd) %>% #add pd events
      dplyr::arrange(USUBJID, NDAY, TPT, CMT, -EVID)
  }

  ###ACTUAL + NOMINAL TIME CALCULATIONS###
  df <- df %>%
    dplyr::mutate(NTFD = dplyr::case_when(time.units=="days" & NDAY>=1 ~ NDAY-1 + TPT,
                                          time.units=="days" & NDAY<0 ~ NDAY+TPT,
                                          time.units=="hours" & NDAY>=1 ~ 24*(NDAY-1) + TPT,
                                          time.units=="hours" & NDAY<0 ~ 24*(NDAY) + TPT), #compute NTFD from NDAY and TPT
                  NDOSE1 = ifelse(EVID %in% c(1, 4), NTFD, NA), #nominal time of dose event
                  NDOSE2 = ifelse(EVID %in% c(1, 4), NTFD+ADDL*II, NA), #nominal time of the last dose of the dose event
                  LDOSE1 = ifelse(EVID %in% c(1, 4), as.character(DTIM), NA), #actual time of the dose event
                  LDOSE1 = ifelse(EVID %in% c(1, 4) & is.na(LDOSE1), "1900-01-01 00:00:00", LDOSE1),
                  LDOSE2 = ifelse(EVID %in% c(1, 4), as.character(DTIM+ADDL*ifelse(time.units=="days", 60*60*24*II, 60*60*II)), NA), #actual time of the last dose of the dose event
                  LDOSE2 = ifelse(EVID %in% c(1, 4) & is.na(LDOSE2), "1900-01-01 00:00:00", LDOSE2)) %>% #actual time of the first dose per subject
    dplyr::arrange(USUBJID, EVID) %>%
    dplyr::group_by(USUBJID, EVID) %>%
    dplyr::mutate(FDOSE = ifelse(EVID==1 & dplyr::row_number()==1, as.character(DTIM), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(USUBJID, DTIM, NDAY, TPT, CMT, -EVID) %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(NDOSE1, NDOSE2, LDOSE1, LDOSE2, FDOSE, .direction="downup") %>% #fill selected dates to all events
    dplyr::ungroup() %>%
    dplyr::mutate(LDOSE1 = ifelse(LDOSE1=="1900-01-01 00:00:00", NA, LDOSE1),
                  LDOSE2 = ifelse(LDOSE2=="1900-01-01 00:00:00", NA, LDOSE2),
                  FDOSE = as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                  LDOSE1 = as.POSIXct(LDOSE1, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                  LDOSE2 = as.POSIXct(LDOSE2, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                  ATFD = as.numeric(difftime(DTIM, FDOSE, units=time.units)), #DTIM - FDOSE
                  ATLD = ifelse(is.na(ATFD), NA,
                                dplyr::case_when(DTIM >= LDOSE2 ~ as.numeric(difftime(DTIM, LDOSE2, units=time.units)), #DTIM - LDOSE2 if after last dose of previous dose event
                                                 is.na(II) ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)), #no additional doses
                                                 ATFD<=0 ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)), #pre-dose records
                                                 TRUE ~ as.numeric(difftime(DTIM, LDOSE1, units=time.units)) %% II)), #remainder of DTIM - LDOSE1 if taken during the dosing interval
                  NTLD = dplyr::case_when(NTFD==-999 ~ -999,
                                          DTIM >= LDOSE2 ~ NTFD-NDOSE2,
                                          is.na(II) ~ NTFD-NDOSE1,
                                          ATFD<=0 ~ NTFD-NDOSE1,
                                          TRUE ~ (NTFD-NDOSE1) %% II)) %>%
    dplyr::select(-LDOSE1, -LDOSE2, -NDOSE1, -NDOSE2) #remove intermediate dates

  ###IMPUTATION METHODS###
  if (!is.na(impute)) {
    if (impute==1) {
      df <- df %>%
        dplyr::arrange(USUBJID, NDAY, TPT) %>%
        dplyr::mutate(IMPEX = ifelse(is.na(ATFD) & !is.na(NTFD) & EVID==1, 1, IMPEX),
                      IMPDV = ifelse(is.na(ATFD) & !is.na(NTFD) & EVID==0, 1, IMPDV),
                      ATFD = ifelse(is.na(ATFD), NTFD, ATFD),
                      ATLD = ifelse(is.na(ATLD), NTLD, ATLD)) %>%
        dplyr::arrange(USUBJID, ATFD, CMT, -EVID)
    }

    if (impute==2) {
      df <- df %>%
        dplyr::arrange(USUBJID, NTFD, EVID) %>%
        dplyr::group_by(USUBJID, NDAY) %>%
        dplyr::mutate(PCATFD = ifelse(EVID==0, ATFD, NA),
                      PCTPT = ifelse(EVID==0 & is.na(ATFD), TPT, NA),
                      PCDTIM = ifelse(EVID==0, as.character(DTIM), NA)) %>%
        tidyr::fill(PCATFD, PCTPT, PCDTIM, .direction="updown") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(IMPEX = dplyr::case_when(EVID==1 & is.na(DTIM) ~ 1,
                                               EVID==1 & is.na(IMPEX) ~ 0,
                                               TRUE ~ IMPEX),
                      PCDTIM = as.POSIXct(PCDTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                      IMPDTIM = ifelse(EVID==1 & is.na(DTIM), as.character(PCDTIM-PCTPT*ifelse(time.units=="days", 24*60*60, 60*60)), NA),
                      IMPFEX = ifelse(is.na(FDOSE), 1, 0),
                      FDOSE = ifelse(is.na(FDOSE) & EVID==1 & NTFD==0, as.character(IMPDTIM), as.character(FDOSE))) %>%
        dplyr::group_by(USUBJID) %>%
        tidyr::fill(FDOSE, .direction="downup") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(FDOSE = as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                      IMPDTIM = as.POSIXct(IMPDTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                      ATFD = dplyr::case_when(!is.na(IMPDTIM) & is.na(ATFD) ~ as.numeric(difftime(IMPDTIM, FDOSE, units=time.units)),
                                              EVID==1 & is.na(ATFD) & is.na(PCATFD) & !is.na(FDOSE) ~ as.numeric(difftime(DTIM, FDOSE, units=time.units)),
                                              EVID==1 & is.na(ATFD) & is.na(PCATFD) ~ NTFD,
                                              EVID==1 & is.na(ATFD) ~ PCATFD-PCTPT,
                                              TRUE ~ ATFD),
                      ATLD = ifelse(EVID==1 & is.na(ATLD), 0, ATLD)) %>%
        dplyr::mutate(EXATFD = ifelse(EVID==1, ATFD, NA),
                      EXNTFD = ifelse(EVID==1, NTFD, NA),
                      IMPDTIM = as.POSIXct(IMPDTIM)) %>%
        dplyr::group_by(USUBJID) %>%
        tidyr::fill(EXATFD, EXNTFD, IMPDTIM, IMPEX, .direction="downup") %>%
        dplyr::mutate(IMPDV = ifelse(EVID==0 & is.na(DTIM), 1, IMPDV),
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
                                              TRUE ~ NTLD)) %>%
        dplyr::ungroup() %>%
        #dplyr::mutate(IMPFEX = ifelse(IMPFEX==1 & is.na(FDOSE), 0, IMPFEX)) %>%
        dplyr::select(-PCATFD, -PCTPT, -EXATFD, -EXNTFD, -IMPDTIM) %>%
        dplyr::arrange(USUBJID, ATFD, EVID, CMT)
    }
  }

  ###DOSE AND OBSERVATION CALCULATIONS###
  if ("DUR" %in% colnames(ex)) {
    df <- df %>%
      dplyr::mutate(RATE = ifelse(!is.na(DUR), AMT/DUR, NA))
    ex.nonmem <- c("RATE", ex.nonmem)
  }

  df <- df %>%
    dplyr::mutate(DOSEA = ifelse(EVID==1, AMT, NA),
                  BLQ = dplyr::case_when(EVID==0 & is.na(ODV) & ATFD<=0 ~ 1, #pre-dose BLQ
                                         EVID==0 & is.na(ODV) ~ 2, #post-dose BLQ
                                         EVID==0 ~ 0, #NO BLQ
                                         TRUE ~ -99),
                  BLQ = ifelse(BLQ==-99, NA, BLQ), #NA for dose or other events
                  MDV = ifelse(is.na(ODV), 1, 0),
                  NTLC = dplyr::case_when(is.na(cycle.length) ~ NTFD,
                                          is.numeric(cycle.length) & NTFD<0 ~ NTFD,
                                          is.numeric(cycle.length) & time.units=="hours" ~ NTFD %% cycle.length*24,
                                          is.numeric(cycle.length) & time.units=="days" ~ NTFD %% cycle.length)) %>%
    dplyr::group_by(USUBJID, NDAY) %>%
    tidyr::fill(IMPEX, .direction="downup") %>% #apply to pre-dose records
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(DOSEA, IMPEX, NROUTEC, NROUTE, NFRQ, NFRQC, DOSENUM, .direction="downup") %>% #apply to all records
    dplyr::ungroup()

  if ("NSTUDY" %in% colnames(ex)) {
    df <- df %>%
      dplyr::group_by(USUBJID) %>%
      tidyr::fill(NSTUDY, NSTUDYC, .direction="downup") %>%
      dplyr::ungroup()
  }

  ###TIME WARNING FUNCTIONS###
  check <- df %>%
    dplyr::filter(!is.na(NTFD) & NTFD!=-999 & NTFD!=na) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(DNTFD = NTFD-dplyr::lag(NTFD)) %>%
    dplyr::filter(DNTFD < 0) %>%
    dplyr::filter(!is.na(ATFD))

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one event that occurred out of protocol order (NTFD is not strictly increasing): ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- df %>%
    dplyr::filter(ATFD >= 0) %>%
    dplyr::filter(!is.na(ATFD)) %>%
    dplyr::filter(NTLD < 0) %>%
    dplyr::filter(NTLD!=-999)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one negative NTLD value after first dose: ", paste0(unique(check$USUBJID), collapse = ", ")))
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
      df <- df %>%
        dplyr::arrange(USUBJID, DVID, ATFD) %>%
        dplyr::mutate(PDOS = ifelse(ATFD<=0 & EVID==0, 1, 0)) %>%
        dplyr::group_by(USUBJID, DVID) %>%
        dplyr::mutate(BDV = ifelse(DVID %in% pd$DVID & PDOS==1, dplyr::last(ODV), NA)) %>%
        dplyr::group_by(USUBJID, DVID) %>%
        tidyr::fill(BDV, .direction="downup") %>%
        dplyr::ungroup() %>%
        dplyr::arrange(USUBJID, ATFD, CMT, -EVID)

      for (i in unique(pd$DVID)) {
        check <- df %>%
          dplyr::filter(DVID==i) %>%
          dplyr::filter(is.na(BDV)) %>%
          dplyr::filter(!is.na(ATFD))

        if (nrow(check)>0) {
          warning(paste0("The following USUBJID(s) do not have a baseline ", i, " observation at or prior to first dose (BDV, DDV, PDV not calculated): ", paste0(sort(unique(check$USUBJID)), collapse = ", ")))
        }
      }
    }

    if(DDV==TRUE & !any("DDV" %in% colnames(pd))) {
      df <- df %>%
        dplyr::mutate(DDV = ODV-BDV)
    }

    if(PDV==TRUE & !any("PDV" %in% colnames(pd))) {
      df <- df %>%
        dplyr::mutate(PDV = 100*DDV/BDV)
    }
  }

  ###PRE-PROCESS COVARIATES###
  s.cat.cov.c <- c() #vector to contain subject-level character categorical covariates
  s.cont.cov <- c() #vector to contain subject-level continuous covariates
  s.cat.cov.n <- c() #vector to contain subject-level numeric categorical covariates
  stud.col.n <- c("NSTUDY") #vector for study column
  stud.col.c <- c("NSTUDYC") #vector for study column

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
        if (nchar(name)>7) {
          stop(paste(name, "column name in sl.cov must be 7 characters or fewer."))
        }
        s.cont.cov <- c(s.cont.cov, paste0("B",name))
        colnames(sl.cov)[i] <- paste0("B", name)
      }
      else {
        if (nchar(name)>6) {
          stop(paste(name, "column name in sl.cov must be 6 characters or fewer."))
        }
        s.cat.cov.c <- c(s.cat.cov.c, paste0("N", name, "C"))
        s.cat.cov.n <- c(s.cat.cov.n, paste0("N", name))
        sl.cov[, paste0("N", name)] <- match(unlist(sl.cov[,name]), sort(unique(unlist(sl.cov[,name]))))
        if(length(unique(sl.cov[, name]))==2) {
          sl.cov[, paste0("N", name)] <- sl.cov[, paste0("N", name)]-1
        }
        colnames(sl.cov)[i] <- paste0("N", name, "C")
      }
    }
  }

  t.cat.cov.c <- c() #vector to contain subject-level character categorical covariates
  t.cont.cov <- c() #vector to contain subject-level continuous covariates
  t.cat.cov.n <- c() #vector to contain subject-level numeric categorical covariates

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
        if (nchar(name)>7) {
          stop(paste(name, "column name in tv.cov must be 7 characters or fewer."))
        }
        t.cont.cov <- c(t.cont.cov, paste0("T",name))
        colnames(tv.cov)[i] <- paste0("T", name)
      }
      else {
        if (nchar(name)>6) {
          stop(paste(name, "column name in tv.cov must be 6 characters or fewer."))
        }
        t.cat.cov.c <- c(t.cat.cov.c, paste0("T", name, "C"))
        t.cat.cov.n <- c(t.cat.cov.n, paste0("T", name))
        tv.cov[, paste0("T", name)] <- match(unlist(tv.cov[,name]), sort(unique(unlist(tv.cov[,name]))))
        if(length(unique(tv.cov[, name]))==2) {
          tv.cov[, paste0("T", name)] <- tv.cov[, paste0("T", name)]-1
        }
        colnames(tv.cov)[i] <- paste0("T", name, "C")
      }
    }
  }

  cat.cov.c <- c(s.cat.cov.c, t.cat.cov.c) #all character categorical variables
  cat.cov.n <- c(s.cat.cov.n, t.cat.cov.n) #all numeric categorical variables
  cont.cov <- c(s.cont.cov, t.cont.cov) #all continuous variables

  ###JOIN SUBJECT-LEVEL COVARIATES###
  if(is.data.frame(sl.cov)==TRUE) {
    df <- df %>%
      dplyr::left_join(sl.cov, by="USUBJID")
  }

  ###JOIN TIME-VARYING COVARIATES###
  if(is.data.frame(tv.cov)==TRUE) {
    df <- df %>%
      dplyr::bind_rows(tv.cov) %>% #add tv.cov
      dplyr::arrange(USUBJID, DTIM) %>% #get in time order
      dplyr::group_by(USUBJID) %>% #group
      tidyr::fill(FDOSE, .direction="downup") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ATFD = ifelse(EVID==2, as.numeric(difftime(DTIM, FDOSE, units=time.units)), ATFD),
                    ATFD = dplyr::case_when(EVID==2 & (is.na(time.rnd) | time.rnd==0) ~ ATFD-0.001,
                                            EVID==2 & time.rnd>=3 ~ ATFD-0.001,
                                            EVID==2 & time.rnd==2 ~ ATFD-0.01,
                                            EVID==2 & time.rnd==1 ~ ATFD-0.1,
                                            TRUE ~ ATFD)) %>%
      dplyr::arrange(USUBJID, ATFD) %>%
      dplyr::group_by(USUBJID)

    if(length(t.cont.cov)>0) {df <- df %>% tidyr::fill(tidyselect::all_of(t.cont.cov), .direction=tv.cov.fill)} #fill variables in the list downup
    if(length(t.cat.cov.c)>0) {df <- df %>% tidyr::fill(tidyselect::all_of(t.cat.cov.c), .direction=tv.cov.fill)}
    if(length(t.cat.cov.n)>0) {df <- df %>% tidyr::fill(tidyselect::all_of(t.cat.cov.n), .direction=tv.cov.fill)}

    df <- df %>%
      dplyr::ungroup()
  }

  ###FLAG ITEMS###
  cmt.dv <- sort(unique(dplyr::filter(df, EVID==0)$CMT))
  if(!is.na(impute)) {
    if (impute==2) {
      flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", cmt.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPFEX", "IMPDV")
    }
    if (impute==1) {
      flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", cmt.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPDV")
    }
  }
  if (is.na(impute)) {
    flags <- c("PDOSEF", "TIMEF", "AMTF", "DUPF", "NOEXF", paste0("NODV", cmt.dv, "F"), "SDF", "PLBOF", "SPARSEF", "TREXF", "IMPEX", "IMPDV")
  }

  #NODV_F
  for (i in cmt.dv) {
    usubjid <- data.frame(unique(dplyr::filter(df, CMT==i & EVID==0)$USUBJID),
                          0)
    colnames(usubjid) <- c("USUBJID", paste0("NODV", i, "F"))
    df <- df %>% dplyr::left_join(usubjid, by="USUBJID")
    df[is.na(df[, paste0("NODV", i, "F")]), paste0("NODV", i, "F")] <- 1
    df[df[, paste0("NODV", i, "F")]==1, "C"] <- "C"
  }

  #DUPF
  df <- df %>%
    dplyr::group_by(USUBJID, ATFD, EVID, CMT) %>%
    dplyr::mutate(DUPF = ifelse(dplyr::row_number()>=2 & !is.na(ATFD), 1, NA)) %>% #flag duplicate records in same usubjid-atfd-evid-amt-odv-cmt
    tidyr::fill(DUPF, .direction="up") %>% #apply flag to all records in the group
    dplyr::ungroup() %>%
    dplyr::mutate(DUPF = ifelse(is.na(DUPF), 0, DUPF)) #0 for all non-duplicate records

  #AMTF
  df <- df %>%
    dplyr::mutate(AMTF = ifelse(is.na(AMT) & EVID==1, 1, 0)) %>% #missing AMT flag
    dplyr::group_by(USUBJID, NDAY) %>%
    tidyr::fill(AMTF, .direction="downup") %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(AMTF, .direction="downup") %>%
    dplyr::ungroup()

  #NOEXF
  df <- df %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(NOEXF = ifelse(!any(1 %in% EVID), 1, 0)) %>% #no dose records for a given subject
    dplyr::ungroup()

  #SPARSEF
  cmts <- c()
  if (is.data.frame(pc)) {
    cmts <- c(cmts, unique(pc$CMT))
  }
  if (is.data.frame(pd)) {
    cmts <- c(cmts, unique(pd$CMT))
  }

  if (!is.null(cmts)) {
    df <- df %>%
      dplyr::arrange(USUBJID, NDAY, EVID, CMT) %>%
      dplyr::group_by(USUBJID, NDAY, EVID, CMT) %>%
      dplyr::mutate(MAX = ifelse(EVID==0 & CMT==min(cmts), max(dplyr::row_number()), NA)) %>% #number of observations on that day in the smallest observation compartment
      dplyr::group_by(USUBJID, NDAY) %>%
      tidyr::fill(MAX, .direction="downup") %>% #apply to all records that day
      dplyr::mutate(SPARSEF = dplyr::case_when(any(1 %in% EVID) & any(0 %in% EVID) & MAX>=sparse ~ 0, #when the NDAY has at least one dose and at least one observation and enough observations to meet the threshold, 0
                                               any(1 %in% EVID) ~ 1)) %>%
      dplyr::group_by(USUBJID) %>%
      tidyr::fill(SPARSEF, .direction="downup") %>%
      dplyr::ungroup()
  }

  #PDOSEF, TIMEF, PLBOF
  df <- df %>%
    dplyr::mutate(PDOSEF = ifelse(ATFD<0, 1, 0), #pre-dose flag
                  TIMEF = ifelse(is.na(ATFD), 1, 0), #missing ATFD flag
                  PLBOF = ifelse(DOSEA==0, 1, 0), #placebo flag
                  C = dplyr::case_when(PDOSEF==1 ~ "C",
                                       TIMEF==1 ~ "C",
                                       AMTF==1 ~ "C",
                                       DUPF==1 ~ "C"))

  #SDF & TREXF
  df <- df %>%
    dplyr::arrange(USUBJID, EVID) %>%
    dplyr::group_by(USUBJID, EVID) %>%
    dplyr::mutate(MAX = ifelse(EVID %in% c(0,2), max(ATFD), NA), #last observation or other ATFD
                  SDF = dplyr::case_when(EVID==1 & max(dplyr::row_number())>1 ~ 0, #if dose event and more than one dose, 0
                                         EVID==1 & max(dplyr::row_number())==1 & ADDL>0 ~ 0, #if dose event and more than one dose, 0
                                         EVID==1 & max(dplyr::row_number())==1 ~ 1))%>% #one dose, 1
    dplyr::ungroup() %>%
    dplyr::arrange(USUBJID, ATFD, CMT, -EVID) %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(MAX, SDF, .direction="downup") %>%
    dplyr::mutate(TREXF = ifelse(EVID==1 & ATFD>MAX, 1, 0),
                  SDF = ifelse(NOEXF==1, NA, SDF)) %>%
    dplyr::ungroup()

  ###FIX NA ITEMS###
  for (i in c("NTFD", "NTLC", "NTLD", ex.col.n, pc.col.n, pd.col.n, cat.cov.n, cont.cov)) {
    df[(is.na(df[, i]) | df[, i]==-999) & df$EVID!=2, i] <- na
  }

  ###ROUDING###
  if(is.numeric(time.rnd)) {
    df <- df %>%
      dplyr::mutate(ATFD = round(ATFD, time.rnd),
                    ATLD = round(ATLD, time.rnd),
                    NTFD = round(NTFD, time.rnd),
                    NTLC = round(NTLC, time.rnd),
                    NTLD = round(NTLD, time.rnd),
                    TPT = round(TPT, time.rnd))

    if ("DUR" %in% ex.nonmem) {
      df <- df %>%
        dplyr::mutate(DUR = round(DUR, time.rnd))
    }
  }

  if(is.numeric(amt.rnd)) {
    df <- df %>%
      dplyr::mutate(AMT = round(AMT, amt.rnd),
                    RATE = ifelse("RATE" %in% ex.nonmem, round(RATE, amt.rnd), NA))

    if ("RATE" %in% ex.nonmem) {
      df <- df %>%
        dplyr::mutate(RATE = round(RATE, time.rnd))
    }
  }

  if(is.numeric(dv.rnd)) {
    df <- df %>%
      dplyr::mutate(ODV = round(ODV, dv.rnd),
                    LDV = round(LDV, dv.rnd))

    if("BDV" %in% colnames(df)) {
      df <- df %>%
        dplyr::mutate(BDV = round(BDV, dv.rnd))
    }

    if("DDV" %in% colnames(df)) {
      df <- df %>%
        dplyr::mutate(DDV = round(DDV, dv.rnd))
    }

    if("PDV" %in% colnames(df)) {
      df <- df %>%
        dplyr::mutate(PDV = round(PDV, dv.rnd))
    }
  }

  if(is.numeric(cov.rnd)) {
    cov.num <- round(df[, cont.cov], cov.rnd)
    df <- df %>%
      dplyr::select(-tidyselect::all_of(cont.cov)) %>%
      dplyr::bind_cols(cov.num)
  }

  ###FLAG WARNINGS###
  check <- df %>%
    dplyr::filter(TIMEF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one event with missing ATFD: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- df %>%
    dplyr::filter(AMTF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one dose event with missing AMT: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  check <- df %>%
    dplyr::filter(DUPF==1)

  if (nrow(check)>0) {
    warning(paste0("The following USUBJID(s) have at least one duplicate event: ", paste0(unique(check$USUBJID), collapse = ", ")))
  }

  ###FINAL SORTING/ARRANGEMENT###
  df <- df %>%
    dplyr::arrange(USUBJID, ATFD, CMT, EVID) %>%
    dplyr::mutate(ID = match(USUBJID, unique(df$USUBJID)),
                  SUBJID = gsub("\\D+", "", USUBJID),
                  TIMEU = time.units,
                  LINE = dplyr::row_number(),
                  DTIM = as.character(DTIM),
                  FDOSE = as.character(FDOSE),
                  VERSN = func.version,
                  BUILD = Sys.Date(),
                  NSTUDY = ifelse(NSTUDY==na, NA, NSTUDY)) %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(NSTUDY, NSTUDYC, .direction="downup") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(NSTUDY = ifelse(is.na(NSTUDY), na, NSTUDY)) %>%
    dplyr::mutate_at(.vars = c(ex.col.c, pc.col.c, pd.col.c, cat.cov.c),
                     .funs = function(x) toupper(x))

  df <- df %>%
    dplyr::select(C, tidyselect::all_of(stud.col.n), SUBJID, ID, ATFD, ATLD, NTFD, NTLC, NTLD, NDAY, TPT,
                  EVID, MDV, CMT, AMT, tidyselect::all_of(ex.nonmem), ODV, LDV, tidyselect::all_of(pd.dvs),
                  BLQ, LLOQ, tidyselect::all_of(ex.col.n), tidyselect::all_of(pc.col.n), tidyselect::all_of(pd.col.n),
                  tidyselect::all_of(cat.cov.n), tidyselect::all_of(cont.cov), tidyselect::all_of(flags),
                  LINE, USUBJID, tidyselect::all_of(stud.col.c), VISIT, TPTC, DOMAIN, DVID, DVIDU, TIMEU,
                  tidyselect::all_of(ex.col.c), tidyselect::all_of(pc.col.c), tidyselect::all_of(pd.col.c),
                  tidyselect::all_of(cat.cov.c), DTIM, FDOSE, VERSN, BUILD)

  ###FILTER OTHER EVENTS###
  if (keep.other==F) {
    df <- df %>%
      dplyr::filter(EVID!=2) %>%
      dplyr::mutate(LINE = dplyr::row_number())
  }

  ###WARNINGS###
  check <- colnames(df)[nchar(colnames(df))>8]
  if (length(check)==1) {
    warning(paste0("The following column name(s) are longer than 8 characters: ", check))
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

#' combine study level datasets to form population dataset
#'
#' Input two datasets created by pk_build().
#' Any character descriptions that were numerically mapped will be re-mapped to the whole population.
#'
#' @param df1 original PK(PD) dataset
#' @param df2 additional PK(PD) dataset
#' @param demo.map toggle pre-set numeric values for SEX, RACE, and ETHNIC demographic variables
#' @param na value for missing numeric items
#'
#' @return population PK(PD) dataset
#' @export
pk_combine <- function(df1, df2, demo.map=T, na=-999) {

  ###QC dataframes###
  if (FALSE %in% (colnames(df1) %in% colnames(df2)) | FALSE %in% (colnames(df2) %in% colnames(df1))) {
    warning("Column names do not match between both datasets")
  }

  if (TRUE %in% (unique(df1$USUBJID) %in% unique(df2$USUBJID)) | TRUE %in% (unique(df2$USUBJID) %in% unique(df1$USUBJID))) {
    warning("At least one USUBJID exists in both datasets")
  }

  if (TRUE %in% (unique(df1$NSTUDYC) %in% unique(df2$NSTUDYC)) | TRUE %in% (unique(df2$NSTUDYC) %in% unique(df1$NSTUDYC))) {
    warning("At least one NSTUDYC exists in both datasets")
  }

  if (unique(df1$TIMEU)!=unique(df2$TIMEU)) {
    stop("Time units must be equal between both datasets.")
  }

  if(is.na(demo.map)) {
    stop("demo.map parameter must be TRUE or FALSE")
  }
  if(!is.na(demo.map)) {
    if(!is.logical(demo.map)) {
      stop("demo.map parameter must be TRUE or FALSE")
    }
  }

  if(!is.na(na)) {
    if(!is.numeric(na)) {stop("na parameter must be numeric")
    }
  }

  cmt1 <- df1 %>%
    dplyr::filter(EVID!=2) %>%
    dplyr::distinct(DVID, DVIDU, CMT) %>%
    dplyr::arrange(CMT, DVID) %>%
    dplyr::select(CMT, DVID1 = DVID, DVIDU1 = DVIDU)

  cmt2 <- df2 %>%
    dplyr::filter(EVID!=2) %>%
    dplyr::distinct(DVID, DVIDU, CMT) %>%
    dplyr::arrange(CMT, DVID)%>%
    dplyr::select(CMT, DVID2 = DVID, DVIDU2 = DVIDU)

  cmt <- cmt1 %>%
    dplyr::full_join(cmt2, by="CMT") %>%
    dplyr::mutate(DVIDDIFF = dplyr::case_when(is.na(DVID1) ~ "Y",
                                              is.na(DVID2) ~ "Y",
                                              DVID1!=DVID2 ~ "Y",
                                              TRUE ~ "N"),
                  DVIDUDIFF = dplyr::case_when(is.na(DVIDU1) ~ "Y",
                                               is.na(DVIDU2) ~ "Y",
                                               DVIDU1!=DVIDU2 ~ "Y",
                                               TRUE ~ "N"))

  if ("Y" %in% cmt$DVIDDIFF) {
    warning("CMT and DVID assignments are not the same bewteen both datasets")
  }
  if ("Y" %in% cmt$DVIDUDIFF) {
    warning("CMT and DVIDU assignments are not the same between both datasets")
  }

  ###Combine datasets###
  df <- dplyr::bind_rows(df1, df2) %>%
    dplyr::arrange(USUBJID, ATFD, CMT, EVID)

  df <- df %>%
    dplyr::mutate(ID = match(USUBJID, unique(df$USUBJID)),
                  LINE = dplyr::row_number(),
                  COMBD = Sys.Date())

  ###Rearrange covariate columns###
  cat.cov.n.1 <- cov_find(df1, cov="categorical", type="numeric")
  cont.cov.n.1 <- cov_find(df1, cov="continuous", type="numeric")
  oth.cov.n.1 <- cov_find(df1, cov="other", type="numeric")

  cat.cov.c.1 <- cov_find(df1, cov="categorical", type="character")
  oth.cov.c.1 <- cov_find(df1, cov="other", type="character")

  cat.cov.n.2 <- cov_find(df2, cov="categorical", type="numeric")
  cont.cov.n.2 <- cov_find(df2, cov="continuous", type="numeric")
  oth.cov.n.2 <- cov_find(df2, cov="other", type="numeric")

  cat.cov.c.2 <- cov_find(df2, cov="categorical", type="character")
  oth.cov.c.2 <- cov_find(df2, cov="other", type="character")

  num.cov <- c(cat.cov.n.1, cat.cov.n.2, cont.cov.n.1, cont.cov.n.2, oth.cov.n.1, oth.cov.n.2)[!duplicated(c(cat.cov.n.1, cat.cov.n.2, cont.cov.n.1, cont.cov.n.2, oth.cov.n.1, oth.cov.n.2))]
  chr.cov <- c(cat.cov.c.1, cat.cov.c.2, oth.cov.c.1, oth.cov.c.2)[!duplicated(c(cat.cov.c.1, cat.cov.c.2, oth.cov.c.1, oth.cov.c.2))]

  df <- df %>%
    dplyr::relocate(tidyselect::all_of(num.cov), .after="DOSEA") %>%
    dplyr::relocate(tidyselect::all_of(chr.cov), .after="TIMEU") %>%
    dplyr::relocate(NSTUDY, .after="C") %>%
    dplyr::relocate(NSTUDYC, .after="USUBJID")

  ###Redo categorical covariates###
  for (i in chr.cov) {
    name <- gsub("C$", "", i)
    if (i=="NSEXC") {
      df$NSEX[grepl("m|male", df$NTSEXC, ignore.case = T)] <- 0
      df$NSEX[grepl("f|female", df$NSEXC, ignore.case = T)] <- 1
      df$NSEX[grepl("unk", df$NSEXC, ignore.case = T)] <- 2
      df$NSEX[grepl("other", df$NSEXC, ignore.case=T)] <- 3
      if(length(sort(unique(df$NSEX)))!=length(sort(unique(df$NSEXC)))) {
        warning("At least one NSEX failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TSEXC") {
      df$TSEX[grepl("m|male", df$TSEXC, ignore.case = T)] <- 0
      df$TSEX[grepl("f|female", df$TSEXC, ignore.case = T)] <- 1
      df$TSEX[grepl("unk", df$TSEXC, ignore.case = T)] <- 2
      df$TSEX[grepl("other", df$TSEXC, ignore.case = T)] <- 3
      if(length(sort(unique(df$TSEX)))!=length(sort(unique(df$TSEXC)))) {
        warning("At least one TSEX failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="NRACEC") {
      df$NRACE[grepl("white|caucasian", df$NRACEC, ignore.case = T)] <- 1
      df$NRACE[grepl("black|african|aa", df$NRACEC, ignore.case = T)] <- 2
      df$NRACE[grepl("asian", df$NRACEC, ignore.case = T) & !grepl("caucasian", df$NRACEC, ignore.case=T)] <- 3
      df$NRACE[grepl("alaskan|native", df$NRACEC, ignore.case = T)] <- 4
      df$NRACE[grepl("hawa|pacific|island", df$NRACEC, ignore.case = T)] <- 5
      df$NRACE[grepl("multiple|mul", df$NRACEC, ignore.case = T)] <- 6
      df$NRACE[grepl("other", df$NRACEC, ignore.case = T)] <- 7
      df$NRACE[grepl("unknown", df$NRACEC, ignore.case = T)] <- 8
      if(length(sort(unique(df$NRACE)))!=length(sort(unique(df$NRACEC)))) {
        warning("At least one NRACE failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TRACEC") {
      df$TRACE[grepl("white|caucasian", df$TRACEC, ignore.case = T)] <- 1
      df$TRACE[grepl("black|african|aa", df$TRACEC, ignore.case = T)] <- 2
      df$TRACE[grepl("asian", df$TRACEC, ignore.case = T) & !grepl("caucasian", df$TRACEC, ignore.case=T)] <- 3
      df$TRACE[grepl("alaskan|native", df$TRACEC, ignore.case = T)] <- 4
      df$TRACE[grepl("hawa|pacific|island", df$TRACEC, ignore.case = T)] <- 5
      df$TRACE[grepl("multiple|mul", df$TRACEC, ignore.case = T)] <- 6
      df$TRACE[grepl("other", df$TRACEC, ignore.case = T)] <- 7
      df$TRACE[grepl("unknown", df$TRACEC, ignore.case = T)] <- 8
      if(length(sort(unique(df$TRACE)))!=length(sort(unique(df$TRACEC)))) {
        warning("At least one TRACE failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="NETHNICC") {
      df$NETHNIC[grepl("not", df$NETHNICC, ignore.case = T)] <- 0
      df$NETHNIC[grepl("his", df$NETHNICC, ignore.case = T) & !grepl("not", df$NETHNICC, ignore.case=T)] <- 1
      df$NETHNIC[grepl("unk", df$NETHNICC, ignore.case = T)] <- 2
      df$NETHNIC[grepl("other", df$NETHNICC, ignore.case = T)] <- 3
      if(length(sort(unique(df$NETHNIC)))!=length(sort(unique(df$NETHNICC)))) {
        warning("At least one NETHNIC failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TETHNICC") {
      df$TETHNIC[grepl("not", df$TETHNICC, ignore.case = T)] <- 0
      df$TETHNIC[grepl("his", df$TETHNICC, ignore.case = T) & !grepl("not", df$TETHNICC, ignore.case=T)] <- 1
      df$TETHNIC[grepl("unk", df$TETHNICC, ignore.case = T)] <- 2
      df$TETHNIC[grepl("other", df$TETHNICC, ignore.case = T)] <- 3
      if(length(sort(unique(df$TETHNIC)))!=length(sort(unique(df$TETHNICC)))) {
        warning("At least one TETHNIC failed to map. Consider setting demo.map = FALSE.")}}
    else if (!i %in% c("NSEXC", "TSEXC", "NRACEC", "TRACEC", "NETHNICC", "TETHNICC")) {
      df[,name] <- match(unlist(df[, i]), sort(unique(unlist(df[, i]))))
      if(length(sort(unique(df[, name])))==2) {
        df[, name] <- df[, name]-1
      }
    }
  }

  ###FIX NA ITEMS###
  for (i in c("NTFD", "NTLC", "NTLD", num.cov)) {
    df[is.na(df[, i]) & df$EVID!=2, i] <- na
  }

  return(df)
}

#' Write PK(PD) dataset to specified location
#'
#' Dataset created by pk_build() or pk_combine() will be outputted as a .csv file with NONMEM-standard formatting.
#'
#' @param df PK(PD) dataframe
#' @param file filepath
#'
#' @return NA
#' @export
pk_write <- function(df, file) {

  func.version <- "V1.0.0"
  func.date <- "2023-03-13"

  dir <- this.path::dirname2(file) #directory of the dataset

  if (dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  name <- this.path::basename2(file) #dataset name including extension

  if (!grepl(".csv$", name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  ###WRITE DATASET TO SERVER###
  utils::write.csv(df, file, na=".", quote=F, row.names = F)
}

#' Create definition file from published dataset
#'
#' Definition file table can be read into a template word document (.docx) or blank document if desired.
#' Definitions are sourced from a variable list stored separately on your server.
#' Please refer to apmx::variable_list_export() for a standard copy of the variable list.
#'
#' @param file filepath of PK(PD) dataset
#' @param project project name
#' @param variable.list filepath of variable definition csv dataset
#' @param template filepath of docx definition file template
#' @param font font for table contents
#' @param size font size for table contents
#' @param na value used for missing or na numeric covariates
#'
#' @return NA
#' @export
pk_define <- function(file, project, variable.list, template,
                      font="Times New Roman", size=9, na = -999) {

  data.dir <- this.path::dirname2(file) #directory of the dataset

  if (data.dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  data.name <- this.path::basename2(file) #dataset name including extension

  if (!grepl(".csv$", data.name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  if(!is.numeric(size)) {
    stop("size parameter must be numeric.")
  }
  if(size<=0) {
    stop("size parameter must be greater than or equal to 0.")
  }

  if(!is.character(font)) {
    stop("font parameter must be character font description.")
  }

  if(!is.numeric(na)) {
    stop("na must be numeric type.")
  }

  ###CREATE DEFINITION FILE###
  df <- utils::read.csv(file, na.strings=".")
  vl <- utils::read.csv(variable.list,
                        col.names = c("Variable", "Categorization", "Description", "Units", "Comment"))

  define <- data.frame("Variable" = colnames(df))

  cmto <- df[df$EVID==0, c("CMT", "DOMAIN", "DVID", "DVIDU")] %>%
    dplyr::distinct() %>%
    dplyr::arrange(CMT) %>%
    dplyr::mutate(DOMAIN = dplyr::case_when(DOMAIN=="EX" ~ "(Dose)",
                                            DOMAIN=="PC" ~ "(PK)",
                                            DOMAIN=="PD" ~ "(PD)",
                                            DOMAIN=="ADA" ~ "(ADA)"))

  cmtd <- df[df$EVID==1, c("CMT", "DOMAIN", "DVID", "DVIDU")] %>%
    dplyr::distinct() %>%
    dplyr::arrange(CMT) %>%
    dplyr::mutate(DOMAIN = dplyr::case_when(DOMAIN=="EX" ~ "(Dose)",
                                            DOMAIN=="PC" ~ "(PK)",
                                            DOMAIN=="PD" ~ "(PD)",
                                            DOMAIN=="ADA" ~ "(ADA)"))

  cmt <- data.frame("Variable" = "CMT",
                    "Values" = c(paste(cmtd$CMT, "=", cmtd$DVID, cmtd$DOMAIN), paste(cmto$CMT, "=", cmto$DVID, cmto$DOMAIN)))

  exdosu <- unique(unlist(df[df$EVID==1, "DVIDU"]))

  cat.cov.c <- cov_find(df, cov="categorical", type="character")
  cat.cov.c <- substr(cat.cov.c, 2, nchar(cat.cov.c)-1)
  vl.cat.cov.c <- data.frame("Variable" = cat.cov.c) %>%
    dplyr::left_join(vl, by="Variable") %>%
    dplyr::mutate(Variable = cov_find(df, cov="categorical", type="character"),
                  Description = dplyr::case_when(grepl("^N", Variable) ~ paste("Subject", Description, "label"),
                                                 grepl("^T", Variable) ~ paste("Time-varying subject", Description, "label"),
                                                 TRUE ~ Description))

  cat.cov.n <- cov_find(df, cov="categorical", type="numeric")
  cat.cov.n <- substr(cat.cov.n, 2, nchar(cat.cov.n))
  vl.cat.cov.n <- data.frame("Variable" = cat.cov.n) %>%
    dplyr::left_join(vl, by="Variable") %>%
    dplyr::mutate(Variable = cov_find(df, cov="categorical", type="numeric"),
                  Description = dplyr::case_when(grepl("^N", Variable) ~ paste("Subject", Description),
                                                 grepl("^T", Variable) ~ paste("Time-varying subject", Description),
                                                 TRUE ~ Description))

  cont.cov <- cov_find(df, cov="continuous", type="numeric")
  cont.cov <- substr(cont.cov, 2, nchar(cont.cov))
  vl.cont.cov <- data.frame("Variable" = cont.cov) %>%
    dplyr::left_join(vl, by="Variable") %>%
    dplyr::mutate(Variable = cov_find(df, cov="continuous", type="numeric"),
                  Description = dplyr::case_when(grepl("^B", Variable) ~ paste("Baseline", Description),
                                                 grepl("^T", Variable) ~ paste("Time-varying", Description),
                                                 TRUE ~ Description))

  vl <- dplyr::bind_rows(vl, vl.cat.cov.c, vl.cat.cov.n, vl.cont.cov)

  cov <- data.frame("Variable" = NA,
                    "Values" = NA)
  for (i in vl.cat.cov.c$Variable) {
    df1 <- df %>%
      dplyr::select(tidyselect::all_of(i), tidyselect::all_of(gsub("C$", "", i))) %>%
      dplyr::distinct()
    df1$Variable <- colnames(df1)[2]
    colnames(df1)[1:2] <- c("Character", "Numeric")
    df1 <- df1 %>%
      dplyr::arrange(Numeric) %>%
      dplyr::filter(Numeric!=-999) %>%
      dplyr::mutate(Values = paste(Numeric, "=", Character)) %>%
      dplyr::select(Variable, Values)

    cov <- dplyr::bind_rows(cov, df1)
  }
  cov <- cov[-1,]

  evid <- df %>%
    dplyr::distinct(EVID) %>%
    dplyr::mutate(Variable = "EVID",
                  Values = dplyr::case_when(EVID==0 ~ "0 = Observation event",
                                            EVID==1 ~ "1 = Dose event",
                                            EVID==2 ~ "2 = Other event",
                                            EVID==3 ~ "3 = Reset event",
                                            EVID==4 ~ "4 = Reset and Dose event")) %>%
    dplyr::arrange(Variable, Values) %>%
    dplyr::select(Variable, Values)

  mdv <- data.frame("Variable" = "MDV",
                    "Values" = c("0 = DV not missing", "1 = DV is missing"))

  ss <- data.frame("Variable" = "SS",
                   "Values" = c("0 = not steady state", "1 = steady state (reset)", "2 = steady state (no reset)"))

  blq <- data.frame("Variable" = "BLQ",
                    "Values" = c("0 = observation not BLQ", "1 = BLQ observation (pre-dose)", "2 = BLQ observation (post-dose)"))

  #Create dataset of flag values
  c <- data.frame("Variable" = "C",
                  "Values" = c("C = unused record"))

  pdosef <- data.frame("Variable" = "PDOSEF",
                       "Values" = c("0 = At or after first dose", "1 = Prior to first dose"))

  timef <- data.frame("Variable" = "TIMEF",
                      "Values" = c("0 = ATFD not missing", "1 = ATFD is missing"))

  amtf <- data.frame("Variable" = "AMTF",
                     "Values" = c("0 = AMT not missing", "1 = AMT is missing"))

  dupf <- data.frame("Variable" = "DUPF",
                     "Values" = c("0 = Not duplicated", "1 = At least one duplicate"))

  noexf <- data.frame("Variable" = "NOEXF",
                      "Values" = c("0 = At least one dose", "1 = No dose"))

  plbof <- data.frame("Variable" = "PLBOF",
                      "Values" = c("0 = Not placebo", "1 = Placebo"))

  sparsef <- data.frame("Variable" = "SPARSEF",
                        "Values" = c("0 = Serial sampling", "1 = Sparse sampling"))

  trexf <- data.frame("Variable" = "TREXF",
                      "Values" = c("0 = At least one future observation", "1 = No future observations"))

  sdf <- data.frame("Variable" = "SDF",
                    "Values" = c("0 = Multi-dose subject", "1 = Single-dose subject"))

  impex <- data.frame("Variable" = "IMPEX",
                      "Values" = c("0 = Dose time not imputed", "1 = Dose time imputed"))

  impdv <- data.frame("Variable" = "IMPDV",
                      "Values" = c("0 = Observation time not imputed", "1 = Observation time imputed"))

  dvf <- c()
  for (i in colnames(df)) {
    if(grepl("NODV", i)) {
      dvf <- c(dvf, i)
    }
  }
  nodvf <- data.frame("Variable" = NA, "Values" = NA)
  for (i in dvf) {
    cmtn <- gsub("\\D+", "", i)
    nodvf1 <- data.frame("Variable" = i,
                         "Values" = c(paste0("0 = At least one observation (CMT = ", cmtn, ")"), paste0("1 = No observations (CMT = ", cmtn, ")")))
    nodvf <- dplyr::bind_rows(nodvf, nodvf1)
  }
  nodvf <- nodvf[-1, ]

  flgs <- dplyr::bind_rows(c, pdosef, timef, amtf, dupf, noexf, plbof, sparsef, trexf, sdf, impex, impdv, nodvf)

  #Combine all values dataframes together
  values <- dplyr::bind_rows(cmt, cov, evid, mdv, blq, ss, flgs)

  #Finalize definition dataset
  define <- define %>%
    dplyr::left_join(vl, by="Variable") %>% #join variable list
    dplyr::group_by(Variable) %>%
    dplyr::mutate(Format = paste0(toupper(substr(typeof(df[, Variable]), 1, 1)), substr(typeof(df[, Variable]), 2,  nchar(typeof(df[, Variable]))))) %>% #Determine type of each column
    dplyr::ungroup() %>%
    dplyr::mutate(Format = dplyr::case_when(Variable %in% c("C", "DTIM", "FDOSE") ~ "Character", #Final formatting for this column
                                            Format %in% c("Integer", "Double", "Logical") ~ "Numeric",
                                            TRUE ~ Format),
                  Comment = ifelse(Format=="Character" & Variable!="C", "Dropped in control stream" ,Comment), #Add comment for characters
                  Units = ifelse(Variable=="CMT",  NA, #Empty the units for CMT
                                 dplyr::case_when(Variable %in% c("ATFD", "ATLD", "NTFD", "NTLC", "NTLD", "TPT", "DUR") ~ unique(df$TIMEU), #Add time units to these variables
                                                  Variable %in% c("AMT", "DOSEA") ~ exdosu, #Add dose units
                                                  Variable=="RATE" ~ paste0(exdosu, "/", gsub("s$", "", unique(df$TIMEU))),
                                                  TRUE ~ Units))) %>%
    dplyr::left_join(values, by="Variable") %>% #join values
    dplyr::mutate(Units = dplyr::case_when(is.na(Units) ~ "",
                                           TRUE ~ Units),
                  Values = dplyr::case_when(is.na(Values) ~ "",
                                            TRUE ~ Values)) %>%
    dplyr::select(Variable, Categorization, Description, Values, Units, Format, Comment)

  define$Units[define$Variable=="CMT"] <- c(cmtd$DVIDU, cmto$DVIDU) #Add CMT units

  flextable::set_flextable_defaults(
    font.size = size,
    font.family = font)

  define <- define %>%
    flextable::flextable() %>%
    flextable::border_inner_h(part = "body",
                              border = officer::fp_border(color = "grey", width = 0.1, style="solid")) %>%
    flextable::merge_v(j = c(1, 2, 3, 6, 7), target = c(1, 2, 3, 6, 7), part = "body", combine = T) %>%
    flextable::bold(part = "header") %>%
    flextable::height(height = 0.3, unit = "in")

  widths <- flextable::dim_pretty(define)
  if (sum(unlist(widths[1])) <= 10) {
    define <- define %>%
      flextable::autofit()
  }

  else {
    define <- define %>%
      flextable::width(j = c(3, 4), width = 1.78, unit = "in") %>%
      flextable::width(j = 7, width = 1.5, unit = "in") %>%
      flextable::width(j = c(1, 5, 6), width = 0.9, unit = "in") %>%
      flextable::width(j = 2, width = 1, unit = "in")
  }


  foot1 <-
  define <- define %>%
    flextable::add_footer_lines(values = '*NA parameters and missing character-type covariates labeled with "."') %>%
    flextable::add_footer_lines(values = paste("**Missing numeric-type covariates labeled with", na))

  ###WRITE DEFINITION FILE TO SERVER###
  tmplt <- officer::read_docx(path=template) %>%
    flextable::body_add_flextable(define) %>%
    officer::headers_replace_all_text("Project", project, warn=F) %>%
    officer::headers_replace_all_text("Dataset", paste("Analysis Dataset:", data.name), warn=F) %>%
    print(target = paste0(data.dir, "\\DEFINE_", gsub(".csv", "", data.name), ".docx"))
}

#' Produce summary tables for a PK(PD) dataset
#'
#' Summarize BLQ distributions, categorical covariates, and continuous covariates in three tables.
#' Outputs are default .csv files, but can also be .docx and/or .pptx
#' Tables are default stratified by study, but can be stratified by any variable requested by the user.
#'
#' @param file filepath PK(PD) dataset produced by pk_build()
#' @param strat.by vector of variables names to stratify the summary tables
#' @param ignore.c ignores records flagged in the C column when TRUE
#' @param na numeric value interpreted by -999
#' @param docx creates summary tables as a Word document when TRUE
#' @param pptx creates summary tables as a PowerPoint document when TRUE
#' @param docx.font font for the summary tables in the Word document
#' @param docx.size font size for the summary tables in the Word document
#' @param docx.template filepath for template .docx file. When NULL, the summary tables print to a blank document.
#' @param pptx.template filepath for template .pptx file. When NULL, the summary tables print to a blank slide.
#' @param pptx.font font for the summary tables in the PowerPoint document
#' @param pptx.size font size for the summary tables in the PowerPoint document
#' @param docx.orientation orientation of .docx files
#' @param dir filepath for output directory. When NA, all files output to the same directory as the file parameter.
#'
#' @return summary tables as .csv, .docx, and .pptx files
#' @export
pk_summarize <- function(file, strat.by = "NSTUDYC",
                         ignore.c = T, na = -999,
                         docx = F, pptx = F,
                         docx.font="Times New Roman", docx.size=9,
                         docx.template=NULL, pptx.template=NULL,
                         pptx.font="Times New Roman", pptx.size=12,
                         docx.orientation = "portrait", dir = NA) {
  ###QC###
  #make sure filepath is valid
  data.dir <- this.path::dirname2(file) #directory of the dataset
  if (data.dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  data.name <- this.path::basename2(file) #dataset name including extension
  if (!grepl(".csv$", data.name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  #make sure strat.by is a character or vector of characters
  if(!is.character(strat.by)) {
    stop("strat.by must be in character form only")
  }

  #ignore.c must be logical
  if(!is.logical(ignore.c)) {
    stop("ignore.c parameter must be TRUE or FALSE.")
  }

  #na must be numeric
  if(!is.numeric(na)) {
    stop("na parameter must be numeric")
  }

  #docx must be logical
  if(!is.logical(docx)) {
    stop("docx parameter must be TRUE or FALSE")
  }

  if(!is.null(docx.template)) {
    docx.dir <- this.path::dirname2(docx.template)
    if (docx.dir==".") {
      stop(paste(docx.template, "is not a valid filepath."))
    }

    docx.name <- this.path::basename2(docx.template) #dataset name including extension
    if (!grepl(".docx$", docx.name)) {
      stop(paste(docx.template, "must include document name and .docx suffix."))
    }
  }

  #pptx must be logical
  if(!is.logical(pptx)) {
    stop("pptx parameter must be TRUE or FALSE")
  }

  if(!is.null(pptx.template)) {
    pptx.dir <- this.path::dirname2(pptx.template)
    if (pptx.dir==".") {
      stop(paste(pptx.template, "is not a valid filepath."))
    }

    pptx.name <- this.path::basename2(pptx.template) #dataset name including extension
    if (!grepl(".pptx$", pptx.name)) {
      stop(paste(pptx.template,  "must include document name and .pptx suffix."))
    }
  }

  if(!is.character(docx.font)) {
    stop("docx.font parameter must be a character expression.")
  }
  if(!is.character(pptx.font)) {
    stop("pptx.font parameter must be a character expression.")
  }

  if(!is.numeric(docx.size)) {
    stop("docx.size parameter must be a number")
  }
  if(!is.numeric(pptx.size)) {
    stop("pptx.size parameter must be a number")
  }

  if(!docx.orientation %in% c("portrait", "landscape")) {
    stop("docx.orientation parameter must be portrait or landscape.")
  }

  if (docx.orientation=="portrait") {
    maxwidth <- 7
  }
  else if (docx.orientation=="landscape") {
    maxwidth <- 10
  }

  #dir must be a filepath
  if (is.na(dir)) {
    dir <- this.path::dirname2(file)
  }

  data.dir <- this.path::dirname2(dir) #directory of the dataset
  if (data.dir==".") {
    stop(paste(dir, "is not a valid filepath."))
  }

  orig <- utils::read.csv(file, na.strings=".")

  for (i in strat.by) {
    if (!i %in% colnames(orig)) {
      stop(paste("Column", i, "does not exist in the PK(PD) dataset"))
    }
  }

  for (i in strat.by) {
    if (ignore.c==T) {
      df <- orig[is.na(orig$C),]
    }
    else {
      df <- orig
    }

    ### Dataset summary ###
    df.total <- data.frame(Item = c("Subjects", "Total Records", "Dose Records",
                                    "Observation Records", "Other Records"),
                           "Total" = NA)

    df.total$Total[1] <- length(unique(df$ID))
    df.total$Total[2] <- nrow(df)
    df.total$Total[3] <- nrow(df[df$EVID==1,])
    df.total$Total[4] <- nrow(df[df$EVID==0,])
    df.total$Total[5] <- nrow(df[df$EVID==2,])
    df.total$Total <- as.character(df.total$Total)

    for(j in sort(unique(df[, i]))) {
      df.temp <- data.frame(Item = c("Subjects", "Total Records", "Dose Records",
                                     "Observation Records", "Other Records"),
                            "Total" = NA)
      colnames(df.temp) <- c("Item", j)

      df.temp[1, 2] <- length(unique(df[df[,i]==j, "ID"]))
      df.temp[2, 2] <- nrow(df[df[,i]==j,])
      df.temp[3, 2] <- nrow(df[df$EVID==1 & df[,i]==j,])
      df.temp[4, 2] <- nrow(df[df$EVID==0 & df[,i]==j,])
      df.temp[5, 2] <- nrow(df[df$EVID==2 & df[,i]==j,])
      df.temp[, 2] <- as.character(df.temp[, 2])

      df.total <- df.total %>% dplyr::left_join(df.temp, by="Item")
    }

    df.total$DVID = "General"
    df.total <- df.total[, c(ncol(df.total), 1, 3:(ncol(df.total)-1), 2)]

    df.total.f <- df.total[1,]
    df.total.f[1,] <- rep("General", ncol(df.total))
    df.total <- dplyr::bind_rows(df.total.f, df.total)

    ###BLQ summary###
    df.blq <- data.frame(DVID = unique(df$DVID[df$EVID==0]),
                         Total = NA) %>%
      tidyr::crossing(Item = c(0, 1, 2, 3)) #1 = total, #2 = quantifiable, # = post-dose BLQ

    df.blq <- df.blq[,c(1, 3, 2)]

    for (k in 1:nrow(df.blq)) {
      if (df.blq$Item[k]==1) {
        df.blq$Total[k] <- nrow(df[df$DVID==df.blq$DVID[k] & df$EVID==0, ])
      }
      else if (df.blq$Item[k]==2) {
        df.blq$Total[k] <- nrow(df[df$DVID==df.blq$DVID[k] & df$EVID==0 & df$BLQ==0, ])
      }
      else if (df.blq$Item[k]==3) {
        df.blq$Total[k] <- nrow(df[df$DVID==df.blq$DVID[k] & df$EVID==0 & df$BLQ==2, ])
      }
    }

    for (k in 1:nrow(df.blq)) {
      if (df.blq$Item[k]==1) {

      }
      else if (df.blq$Item[k]==2) {
        df.blq$Total[k] <- paste0(df.blq$Total[k], " (", round(100*as.numeric(df.blq$Total[k])/as.numeric(df.blq$Total[k-1]), 1), "%)")
      }
      else if (df.blq$Item[k]==3) {
        df.blq$Total[k] <- paste0(df.blq$Total[k], " (", round(100*as.numeric(df.blq$Total[k])/as.numeric(df.blq$Total[k-2]), 1), "%)")
      }
    }

    for (j in sort(unique(df[, i]))) {
      df.temp <- df.blq[, 1:2]
      df.temp$Temp <- NA

      for (k in 1:nrow(df.temp)) {
        if (df.temp$Item[k]==1) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVID==df.temp$DVID[k] & df$EVID==0, ])
        }
        else if (df.temp$Item[k]==2) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVID==df.temp$DVID[k] & df$EVID==0 & df$BLQ==0, ])
        }
        else if (df.temp$Item[k]==3) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVID==df.temp$DVID[k] & df$EVID==0 & df$BLQ==2, ])
        }
      }

      for (k in 1:nrow(df.temp)) {
        if (df.temp$Item[k]==1) {

        }
        else if (df.temp$Item[k]==2) {
          df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/as.numeric(df.temp$Temp[k-1]), 1), "%)")
        }
        else if (df.temp$Item[k]==3) {
          df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/as.numeric(df.temp$Temp[k-2]), 1), "%)")
        }
      }

      colnames(df.temp) <- c("DVID", "Item", j)
      df.blq <- df.blq %>% dplyr::left_join(df.temp, by=c("DVID", "Item"))
    }

    df.blq <- df.blq[,c(1, 2, 4:ncol(df.blq), 3)] %>%
      dplyr::mutate(Item = dplyr::case_when(Item==0 ~ "overwrite",
                                            Item==1 ~ "Total",
                                            Item==2 ~ "Quantifiable",
                                            Item==3 ~ "BLQ"))

    for (j in 1:nrow(df.blq)) {
      if(df.blq$Item[j]=="overwrite") {
        df.blq[j,] <- df.blq[j, 1]
      }
    }

    df.summary <- dplyr::bind_rows(df.total, df.blq)
    merge.col <- c(1)
    for (j in 2:nrow(df.summary)) {
      if (df.summary[j,1]!=df.summary[j-1,1]) {
        merge.col <- c(merge.col, j)
      }
    }
    df.summary <- df.summary[, -1]

    for (j in merge.col) {
      df.summary[j,] <- paste(df.summary[j,], "Observations")
    }

    utils::write.csv(df.summary, paste0(dir, paste0("\\BLQ_by_", i, ".csv")), row.names = F, quote = F, na=".")

    if(docx==T) {
      flextable::set_flextable_defaults(
        font.size = docx.size,
        font.family = docx.font)

      df.summary1 <- df.summary %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::font(fontname = docx.font, part = "all") %>%
        flextable::fontsize(size = docx.size, part = "all") %>%
        flextable::bold(part = "header")

      widths <- flextable::dim_pretty(df.summary1)
      if (sum(unlist(widths[1])) <= maxwidth) {
        df.summary1 <- df.summary1 %>%
          flextable::autofit()
      }

      if (sum(unlist(widths[1])) > maxwidth) {
        result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
        if (result.col * (ncol(df.summary)-1) < (maxwidth - 1)) {
          df.summary1 <- df.summary1 %>%
            flextable::width(j = 2:ncol(df.summary), width = result.col) %>%
            flextable::width(j = 1, width = maxwidth - result.col * (ncol(df.summary)-1))
        }
      }

      if (ignore.c==T) {
        df.summary1 <- df.summary1 %>%
          flextable::add_footer_lines(values = "*Ignores records flagged by C")
      }

      tmplt <- officer::read_docx(path=docx.template) %>%
        flextable::body_add_flextable(df.summary1) %>%
        print(target = paste0(dir, "\\BLQ_by_", i, ".docx"))
    }

    if(pptx==T) {
      flextable::set_flextable_defaults(
        font.size = pptx.size,
        font.family = pptx.font)

      df.summary1 <- df.summary %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::font(fontname = pptx.font, part = "all") %>%
        flextable::fontsize(size = pptx.size, part = "all") %>%
        flextable::bold(part = "header")

      tmplt <- officer::read_pptx(path=pptx.template) %>%
        officer::add_slide() %>%
        officer::ph_with(value = df.summary1, location = officer::ph_location_fullsize()) %>%
        print(target = paste0(dir, "\\BLQ_by_", i, ".pptx"))
    }

    ###Cat covariate summary###
    cov.c <- cov_find(df, cov="categorical", type="character")
    cov.n <- cov_find(df, cov="categorical", type="numeric")

    cov <- data.frame("Covariate" = NA,
                      "ValueN" = NA,
                      "Value" = NA)

    df <- df %>% dplyr::filter(EVID<2)

    for (j in cov.n) {
      df.temp <- df[, c(j, paste0(j, "C"))] %>%
        dplyr::mutate(Covariate = j) %>%
        dplyr::distinct()

      colnames(df.temp) <- c("ValueN", "Value", "Covariate")
      cov <- dplyr::bind_rows(cov, df.temp) %>%
        dplyr::filter(!is.na(Covariate))
    }

    nsub <- length(unique(df$USUBJID))

    cov$Total <- NA
    for (k in 1:nrow(cov)) {
      cov$Total[k] <- as.character(length(unique(df$USUBJID[df[,cov$Covariate[k]]==cov$ValueN[k]])))
    }

    for (k in 1:nrow(cov)) {
      cov$Total[k] <- paste0(cov$Total[k], " (", round(100*as.numeric(cov$Total[k])/nsub, 1), "%)")
    }

    for (j in sort(unique(df[,i]))) {
      df.temp <- cov[, 1:3]
      df.temp$Temp <- NA

      nsub <- length(unique(df$USUBJID[df[,i]==j]))

      for (k in 1:nrow(df.temp)) {
        df.temp$Temp[k] <- length(unique(df$USUBJID[df[,df.temp$Covariate[k]]==df.temp$ValueN[k] & df[,i]==j]))
      }

      for (k in 1:nrow(df.temp)) {
        df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/nsub, 1), "%)")
      }

      colnames(df.temp) <- c("Covariate", "ValueN", "Value", j)
      cov <- cov %>% dplyr::left_join(df.temp, by=c("Covariate", "ValueN", "Value"))
    }

    cov <- cov[,c(1, 3, 5:ncol(cov), 4)]
    cov$Value[is.na(cov$Value)] <- "MISSING"

    df.temp <- cov
    for (j in 1:nrow(df.temp)) {
      df.temp[j,] <- df.temp[j, 1]
    }
    df.temp <- df.temp %>%
      dplyr::distinct() %>%
      dplyr::mutate(Order = 0)

    cov <- cov %>%
      dplyr::mutate(Order = 1) %>%
      dplyr::bind_rows(df.temp) %>%
      dplyr::arrange(Covariate, Order) %>%
      dplyr::select(-Order)

    merge.col <- c()
    for (j in 1:nrow(cov)) {
      if (cov[j,1]==cov[j,ncol(cov)]) {
        merge.col <- c(merge.col, j)
      }
    }
    cov <- cov[,-1]

    utils::write.csv(cov, paste0(dir, paste0("\\CATCOV_by_", i, ".csv")), row.names = F, quote = F, na=".")

    if(docx==T) {
      flextable::set_flextable_defaults(
        font.size = docx.size,
        font.family = docx.font)

      cov1 <- cov %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::bold(part = "header") %>%
        flextable::width(j = 1, width = 2, unit="in")

      widths <- flextable::dim_pretty(cov1)
      if (sum(unlist(widths[1])) <= maxwidth) {
        cov1 <- cov1 %>%
          flextable::autofit()
      }

      if (sum(unlist(widths[1])) > maxwidth) {
        result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
        if (result.col * (ncol(cov)-1) < (maxwidth - 1)) {
          cov1 <- cov1 %>%
            flextable::width(j = 2:ncol(cov), width = result.col) %>%
            flextable::width(j = 1, width = maxwidth - result.col * (ncol(cov)-1))
        }
      }

      if (ignore.c==T) {
        cov1 <- cov1 %>%
          flextable::add_footer_lines(values = "*Ignores records flagged by C")
      }

      tmplt <- officer::read_docx(path=docx.template) %>%
        flextable::body_add_flextable(cov1) %>%
        print(target = paste0(dir, "\\CATCOV_by_", i, ".docx"))
    }

    if(pptx==T) {
      flextable::set_flextable_defaults(
        font.size = pptx.size,
        font.family = pptx.font)

      cov1 <- cov %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::bold(part = "header")

      tmplt <- officer::read_pptx(path=pptx.template) %>%
        officer::add_slide() %>%
        officer::ph_with(value = cov1, location = officer::ph_location_fullsize()) %>%
        print(target = paste0(dir, "\\CATCOV_by_", i, ".pptx"))
    }

    ###Cont cov summary###
    df.cont <- df %>%
      dplyr::select(ID, dplyr::all_of(i), dplyr::all_of(cov_find(df, cov="continuous", type="numeric")))

    cov <- data.frame(Covariate = colnames(df.cont)[3:ncol(df.cont)],
                      Total = NA) %>%
      tidyr::crossing(Measure = c(0, 1, 2, 3, 4, 5, 6))

    cov <- cov[,c(1, 3, 2)]

    for (k in 1:nrow(cov)) {
      df.temp <- df.cont %>%
        dplyr::select(ID, dplyr::all_of(cov$Covariate[k])) %>%
        dplyr::distinct()

      df.temp.1 <- df.temp[df.temp[, cov$Covariate[k]]!=na, ]
      df.temp.2 <- df.temp[df.temp[, cov$Covariate[k]]==na, ]

      if (cov$Measure[k]==1) {
        cov$Total[k] <- as.character(length(unique(df.temp.1$ID)))
      }
      else if (cov$Measure[k]==2) {
        cov$Total[k] <- paste0(round(mean(df.temp.1[,2]), 2), " (", round(stats::sd(df.temp.1[,2]), 2), ")")
      }
      else if (cov$Measure[k]==3) {
        cov$Total[k] <- paste0(round(stats::median(df.temp.1[,2]), 2), " (", round(stats::quantile(df.temp.1[,2], probs = 0.25), 2), "; ", round(stats::quantile(df.temp.1[,2], probs = 0.75), 2), ")")
      }
      else if (cov$Measure[k]==4) {
        cov$Total[k] <- paste0(round(stats::quantile(df.temp.1[,2], probs = 0.05), 2), "; ", round(stats::quantile(df.temp.1[,2], probs = 0.95), 2))
      }
      else if (cov$Measure[k]==5) {
        cov$Total[k] <- paste0(round(min(df.temp.1[,2]), 2), "; ", round(max(df.temp.1[,2]), 2))
      }
      else if (cov$Measure[k]==6) {
        cov$Total[k] <- as.character(length(unique(df.temp.2$ID)))
      }
    }

    for (j in sort(unique(df.cont[,i]))) {
      df.temp <- cov[, 1:2]
      df.temp$Temp <- NA

      for (k in 1:nrow(cov)) {
        df.temp.1 <- df.cont[df.cont[,i]==j, ] %>%
          dplyr::select(ID, dplyr::all_of(cov$Covariate[k])) %>%
          dplyr::distinct()

        df.temp.1.1 <- df.temp.1[df.temp.1[, df.temp$Covariate[k]]!=na, ]
        df.temp.1.2 <- df.temp.1[df.temp.1[, df.temp$Covariate[k]]==na, ]

        if (df.temp$Measure[k]==1) {
          df.temp$Temp[k] <- as.character(length(unique(df.temp.1.1$ID)))
        }
        else if (df.temp$Measure[k]==2 & nrow(df.temp.1.1)==0) {
          df.temp$Temp[k] <- "--"
        }
        else if (df.temp$Measure[k]==2) {
          df.temp$Temp[k] <- paste0(round(mean(df.temp.1.1[,2]), 2), " (", round(stats::sd(df.temp.1.1[,2]), 2), ")")
        }
        else if (df.temp$Measure[k]==3 & nrow(df.temp.1.1)==0) {
          df.temp$Temp[k] <- "--"
        }
        else if (df.temp$Measure[k]==3) {
          df.temp$Temp[k] <- paste0(round(stats::median(df.temp.1.1[,2]), 2), " (", round(stats::quantile(df.temp.1.1[,2], probs = 0.25), 2), "; ", round(stats::quantile(df.temp.1[,2], probs = 0.75), 2), ")")
        }
        else if (df.temp$Measure[k]==4 & nrow(df.temp.1.1)==0) {
          df.temp$Temp[k] <- "--"
        }
        else if (df.temp$Measure[k]==4) {
          df.temp$Temp[k] <- paste0(round(stats::quantile(df.temp.1.1[,2], probs = 0.05), 2), "; ", round(stats::quantile(df.temp.1.1[,2], probs = 0.95), 2))
        }
        else if (df.temp$Measure[k]==5 & nrow(df.temp.1.1)==0) {
          df.temp$Temp[k] <- "--"
        }
        else if (df.temp$Measure[k]==5) {
          df.temp$Temp[k] <- paste0(round(min(df.temp.1.1[,2]), 2), "; ", round(max(df.temp.1.1[,2]), 2))
        }
        else if (df.temp$Measure[k]==6) {
          df.temp$Temp[k] <- as.character(length(unique(df.temp.1.2$ID)))
        }
      }

      colnames(df.temp) <- c("Covariate", "Measure", j)
      cov <- cov %>% dplyr::left_join(df.temp, by=c("Covariate", "Measure"))
    }

    cov <- cov[,c(1:2, 4:ncol(cov), 3)] %>%
      dplyr::mutate(Measure = dplyr::case_when(Measure==0 ~ "overwrite",
                                               Measure==1 ~ "NSUB",
                                               Measure==2 ~ "MEAN (SD)",
                                               Measure==3 ~ "MEDIAN (IQR)",
                                               Measure==4 ~ "P05-P95",
                                               Measure==5 ~ "MIN-MAX",
                                               Measure==6 ~ "MISSING"))

    for (j in 1:nrow(cov)) {
      if (cov$Measure[j]=="overwrite") {
        cov[j,] <- cov$Covariate[j]
      }
    }

    merge.col <- c(1)
    for (j in 2:nrow(cov)) {
      if (cov[j,1]!=cov[j-1,1]) {
        merge.col <- c(merge.col, j)
      }
    }
    cov <- cov[, -1]

    utils::write.csv(cov, paste0(dir, paste0("\\CONTCOV_by_", i, ".csv")), row.names = F, quote = F, na=".")

    if(docx==T) {
      flextable::set_flextable_defaults(
        font.size = docx.size,
        font.family = docx.font)

      cov1 <- cov %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::bold(part = "header")

      widths <- flextable::dim_pretty(cov1)
      if (sum(unlist(widths[1])) <= maxwidth) {
        cov1 <- cov1 %>%
          flextable::autofit()
      }

      if (sum(unlist(widths[1])) > maxwidth) {
        result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
        if (result.col * (ncol(cov)-1) < (maxwidth-1)) {
          cov1 <- cov1 %>%
            flextable::width(j = 2:ncol(cov), width = result.col) %>%
            flextable::width(j = 1, width = maxwidth - result.col * (ncol(cov)-1))
        }
      }

      if (ignore.c==T) {
        cov1 <- cov1 %>%
          flextable::add_footer_lines(values = "*Ignores records flagged by C")
      }

      tmplt <- officer::read_docx(path=docx.template) %>%
        flextable::body_add_flextable(cov1) %>%
        print(target = paste0(dir, "\\CONTCOV_by_", i, ".docx"))
    }

    if(pptx==T) {
      flextable::set_flextable_defaults(
        font.size = pptx.size,
        font.family = pptx.font)

      cov1 <- cov %>%
        flextable::flextable() %>%
        flextable::merge_h(i = merge.col) %>%
        flextable::align(i = merge.col, align="center") %>%
        flextable::bold(i = merge.col) %>%
        flextable::bold(part = "header")

      tmplt <- officer::read_pptx(path=pptx.template) %>%
        officer::add_slide() %>%
        officer::ph_with(value = cov1, location = officer::ph_location_fullsize()) %>%
        print(target = paste0(dir, "\\CONTCOV_by_", i, ".pptx"))
    }
  }
}

#' Apply covariates to PK(PD) dataset
#'
#' Add covariates to a dataset built by pk_build() or pk_combine()
#' Can add subject-level covariates (by any ID variable) or time-varying (by any time variable)
#'
#' @param df PK(PD) dataframe generated by pk_build
#' @param cov dataframe of covariates
#' @param id.by id variable to merge covariates
#' @param time.by time variable to merge covariates
#' @param direction fill direction for time-varying covariates
#' @param cov.rnd covariate rounding parameter
#' @param na value to replace NA numeric covariates
#' @param demo.map toggle pre-set numeric values for SEX, RACE, and ETHNIC demographic variables
#' @param keep.other filter to keep or remove other events, EVID = 2
#'
#' @return PK(PD) dataset with additional covariates
#' @export
cov_apply <- function(df, cov, id.by="USUBJID", time.by=NA,
                      direction="downup", cov.rnd=NA, na=-999,
                      demo.map=T, keep.other=T) {


  ###QC id.by###
  if (!(as.character(id.by) %in% c("USUBJID", "SUBJID", "ID"))) { #limit id.by inputs
    stop("id.by must be one of the following options: USUBJID, SUBJID, ID")
  }

  if (length(id.by)>1) { #only one id type
    stop("cov_apply can only fill by one ID type.")
  }

  ###QC time.by###
  if (!(is.na(time.by) | as.character(time.by) %in% c(NA, "DTIM", "ATFD", "NTFD", "ATLD", "NTLD", "NTLC", "NDAY"))) {
    stop("time.by must be one of the following options: NA (subject-level attribute), DTIM, ATFD, ATLD, NTFD, NTLC, NTLD, NDAY")
  }

  if (length(time.by)>1) {
    stop("cov_apply can only fill by one time type.")
  }

  ###QC direction###
  if (!(direction %in% c("down", "up", "downup", "updown"))) {
    stop("direction must be one of the following (tidy) options: down, up, downup, updown")
  }

  if (length(direction)>1) {
    stop("cov_apply can only fill in one direction.")
  }

  ###QC cov.rnd###
  if (!is.na(cov.rnd)) {
    if (!is.numeric(cov.rnd)) {
      stop("cov.rnd parameter must be an integer or NA.")
    }
    if (cov.rnd %% 1 != 0) {
      stop("cov.rnd parameter must be an integer or NA.")
    }
  }

  ###QC na###
  if (!is.na(na)) {
    if (!is.numeric(na)) {
      stop("na parameter must be NA or numeric,")
    }
  }

  ###QC demo map###
  if (is.na(demo.map)) {
    stop("demo.map parameter must be TRUE or FALSE.")
  }
  if (!is.na(demo.map)) {
    if(!is.logical(demo.map)) {
      stop("demo.map parameter must be TRUE or FALSE.")
    }
  }

  ###QC keep.other###
  if (!is.logical(keep.other)) {
    stop("keep.other parameter must be TRUE or FALSE.")
  }

  ###QC df###
  req.cols <- c(id.by) #always required
  if(!is.na(time.by)) {
    req.cols <- c(req.cols, time.by)
  }

  for (i in req.cols) {
    if (!i %in% colnames(df)) { #make sure req.col are in df
      stop(paste(i, "column not in PKPD dataframe. Cannot merge with covariate dataframe."))
    }
  }

  if (!is.na(time.by)) {
    if (time.by=="DTIM") {
      if(!("FDOSE" %in% colnames(df))) {
        stop(paste(df, "must contain FDOSE (date/time of first dose) if merging with DTIM."))
      }

      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", df$DTIM[!is.na(df$DTIM)])) {
        if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", df$DTIM[!is.na(df$DTIM)])) {
          if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", df$DTIM[!is.na(DTIM)])) {
            stop(paste("DTIM in df is not ISO 8601 format."))
          }
        }
      }

      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", df$FDOSE[!is.na(df$FDOSE)])) {
        if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", df$FDOSE[!is.na(df$FDOSE)])) {
          if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", df$FDOSE[!is.na(FDOSE)])) {
            stop(paste("FDOSE in df is not ISO 8601 format."))
          }
        }
      }

      df <- df %>%
        dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d")),
                      FDOSE = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", FDOSE) ~ as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                               grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", FDOSE) ~ as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                               grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", FDOSE) ~ as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                               grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", FDOSE) ~ as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d %H:%M"),
                                               grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", FDOSE) ~ as.POSIXct(FDOSE, tz="UTC", format="%Y-%m-%d")))
    }
  }

  ###QC cov###
  req.cols <- c(id.by)
  if(!is.na(time.by)) {
    req.cols <- c(req.cols, time.by)
  }

  for (i in req.cols) {
    if (!(i %in% colnames(cov))) {
      stop(paste(i, "column not in covariate dataframe. Cannot merge with PKPD dataframe.")) #make sure req.col is in cov
    }
    if (is.na(time.by) & i %in% c("DTIM", "ATFD", "ATLD", "NTFD", "NTLC", "NTLD", "NDAY")) {
      stop(paste("cov dataset cannot include time variable", i, "while time.by is NA.")) #if time.by is NA, make sure no time variables in cov
    }
  }

  for (i in colnames(cov)) {
    if (i %in% colnames(df) & !(i %in% req.cols)) {
      stop(paste(i, "already exists in PKPD dataframe.")) #make sure covariate column is not already in df
    }
  }

  if (is.na(time.by)) {
    if (nrow(cov)>length(unique(unlist(cov[, id.by])))) {
      stop("Cannot merge cov at subject-level. At least one subject has more than one observation.")
    }
  }

  if (TRUE %in% is.na(df[, id.by])) {
    stop(paste(id.by, "is missing for at least one row."))
  }

  if (length(unique(df[, id.by]))>length(unique(df[, id.by]))) {
    warning(paste("At least one subject is included in", df, "but not in", cov))
  }

  if (!is.na(time.by)) {
    check <- cov %>%
      dplyr::mutate(Check = paste0(.data[[id.by]], .data[[time.by]]))

    if (nrow(check)>length(unique(check$Check))) {
      warning(paste("Some covariates may not be filled. Some rows have duplicate", id.by, "and", time.by, "values."))
    }

    if (time.by=="DTIM") {
      if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", cov[!is.na(cov$DTIM),"DTIM"])) {
        if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", cov[!is.na(cov$DTIM),"DTIM"])) {
          if(FALSE %in% grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", cov[!is.na(cov$DTIM),"DTIM"])) {
            stop(paste("DTIM in", cov, "is not ISO 8601 format."))
          }
        }
      }

      cov <- cov %>%
        dplyr::mutate(DTIM = dplyr::case_when(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M:%S"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M:%S"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%dT%H:%M"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d %H:%M"),
                                              grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", DTIM) ~ as.POSIXct(DTIM, tz="UTC", format="%Y-%m-%d")))
    }
  }

  ###Covariate Pre-processing###
  cat.cov.n <- c()
  cat.cov.c <- c()
  cont.cov <- c()

  for (i in 1:length(colnames(cov))) {
    name = colnames(cov)[i]
    if (name %in% c(id.by, time.by)) {next}
    if (name=="SEX" & demo.map==T) {
      if (is.na(time.by)) {
        nname <- paste0("N", name)
      }
      else {
        nname <- paste0("T", name)
      }
      cov[, nname] <- NA
      cov[grepl("m|male", cov$SEX, ignore.case = T), nname] <- 0
      cov[grepl("f|female", cov$SEX, ignore.case = T), nname] <- 1
      cov[grepl("unk", cov$SEX, ignore.case = T), nname] <- 2
      cov[grepl("other", cov$SEX, ignore.case = T), nname] <- 3
      cat.cov.n <- c(cat.cov.n, nname)
      cat.cov.c <- c(cat.cov.c, paste0(nname, "C"))
      colnames(cov)[i] <- paste0(nname, "C")
      if(length(sort(unique(unlist(cov[, nname]))))!=length(sort(unique(unlist(cov[, paste0(nname, "C")]))))) {
        warning(paste0("At least one ", nname, " failed to map. Consider setting demo.map = FALSE."))
      }
    }
    else if (name=="RACE" & demo.map==T) {
      if (is.na(time.by)) {
        nname <- paste0("N", name)
      }
      else {
        nname <- paste0("T", name)
      }
      cov[, nname] <- NA
      cov[grepl("white|caucasian", cov$RACE, ignore.case = T), nname] <- 1
      cov[grepl("black|african|aa", cov$RACE, ignore.case = T), nname] <- 2
      cov[grepl("asian", cov$RACE, ignore.case = T) & !grepl("caucasian", cov$RACE, ignore.case=T), nname] <- 3
      cov[grepl("alaskan|native", cov$RACE, ignore.case = T), nname] <- 4
      cov[grepl("hawa|pacific|island", cov$RACE, ignore.case = T), nname] <- 5
      cov[grepl("multiple|mul", cov$RACE, ignore.case = T), nname] <- 6
      cov[grepl("other", cov$RACE, ignore.case = T), nname] <- 7
      cov[grepl("unknown", cov$RACE, ignore.case = T), nname] <- 8
      cat.cov.n <- c(cat.cov.n, nname)
      cat.cov.c <- c(cat.cov.c, paste0(nname, "C"))
      colnames(cov)[i] <- paste0(nname, "C")
      if(length(sort(unique(unlist(cov[, nname]))))!=length(sort(unique(unlist(cov[, paste0(nname, "C")]))))) {
        warning(paste0("At least one ", nname, " failed to map. Consider setting demo.map = FALSE."))
      }
    }
    else if (name=="ETHNIC" & demo.map==T) {
      if (is.na(time.by)) {
        nname <- paste0("N", name)
      }
      else {
        nname <- paste0("T", name)
      }
      cov[, nname] <- NA
      cov[grepl("not", cov$ETHNIC, ignore.case = T), nname] <- 0
      cov[grepl("his", cov$ETHNIC, ignore.case = T) & !grepl("not", cov$ETHNIC, ignore.case=T), nname] <- 1
      cov[grepl("unk", cov$ETHNIC, ignore.case = T), nname] <- 2
      cov[grepl("other", cov$ETHNIC, ignore.case = T), nname] <- 3
      cat.cov.n <- c(cat.cov.n, nname)
      cat.cov.c <- c(cat.cov.c, paste0(nname, "C"))
      colnames(cov)[i] <- paste0(nname, "C")
      if(length(sort(unique(unlist(cov[, nname]))))!=length(sort(unique(unlist(cov[, paste0(nname, "C")]))))) {
        warning(paste0("At least one ", nname, " failed to map. Consider setting demo.map = FALSE."))
      }
    }
    else if (is.numeric(unlist(cov[,name]))){
      if (nchar(name)>7) {
        stop(paste(name, "column name in cov must be 7 characters or fewer."))
      }
      if (is.na(time.by)) {
        cont.cov <- c(cont.cov, paste0("B", name))
        colnames(cov)[i] <- paste0("B", name)
      }
      else {
        cont.cov <- c(cont.cov, paste0("T", name))
        colnames(cov)[i] <- paste0("T", name)
      }
    }
    else {
      if (nchar(name)>6) {
        stop(paste(name, "column name in cov must be 6 characters or fewer."))
      }
      if (is.na(time.by)) {
        nname <- paste0("N", name)
      }
      else {
        nname <- paste0("T", name)
      }
      cat.cov.c <- c(cat.cov.c, paste0(nname, "C"))
      cat.cov.n <- c(cat.cov.n, paste0(nname))
      cov[, nname] <- match(unlist(cov[,name]), sort(unique(unlist(cov[,name]))))
      if(length(unique(unlist(cov[, nname])))==2) {
        cov[, nname] <- cov[, nname]-1
      }
      colnames(cov)[i] <- paste0(nname, "C")
    }
  }

  covs <- c(cat.cov.n, cat.cov.c, cont.cov)

  ###FILL###
  if (is.na(time.by)) { #subject-level covariate
    df <- df %>%
      dplyr::left_join(cov, by=id.by)
  }

  else {
    cov <- cov %>%
      dplyr::mutate(EVID = 2,
                    DOMAIN = "TVCOV",
                    KEEP = 0,
                    BUILD = Sys.Date())

    if (time.by=="DTIM") {
      if (!"ATFD" %in% colnames(df)) {
        stop(paste0("If merging by DTIM, ATFD must be included in df."))
      }

      df <- df %>%
        dplyr::mutate(KEEP = 1) %>%
        dplyr::bind_rows(cov) %>%
        dplyr::arrange(.data[[id.by]], .data[[time.by]]) %>%
        dplyr::group_by(.data[[id.by]]) %>%
        tidyr::fill(FDOSE, .direction="downup") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ATFD = ifelse(EVID==2, as.numeric(difftime(DTIM, FDOSE, units=sort(unique(df$TIMEU)))), ATFD),
                      ATFD = dplyr::case_when(EVID==2 & ATFD%%1==1 ~ ATFD-1,
                                              EVID==2 & ATFD%%0.1==0.1 ~ ATFD-0.1,
                                              EVID==2 & ATFD%%0.01==0.01 ~ ATFD-0.01,
                                              EVID==2 & ATFD%%0.001==0.001 ~ ATFD-0.001,
                                              EVID==2 ~ ATFD-0.0001,
                                              TRUE ~ ATFD)) %>%
        dplyr::arrange(USUBJID, ATFD) %>%
        dplyr::group_by(USUBJID) %>%
        tidyr::fill(ID, SUBJID, NSTUDY, NSTUDYC, TIMEU, tidyselect::all_of(covs), .direction=direction) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(ID, ATFD, CMT, EVID)
    }

    else {
      df <- df %>%
        dplyr::mutate(KEEP = 1) %>%
        dplyr::bind_rows(cov) %>%
        dplyr::arrange(.data[[id.by]], .data[[time.by]], -EVID) %>% #get in proper order
        dplyr::group_by(.data[[id.by]]) %>% #group
        tidyr::fill(ID, SUBJID, NSTUDY, NSTUDYC, TIMEU, tidyselect::all_of(covs), .direction=direction) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ATFD = dplyr::case_when(KEEP==0 & time.by=="NTFD" ~ NTFD,
                                              KEEP==0 & time.by=="NDAY" & TIMEU=="days" ~ NDAY-1,
                                              KEEP==0 & time.by=="NDAY" & TIMEU=="hours" ~ (24*NDAY-1),
                                              TRUE ~ ATFD)) %>%
        dplyr::arrange(ID, ATFD, CMT, EVID)
    }
  }

  ###Round covariates###
  if(is.numeric(cov.rnd)) {
    for (i in c(cat.cov.n, cont.cov)) {
      cov.num <- round(df[, i], cov.rnd)
      df <- df %>%
        dplyr::select(-tidyselect::all_of(i)) %>%
        dplyr::bind_cols(cov.num)
    }
  }

  ###Fill NA values###
  for (i in c(cat.cov.n, cont.cov)) {
    df[is.na(df[, i]) | df[, i]==-999, i] <- na
  }

  ###Column order###
  df.cat.cov.n <- cov_find(df, cov="categorical", type="numeric")
  df.cat.cov.c <- cov_find(df, cov="categorical", type="character")

  if(!is.null(cat.cov.n)) {
    df <- df %>%
      dplyr::relocate(tidyselect::all_of(cat.cov.n), .after=df.cat.cov.n[length(df.cat.cov.n)])
  }
  if(!is.null(cat.cov.c)) {
    df <- df %>%
      dplyr::relocate(tidyselect::all_of(cat.cov.c), .after=df.cat.cov.c[length(df.cat.cov.c)])
  }
  if(!is.null(cont.cov)) {
    df <- df %>%
      dplyr::relocate(tidyselect::all_of(cont.cov), .before="PDOSEF")
  }

  ###Final filter###
  if (is.na(time.by)) {
  }

  else if(keep.other==F) {
    df <- df %>%
      dplyr::filter(KEEP == 1) %>%
      dplyr::select(-KEEP)
  }

  else if (time.by %in% c("ATLD", "NTLC", "NTLD")) {
    df <- df %>%
      dplyr::filter(KEEP == 1) %>%
      dplyr::select(-KEEP)
  }

  else {
    df <- df %>%
      dplyr::select(-KEEP) %>%
      dplyr::mutate(LINE = dplyr::row_number())
  }

  ###Read out updated dataset###
  return(df)
}





#' Find covariates of particular types
#'
#' Can filter for categorical, continuous, or other covariates
#' Can filter for numeric or character type
#'
#' @param df PK(PD) dataset
#' @param cov covariate distribution
#' @param type covariate type
#'
#' @return vector of desired column names
#' @export
cov_find <- function(df, cov, type) {

  df1 <- df %>%
    dplyr::select(NSTUDY, DOSEA:PDOSEF) %>%
    dplyr::select(-DOSEA, -PDOSEF)

  df2 <- df %>%
    dplyr::select(NSTUDY, TIMEU:DTIM) %>%
    dplyr::select(-TIMEU, -DTIM)

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
      stop("type must be numeric or character")
    }
  }

  else if (cov=="other") {
    if (type=="numeric") {
      covs <- colnames(df1)[!grepl("^N", colnames(df1)) & !grepl("^T", colnames(df1)) & !grepl("^B", colnames(df1))]
      return(covs)
    }
    else if (type=="character") {
      covs <- colnames(df2)[!grepl("^N", colnames(df2)) & !grepl("^T", colnames(df2))]
      return(covs)
    }
    else {
      stop("type must be numeric or character")
    }
  }

  else {
    stop("cov must be categorical,  continuous, or other")
  }
}



#' Create and maintain a dataset version log
#'
#' Version log is outputted as a .docx file
#' Document tracks changes in subject count, record count, new variables, and changing variables
#' User comments in the word document are preserved between versions
#'
#' @param file filepath of new dataset
#' @param orig original dataset flag
#' @param outdir output directory, defaults to dataset directory
#' @param prevdata comparison dataset filepath
#' @param template template .docx filepath
#' @param comp_var grouping variables for comparison
#'
#' @return version log as a .docx file
#' @export
version_log <- function(file, orig = F, outdir = NULL,
                        prevdata = NULL, template,
                        comp_var) {
  data <- utils::read.csv(file, na.strings=".")
  name <- basename(file)
  if(is.null(outdir)) {
    outpath <- paste0(dirname(file), "/")
  }
  else {
    outpath <- outdir
  }
  if(orig == T) {
    VersionLog <- data.frame(
      ROW = c("1"),
      DATASET = c(name),
      NSUB = c(as.character(length(unique(data$USUBJID)))),
      NROW = c(as.character(nrow(data))),
      NEW_VAR = c("Original Dataset"),
      CHG_VAR = c("Original Dataset"),
      REF_ROW = c("-"),
      COMMENTS = c("")
    )
    VersionLog2 <- VersionLog %>%
      flextable::flextable() %>% #creates flextable object
      flextable::border_inner_h(part = "body", #removes inside borders
                                border = officer::fp_border(color = "grey", width = 0.1, style="solid")) %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>% #declare font name
      flextable::fontsize(size = 11, part = "all") %>% #declare font size
      flextable::bold(part = "header") %>% #bold the header
      flextable::width(j = c(1), width = 0.53, unit = "in") %>% #set column width for particular columns
      flextable::width(j = c(2), width = 2.33, unit = "in") %>%
      flextable::width(j = c(3), width = 0.55, unit = "in") %>%
      flextable::width(j = c(4), width = 0.65, unit = "in") %>%
      flextable::width(j = c(5), width = 1.15, unit = "in") %>%
      flextable::width(j = c(6), width = 1.15, unit = "in") %>%
      flextable::width(j = c(7), width = 0.91, unit = "in") %>%
      flextable::width(j = c(8), width = 1.73, unit = "in") %>%
      flextable::height(height = 0.3, unit = "in") %>% #set row height
      flextable::align(j = c(1, 3, 4, 7), align = "center")
    tmplt <- officer::read_docx(path = template) %>% #read in template form
      flextable::body_add_flextable(VersionLog2) %>% #add flextable to the document
      print(target = paste0(outpath, "/VersionLog.docx"))
  }
  else {
    if(orig==F & !is.null(outdir) &
       !(file.exists(paste0(outpath, "/VersionLog.docx")))) {
      stop("There is no preexisting version log in the outpath provided")
    }
    VersionLogdoc <- officer::read_docx(paste0(outpath, "/VersionLog.docx"))
    VersionLogsum <- officer::docx_summary(VersionLogdoc)
    table_cells <- VersionLogsum %>% dplyr::filter(content_type == "table cell")
    table_data <- table_cells %>% dplyr::filter(!is_header) %>% dplyr::select(row_id, cell_id, text)
    # split data into individual columns
    splits <- split(table_data, table_data$cell_id)
    splits <- lapply(splits, function(x) x$text)
    # combine columns back together in wide format
    table_result <- dplyr::bind_cols(splits)
    # get table headers
    cols <- table_cells %>% dplyr::filter(is_header)
    names(table_result) <- cols$text
    VersionLog <- table_result
    comments <- VersionLog %>% dplyr::select(DATASET, COMMENTS)
    for(i in 1:nrow(VersionLog)) {
      if(VersionLog$DATASET[i] == basename(file) & i < nrow(VersionLog)) {
        stop("This file is not the most recent dataset version")
      }
    }
    if(nrow(VersionLog) > 1) {
      VersionLog2 <- VersionLog %>%
        dplyr::filter(DATASET != basename(file)) %>% dplyr::select(-COMMENTS)
    } else {
      VersionLog2 <- VersionLog %>% dplyr::select(-COMMENTS)
    }
    if(is.null(prevdata)) {
      prev_data <- utils::read.csv(gsub(basename(file), VersionLog2$DATASET[nrow(VersionLog2)], file), na.strings = ".")
    } else {
      prev_data <- utils::read.csv(prevdata, na.strings = ".")
    }
    check1 <- prev_data %>% dplyr::group_by_at(comp_var) %>%
      dplyr::mutate(
        COMPROWN = dplyr::row_number()
      ) %>% dplyr::ungroup()
    check2 <- data %>% dplyr::group_by_at(comp_var) %>%
      dplyr::mutate(
        COMPROWN = dplyr::row_number()
      ) %>% dplyr::ungroup()
    if(max(check1$COMPROWN) > 1 | max(check2$COMPROWN) > 1) {
      stop("The compare variables do not provide unique records")
    }
    table <- summary(arsenal::comparedf(prev_data, data, by = comp_var, int.as.num = TRUE))
    new_var <- table$vars.ns.table$variable
    new_var2 <- paste(new_var, collapse=", ")
    changes <- table$diffs.table$var.y
    changes2 <- unique(changes)
    changes3 <- paste(changes2, collapse=", ")
    if(is.null(prevdata)) {
      ref_row <- VersionLog2 %>%
        dplyr::mutate(
          ROW = max(ROW)
        )
    } else {
      ref_row <- VersionLog2 %>%
        dplyr::filter(DATASET == basename(prevdata))
    }
    VersionLog3 <- VersionLog2 %>%
      dplyr::add_row(DATASET = name,
                     NSUB = as.character(length(unique(data$USUBJID))),
                     NROW = as.character(nrow(data)),
                     NEW_VAR = new_var2,
                     CHG_VAR = changes3,
                     REF_ROW = ref_row$ROW) %>%
      dplyr::mutate(
        ROW = as.character(dplyr::row_number())
      ) %>%
      dplyr::left_join(comments, by = "DATASET") %>%
      dplyr::filter(!duplicated(DATASET))
    VersionLog4 <- VersionLog3 %>%
      flextable::flextable() %>% #creates flextable object
      flextable::border_inner_h(part = "body", #removes inside borders
                                border = officer::fp_border(color = "grey", width = 0.1, style="solid")) %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>% #declare font name
      flextable::fontsize(size = 11, part = "all") %>% #declare font size
      flextable::bold(part = "header") %>% #bold the header
      flextable::width(j = c(1), width = 0.53, unit = "in") %>% #set column width for particular columns
      flextable::width(j = c(2), width = 2.33, unit = "in") %>%
      flextable::width(j = c(3), width = 0.55, unit = "in") %>%
      flextable::width(j = c(4), width = 0.65, unit = "in") %>%
      flextable::width(j = c(5), width = 1.15, unit = "in") %>%
      flextable::width(j = c(6), width = 1.15, unit = "in") %>%
      flextable::width(j = c(7), width = 0.91, unit = "in") %>%
      flextable::width(j = c(8), width = 1.73, unit = "in") %>%
      flextable::height(height = 0.3, unit = "in") %>% #set row height
      flextable::align(j = c(1, 3, 4, 7), align = "center")
    tmplt <- officer::read_docx(path = template) %>% #read in template form
      flextable::body_add_flextable(VersionLog4) %>% #add flextable to the document
      print(target = paste0(outpath, "/VersionLog.docx"))
  }
}


#' Export a standard variable list to support the pk_define() function.
#'
#' Variable list outputted as a .csv file
#' The user should add additional definitions to the file for custom covariates.
#'
#' @param file desired filepath for the variable list
#'
#' @return standard variable list as a .csv file
#' @export
variable_list_export <- function(file) {
  dir <- this.path::dirname2(file) #directory of the dataset

  if (dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  name <- this.path::basename2(file) #dataset name including extension

  if (!grepl(".csv$", name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  VL <- as.data.frame(VL)
  utils::write.csv(VL, file, row.names = F, na="")
}
