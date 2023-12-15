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
#'
#' @examples
#' ## Simple ex domain with 1 subject and 1 dose, study 101
#' ex101 <- data.frame(STUDYID = "ABC101",
#'                     USUBJID = "ABC101-001",
#'                     EXSTDTC = "2000-01-01 10:00:00",
#'                     EXSTDY = 1,
#'                     EXTPTNUM = 0,
#'                     EXDOSE = 100,
#'                     CMT = 1,
#'                     EXTRT = "ABC",
#'                     EXDOSU = "mg",
#'                     VISIT = "Day 1",
#'                     EXTPT = "Dose",
#'                     EXDOSFRQ = "Once",
#'                     EXROUTE = "Oral")
#'
#' ## Simple ex domain with 1 subject and 1 dose, study 102
#' ex102 <- data.frame(STUDYID = "ABC102",
#'                     USUBJID = "ABC102-001",
#'                     EXSTDTC = "2001-01-01 08:09:00",
#'                     EXSTDY = 1,
#'                     EXTPTNUM = 0,
#'                     EXDOSE = 200,
#'                     CMT = 1,
#'                     EXTRT = "ABC",
#'                     EXDOSU = "mg",
#'                     VISIT = "Day 1",
#'                     EXTPT = "Dose",
#'                     EXDOSFRQ = "QW",
#'                     EXROUTE = "Oral")
#'
#' ## Simple pc domain with 1 subject and 3 observations, study 101
#' pc101 <- data.frame(USUBJID = "ABC101-001",
#'                     PCDTC = c("2000-01-01 09:40:00",
#'                               "2000-01-01 10:29:00",
#'                               "2000-01-01 12:05:00"),
#'                     PCDY = 1,
#'                     PCTPTNUM = c(0, ##Units of hours
#'                                  0.021,
#'                                  0.083),
#'                     PCSTRESN = c(NA,
#'                                  469,
#'                                  870),
#'                     PCLLOQ = 25,
#'                     CMT = 2,
#'                     VISIT = "Day 1",
#'                     PCTPT = c("Pre-dose",
#'                               "30-min post-dose",
#'                               "2-hr post-dose"),
#'                     PCTEST = "ABC",
#'                     PCSTRESU = "ug/mL")
#'
#' ## Simple pc domain with 1 subject and 3 observations, study 102
#' pc102 <- data.frame(USUBJID = "ABC102-001",
#'                     PCDTC = c("2001-01-01 08:05:00",
#'                               "2001-01-01 11:38:00",
#'                               "2001-01-02 08:11:00"),
#'                     PCDY = 1,
#'                     PCTPTNUM = c(0, ##Units of hours
#'                                  0.125,
#'                                  1),
#'                     PCSTRESN = c(NA,
#'                                  1150,
#'                                  591),
#'                     PCLLOQ = 25,
#'                     CMT = 2,
#'                     VISIT = "Day 1",
#'                     PCTPT = c("Pre-dose",
#'                               "2-4 hr post-dose",
#'                               "24 hr post-dose"),
#'                     PCTEST = "ABC",
#'                     PCSTRESU = "ug/mL")
#'
#' ## Create with pk_build()
#' df101 <- pk_build(ex101, pc101)
#' df102 <- pk_build(ex102, pc102)
#'
#' ## Combine with pk_combine()
#' df_combine <- pk_combine(df101, df102)
#'
#' @export
pk_combine <- function(df1, df2, demo.map=TRUE, na=-999) {
  EVID <- DVID <- DVIDC <- DVIDU <- USUBJID <- NULL
  ATFD <- CMT <- NSTUDY <- NSTUDYC <- NULL

  ###QC dataframes###
  if (FALSE %in% (colnames(df1) %in% colnames(df2)) | FALSE %in% (colnames(df2) %in% colnames(df1))) {
    warning("Column names do not match between both datasets")
  }

  if (TRUE %in% (unique(df1$USUBJID) %in% unique(df2$USUBJID)) | TRUE %in% (unique(df2$USUBJID) %in% unique(df1$USUBJID))) {
    stop("At least one USUBJID exists in both datasets. Please ensure all USUBJID values are unique.")
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
    if(!is.numeric(na)) {
      stop("na parameter must be numeric")
    }
  }

  dvid1 <- dplyr::filter(df1, EVID==0)
  dvid1 <- dplyr::distinct(dvid1, DVID, DVIDC, DVIDU)
  dvid1 <- dplyr::arrange(dvid1, DVIDC)
  dvid1 <- dplyr::select(dvid1, DVID, DVIDC1 = DVIDC, DVIDU1 = DVIDU)

  dvid2 <- dplyr::filter(df2, EVID==0)
  dvid2 <- dplyr::distinct(dvid2, DVID, DVIDC, DVIDU)
  dvid2 <- dplyr::arrange(dvid2, DVIDC)
  dvid2 <- dplyr::select(dvid2, DVID, DVIDC2 = DVIDC, DVIDU2 = DVIDU)

  dvid <- dplyr::full_join(dvid1, dvid2, by=c("DVID"))
  dvid <- dplyr::mutate(dvid,
                        DVIDCDIFF = dplyr::case_when(is.na(DVIDC1) ~ "S",
                                                     is.na(DVIDC2) ~ "S",
                                                     DVIDC1!=DVIDC2 ~ "Y",
                                                     TRUE ~ "N"),
                        DVIDUDIFF = dplyr::case_when(is.na(DVIDU1) ~ "S",
                                                     is.na(DVIDU2) ~ "S",
                                                     DVIDU1!=DVIDU2 ~ "Y",
                                                     TRUE ~ "N"))

  if ("Y" %in% dvid$DVIDCDIFF) {
    stop("DVID and DVIDC observation assignments are not the same bewteen both datasets")
  }
  if ("Y" %in% dvid$DVIDUDIFF) {
    stop("DVID and DVIDU observation assignments are not the same between both datasets")
  }

  if ("S" %in% dvid$DVIDCDIFF | "S" %in% dvid$DVIDUDIFF) {
    warning("Datasets have different number of DVIDs.")
  }

  df1_doseu <- unique(df1$DVIDU[df1$EVID==1])
  df2_doseu <- unique(df2$DVIDU[df2$EVID==1])

  if (FALSE %in% df1_doseu==df2_doseu) {
    stop("Datasets have different dose units")
  }

  df1_doseu <- unique(df1$DVIDC[df1$EVID==1])
  df2_doseu <- unique(df2$DVIDC[df2$EVID==1])

  if (FALSE %in% df1_doseu==df2_doseu) {
    stop("Datasets have different dose labels")
  }

  for (i in unique(c(df1$CMT[df1$EVID!=2], df2$CMT[df2$EVID!=2]))) {
    if (!i %in% df1$CMT) {
      warning(paste("CMT =", i, "not included df1"))
    }

    if (!i %in% df2$CMT) {
      warning(paste("CMT =", i, "not included in df2"))
    }

    if (i %in% df1$CMT & i %in% df2$CMT) {
      dvid1 <- sort(unique(df1$DVID[df1$CMT==i]))
      dvid2 <- sort(unique(df2$DVID[df2$CMT==i]))
      dvid <- dvid1==dvid2
      if (FALSE %in% dvid) {
        stop("DVID and CMT assignments are not the same between both datasets")
      }
    }
  }

  for (i in cov_find(df1, cov="units", type="character")) {
    if (i %in% colnames(df2)) {
      if(unique(unlist(df1[df1$EVID!=2,i]))!=unique(unlist(df2[df2$EVID!=2,i]))) {
        warning(paste(i, "units are not the same between both datasets"))
      }
    }
  }

  ###Combine datasets###
  df <- dplyr::bind_rows(df1, df2)
  df <- dplyr::arrange(df, USUBJID, ATFD, CMT, EVID)

  df <- dplyr::mutate(df,
                      ID = match(USUBJID, unique(df$USUBJID)),
                      LINE = dplyr::row_number(),
                      COMBD = Sys.Date())

  ###Rearrange covariate columns###
  cat.cov.n.1 <- cov_find(df1, cov="categorical", type="numeric")
  cont.cov.n.1 <- cov_find(df1, cov="continuous", type="numeric")
  exp.cov.1 <- cov_find(df1, cov="exposure", type="numeric")
  ebe.cov.1 <- cov_find(df1, cov="empirical bayes estimate", type="numeric")
  oth.cov.n.1 <- cov_find(df1, cov="other", type="numeric")

  cat.cov.c.1 <- cov_find(df1, cov="categorical", type="character")
  oth.cov.c.1 <- cov_find(df1, cov="other", type="character")
  cont.cov.c.1 <- cov_find(df1, cov="units", type="character")

  cat.cov.n.2 <- cov_find(df2, cov="categorical", type="numeric")
  cont.cov.n.2 <- cov_find(df2, cov="continuous", type="numeric")
  exp.cov.2 <- cov_find(df2, cov="exposure", type="numeric")
  ebe.cov.2 <- cov_find(df2, cov="empirical bayes estimate", type="numeric")
  oth.cov.n.2 <- cov_find(df2, cov="other", type="numeric")

  cat.cov.c.2 <- cov_find(df2, cov="categorical", type="character")
  oth.cov.c.2 <- cov_find(df2, cov="other", type="character")
  cont.cov.c.2 <- cov_find(df2, cov="units", type="character")

  num.cov <- c(cat.cov.n.1, cat.cov.n.2, cont.cov.n.1, cont.cov.n.2, oth.cov.n.1, oth.cov.n.2, exp.cov.1, exp.cov.2, ebe.cov.1, ebe.cov.2)[!duplicated(c(cat.cov.n.1, cat.cov.n.2, cont.cov.n.1, cont.cov.n.2, oth.cov.n.1, oth.cov.n.2, exp.cov.1, exp.cov.2, ebe.cov.1, ebe.cov.2))]
  chr.cov <- c(cat.cov.c.1, cat.cov.c.2, oth.cov.c.1, oth.cov.c.2, cont.cov.c.1, cont.cov.c.2)[!duplicated(c(cat.cov.c.1, cat.cov.c.2, oth.cov.c.1, oth.cov.c.2, cont.cov.c.1, cont.cov.c.2))]

  df <- dplyr::relocate(df, tidyselect::all_of(num.cov), .after="DOSEA")
  df <- dplyr::relocate(df, tidyselect::all_of(chr.cov), .after="TIMEU")
  df <- dplyr::relocate(df, NSTUDY, .after="C")
  df <- dplyr::relocate(df, NSTUDYC, .after="USUBJID")

  ###Redo categorical covariates###
  for (i in chr.cov) {
    name <- gsub("C$", "", i)
    if (i=="NSEXC") {
      df$NSEX[grepl("m|male", df$NSEXC, ignore.case = TRUE)] <- 0
      df$NSEX[grepl("f|female", df$NSEXC, ignore.case = TRUE)] <- 1
      df$NSEX[grepl("unk", df$NSEXC, ignore.case = TRUE)] <- 2
      df$NSEX[grepl("other", df$NSEXC, ignore.case=TRUE)] <- 3
      if(length(sort(unique(df$NSEX)))!=length(sort(unique(df$NSEXC)))) {
        warning("At least one NSEX failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TSEXC") {
      df$TSEX[grepl("m|male", df$TSEXC, ignore.case = TRUE)] <- 0
      df$TSEX[grepl("f|female", df$TSEXC, ignore.case = TRUE)] <- 1
      df$TSEX[grepl("unk", df$TSEXC, ignore.case = TRUE)] <- 2
      df$TSEX[grepl("other", df$TSEXC, ignore.case = TRUE)] <- 3
      if(length(sort(unique(df$TSEX)))!=length(sort(unique(df$TSEXC)))) {
        warning("At least one TSEX failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="NRACEC") {
      df$NRACE[grepl("white|caucasian", df$NRACEC, ignore.case = TRUE)] <- 1
      df$NRACE[grepl("black|african|aa", df$NRACEC, ignore.case = TRUE)] <- 2
      df$NRACE[grepl("asian", df$NRACEC, ignore.case = TRUE) & !grepl("caucasian", df$NRACEC, ignore.case=TRUE)] <- 3
      df$NRACE[grepl("alaskan|native", df$NRACEC, ignore.case = TRUE)] <- 4
      df$NRACE[grepl("hawa|pacific|island", df$NRACEC, ignore.case = TRUE)] <- 5
      df$NRACE[grepl("multiple|mul", df$NRACEC, ignore.case = TRUE)] <- 6
      df$NRACE[grepl("other", df$NRACEC, ignore.case = TRUE)] <- 7
      df$NRACE[grepl("unknown", df$NRACEC, ignore.case = TRUE)] <- 8
      if(length(sort(unique(df$NRACE)))!=length(sort(unique(df$NRACEC)))) {
        warning("At least one NRACE failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TRACEC") {
      df$TRACE[grepl("white|caucasian", df$TRACEC, ignore.case = TRUE)] <- 1
      df$TRACE[grepl("black|african|aa", df$TRACEC, ignore.case = TRUE)] <- 2
      df$TRACE[grepl("asian", df$TRACEC, ignore.case = TRUE) & !grepl("caucasian", df$TRACEC, ignore.case=TRUE)] <- 3
      df$TRACE[grepl("alaskan|native", df$TRACEC, ignore.case = TRUE)] <- 4
      df$TRACE[grepl("hawa|pacific|island", df$TRACEC, ignore.case = TRUE)] <- 5
      df$TRACE[grepl("multiple|mul", df$TRACEC, ignore.case = TRUE)] <- 6
      df$TRACE[grepl("other", df$TRACEC, ignore.case = TRUE)] <- 7
      df$TRACE[grepl("unknown", df$TRACEC, ignore.case = TRUE)] <- 8
      if(length(sort(unique(df$TRACE)))!=length(sort(unique(df$TRACEC)))) {
        warning("At least one TRACE failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="NETHNICC") {
      df$NETHNIC[grepl("not", df$NETHNICC, ignore.case = TRUE)] <- 0
      df$NETHNIC[grepl("his", df$NETHNICC, ignore.case = TRUE) & !grepl("not", df$NETHNICC, ignore.case=TRUE)] <- 1
      df$NETHNIC[grepl("unk", df$NETHNICC, ignore.case = TRUE)] <- 2
      df$NETHNIC[grepl("other", df$NETHNICC, ignore.case = TRUE)] <- 3
      if(length(sort(unique(df$NETHNIC)))!=length(sort(unique(df$NETHNICC)))) {
        warning("At least one NETHNIC failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (i=="TETHNICC") {
      df$TETHNIC[grepl("not", df$TETHNICC, ignore.case = TRUE)] <- 0
      df$TETHNIC[grepl("his", df$TETHNICC, ignore.case = TRUE) & !grepl("not", df$TETHNICC, ignore.case=TRUE)] <- 1
      df$TETHNIC[grepl("unk", df$TETHNICC, ignore.case = TRUE)] <- 2
      df$TETHNIC[grepl("other", df$TETHNICC, ignore.case = TRUE)] <- 3
      if(length(sort(unique(df$TETHNIC)))!=length(sort(unique(df$TETHNICC)))) {
        warning("At least one TETHNIC failed to map. Consider setting demo.map = FALSE.")
      }
    }
    else if (substr(i, nchar(i), nchar(i))=="U" & gsub("U$", "", i) %in% num.cov) {
      next
    }
    else if (!i %in% c("NSEXC", "TSEXC", "NRACEC", "TRACEC", "NETHNICC", "TETHNICC")) {
      df[,name] <- match(unlist(df[, i]), sort(unique(unlist(df[, i]))))
      if(length(sort(unique(unlist(df[, name]))))==2) {
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
