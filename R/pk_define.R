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
#'
#' @examplesIf exists("df_path") & exists("vl_path")
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
#' ## Write with pk_write()
#' df_path ##User designated filepath "C:/.../dataset.csv"
#' if (file.exists(df_path)) {
#'   pk_write(df, df_path)
#' }
#'
#' ## Create definition file with pk_define()
#' vl_path ##User designated variable list filepath "C:/.../variablelist.csv"
#' if (file.exists(vl_path)) {
#'   pk_define(df_path, "Project Name", vl_path)
#' }
#'
#' @export
pk_define <- function(file, project, variable.list, template=NULL,
                      font="Times New Roman", size=9, na = -999) {
  CMT <- DVID <- EVID <- Variable <- Description <- Units <- Numeric <- NULL
  Character <- Values <- Format <- Comment <- Categorization <- NULL

  data.dir <- this.path::dirname2(file) #directory of the dataset

  if (data.dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  data.name <- this.path::basename2(file) #dataset name including extension

  if (!grepl(".csv$", data.name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  if(!is.null(template)) {
    temp.dir <- this.path::dirname2(template) #directory of the dataset

    if (temp.dir==".") {
      stop(paste(file, "is not a valid filepath."))
    }

    temp.name <- this.path::basename2(template) #dataset name including extension

    if (!grepl(".docx$", temp.name)) {
      stop(paste("filepath must include document name and .docx suffix."))
    }
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
                        col.names = c("Variable", "Categorization", "Description", "Comment"))

  define <- data.frame("Variable" = colnames(df))

  cmto <- df[df$EVID==0, c("CMT", "DOMAIN", "DVIDC", "DVIDU")]
  cmto <- dplyr::distinct(cmto)
  cmto <- dplyr::arrange(cmto, CMT)
  cmto <- dplyr::mutate(cmto,
                        DOMAIN = dplyr::case_when(DOMAIN=="EX" ~ "(Dose)",
                                                  DOMAIN=="PC" ~ "(PK)",
                                                  DOMAIN=="PD" ~ "(PD)",
                                                  DOMAIN=="ADA" ~ "(ADA)"))

  cmtd <- df[df$EVID==1, c("CMT", "DOMAIN", "DVIDC", "DVIDU")]
  cmtd <- dplyr::distinct(cmtd)
  cmtd <- dplyr::arrange(cmtd, CMT)
  cmtd <- dplyr::mutate(cmtd,
                        DOMAIN = dplyr::case_when(DOMAIN=="EX" ~ "(Dose)",
                                                  DOMAIN=="PC" ~ "(PK)",
                                                  DOMAIN=="PD" ~ "(PD)",
                                                  DOMAIN=="ADA" ~ "(ADA)"))

  cmt <- data.frame("Variable" = "CMT",
                    "Values" = c(paste(cmtd$CMT, "=", cmtd$DVIDC, cmtd$DOMAIN), paste(cmto$CMT, "=", cmto$DVIDC, cmto$DOMAIN)))

  dvid <- df[df$EVID==0, c("DVID", "DVIDC", "DVIDU")]
  dvid <- dplyr::distinct(dvid)
  dvid <- dplyr::arrange(dvid, DVID)

  dvid1 <- data.frame("Variable" = "DVID",
                      "Values" = paste(dvid$DVID, "=", dvid$DVIDC))
  dvid1 <- dplyr::distinct(dvid1)

  exdosu <- unique(unlist(df[df$EVID==1, "DVIDU"]))

  cat.cov.c <- cov_find(df, cov="categorical", type="character")
  cat.cov.c <- substr(cat.cov.c, 2, nchar(cat.cov.c)-1)
  vl.cat.cov.c <- data.frame("Variable" = cat.cov.c)
  vl.cat.cov.c <- dplyr::left_join(vl.cat.cov.c, vl, by="Variable")
  vl.cat.cov.c <- dplyr::mutate(vl.cat.cov.c,
                                Variable = cov_find(df, cov="categorical", type="character"),
                                Description = dplyr::case_when(grepl("^N", Variable) ~ paste("Subject", Description, "label"),
                                                               grepl("^T", Variable) ~ paste("Time-varying subject", Description, "label"),
                                                               TRUE ~ Description))

  cat.cov.n <- cov_find(df, cov="categorical", type="numeric")
  cat.cov.n <- substr(cat.cov.n, 2, nchar(cat.cov.n))
  vl.cat.cov.n <- data.frame("Variable" = cat.cov.n)
  vl.cat.cov.n <- dplyr::left_join(vl.cat.cov.n, vl, by="Variable")
  vl.cat.cov.n <- dplyr::mutate(vl.cat.cov.n,
                                Variable = cov_find(df, cov="categorical", type="numeric"),
                                Description = dplyr::case_when(grepl("^N", Variable) ~ paste("Subject", Description),
                                                               grepl("^T", Variable) ~ paste("Time-varying subject", Description),
                                                               TRUE ~ Description))

  cont.cov <- cov_find(df, cov="continuous", type="numeric")
  cont.cov <- substr(cont.cov, 2, nchar(cont.cov))
  cont.cov.u <- cov_find(df, cov="units", type="character")

  if (length(cont.cov) > 0) {
    unts <- dplyr::filter(df, EVID!=2)
    unts <- dplyr::select(unts, tidyselect::all_of(cont.cov.u))
    unts <- dplyr::filter(unts, dplyr::row_number()==1)
    unts <- tidyr::pivot_longer(unts,
                                cols = tidyselect::all_of(cont.cov.u),
                                values_to = "Units",
                                names_to = "Variable")
    unts <- dplyr::mutate(unts, Variable = gsub("U$", "", Variable))

    vl.cont.cov <- data.frame("Variable" = cont.cov)
    vl.cont.cov <- dplyr::left_join(vl.cont.cov, vl, by="Variable")
    vl.cont.cov <- dplyr::mutate(vl.cont.cov,
                                 Variable = cov_find(df, cov="continuous", type="numeric"),
                                 Description = dplyr::case_when(grepl("^B", Variable) ~ paste("Baseline", Description),
                                                                grepl("^T", Variable) ~ paste("Time-varying", Description),
                                                                TRUE ~ Description))
    vl.cont.cov <- dplyr::left_join(vl.cont.cov, unts, by="Variable")

    vl.cont.cov.u <- data.frame("Variable" = cont.cov)
    vl.cont.cov.u <- dplyr::left_join(vl.cont.cov.u, vl, by="Variable")
    vl.cont.cov.u <- dplyr::mutate(vl.cont.cov.u,
                                   Variable = cov_find(df, cov="units", type="character"),
                                   Description = dplyr::case_when(grepl("^B", Variable) ~ paste("Baseline", Description),
                                                                  grepl("^T", Variable) ~ paste("Time-varying", Description),
                                                                  TRUE ~ Description))
    vl.cont.cov.u <- dplyr::mutate(vl.cont.cov.u, Description = paste(Description, "units"))
  }

  vl <- dplyr::bind_rows(vl, vl.cat.cov.c, vl.cat.cov.n)

  if (length(cont.cov) > 0) {
    vl <- dplyr::bind_rows(vl, vl.cont.cov, vl.cont.cov.u)
    vl <- dplyr::relocate(vl, Units, .before="Comment")
  }

  else {
    vl <- dplyr::mutate(vl, Units = "")
    vl <- dplyr::relocate(vl, Units, .before="Comment")
  }

  cov <- data.frame("Variable" = NA,
                    "Values" = NA)
  for (i in vl.cat.cov.c$Variable) {
    df1 <- dplyr::select(df, tidyselect::all_of(i), tidyselect::all_of(gsub("C$", "", i)))
    df1 <- dplyr::distinct(df1)

    df1$Variable <- colnames(df1)[2]
    colnames(df1)[1:2] <- c("Character", "Numeric")

    df1 <- dplyr::arrange(df1, Numeric)
    df1 <- dplyr::filter(df1, Numeric!=-999)
    df1 <- dplyr::mutate(df1, Values = paste(Numeric, "=", Character))
    df1 <- dplyr::select(df1, Variable, Values)

    cov <- dplyr::bind_rows(cov, df1)
  }
  cov <- cov[-1,]

  evid <- dplyr::distinct(df, EVID)
  evid <- dplyr::mutate(evid,
                        Variable = "EVID",
                        Values = dplyr::case_when(EVID==0 ~ "0 = Observation event",
                                                  EVID==1 ~ "1 = Dose event",
                                                  EVID==2 ~ "2 = Other event",
                                                  EVID==3 ~ "3 = Reset event",
                                                  EVID==4 ~ "4 = Reset and Dose event"))
  evid <- dplyr::arrange(evid, Variable, Values)
  evid <- dplyr::select(evid, Variable, Values)

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
                         "Values" = c(paste0("0 = At least one observation (DVID = ", cmtn, ")"), paste0("1 = No observations (DVID = ", cmtn, ")")))
    nodvf <- dplyr::bind_rows(nodvf, nodvf1)
  }
  nodvf <- nodvf[-1, ]

  flgs <- dplyr::bind_rows(c, pdosef, timef, amtf, dupf, noexf, plbof, sparsef, trexf, sdf, impex, impdv, nodvf)

  #Combine all values dataframes together
  values <- dplyr::bind_rows(cmt, dvid1, cov, evid, mdv, blq, ss, flgs)

  #Finalize definition dataset
  define <- dplyr::left_join(define, vl, by="Variable") #join variable list
  define <- dplyr::group_by(define, Variable)
  define <- dplyr::mutate(define,
                          Format = paste0(toupper(substr(typeof(df[, Variable]), 1, 1)), substr(typeof(df[, Variable]), 2,  nchar(typeof(df[, Variable]))))) #Determine type of each column
  define <- dplyr::ungroup(define)
  define <- dplyr::mutate(define,
                          Format = dplyr::case_when(Variable %in% c("C", "DTIM", "FDOSE") ~ "Character", #Final formatting for this column
                                                    Format %in% c("Integer", "Double", "Logical") ~ "Numeric",
                                                    TRUE ~ Format),
                          Comment = ifelse(Format=="Character" & Variable!="C", "Dropped in control stream" ,Comment), #Add comment for characters
                          Units = ifelse(Variable=="CMT",  NA, #Empty the units for CMT
                                         dplyr::case_when(Variable %in% c("ATFD", "ATLD", "NTFD", "NTLC", "NTLD", "TPT", "DUR") ~ unique(df$TIMEU), #Add time units to these variables
                                                          Variable %in% c("AMT", "DOSEA") ~ exdosu, #Add dose units
                                                          Variable=="RATE" ~ paste0(exdosu, "/", gsub("s$", "", unique(df$TIMEU))),
                                                          Variable=="NDAY" ~ "days",
                                                          TRUE ~ Units)))
  define <- dplyr::left_join(define, values, by="Variable") #join values
  define <- dplyr::mutate(define,
                          Units = dplyr::case_when(is.na(Units) ~ "",
                                                   TRUE ~ Units),
                          Values = dplyr::case_when(is.na(Values) ~ "",
                                                    TRUE ~ Values))
  define <- dplyr::select(define, Variable, Categorization, Description, Values, Units, Format, Comment)

  define$Units[define$Variable=="CMT"] <- c(cmtd$DVIDU, cmto$DVIDU) #Add CMT units

  flextable::set_flextable_defaults(
    font.size = size,
    font.family = font)

  define <- flextable::flextable(define)
  define <- flextable::border_inner_h(define,
                                      part = "body",
                                      border = officer::fp_border(color = "grey", width = 0.1, style="solid"))
  define <- flextable::merge_v(define,
                               j = c(1, 2, 3, 6, 7), target = c(1, 2, 3, 6, 7), part = "body", combine = T)
  define <- flextable::bold(define, part = "header")
  define <- flextable::height(define, height = 0.3, unit = "in")

  widths <- flextable::dim_pretty(define)
  if (sum(unlist(widths[1])) <= 9) {
    define <- flextable::autofit(define)
  }

  else {
    define <- flextable::width(define,
                               j = c(3, 4), width = 1.9, unit = "in")
    define <- flextable::width(define,
                               j = 7, width = 1.5, unit = "in")
    define <- flextable::width(define,
                               j = c(1, 5, 6), width = 0.9, unit = "in")
    define <- flextable::width(define,
                               j = 2, width = 1, unit = "in")
  }

  define <- flextable::add_footer_lines(define,
                                        values = 'NA parameters and missing character-type covariates labeled with "."')
  define <- flextable::add_footer_lines(define,
                                        values = paste("Missing numeric-type covariates labeled with", na))

  ###WRITE DEFINITION FILE TO SERVER###
  if (is.null(template)) {
    tmplt <- officer::read_docx()
    tmplt <- flextable::body_add_flextable(tmplt, define)
    tmplt <- officer::body_end_section_landscape(tmplt)

    print(tmplt, target = paste0(data.dir, "\\DEFINE_", gsub(".csv", "", data.name), ".docx"))
  }

  else {
    tmplt <- officer::read_docx(path=template)
    tmplt <- flextable::body_add_flextable(tmplt, define)
    tmplt <- officer::headers_replace_all_text(tmplt,
                                               "Project", project, warn=F)
    tmplt <- officer::headers_replace_all_text(tmplt,
                                               "Dataset", paste("Analysis Dataset:", data.name), warn=F)
    print(tmplt, target = paste0(data.dir, "\\DEFINE_", gsub(".csv", "", data.name), ".docx"))
  }
}
