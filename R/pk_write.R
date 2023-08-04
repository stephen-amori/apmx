#' Write PK(PD) dataset to specified location
#'
#' Dataset created by pk_build() or pk_combine() will be outputted as a .csv file with NONMEM-standard formatting.
#'
#' @param df PK(PD) dataframe
#' @param file filepath
#'
#' @return NA
#'
#' @examplesIf exists("df_path")
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
#' if(file.exists(df_path)) {
#'   pk_write(df, df_path)
#' }
#'
#' @export
pk_write <- function(df, file) {

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
