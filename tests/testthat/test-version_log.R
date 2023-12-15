# LIBRARIES FOR DEBUGGING PURPOSES:
# library(testthat)
# library(apmx)
library(tidyr)
library(tibble)
library(dplyr)

#################### START: DUMMY DATA ####################

current_dir <- getwd()
load(paste0(current_dir, "/data/EX.rda"))
load(paste0(current_dir, "/data/PC.rda"))
load(paste0(current_dir, "/data/DM.rda"))
load(paste0(current_dir, "/data/LB.rda"))

ex <- EX %>%
  dplyr::mutate(TPTC = "Dose event",
                TPT = 0,
                NDAY = ifelse(VISIT=="Baseline (D1)", 1, 15),
                FRQ = "Q2W",
                CMT = 1) %>%
  dplyr::select(USUBJID, STUDYID, EXSTDTC, VISIT, NDAY, TPT, EXDOSE,
                CMT, EXTRT, TPTC, EXROUTE, FRQ, EXDOSU)




suppressWarnings({
pc <- PC %>%
  dplyr::filter(PCSTAT=="Y") %>%
  dplyr::mutate(CMT = 2,
                PCSTRESN = as.numeric(PCSTRESN),
                NDAY = ifelse(VISIT=="Baseline (D1)", 1, 15),
                TPT = dplyr::case_when(PCTPT=="<1 hour Pre-dose" ~ 0,
                                       PCTPT=="30 minutes post-dose" ~ 0.5,
                                       PCTPT=="1 hour post-dose" ~ 1,
                                       PCTPT=="2 hours post-dose" ~ 2,
                                       PCTPT=="4 hours post-dose" ~ 4,
                                       PCTPT=="6 hours post-dose" ~ 6,
                                       PCTPT=="8 hours post-dose" ~ 8,
                                       PCTPT=="12 hours post-dose" ~ 12,
                                       PCTPT=="24 hours post-dose" ~ 24,
                                       PCTPT=="48 hours post-dose" ~ 48),
                TPT = TPT/24) %>%
  dplyr::select(USUBJID, PCDTC, NDAY, VISIT, TPT, PCSTRESN,
                PCLLOQ, CMT, PCTEST, PCTPT, PCSTRESU)
})

dm <- DM %>%
  dplyr::select(USUBJID, AGE, SEX, RACE, ETHNIC)

# Adding units to DM.
dm <- dm %>% mutate(AGEU = "Years")


pkdf <- pk_build(ex = ex,
                 pc = pc,
                 sl.cov = dm,
                 cycle.length = 14,
                 time.rnd = 3)
# d <- read.csv(paste0(getwd(), "/tests/testthat/test-pk-define-files/pkdf.csv"))
# pk_write(pkdf, (paste0(getwd(), "/tests/testthat/version_log-data/pkdf.csv")))
# pk_write(pkdf, (paste0(getwd(), "/tests/testthat/version_log-data/pkdf2.csv")))

# pk_write(pkdf, (paste0(getwd(), "/tests/testthat/version_log-data/pkdf3.csv")))

# pk_write(pkdf, (paste0(getwd(), "/tests/testthat/version_log-data/pkdf4.csv")))

# pk_write(d, (paste0(getwd(), "/tests/testthat/test-pk-define-files/pkdf3.csv")))

# pk_write(d, (paste0(getwd(), "/tests/testthat/test-pk-define-files/pkdf4.csv")))

################### END: DUMMY DATA ####################

test_that("QC Checks", {
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    # source("R/version_log.R")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

    #dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    # dir <- paste0(dir, "/tests/testthat/version_log-data/pkdf")
    # out <- paste0(getwd(), "/tests/testthat/version_log-data/")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
    #dir <- paste0(dir, "/version_log-data/pkdf")
    #out <- paste0(getwd(), "/version_log-data")
    expect_error(version_log(df = pkdf, name = "dataset"),
                 "name parameter must end with .csv suffix to refer to a dataset file")

    expect_error(version_log(df = pkdf, name = "dataset.csv", file = "notarealpath"),
                 "directory in file parameter does not exist")

    version_log(df = pkdf, name = "dataset.csv",
                file = "version_log-data/versionlog.docx")

    expect_true(file.exists("version_log-data/versionlog.docx"))

    pk_write(pkdf, "version_log-data/dataset.csv")

    pkdf2 <- pkdf
    pkdf2$NEWCOLUMN <- 2*1:nrow(pkdf2)

    expect_error(version_log(df = pkdf2, name = "dataset2.csv",
                             file = "notarealpath",
                             prevdata = "version_log-data/dataset.csv", src_data = "same as dataset.csv",
                             comp_var = c("USUBJID", "ATFD", "EVID", "DVID")),
                 "directory in file parameter does not exist")

    expect_error(version_log(df = pkdf2, name = "dataset2.csv",
                             file = "version_log-data/versionlog.docx",
                             src_data = "same as dataset.csv",
                             comp_var = c("USUBJID", "ATFD", "EVID", "DVID")),
                 "Version log already exists. Please include filepath to previous dataset in prevdata argument for comparison.")

    expect_error(version_log(df = pkdf2, name = "dataset2.csv",
                             file = "version_log-data/versionlog.docx",
                             prevdata = "version_log-data/dataset.csv", src_data = "same as dataset.csv",
                             comp_var = c("USUBJID")),
                 "The compare variables do not provide unique records")
    unlink("version_log-data/versionlog.docx")
    unlink("version_log-data/dataset.csv")
})
