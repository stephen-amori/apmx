library(testthat)
library(tidyr)
library(apmx)
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

dm <- DM %>%
  dplyr::select(USUBJID, AGE, SEX, RACE, ETHNIC)

# Adding units to DM.
dm <- dm %>% mutate(AGEU = "Years")


pkdf <- pk_build(ex = ex,
                 pc = pc,
                 sl.cov = dm,
                 cycle.length = 14,
                 time.rnd = 3)
################### END: DUMMY DATA ####################

test_that("QC Checks", {
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    source("R/version_log.R")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

    dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    dir <- paste0(dir, "/tests/testthat/test-pk-define-files/pkdf")
    out <- paste0(getwd(), "/tests/testthat/version_log-data/")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


    # dir <- paste0(dir, "/test-pk-define-files/pkdf")
    # out <- paste0(getwd(), "/version_log-data")

    expect_error(version_log(file = dir, orig = T, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT")))
    
    # Correction:
    dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    dir <- paste0(dir, "/tests/testthat/test-pk-define-files/pkdf.csv") 
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
    # dir <- paste0(dir, "/test-pk-define-files/pkdf.csv")
    
    version_log(file = dir, orig = T, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT"))

    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    version_log_dir <- paste0(getwd(), "/tests/testthat/version_log-data/VersionLog.docx")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

    # version_log_dir <- paste0(getwd(), "/version_log-data/VersionLog.docx")
    # Appending the version log.
    dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    dir <- paste0(dir, "/tests/testthat/test-pk-define-files/pkdf2.csv")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
    
    # dir <- paste0(dir, "/test-pk-define-files/pkdf2.csv")

    version_log(file = dir, orig = F, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT"))
    dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    # dir <- paste0(dir, "/tests/testthat/test-pk-define-files/pkdf3.csv")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

    dir <- paste0(dir, "/test-pk-define-files/pkdf3.csv")
    version_log(file = dir, orig = F, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT"))

    dir <- getwd()
    # ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ DEBUG MODE ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    # dir <- paste0(dir, "/tests/testthat/test-pk-define-files/pkdf.csv")
    # ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ DEBUG MODE ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

    dir <- paste0(dir, "/test-pk-define-files/pkdf.csv")

    # Having original data to false, and version log is expecting data, but there is non.
    expect_error(version_log(file = dir, orig = F, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT")),
    "This file is not the most recent dataset version")

    # Check to see that there is only one file.
    out <- paste0(getwd(), "/version_log-data")
    files_to_append <- list.files(out, pattern = "^V.*", full.names = TRUE)
    expect_length(files_to_append, 1)
    browser()
    # Clean up files.
    unlink(version_log_dir)

    expect_error(version_log(file = dir, orig = F, outdir = out, comp_var = c("USUBJID", "ATFD", "CMT")), 
    "There is no preexisting version log in the outpath provided")

})
