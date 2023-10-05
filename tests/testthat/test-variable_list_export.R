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

test_that("variable_list_export QC", {
    # source("R/variable_list_export.R")
    
    expect_error(variable_list_export("not-valid-path"), "not-valid-path is not a valid filepath.")
    current_path <- getwd()
    expect_error(variable_list_export(current_path), "filepath must include document name and .csv suffix.")
    current_path <- paste0(current_path, "/test-pk-define-files/pkdf4.csv")
    # print(current_path)
    variable_list_export(current_path)
    expect_true(file.exists(current_path))
    # unlink(current_path)
})