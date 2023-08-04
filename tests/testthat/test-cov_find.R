library(testthat)
library(tidyr)
library(apmx)
library(tibble)
library(dplyr)

# #################### START: DUMMY DATA ####################

current_dir <- getwd()
load(paste0(current_dir, "/data/EX.rda"))
load(paste0(current_dir, "/data/PC.rda"))
load(paste0(current_dir, "/data/DM.rda"))
load(paste0(current_dir, "/data/LB.rda"))

# # print(current_dir)



# pkdf <- pk_build(ex = EX, pc = PC,  sl.cov = sl_cov_dm, tv.cov = list(tast, talt), time.units = "days",
#                 BDV = T, DDV = T, PDV = T, time.rnd = 3, dv.rnd = 3, cov.rnd = 3, keep.other = F)

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
# source("R/pk_build.R")
# source("R/cov_find.R")
pkdf <- pk_build(ex = ex,
                 pc = pc,
                 sl.cov = dm,
                 cycle.length = 14,
                 time.rnd = 3)

# Passed!
# test_that("QC for cov_find()", {
#   expect_error(cov_find(pkdf, cov = "categorical", "error"), "type must be numeric or character")
#   expect_error(cov_find(pkdf, cov = "continuous", "character", "continuous covariates must be numeric only"))
#   expect_error(cov_find(pkdf, cov = "continuous", "error"), "type must be numeric")
#   expect_error(cov_find(pkdf, "units", "numeric"), "units must be numeric only")
#   expect_error(cov_find(pkdf, "units", "something"), "type must be character")
#   expect_error(cov_find(pkdf, "exposure", "character"), "type must be numeric")
#   expect_error(cov_find(pkdf, "empirical bayes estimate", "character"), "type must be numeric")
#   expect_error(cov_find(pkdf, "other", "something"), "type must be numeric or character")
#   expect_error(cov_find(pkdf, "error", "something"), "cov must be categorical, continuous, exposure, empirical bayes estiamte, or other")
# })

# test_that("Functionality for cov_find()", {
#   # source("R/pk_build.R")
#   # source("R/cov_find.R")
#   check_cat_c <- cov_find(pkdf, cov = "categorical", "character") 
#   # List extraction to get value.
#   expect_true(is.character((pkdf %>% select(!!as.symbol(check_cat_c)))[[1]]))
#   check_cat_n <- cov_find(pkdf, cov = "categorical", "numeric") 
#   check_col <- check_cat_n[1]
#   expect_true(is.integer((pkdf %>% select(!!as.symbol(check_col)))[[1]]))
#   check_cont_n <- cov_find(pkdf, cov = "continuous", "numeric") 
#   expect_true(is.integer((pkdf %>% select(!!as.symbol(check_cont_n)))[[1]]))
#   exposure_check <- cov_find(pkdf, cov = "exposure", "numeric")
#   check_units <- cov_find(pkdf, cov = "units", "character")
#   expect_equal(check_units, "BAGEU")
#   expect_error(cov_find(pkdf, cov = "units", "numeric"), "units must be numeric only")
#   expect_error(cov_find(pkdf, cov = "units", "other"), "type must be character")
# })



