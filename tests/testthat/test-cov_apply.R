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

pkdf_cov_me <- pk_build(ex = ex, pc = pc,
                BDV=T, DDV=T, PDV=T,
                time.rnd=3, dv.rnd=3, cov.rnd=3)



pkdf <- pk_build(ex = ex,
                 pc = pc,
                 sl.cov = dm,
                 cycle.length = 14,
                 time.rnd = 3)
################### END: DUMMY DATA ####################

test_that("QC Checks", {
# NEED FOR DEBUGGING.
# source("R/pk_build.R")
# source("R/cov_apply.R")
pkdf_cov_me <- pk_build(ex = ex, pc = pc, sl.cov = dm,
                     BDV=T, DDV=T, PDV=T,
                     time.rnd=3, dv.rnd=3, cov.rnd=3)
    # Check the id.by is OK.
    expect_error(cov_apply(df = pkdf_cov_me, id.by = "ERROR", time.by = "DTIM", cov.rnd = 3),
    "id.by must be one of the following options: USUBJID, SUBJID, ID")

    # Checking id.by is only one thing.
    expect_error(cov_apply(df = pkdf_cov_me, id.by = c("More", "Errors"), time.by = "DTIM", cov.rnd = 3),
    "cov_apply can only fill by one ID type.")

    # QC by time.by
    expect_error(cov_apply(df, id.by = "USUBJID", time.by = "NOTIME"), 
    "time.by must be one of the following options: NA \\(subject-level attribute\\), DTIM, ATFD, ATLD, NTFD, NTLC, NTLD, NDAY")

    # QC direction.
    expect_error(cov_apply(df = pkdf_cov_me, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3, 
        direction = "not a direction"), "direction must be one of the following \\(tidy\\) options: down, up, downup, updown")
    expect_error(cov_apply(df = pkdf_cov_me, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3, 
        direction = c("up", "down", "up")), "cov_apply can only fill in one direction.")
})

test_that("Test when .by is not in the pkdf.", {
pkdf_cov_me <- pk_build(ex = ex, pc = pc, sl.cov = dm,
                     BDV=T, DDV=T, PDV=T,
                     time.rnd=3, dv.rnd=3, cov.rnd=3)
  # NEED FOR DEBUGGING.
  # source("R/pk_build.R")
  # source("R/cov_apply.R")
  # Check if id.by that is not in PKDF triggers error.
  pkdf_cov_me <- pkdf_cov_me %>% select(-ID)
  expect_error(cov_apply(df = pkdf_cov_me, id.by = "ID", time.by = "DTIM", cov.rnd = 3),
    "ID column not in PKPD dataframe. Cannot merge with covariate dataframe.")

  # Restore PKDF.
  pkdf_cov_me <- pk_build(ex = ex, pc = pc, sl.cov = dm,
                     BDV=T, DDV=T, PDV=T,
                     time.rnd=3, dv.rnd=3, cov.rnd=3)

  # Check if time.by is not in PKDF triggers error.
  pkdf_cov_me <- pkdf_cov_me %>% select(-DTIM)
  expect_error(cov_apply(df = pkdf_cov_me, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3),
    "DTIM column not in PKPD dataframe. Cannot merge with covariate dataframe.")
})

test_that("FDOSE not in pkdf", {
# NEED FOR DEBUGGING.
# source("R/pk_build.R")
# source("R/cov_apply.R")
pkdf_cov_me <- pk_build(ex = ex, pc = pc, sl.cov = dm,
                     BDV=T, DDV=T, PDV=T,
                     time.rnd=3, dv.rnd=3, cov.rnd=3)
pkdf_cov_me <- pkdf_cov_me %>% select(-FDOSE)
expect_error(cov_apply(df = pkdf_cov_me, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3),
    "Dataframe \\(df\\) must contain FDOSE \\(date/time of first dose\\) if merging with DTIM.")
})

test_that("Column in covariate DF, but not in the main DF.", {
# source("R/PK_ASSEMBLY.R")

  dm <- dm %>% select(-USUBJID)
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3),
    "USUBJID column not in covariate dataframe. Cannot merge with PKPD dataframe.")      
})

test_that("Making sure that covariate column is not in the PKDF", {
# source("R/PK_ASSEMBLY.R")
  pkdf_cov_me <- pk_build(ex = ex, pc = pc,
                     BDV=T, DDV=T, PDV=T,
                     time.rnd=3, dv.rnd=3, cov.rnd=3)
  # Adding in DTIM to trigger stop.
  dm <- dm %>% mutate(DTIM = pkdf_cov_me$ATLD[1:22])
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3),
          "DTIM already exists in PKPD dataframe.")
})

test_that("Observation QC for Covariate", {
  
  # source("R/PK_ASSEMBLY.R")
  pkdf_cov_me <- pk_build(ex = ex, pc = pc,
                  BDV=T, DDV=T, PDV=T,
                  time.rnd=3, dv.rnd=3, cov.rnd=3)
  dm$USUBJID <- dm$USUBJID[1]
  # Making the covariates have all of the same subject so that it seems like
  # a subject has more than one observation.
  
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3),
          "Cannot merge cov at subject-level. At least one subject has more than one observation.")

})

test_that("QC: Record is missing data", {
  # source("R/PK_ASSEMBLY.R")

  pkdf_cov_me$USUBJID[5] = NA
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3),
      "USUBJID is missing for at least one row.")
})

test_that("Warnings for more subjects in PKDF than the Covariate dataframe.", {
  # # NEED FOR DEBUGGING.
  # source("R/pk_build.R")
  # source("R/cov_apply.R")
  pkdf_cov_me <- pkdf_cov_me %>% add_row(USUBJID = "Pateint_1")
  pkdf_cov_me <- pkdf_cov_me %>% add_row(USUBJID = "Pateint_2")
  expect_warning(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3), 
      "At least one subject is included in the dataframe \\(df\\), but not in covariate dataframe \\(cov\\).")
})

test_that("Trigger warning about dupe values in covariate dataframe.", {
  # source("R/PK_ASSEMBLY.R")
  dm <- dm %>% mutate(DTIM = pkdf_cov_me$DTIM[1:22])
  dm <- dm %>% add_row(USUBJID = "pateint_1", DTIM = pkdf_cov_me$DTIM[1])
  dm <- dm %>% add_row(USUBJID = "pateint_1", DTIM = pkdf_cov_me$DTIM[1])
  expect_warning(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3), 
    "Some covariates may not be filled. Some rows have duplicate USUBJID and DTIM values.")
})

test_that("Check Covariate Pre-Processing", {
  # source("R/PK_ASSEMBLY.R")
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3)
  expect_equal(check$NSEX[111], 1)
  expect_equal(check$NSEX[110], 0)
  expect_equal(check$NRACE[151], 1)
  expect_equal(check$NRACE[185], 2)
  expect_equal(names(check[23:28]), c("NROUTE", "NFRQ", "NSEX", "NRACE", "NETHNIC", "BAGE"))
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, demo.map = T, exp = T, ebe = T),
    "Only one of exp or ebe can be selected as TRUE.")
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3, demo.map = F)
  expect_equal(check$NSEX[137], 0)
  expect_equal(check$NRACE[183], 0)
  dm <- dm %>% mutate(DTIM = pkdf_cov_me$DTIM[1:22])
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3, demo.map = F)
  expect_equal(names(check[23:28]), c("NROUTE", "NFRQ", "TSEX", "TRACE", "TETHNIC", "TAGE"))
})

test_that("Filling NAs QC:", {
  # source("R/PK_ASSEMBLY.R")
  dm$AGE[1] <- NA
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = NA, cov.rnd = 3, demo.map = T)
  expect_true(all(unlist(check$BAGE[1:21]) == -999))
})

test_that("Testing Fill when Time is not NA:", {
  # source("R/PK_ASSEMBLY.R")
  dm <- dm %>% mutate(DTIM = pkdf_cov_me$DTIM[1:22])
  # Remove ATFD from pkdf_cov_me to trigger stop
  pkdf_cov_me <- pkdf_cov_me %>% select(-ATFD)
  expect_error(cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3, demo.map = T), "If merging by DTIM, ATFD must be included in df.")
})

test_that("Check final filter order", {
  # source("R/PK_ASSEMBLY.R")
  # time.by is DTIM
  dm <- dm %>% mutate(DTIM = pkdf_cov_me$DTIM[1:22])
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "DTIM", cov.rnd = 3, demo.map = T)
  # # Checking to see that KEEP is removed from the PK dataset.
  expect_true(!("KEEP" %in% names(check)))

  # time.by is ATLD or NTLC or NTLD.
  dm <- dm %>%
           mutate(ATLD = pkdf_cov_me$ATLD[1:22]) %>%
           select(-DTIM)
           
  check <- cov_apply(df = pkdf_cov_me, cov = dm, id.by = "USUBJID", time.by = "ATLD", cov.rnd = 3, demo.map = T)
  expect_equal(nrow(check), 446)
})
