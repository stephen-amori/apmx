# apmx
Automated Approach to PK and PKPD Dataset Assembly  

## Description

This repository contains several R functions to support the assembly of PK(PD) datasets to be used in NONMEM. The functions will create a PK(PD) dataset with consistent column names and covariate labels. Additional functions are used to support covariate analysis, combine datasets to form a population dataset, and create dataset definition files to support regulatory submissions.  

## Instructions for Use

### Download the Repository onto your Computer
The latest CRAN release can be installed with the following commands:  
```
install.packages("apmx")
library(apmx)
```

The current development package can be downloaded from GitHub with the following commands:  
```
devtools::install_github("stephen-amori/apmx")
library(apmx)
```

### Function summary

#### pk_build()
`pk_build()` creates a PK(PD) dataset for analysis in NONMEM from source data. The functions is not intended to produce datasets for NCA.  
The function automatically maps CDISC terminology to a uniform variable name (apmx name) appropriate for pharmacometric analysis.  
The function issues a variety of warnings and errors to inform the user of problematic subjects and records.  

General comments:  
* The `ex` and `pc` domain may accept standard CDISC attribute names or apmx names. apmx names are required for pd events.  
* `DTIM` (the date/time of the record) must be in ISO-8601 format to be processed correctly. All date/times assumed to be UTC. Accepted forms:  
    + YYYY:mm:ddTHH:MM:SS  
    + YYYY:mm:dd HH:MM:SS  
    + YYYY:mm:ddTHH:MM  
    + YYYY:mm:dd HH:MM  
    + YYYY:mm:dd (this format is not accepted for ex, pc, or pd events)  
* All covariates are automatically renamed based on type and categorical covariates are automatically mapped to a numeric type in the following manner:  
    + All character-type covariates are considered categorical. The given covariate will be mapped to a numeric value and the column name will start with a prefix "N" (subject-level) or "T" (time-varying). The character description is retained and the column name will end with the suffix "C". For example, an input subject-level covariate "SEX" will mapped to "NSEX" (numeric) and "NSEXC" (character).  
    + All numeric-type covariates are considered continuous. The covariate column name will start with a prefix "B" (baseline) or "T" (time-varying).  
    + All numeric-type covariates must also have an associated character column for units. For example, an input dataframe with covariate "AGE" must have an accompanying column "AGEU".  
* The study label `STUDY` must be provided in either the `ex` domain or `sl.cov` domain.  
* Missing date/times can be handled three different ways with the `impute` parameter.  
    + `impute` can be left empty, which will not impute times for any event missing `DTIM`.  
    + `impute = 1` will set all actual time variables equal to the nominal time for events where `DTIM` is missing. This method is appropriate for pre-clinical analysis when actual times may not be collected.  
    + `impute = 2` will estimate actual time variables relative to other events occurring that day for events where `DTIM` is missing. This method is appropriate for phase I-III analyses when individual events are missing a date/time.  
  
apmx attribute names and definitions:  
* `USUBJID`: unique subject ID (character or numeric)  
* `DTIM`: date-time of the event (character)  
* `NDAY`: nominal day of event, derived from protocol-defined visit (numeric)
    + Day of first dose should start at nominal (study) day 1, not day 0.  
    + Day prior to first dose should be documented as day -1, not day 0.  
* `TPT`: nominal timepoint of event, derived from protocol-defined timepoint (numeric)  
* `VISIT`: visit label (character)  
* `TPTC`: timepoint label (character)  
* `ROUTE`: dose route description (character)  
* `FRQ`: dose frequency description (character)  
* `AMT`: administered dose amount for dose events only (numeric)  
* `CMT`: compartment assignemt for each event (numeric)  
* `DVID`: dependent variable label (character)  
    + dose events should have the same label as the parent PK metabolite  
* `DVIDU`: dependent variable unit label (character)  
    + dose events should have the dose units listed in this variable  
* `ODV`: original dependent variable (numeric)  
* `LLOQ`: lower limit of quantification (numeric)  
* `STUDY`: study label (character)  

apmx derived attribute names and definitions:  
* `SUBJID`: numeric form of `USUBJID`  
* `ID`: ID counting variable  
* `ATFD`: actual time since first dose  
* `ATLD`: actual time since last (most recent) dose  
* `NTFD`: nominal time since first dose  
* `NTLC`: nominal time since last cycle  
* `NTLD`: nominal time since last (most recent) dose  
* `EVID`: event ID (NONMEM-required)  
* `MDV`: missing dependent variable (NONMEM-required)
* `DVID`: numeric mapping of input DVID  
* `LDV`: log-transformed dependent variable  
* `BDV`: baseline dependent variable (for PD events only)  
* `DDV`: delta from baseline dependent variable (for PD events only)  
* `PDV`: percent change from baseline dependent variable (for PD events only)  
* `BLQ`: flag for BLQ records  
    + `BLQ = 0` when observation is not BLQ  
    + `BLQ = 1` when observation is BLQ and prior to first dose  
    + `BLQ = 2` when observation is BLQ and after first dose  
* `DOSEA`: most recently administered dose amount  
* `DOMAIN`: event domain
* `DVIDC`: character label for DVID  
* `TIMEU`: units for all time variables  
* `FDOSE`: date/time of first dose  
* `VERSN`: apmx package version number
* `BUILD`: date of dataset construction  
* `COMBD`: date of dataset combination (generated from `apmx::pk_combine()` only)  

apmx derived binary flag names and definitions:  
* `PDOSEF`: records that occur prior to first dose  
* `TIMEF`: records with no TIME information  
* `AMTF`: dose events with missing `AMT`  
* `DUPF`: duplicated events within the same `ID`-`ATFD`-`EVID`-`CMT`  
* `NOEXF`: subjects with no dose events  
* `NODVNF`: subjects with no observations in compartment n (one flag per observation compartment)  
* `SDF`: subjects who are single-dose (as opposed to multi-dose)  
* `PLBOF`: records where the most recent dose is placebo  
* `SPARSEF`: records associated with sparse sampling (as opposed to serial)  
* `TREXF`: dose records that trail the final observation record  
* `IMPEX`: time of last (most recent) dose event was imputed  
* `IMPDV`: time of observation event was imputed  
* `IMPFEX`: time of first dose event was imputed  
* `C`: comment flag to indicate a record will be ignored in the analysis  

#### pk_combine()
`pk_combine()` will combine two PK(PD) datasets built by `pk_build()` to form a population dataset.  
All categorical covariates are re-calculated to ensure they are consistently labeled throughout the population analysis.  
The function confirms the analytes and compartments are in agreement between both studies.  

#### cov_find()
`cov_find()` will identify the columns in a PK(PD) dataset that belong to a certain covariate class.  
Covariates can be "categorical", "continuous", "exposure", "empirical bayes estimate", or "other".  
Types can be "numeric" or "character".  

#### cov_apply()
`cov_apply()` will add additional covariates to a PK(PD) dataset already built by `pk_build()`.  
It can apply covariates of any type, either subject-level or time-varying.  
Covariates can be merged by any ID variable (USUBJID, SUBJID, or ID) or any time variable (ATFD, ATLD, NTFD, NTLC, NTLD, NDAY, TPT).  
The same prefix and suffix system is applied to covariates built with `cov_apply()`  
* Exposure metrics can be added with this function and will receive the prefix "C"  
* Empirical bayes estimates can be added with this functoin and will receive the prefix "I"  


#### pk_write()
`pk_write()` will write out a PK(PD) dataset as a .csv file to the filepath of your choice in a NONMEM-ready format.  

#### pk_define()
`pk_define()` will create a definition file for a PK(PD) dataset built by `pk_build()` as a .docx document.  
The dataset is read into the function from its filepath and output the definition file to the same directory.  

Two other inputs are required:
* `project`: character string to specify project name. This name will appear in the header of the definition file.  
* `variable.list`: this is a file path to the list of variable definitions (.csv file) you wish to use. We recommend maintaining a list of global variable definitions for all projects. You can download a starter variable list with the function `apmx::variable_list_export()` that includes the apmx variable names. You can then add your own covariates or additional variables to the list. The variable list should have the following columns in this order:  
    + Variable: variable name (covariates just need the root term for proper definitions. For example, the variable for covariates "NSEX" and "NSEXC" only need to be listed once as "SEX")  
    + Category: desired variable category  
    + Description: desired variable description (covariates are automatically detected as "subject-level" or "time-varying" and labeled as such, you only need to provide the root definition. For example, the description for "SEX" can be listed "sex". For NSEX, the definition file would read "Subject sex".)  
    + Comment: desired comment  
* `template`: optoinal template .docx document you wish to use. The definition table will append to the end of the document. If you leave the template blank, the definition table will read into a blank document.  
  
#### pk_summarize()
`pk_summarize()` will create summary tables (BLQ, categorical covariates, and continuous covariates) of the dataset created by `pk_build()` or `pk_combine()`.  
Summary tables are produced as .csv files, .docx files, and/or .pptx files. They can be stratified by any variable in the dataset.  
  
 #### version_log()
 `version_log()` will create a version log of datasets created by `pk_build()` or `pk_combine()`. All datasets should be stored in the same folder for easy version control. The version log will document subject totals, record totals, new variables, changed variables, and user comments.  
 The version log is published as a .docx file.  
 
 #### variable_list_export()
 `variable_list_export()` is a helper function for `pk_define()` that writes out a standard variable list to your preferred filepath.  
  
## Future Development

In addition to bug fixes and runtime improvements, future functions will focus on the following areas:  
* Assembly of datasets to support QTC-prolongation analysis   
* Assembly of datasets to support TTE and other ER analyses  
* Additional dataset QC and documentation tools  
* Auxiliary functions to support dataset assembly  

## Authors and Acknowledgements in Alphabetical Order
Mildred Afoumbom  
Joyceline Afumbom  
Stephen Amori  
Ethan DellaMaestra  
Michael Dick  
Ekiti Ekote  
Daniel Litow  
Jonah Lyon  
