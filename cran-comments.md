## Resubmission
This is a resubmission. In this version I have:  

* Added a DOI reference to the DESCRPTION file for the NONMEM software and https reference for SDTM/ADaM data standards.  

* Replaced letters T and F with TRUE and FALSE in function code, examples, and vignettes.  

* Re-wrote pk_define(), pk_summarize(), version_log(), and variable_list_create() to remove default filepaths. Examples and vignettes no longer require filepaths, any required filepaths use tempdir().  

* Renamed vignette to a more descriptive title of the workflow involved.  

* Updated filepaths in testthat folder to forward-slashes only.  

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.  
