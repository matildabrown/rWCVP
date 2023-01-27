## Resubmission

We have moved the non-mainstream dependency rWCVPdata to a drat repository and 
    listed this in Additional repositories in the DESCRIPTION.
We have made sure that the vignettes folder is included in the build.
Examples are tagged with 'dontrun' because the simplest syntax requires rWCVPdata


## R CMD check results

0 errors | 0 warnings | 2 notes

* checking package dependencies ... NOTE
Package suggested but not available for checking: 'rWCVPdata'

--- MJMB: See above


* checking CRAN incoming feasibility ... NOTE

Possibly misspelled words in DESCRIPTION:
  Govaerts (12:42)
  Lughadha (12:79)
  Nic (12:75)
  Ondo (12:55)
  Phytologist (13:97)
  WCVP (11:32)
  rWCVP (13:21)
  
--- MJMB: these are not misspelled

Found the following (possibly) invalid URLs:
  URL: https://wcvp.science.kew.org (moved to https://powo.science.kew.org/)
    From: inst/doc/rWCVP.html
    Status: 200
    Message: OK
    
--- MJMB: this is OK
