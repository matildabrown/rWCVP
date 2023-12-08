## Resubmission
2023-12-08
-- MJMB: Fixes offline errors, mapping bugs. Passing checks with no errors, warnings or notes. 

2023-08-04
-- MJMB: New version that fixes checklist report issues (see NEWS.md for other changes). Passing checks with no errors, warnings or notes. 

2022-02-15 (2)
-- MJMB: Thanks so much for your help Benjamin - I've wrapped the examples in \donttest{} and if(requireNamespace("rWCVPdata")){}. Passing local checks with no errors, warnings or notes. 

2022-02-15
-- MJMB: I'm so sorry, I didn't see that a string of colours was accidentally 
         included in the DESCRIPTION file. Please ignore previous sub. 

2022-02-14

- BA: You said you have the examples wrapped in \dontrun{} because they take such a long time to execute. If this is the only reason for the use of \dontrun{}, please wrap them in \donttest{} instead. This is how lengthy examples should be flagged so that the user can run them, but they won't block the testservers.
-- MJMB: emailed BA for clarification; examples previously failed automated
         checks, presumably because of dependency on 'rWCVPdata'. Although the 
         data package is not required, using 'rWCVP' without it requires a local 
         copy of the dataset which exceeds the size allowance for CRAN, so this 
         is not a feasible alternative. Therefore, we have used \dontrun{} 
         with the justification that the examples require additional software.
         I apologise for not explaining this fully and in the correct order in 
         the last submission. 

- BA: Please write the reference title in normal quotes instead of single quotes. e.g.:
"rWCVP: A companion R package to the World Checklist of Vascular Plants"
-- MJMB: Fixed.


2022-02-13

- VW: Please do not start the description with "An R interface to", "This 
      package", package name, title or similar.
-- MJMB: Emended to: A companion to the World Checklist of 
         Vascular Plants (WCVP) - also fixes below point.

- VW: Please always explain all acronyms in the description text. e.g.: WCVP
-- MJMB: See above for fix in DESCRIPTION. We have not spelled out the acronym
         in the documentation for every function - because this package is so 
         closely linked to the WCVP, we assume that the user will be familiar 
         with the acronym by the time they are using the functions in the package. 

- VW: Please always write package names, software names and API (application 
      programming interface) names in single quotes in title and description.
      e.g: --> 'rWCVP'
      Please note that package names are case sensitive.
-- MJMB: Fixed - we have used quotes to wrap the entire journal article title 
         (which includes rWCVP, though not in its own quotes) for consistency 
         with the journal. Elsewhere in the package we have used \code{code} 
         formatting


- VW: Please add \value to .Rd files regarding exported methods and explain the 
      functions results in the documentation. Please write about the structure 
      of the output (class) and also what the output means. (If a function does 
      not return a value, please document that too, e.g.
      \value{No return value, called for side effects} or similar) 
      Missing Rd-tags:
      powo_pal.Rd: \value
-- MJMB: We have added the following to the .Rd file:
         @return Character. Vector of names and HEX values to match those of 
         [POWO](https://powo.science.kew.org/). 

- VW: \dontrun{} should only be used if the example really cannot be executed 
      (e.g. because of missing additional software, missing API keys, ...) by 
      the user. That's why wrapping examples in \dontrun{} adds the comment 
      ("# Not run:") as a warning for the user.
      Does not seem necessary.
      Please unwrap the examples if they are executable in < 5 sec, or replace 
      \dontrun{} with \donttest{}.

-- MJMB: This is necessary - most examples take >5seconds and many take >10secs.
         Additionally, although the examples could be designed to use a
         local copy of the WCVP (rather than 'rWCVPdata'), this would then involve 
         bundling a version (>>10Mb) into this package. To get them to run in <5
         seconds, we could use a toy version of the WCVP dataset, but this would not 
         be appropriate as it would give incorrect results. Happy to add inline
         explanations if needed. 


- VW: Please do not install packages in your functions, examples or vignette.
      This can make the functions,examples and cran-check very slow.
-- MJMB: We have fixed this by pre-compiling the vignette, as it is not possible
         to run the code blocks in the vignette without the full dataset (see 
         above for issues with examples) - please let us know if there is a better
         alternative to this fix. 
         
################################################################################

2022-02-07
We have updated CITATION to use bibentry() instead of citEntry()
We have updated the link to WCVP to follow moved content (incl. https and trailing slash)
We have also added the same link to the missing URI in powo_pal.Rd

------------------
We have moved the non-mainstream dependency rWCVPdata to a drat repository and 
    listed this in Additional repositories in the DESCRIPTION.
We have made sure that the vignettes folder is included in the build.
Examples are tagged with 'dontrun' because they take >10seconds to run, and the
    simplest syntax requires rWCVPdata (we have added this where it was missed in 
    the previous submission)


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
