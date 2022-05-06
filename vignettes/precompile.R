#' precompile vignettes that take a long time to run
library(knitr)
library(here)

# remove the figures folder for regeneration
if (dir.exists(here("vignettes/figure"))) {
  unlink(here("vignettes/figure"), recursive=TRUE)
}

# Conservation status on the Tree of Life
knit("vignettes/Getting-started-with-rWCVP.Rmd.orig",
     "vignettes/Getting-started-with-rWCVP.Rmd")

# move any figures that have been created to the articles folder
file.rename(here("figure"), here("vignettes/figure"))
