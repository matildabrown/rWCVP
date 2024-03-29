% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wgsrpd3_code_mapping.R
\name{get_wgsrpd3_codes}
\alias{get_wgsrpd3_codes}
\title{Extract WGSRPD Level 3 (area) codes.}
\usage{
get_wgsrpd3_codes(geography, include_equatorial = NULL)
}
\arguments{
\item{geography}{Character. The geography to convert into Level 3 codes. May be a WGSRPD area (Level 3), region (Level 2) or continent (Level 1), country (political) or hemisphere ("Northern Hemisphere", "Southern Hemisphere" or "Equatorial")}

\item{include_equatorial}{Logical. Include Level 3 areas that span the equator? Defaults to \code{NULL}, which generates a message and includes these areas. Ignored if geography is not a hemisphere.}
}
\value{
Character with area codes (Level 3) that fall within the geography.
}
\description{
Extract WGSRPD Level 3 (area) codes.
}
\details{
Country mapping follows Gallagher et al. (2020). Importantly, this means that some overseas territories are not considered part of the country in this system, e.g. the Canary Islands are designated as their own Level 3 area, rather than part of Spain in this mapping. Where this is ambiguous, the mapping can be explored using \code{View(wgsrpd_mapping)}.

Gallagher, R. V., Allen, S., Rivers, M. C., Allen, A. P., Butt, N., Keith, D., & Adams, V. M. (2020). Global shortfalls in extinction risk assessments for endemic flora. bioRxiv, 2020.2003.2012.984559. https://doi.org/10.1101/2020.03.12.984559
}
\examples{
get_wgsrpd3_codes("Brazil")

}
