% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/italic_ecoregions_distribution.R
\name{italic_ecoregions_distribution}
\alias{italic_ecoregions_distribution}
\title{Get distribution of lichen taxa across Italian ecoregions}
\usage{
italic_ecoregions_distribution(sp_names, result_data = "rarity")
}
\arguments{
\item{sp_names}{Character vector of accepted names}

\item{result_data}{Character string specifying the output format: "rarity" (default) returns
commonness/rarity categories, "presence-absence" returns only values for presence/absence (0/1)}
}
\value{
A data frame with:
\describe{
\item{scientific_name}{The scientific name provided as input}
\item{alpine}{Status in alpine belt (extremely common to absent)}
\item{subalpine}{Status in subalpine belt (extremely common to absent)}
\item{oromediterranean}{Status in oromediterranean belt (extremely common to absent)}
\item{montane}{Status in montane belt (extremely common to absent)}
\item{dry_submediterranean}{Status in dry submediterranean belt (extremely common to absent)}
\item{padanian}{Status in padanian belt (extremely common to absent)}
\item{humid_submediterranean}{Status in humid submediterranean belt (extremely common to absent)}
\item{humid_mediterranean}{Status in humid mediterranean belt (extremely common to absent)}
\item{dry_mediterranean}{Status in dry mediterranean belt (extremely common to absent)}
}
The possible values of commonness/rarity are: "extremely common", "very common", "common", "rather common",
"rather rare", "rare", "very rare", "extremely rare", "absent"
}
\description{
Returns the distribution and commonness status of lichen taxa across Italian
ecoregions. Only accepts names that exist in the database of ITALIC.
}
\note{
Before using this function with a list of names, first obtain their accepted names
using \code{italic_match()}.
Example workflow:
\preformatted{
      names_matched <- italic_match(your_names)
      ecoregions_distribution <- italic_ecoregions_distribution(names_matched$accepted_name)
      }
}
\examples{
\dontrun{
# Get commonness/rarity categories
italic_ecoregions_distribution("Cetraria ericetorum Opiz")

# Get presence/absence data
italic_ecoregions_distribution("Cetraria ericetorum Opiz", result_data="presence-absence")
}

}
\references{
For more information about Italian ecoregions see ITALIC ecoregions distribution
\url{https://italic.units.it/?procedure=base&t=59&c=60#commonness}
and the scientific publication describing the ecoregions
\url{https://www.mdpi.com/1424-2818/12/8/294}
}
