#' My name is Steve Lombardo. I'm a first-year spatial modeler for CSS/NCCOS and
#' am currently working on a biogeographic assessment project in support of
#' ONMS's Hudson Canyon designation.
#' As part of this assessment, I have created a workflow using the NEFSC bottom
#'  trawl survey data (NEFOP data too, but not addressed here) to look at trends
#'   in survey coverage, catch numbers and weight, biodiversity, and a few other
#'    metrics of interest. I came up against a bit of a wall talking with the
#'     trawl survey folks primarily due to their busy schedule, and I was hoping
#'      to get your thoughts and insights on working with these data.
#' The crux of my issue/concern is locating and properly applying conversion
#'  factors on the data that I sourced from the InPort portal (I do not have
#'  a NEFSC database login to use your survdat package). My spatially distilled
#'  dataset includes tows from all three vessels (1963-2024), and I do have
#'  the Miller et al. 2010 paper with the Albatross to Bigelow conversion factors.
#' Looking at examples in the survdat vignettes, some of the applications
#' of the conversion factors make sense when referencing the Miller paper (i.e., factors are found in Tables 53-55), however, some conversion factors are sourced from Table 2 - e.g., Atlantic Herring. Furthermore, I cannot find the Delware II vessel, door, gear conversions (referenced in Table 2 of this manuscript).
#' If you have time, would you be able to provide your thoughts on how to
#' properly apply the correct conversion factors, and whether examining
#' species trends is appropriate beyond the contemporary period where the
#' Bigelow has been used?
#'   To share some species of particular interest, attached are a couple
#'   example visuals created from the data without correction factors applied
#'   and the Albatross to Bigelow switch denoted.
#'
#'

survdat::get_survdat_data(channel, getLengths = FALSE)
