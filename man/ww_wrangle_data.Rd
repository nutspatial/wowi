% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-wranglers.R
\name{ww_wrangle_data}
\alias{ww_wrangle_data}
\title{Prepare SaTScan-required input data files for a Bernoulli spatial scan
analysis and save them in a user-defined working directory}
\usage{
ww_wrangle_data(
  .data,
  filename = character(),
  dir = character(),
  .gam_based = c("wfhz", "muac", "combined")
)
}
\arguments{
\item{.data}{A data frame object that has been wrangled using
\verb{mwana::mw_wrangle_*()} functions.}

\item{filename}{A quoted string identifying the analysis area.}

\item{dir}{A quoted string of the folder or directory in which the files
should be saved.}

\item{.gam_based}{A quoted string indicating the criterion used to define acute
malnutrition. This is used to identify the right vector where flagged values
are identified, and for which should be excluded from the analysis. Default
is \code{wfhz}.}
}
\value{
Three files are created and saved in the user-defined directory as defined
by \code{dir}: a \code{.cas} file for cases, a \code{.ctl} for controls, and
a \code{.geo} file for geographical coordinates. The full filenames will incorporate
the use-defined \code{filename} string.

The \code{.cas} and \code{.ctl} files will each have two columns: the first containing
survey cluster or enumeration area IDs, and the second containing only \code{1}s,
representing either cases or controls, respectively. The length of the \code{.cas}
file depends on the number of positive acute malnutrition cases
(\code{gam == 1}), and the \code{.ctl} file on the number of negative cases
(\code{gam == 0}).

The \code{.geo} file will have three columns: cluster or enumeration area IDs,
latitude, and longitude.
}
\description{
SaTScan's Bernoulli-based spatial scan requires the input data to be split
into cases, controls, and geographical coordinates files, saved in a format
readable by the software, and placed in a directory it can access.

\code{ww_wrangle_data()} is a convenient function designed for this task. It
assumes that the input anthropometric data has been pre-processed using the
\code{{mwana}} data wrangling functions.
}
\examples{

## Given a temporary directory ----
tmp <- withr::local_tempdir()
directory <- file.path(tmp, "input-files")

## Wrangle data with `{mwana}` ----
x <- anthro |>
  dplyr::rename(longitude = x, latitude = y) |>
  mwana::mw_wrangle_wfhz(
    sex = sex,
    .recode_sex = TRUE,
    weight = weight,
    height = height
  ) |>
  mwana::define_wasting(
    zscores = wfhz,
    .by = "zscores",
    edema = oedema
  )

## Apply the function ----
ww_wrangle_data(
  .data = x,
  filename = "Locality",
  dir = directory,
  .gam_based = "wfhz"
)

## Show created files ----
list.files(file.path(tmp, "input-files"))

## Display each files' content ----
file.show(file.path(tmp, "input-files/Locality.cas"))
file.show(file.path(tmp, "input-files/Locality.ctl"))
file.show(file.path(tmp, "input-files/Locality.geo"))

}
