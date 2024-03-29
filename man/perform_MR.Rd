% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perform_MR.R
\name{perform_MR}
\alias{perform_MR}
\title{perform MR for one code}
\usage{
perform_MR(
  data,
  path = "",
  save_name = "exposure_outcome",
  phenoscanner = F,
  MR_CSV = T,
  MR_PIC = T,
  method = c("mr_ivw", "mr_egger_regression", "mr_weighted_median")
)
}
\arguments{
\item{data}{After using TwosampleMR harmorize's data}

\item{path}{which folder you want to save}

\item{save_name}{the name you want to save}

\item{phenoscanner}{to check the snp in phenoscanner}

\item{MR_CSV}{to perform mr and write it to csv}

\item{MR_PIC}{to get the picture}
}
\value{
three csv file and a PDF which includ three picture
}
\description{
perform MR for one code
}
