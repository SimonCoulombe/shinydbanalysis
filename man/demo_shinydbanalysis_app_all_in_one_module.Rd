% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo_shinydbanalysis_app_all_in_one_module.R
\name{demo_shinydbanalysis_app_all_in_one_module}
\alias{demo_shinydbanalysis_app_all_in_one_module}
\title{demo_shinydbanalysis_app_all_in_one_module}
\usage{
demo_shinydbanalysis_app_all_in_one_module(
  pool,
  storage_info,
  restricted_columns
)
}
\arguments{
\item{pool}{}

\item{storage_info}{}

\item{restricted_columns}{}
}
\description{
demo_shinydbanalysis_app_all_in_one_module
}
\examples{
library(bslib)
storage_info <- list(  storage_type = "local",column_info_dir = "column_info")
restricted_columns <- character(0)
demo_shinydbanalysis_app_all_in_one_module(pool, storage_info, restricted_columns)
}
