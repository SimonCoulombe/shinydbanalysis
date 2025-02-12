% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{run_app}
\alias{run_app}
\title{Run the Shiny Database Analysis Application}
\usage{
run_app(
  pool,
  storage_type = "local",
  local_dir = "column_info",
  adls_endpoint = NULL,
  adls_container = NULL,
  sas_token = NULL,
  restricted_columns = character(0)
)
}
\arguments{
\item{pool}{A database connection pool object}

\item{storage_type}{Either "local" or "adls"}

\item{local_dir}{Path for local storage (default: "column_info")}

\item{adls_endpoint}{ADLS endpoint URL (required if storage_type = "adls")}

\item{adls_container}{ADLS container name (required if storage_type = "adls")}

\item{sas_token}{ADLS SAS token (required if storage_type = "adls")}
}
\description{
Run the Shiny Database Analysis Application
}
