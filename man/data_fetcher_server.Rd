% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_fetcher.R
\name{data_fetcher_server}
\alias{data_fetcher_server}
\title{Create data fetcher server logic}
\usage{
data_fetcher_server(id, pool, query, needs_summary)
}
\arguments{
\item{id}{Character. The module ID}

\item{pool}{Database connection pool}

\item{query}{Reactive expression for the built query}

\item{needs_summary}{Reactive expression for needs summary flag}
}
\value{
List of reactive expressions
}
\description{
Create data fetcher server logic
}
