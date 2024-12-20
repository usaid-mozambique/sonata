% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{mysql_connection}
\alias{mysql_connection}
\title{MozART 2.0 Utilities}
\usage{
mysql_connection(
  db_name,
  db_user,
  db_pass,
  db_host = "localhost",
  db_port = 3306
)
}
\arguments{
\item{db_name}{Database name}

\item{db_user}{Database user}

\item{db_pass}{Database password}

\item{db_host}{Database host}

\item{db_port}{Database port}
}
\value{
A connection to the database
}
\description{
MozART 2.0 Utilities
}
\examples{
\dontrun{

 df <- mysql_connection()}
}
