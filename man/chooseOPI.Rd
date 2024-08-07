% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opi.r
\name{chooseOPI}
\alias{chooseOPI}
\alias{chooseOpi}
\title{chooseOPI selects an OPI machine to use.}
\usage{
chooseOPI(machine = NULL)

chooseOpi(machine = NULL)
}
\arguments{
\item{machine}{Machine name to use. Set to NULL to get a list.}
}
\value{
NULL on success or list of machines otherwise.
}
\description{
It should be called before any other OPI functions.
}
