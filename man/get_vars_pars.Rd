% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vars_pars.R
\name{get_vars_pars}
\alias{get_vars_pars}
\title{Extract the variables and parameters from a flow. used by prepare_diagram}
\usage{
get_vars_pars(flow)
}
\arguments{
\item{flow}{A flow, which is a character string.}
}
\value{
A character vector of the variables and parameters, in order.
}
\description{
The flow must be a character string as used in flowdiagramr
and modelbuilder.
}
\details{
This function takes as input a single flow expression
and extracts the variables and parameters (excludes the
math notation).
}
\author{
Andrew Tredennick and Andreas Handel
}
