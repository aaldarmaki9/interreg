% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Multiple regression .R
\name{multreg}
\alias{multreg}
\title{Interactive Multiple Linear Regression}
\usage{
multreg(data)
}
\arguments{
\item{data}{an object of class dataframe.}
}
\value{
an interactive shiny application for choosing outcome and independent variables from the dataset inputted to run multiple linear regression on.
}
\description{
\code{multreg} builds an interactive multiple linear regression application for the inputted dataset.
}
\details{
This function builds an interactive multiple linear regression application for the inputted dataset, and includes model summary, distribution of variables,
linear assumptions plots, fitted and residuals graphs and table values, and correlation matrix of the variables.
}
\examples{
\dontrun{multreg(mtcars)}
}
