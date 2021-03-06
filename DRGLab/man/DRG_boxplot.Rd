% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DRG_boxplot.R
\name{DRG_boxplot}
\alias{DRG_boxplot}
\title{Boxplot Function}
\usage{
DRG_boxplot(fun.data, fun.y)
}
\arguments{
\item{fun.data}{This is the dataframe of DRG data to be used (DRG_data.csv in this lab)}

\item{fun.y}{This is the payment/charge variable to be plotted}
}
\value{
A box plot of the chosen fun.y payment or charge variable over different DRGs
}
\description{
This function creates a box plot of DRG codes vs. a chosen payment variable
}
\examples{
DRGData <- read.csv("DRG_data.csv")
DRG_boxplot(DRGdata, fun.y = "Average.Medicare.Payments")
}
