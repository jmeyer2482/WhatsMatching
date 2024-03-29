% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_multimatchplot.R
\name{combined.plot}
\alias{combined.plot}
\title{Plot and visualise a comparison between matching methods.}
\usage{
combined.plot(xvar, yvar, M1, M2, te = NULL, outcome.f = y ~ t)
}
\arguments{
\item{xvar}{a scalar character of the name of the variable you want on the x-axis}

\item{yvar}{a scalar character of the name of the variable you want on the y-axis}

\item{M1}{the output from the `matched.data()` function}

\item{M2}{the output from the `matched.data()` function}

\item{te}{the treatment effect you would like displayed, can be NULL (default)}

\item{outcome.f}{a two-sided formula to estimate the the effect of treatment on the outcome, must at least contain the treatment variable on the right side (e.g. outcome ~ treatment)}
}
\value{
Returns a Plotly object with 2 matching comparisons as specified in M1 and M2 as well as a  plot showing the standardised mean differences between covariates and matching methods and a plot showing the estimated treatment effect for the two matching methods with the raw, and weighted and stratified propensity score estimates.
}
\description{
ReA fct function
}
