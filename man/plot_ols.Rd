% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_ols}
\alias{plot_ols}
\title{Simple demo of OLS}
\usage{
plot_ols(
  n = 10,
  betas = c(intercept = 1, slope = 1),
  x = c(mean = 0, sd = 1),
  error = c(mean = 0, sd = 1),
  seed = NULL,
  show_raw_data = TRUE,
  ...
)
}
\arguments{
\item{n}{number of observations. Defaults to 10 (smaller `n` is easier to visualize).}

\item{betas}{vector of coefficients (intercept and slope). Defaults to \code{c(intercept = 1, slope = 1)}}

\item{x}{vector of parameters of the normal distribution of the independent variable. Defaults to \code{c(mean = 0, sd = 1)}}

\item{error}{vector of parameters of the normal distribution of the error term. Defaults to \code{c(mean = 0, sd = 1)}}

\item{seed}{seed for the random number generator. Defaults to NULL (i.e., varies each time you run it)}

\item{show_raw_data}{plot the raw data alongside the plot with the regression line. Defaults to \code{TRUE}}
}
\value{
\code{ggplot} object if \code{plot = TRUE}, otherwise a \code{data.frame}
}
\description{
Plot a random scatter of Y and X variables, then plot the OLS line with errors
}
\examples{
# default values are probably sufficient to the the main idea across
plot_ols()

# add some noise
plot_ols(error = c(mean = 0, sd = 10))

# add some more points and make the relationship negative
plot_ols(n=50, betas = c(intercept = 10, slope = -5), error = c(mean = 0, sd = 10))
}
\author{
Lawrence R. De Geest
}
