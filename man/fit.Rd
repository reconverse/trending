% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit}
\alias{fit}
\alias{fit.default}
\title{Fit generic}
\usage{
fit(x, data, ...)

\method{fit}{default}(x, data, ...)
}
\arguments{
\item{x}{An \R object}

\item{data}{A data frame containing the data to fit.}

\item{...}{Arguments passed to underlying methods.}
}
\value{
The value returned depends on the class of the input argument.
}
\description{
\code{fit()} is a generic to fit a specified model.
}
\examples{
x = rnorm(100, mean = 0)
y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
dat <- data.frame(x = x, y = y)

poisson_model <- glm_model(y ~ x , family = poisson)
negbin_model <- glm_nb_model(y ~ x)

fit(poisson_model, dat)
fit(negbin_model, dat)
fit(list(poisson_model, negbin_model), dat)
fit(list(pm = poisson_model, nm = negbin_model), dat)

}
\seealso{
\code{\link[=fit.trending_model]{fit.trending_model()}} and \code{\link[=fit.list]{fit.list()}}
}
\author{
Tim Taylor
}
