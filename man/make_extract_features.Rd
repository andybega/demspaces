% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic-reg-featx.R
\name{make_extract_features}
\alias{make_extract_features}
\title{Make feature extractor}
\usage{
make_extract_features(x, n_comp = 5)
}
\arguments{
\item{x}{a \code{\link[base:data.frame]{base::data.frame()}} or similar object with only feature variables}

\item{n_comp}{the number of components to keep}
}
\description{
Creates a feature extractor function
}
\examples{
library("dplyr")
library("stats")
data("states")

# assume that the input consists entirely of features, and no NAs
train_x <- states \%>\%
  filter(year < 2010) \%>\%
  select(-starts_with("dv_"), -gwcode, -year) \%>\%
  filter(complete.cases(.))

test_x <- states \%>\%
  filter(year > 2009) \%>\%
  select(-starts_with("dv_"), -gwcode, -year) \%>\%
  filter(complete.cases(.))

featx <- make_extract_features(train_x)
featx(train_x)

}
