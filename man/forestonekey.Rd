% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forestonekey.R
\name{forestonekey}
\alias{forestonekey}
\title{forestonekey}
\usage{
forestonekey(data, features, HR, l95, h95, pvalue, title = NULL)
}
\arguments{
\item{data}{the data.frame that is to be drawn}

\item{features}{the colname of which contains the features}

\item{HR}{the colname of which contains Hazard Rate}

\item{l95}{the colname of which contains the lower .95 CI}

\item{h95}{the colname of which contains the higher .95 CI}

\item{pvalue}{the colname of which contains the pvalue}

\item{title}{the title of the plot}
}
\value{
a ggplot object of a forest plot
}
\description{
forestonekey
}
\examples{
library(ggplot2)
unicox <- data.frame(
  id = c("a", "b", "c"),
  hr = c(0.5, 1, 1.5),
  low.95 = c(0.45, 0.9, 1.1),
  high.95 = c(0.55, 1.1, 1.9),
  p.value = c(0.01, 0.01, 0.01)
)

forestonekey(
  data = unicox,
  features = "id",
  l95 = "low.95",
  h95 = "high.95",
  HR = "hr",
  pvalue = "p.value",
  title = "TITLE"
)
}
