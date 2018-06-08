\name{sitc}
\alias{sitc}
\title{SITC products}
\docType{data}
\description{
Provides a detail of SITC codes meaning.
}
\usage{sitc}
\format{
  A data frame with 6472 observations on the following 5 variables.
  \describe{
    \item{\code{product_name}}{Contains the SITC products' names (e.g. horses, bovine, pigs, etc)}
    \item{\code{id}}{Contains the associated codes of every product (e.g. 0101 means horses)}
    \item{\code{group_name}}{Contains the SITC groups (e.g. animal products, vegetable products, etc)}
    \item{\code{group_id}}{Contains the associated codes of every group (e.g. 10 means animal products)}
    \item{\code{color}}{One colour per group used to create visualizations}
  }
}
\examples{
sitc
}
\keyword{datasets}
