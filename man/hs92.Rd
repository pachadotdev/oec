\name{hs92}
\alias{hs92}
\title{HS92 products}
\docType{data}
\description{
Provides a detail of HS92 codes.
}
\usage{hs92}
\format{
  A data frame with 6282 observations on the following 5 variables.
  \describe{
    \item{\code{product_name}}{Contains the SITC products' names (e.g. horses, bovine, pigs, etc)}
    \item{\code{id}}{Contains the associated codes of every product (e.g. 0101 means horses)}
    \item{\code{group_name}}{Contains the SITC groups (e.g. animal products, vegetable products, etc)}
    \item{\code{group_id}}{Contains the associated codes of every group (e.g. 01 means animal products)}
    \item{\code{color}}{One colour per group used to create visualizations}
  }
}
\examples{
hs92
}
\keyword{datasets}
