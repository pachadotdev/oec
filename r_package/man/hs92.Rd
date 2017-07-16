\name{hs92}
\alias{hs92}
\title{HS92 products}
\docType{data}
\description{
This file is used to create the visualizations and match product codes to product names and groups.
}
\usage{hs92}
\format{
  A data frame with 6282 observations on the following 5 variables.
  \describe{
    \item{\code{product_name}}{Contains the H292 products' names (e.g. horses, bovine, pigs, etc)}
    \item{\code{group_name}}{Contains the H292 groups (e.g. animal products, vegetable products, etc)}
    \item{\code{group_id}}{Contains the associated codes of every group (e.g. animal products is 01)}
    \item{\code{hs92}}{Contains the associated codes of every product (e.g. horses is 010101)}
    \item{\code{color}}{One colour per group used to create visualizations}
  }
}
\examples{
hs92
}
\keyword{datasets}
