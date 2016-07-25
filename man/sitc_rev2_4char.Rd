\name{sitc_rev2_4char}
\alias{sitc_rev2_4char}
\docType{data}
\title{
SITC (rev 2.) products (4 characters)
}
\description{
This file contains the SITC (rev. 2) products. This file is used to create spreadsheets and visualizations with trade data.
}
\usage{sitc_rev2_4char}
\format{
  A data frame with 1242 observations on the following 5 variables.
  \describe{
    \item{\code{product}}{Contains the H292 products' names (e.g. initiating devices, polymerization ion exchangers, etc)}
    \item{\code{group}}{Contains the H292 groups (e.g. machinery, electronics products, etc)}
    \item{\code{product_id}}{Contains the associated codes of every product (e.g. initiating devices is 5722)}
    \item{\code{group_id}}{Contains the associated codes of every group (e.g. machinery is 10)}
    \item{\code{id}}{Contains the associated extended codes of every group (e.g. machinery is 105722 that is group code + product code)}
  }
}
\examples{
sitc_rev2_4char
}
\keyword{datasets}
