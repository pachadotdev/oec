\name{sitc}
\alias{sitc}
\title{SITC products}
\docType{data}
\description{
Provides a detail of SITC codes meaning.
}
\usage{sitc}
\format{
  A data frame with 988 observations on the following 5 variables.
  \describe{
    \item{\code{product_name}}{Contains the SITC products' names (e.g. initiating devices, polymerization ion exchangers, etc)}
    \item{\code{group_name}}{Contains the SITC groups (e.g. machinery, electronics products, etc)}
    \item{\code{group_id}}{Contains the associated codes of every group (e.g. animal products is 10)}
    \item{\code{sitc}}{Contains the associated codes of every product (e.g. initiating devices is 5722)}
    \item{\code{color}}{One colour per group used to create visualizations}
  }
}
\examples{
sitc
}
\keyword{datasets}
