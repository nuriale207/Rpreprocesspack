% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSet.R
\name{dataset}
\alias{dataset}
\title{Basic constructor of the \code{\linkS4class{Dataset}} class}
\usage{
dataset(data, name)
}
\arguments{
\item{data}{List containing \code{\linkS4class{Attribute}} objects}

\item{name}{Character with the name of the \code{\linkS4class{DataSet}}}
}
\value{
An object of class \code{\linkS4class{Dataset}}
}
\description{
This function creates an object of class \code{\linkS4class{DataSet}}
}
\examples{
attr1<-attribute(c(2,3,6,4))
attr2<-attribute(c(1.2,2.3,4.5,7.8))
dataset(c(attr1,attr2),"Data")
dataset(c(attr1,attr2))
}
