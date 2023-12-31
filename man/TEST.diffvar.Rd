% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UBStats_Main_Inference.R
\name{TEST.diffvar}
\alias{TEST.diffvar}
\title{Test on the equality of variances}
\usage{
TEST.diffvar(x, y, by, digits = 2, data, ...)
}
\arguments{
\item{x, y}{Unquoted strings identifying the variables whose
distribution has to be analysed. \code{x} and \code{y} can be the
name of a vector or a factor in the workspace or the name of one of
the columns in the data frame specified in the \code{data} argument.
It is possible to use a mixed specification (e.g, one vector and one
column in data).}

\item{by}{Unquoted string identifying the variable used to
identify the two samples whose means have to be compared. Note that
only \bold{one} between \code{y} and \code{by} can be specified. More
specifically:
\itemize{
\item \code{x}, \code{y} should be specified when data on the two sub-samples
are stored in two distinct vectors,
\item \code{x}, \code{by} should be specified when data on the two sub-samples
should be obtained by splitting \code{x} into two groups based on the two
values of the by vector; in this case, \code{x} and \code{by} must have
the same length, \code{x} must be numeric, and \code{by} can take only
two values. Note that this option is available only for \bold{independent}
samples.
}}

\item{digits}{Integer value specifying the number of decimals used
to round statistics; default to 2.}

\item{data}{An optional data frame containing the variable to be analysed.
If not found in \code{data}, the variables are taken from the environment
from which \code{TEST.diffvar()} is called.}

\item{...}{Additional arguments to be passed to low level functions.}
}
\value{
A table reporting the tests for the population mean difference.
}
\description{
\code{TEST.diffvar()} allows testing the equality between the variances
of two independent samples.
}
\examples{
data(MktDATA, package = "UBStats")

MktDATA$Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
TEST.diffvar(x = AOV, by = Gender.R, data = MktDATA)

}
\seealso{
\code{\link{CI.diffmean}()} for computing confidence intervals on
the difference between two population means.

\code{\link{TEST.diffmean}()} for computing tests on the difference
between two population means.
}
\author{
Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
}
