% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UBStats_Main_TablesPlots.R
\name{distr.plot.x}
\alias{distr.plot.x}
\title{Analysis of a univariate distribution using plots}
\usage{
distr.plot.x(
  x,
  freq = "counts",
  plot.type,
  ord.freq = "none",
  breaks,
  adj.breaks = TRUE,
  interval = FALSE,
  bw = FALSE,
  color = NULL,
  data,
  ...
)
}
\arguments{
\item{x}{An unquoted string identifying the variable whose
distribution has to be analysed. \code{x} can be the name of a vector
or a factor in the workspace or the name of one of the columns in the
data frame specified in the \code{data} argument.}

\item{freq}{A single character providing the frequencies to be
displayed. Allowed values (possibly abbreviated) are \code{"counts"},
\code{"percentages"}, \code{"proportions"}, \code{"densities"}
(for histograms and density plots).}

\item{plot.type}{A single character providing the type of plot to build.
Allowed values are \code{"pie"}, \code{"bars"}, \code{"spike"},
\code{"histogram"}, \code{"density"}, \code{"boxplot"}, and
\code{"cumulative"}.}

\item{ord.freq}{A single character vector that is allowed only when
\code{plot.type = "pie"} or \code{plot.type = "bars"}. It specifies
whether the levels of \code{x} should be displayed in a standard order
(\code{ord.freq = "none"}) or in an increasing or decreasing order
(\code{ord.freq = "increasing"} or \code{ord.freq = "decreasing"}).}

\item{breaks}{Allows to classify a \emph{numerical} variable \code{x} into
intervals. It can be an integer specifying the number of intervals of
equal width used to classify \code{x}, or a vector of increasing numeric
values defining the endpoints of intervals (closed on the left and open
on the right; the last interval is closed on the right too). To cover
he entire range of values the maximum and the minimum values should be
included between the first and the last break. It is possible to specify
a set of breaks covering only a portion of the \code{x} range.}

\item{adj.breaks}{Logical value indicating whether numbers displayed
should avoid using scientific notation; default to \code{TRUE}.}

\item{interval}{Logical value indicating whether \code{x} is a variable
measured in intervals (\code{TRUE}). If the detected intervals are not
consistent (e.g. overlapping intervals, or intervals with upper endpoint
higher than the lower one), the variable is tabulated as it is, even if
results are not necessarily consistent.}

\item{bw}{Logical value indicating whether plots are coloured using a standard palette
(\code{FALSE}) rather than in scale of greys.}

\item{color}{String vector indicating the specific colors to use in the plot, or
\code{NULL} (defualt) which indicates to use a standard palette.}

\item{data}{An optional data frame containing the variable to be analysed.
If not found in \code{data}, the variables are taken from the environment
from which \code{distr.plot.x()} is called.}

\item{...}{Additional arguments to be passed to low level functions.}
}
\description{
\code{distr.plot.x()} generates plots of a univariate distribution.
}
\examples{
data(MktDATA, package = "UBStats")

# Pie charts
distr.plot.x(x = LikeMost, plot.type = "pie", bw = TRUE, data = MktDATA)

# Bar charts
distr.plot.x(x = Education, plot.type = "bars", freq = "percentage",
  ord.freq = "dec", data = MktDATA)

# Spike charts
distr.plot.x(x = NPickUp_Purch, plot.type = "spike", freq = "prop", data = MktDATA)

# Histograms
#   - no breaks provided
distr.plot.x(x = AOV, plot.type = "histogram", data = MktDATA)
#   - 10 equal width intervals
distr.plot.x(x = AOV, plot.type = "histogram", breaks = 10, data = MktDATA)
#   - with breaks
distr.plot.x(AOV, plot.type = "histogram", breaks = c(0, 20, 40, 60, 80, 100, 180), data = MktDATA)
distr.plot.x(Income, plot.type = "histogram", interval = TRUE, data = MktDATA)

# Density plot
distr.plot.x(Income, plot.type = "density", interval = TRUE, data = MktDATA)

# Boxplots
distr.plot.x(x = TotVal, plot.type = "boxplot", data = MktDATA)

# Cumulative distribution plots
distr.plot.x(x = Children, plot.type = "cum", freq = "percent", data = MktDATA)
distr.plot.x(Income, plot.type = "cum", interval = TRUE, data = MktDATA)

}
\seealso{
\code{\link{distr.table.x}()} for computing a univariate
distribution.

\code{\link{distr.table.xy}()} for computing a bivariate
distribution.

\code{\link{distr.plot.xy}()} for plotting a bivariate
distribution.
}
\author{
Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
}
