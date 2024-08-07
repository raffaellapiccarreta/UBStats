% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UBStats_Main_Visible_ALL_202406.R
\name{distr.plot.xy}
\alias{distr.plot.xy}
\title{Analysis of a bivariate distribution using plots}
\usage{
distr.plot.xy(
  x,
  y,
  plot.type,
  bar.type = "stacked",
  freq = "counts",
  freq.type = "joint",
  breaks.x,
  breaks.y,
  interval.x = FALSE,
  interval.y = FALSE,
  bw = FALSE,
  color = NULL,
  var.c,
  breaks.c,
  interval.c = FALSE,
  adj.breaks = TRUE,
  fitline = FALSE,
  legend = TRUE,
  use.scientific = FALSE,
  data,
  ...
)
}
\arguments{
\item{x, y}{Unquoted strings identifying the variables whose
distribution has to be graphically displayed. \code{x} and
\code{y} can be the name of a vector or a factor in the
workspace or the name of one of the columns in the data frame
specified in the \code{data} argument.
Note that in the plot \code{x} is reported on the
\emph{horizontal} axis while \code{y} is reported on
the \emph{vertical} axis.}

\item{plot.type}{A single character specifying the type of plot to build.
Allowed options are \code{"bars"}, \code{"scatter"}, and
\code{"boxplot"}. If both \code{x} and \code{y} are character vectors
or factors and \code{bar.type = "scatter"} a bubble plot is
built, with dots having a size proportional to the joint frequency of
each pair of observed values. If \code{bar.type = "boxplot"}, at least
one input variable must be numeric; when both the variables are numeric
the conditional distributions of \code{y|x} are displayed, unless
otherwise specified using \code{freq.type="x|y"}.}

\item{bar.type}{A single character indicating whether in a bar plot
stacked (\code{bar.type = "stacked"}, default) or side-by-side
(\code{bar.type = "beside"}) bars should be displayed.}

\item{freq}{A single character specifying the frequencies
to be displayed when a bar plot is requested (\code{plot.type="bars"}).
Allowed options (possibly abbreviated) are \code{"counts"},
\code{"percentages"} and \code{"proportions"}.}

\item{freq.type}{A single character specifying the type of
frequencies to be displayed when a bar plot is requested
(\code{plot.type="bars"}). Allowed options are \code{joint} (default)
for joint frequencies, \code{x|y} for the distributions
of \code{x} conditioned to \code{y}, and \code{y|x} for
the distributions of \code{y} conditioned to \code{x}. The option
\code{x|y} can also be used when \code{plot.type="boxplot"}.}

\item{breaks.x, breaks.y}{Allow to classify the variables \code{x}
and/or \code{y}, if \emph{numerical}, into intervals.
They can be integers indicating the number of intervals of
equal width used to classify \code{x} and/or \code{y}, or
vectors of increasing numeric values defining the endpoints of
the intervals (closed on the left and open
on the right; the last interval is closed on the right too).
To cover the entire range of values taken by one variable,
the maximum and the minimum values should be included between
the first and the last break.
It is possible to specify a set of breaks covering only a portion
of the variable's range.}

\item{interval.x, interval.y}{Logical values indicating whether
\code{x} and/or \code{y} are variables measured in classes
(\code{TRUE}). If the detected intervals are not
consistent (e.g. overlapping intervals, or intervals with
upper endpoint higher than the lower one), the variable is
analyzed as it is, even if results are not necessarily
consistent; default to \code{FALSE}.}

\item{bw}{Logical value indicating whether plots should be colored
in scale of greys (\code{TRUE}) rather than using a standard
palette (\code{FALSE}, default).}

\item{color}{Optional string vector allowing to specify colors
to use in the plot rather than a standard palette
(\code{NULL}, default).}

\item{var.c}{An optional unquoted string identifying one variable
used to color points in a scatter plot (\code{plot.type="scatter"}),
that can be defined same way as \code{x}. This is allowed only when
at least one of the input variables \code{x} and \code{y} is numeric.}

\item{breaks.c}{Allows to classify the variable \code{var.c}, if
\emph{numerical}, into intervals. It can be defined as \code{breaks.x}.}

\item{interval.c}{Logical value indicating whether \code{var.c} is a variable
measured in intervals (\code{TRUE}) or not, as described for
\code{interval.x}; default to \code{FALSE}.}

\item{adj.breaks}{Logical value indicating whether the endpoints of
intervals of a numerical variable (\code{x}, or \code{y},
or \code{var.c}) when classified into intervals should be
displayed avoiding scientific notation; default to \code{TRUE}.}

\item{fitline}{Logical value indicating whether the line of best fit (also
called trend line or regression line) should be added to a scatter plot
(\code{fitline = TRUE}) or not (\code{fitline = FALSE}; default).}

\item{legend}{Logical value indicating whether a legend should be displayed
in the plot (\code{legend = TRUE}; default) or not (\code{legend = FALSE}).}

\item{use.scientific}{Logical value indicating whether numbers on
axes should be displayed using scientific notation
(\code{TRUE}); default to \code{FALSE}.}

\item{data}{An optional data frame containing \code{x} and/or \code{y}
and/or \code{var.c} (the variable used to color points in scatter plots).
If not found in \code{data}, the variables are taken from the environment
from which \code{distr.plot.xy()} is called.}

\item{...}{Additional arguments to be passed to low level functions.}
}
\value{
No return value, called for side effects.
}
\description{
\code{distr.plot.xy()} generates plots of a bivariate distribution.
}
\examples{
data(MktDATA, package = "UBStats")

# Bivariate bar plots
# - Two discrete variables (factor or vector with few levels)
#   Joint counts
distr.plot.xy(CustClass, Children,plot.type = "bars", 
              freq = "Counts", freq.type = "joint",
              data = MktDATA)
# - Two discrete variables (factor or vector with few levels)
#   Joint percentages, side-by-side bars
#   User-defined colors
distr.plot.xy(Children,CustClass, plot.type = "bars", 
              bar.type = "beside",
              freq = "percent", freq.type = "joint",
              color = c("red","gold","green","forestgreen"),
              data = MktDATA)
# - One numeric variable classified into intervals
#   and one variable measured in classes
#   Conditional percentages of x|y 
distr.plot.xy(TotPurch, Income, plot.type = "bars", 
              freq = "percent",freq.type = "x|y",
              breaks.x = c(0,5,10,15,20,35),
              interval.y = TRUE, data = MktDATA)
#   Conditional percentages of y|x 
distr.plot.xy(TotPurch, Income, plot.type = "bars", 
              freq = "percent",freq.type = "y|x",
              breaks.x = c(0,5,10,15,20,35),
              interval.y = TRUE, data = MktDATA)

# Side-by-side boxplots
# - A continuous variable conditioned to a factor, 
#   a character, or a classified variable
#   The distributions of the numeric variable conditioned
#   to the factor (or character) are displayed
distr.plot.xy(x = AOV, y = Education, plot.type = "boxplot",
              data = MktDATA)
distr.plot.xy(x = Income.S, y = AOV, plot.type = "boxplot",
              interval.x = TRUE, data = MktDATA)
distr.plot.xy(x = Baseline, y = TotPurch, plot.type = "boxplot",
              breaks.y = c(0,5,10,15,20,35),
              data = MktDATA)
# - Two numerical variables. By default distributions 
#   of y|x are displayed unless differently 
#   specified in freq.type
distr.plot.xy(x = NPickUp_Purch, y = NWeb_Purch,
              plot.type = "boxplot", data = MktDATA)
distr.plot.xy(x = NPickUp_Purch, y = NWeb_Purch,
              plot.type = "boxplot",freq.type = "x|y",
              data = MktDATA)

# Scatter plots
# - Two numerical variables: default options
distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
              fitline = TRUE, data = MktDATA)
# - Two numerical variables: colors based on discrete var 
distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
              var.c = Marital_Status,  
              fitline = TRUE, data = MktDATA)
distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
              var.c = Income, interval.c = TRUE, 
              fitline = TRUE, data = MktDATA)
distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
              var.c = TotPurch, breaks.c = 10, 
              fitline = TRUE, data = MktDATA)
# - Two numerical variables: colors based 
#   on a continuous numerical variable
distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
              var.c = AOV, fitline = TRUE, data = MktDATA)

# - One numerical variable and one factor or character 
distr.plot.xy(Baseline, Marital_Status, plot.type = "scatter", 
              fitline = TRUE, data = MktDATA)
distr.plot.xy(Income.S, Baseline, plot.type = "scatter", 
              interval.x = TRUE,
              fitline = TRUE, data = MktDATA)
#   color based on a third variable
distr.plot.xy(TotPurch, TotVal, plot.type = "scatter", 
              breaks.x = c(0,5,10,15,20,35),
              var.c = AOV,
              fitline = TRUE, data = MktDATA)

# - Two factors or character vectors: bubble plots
distr.plot.xy(Education, LikeMost, plot.type = "scatter", 
              data = MktDATA)
# - Two classified variables (i.e. not properly numerical): 
#   bubble plots, changed color
distr.plot.xy(Income.S, TotPurch, plot.type = "scatter",
              interval.x = TRUE,
              breaks.y = c(0,5,10,15,20,35),
              color = "orchid", data = MktDATA)

# Arguments adj.breaks and use.scientific 
#  Variable with very wide ranges
LargeC<-MktDATA$AOV*5000000 
LargeX<-MktDATA$Baseline*1000000 
LargeY<-MktDATA$TotVal*1000000
#  - Default: no scientific notation
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              var.c = LargeC, data = MktDATA)
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              breaks.x = 10, var.c = LargeC, 
              data = MktDATA)
#  - Scientific notation for axes 
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              breaks.x = 10, var.c = LargeC, 
              use.scientific = TRUE,
              data = MktDATA)
#  - Scientific notation for intervals' endpoints
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              breaks.x = 10, var.c = LargeC, 
              adj.breaks = FALSE,
              data = MktDATA)
#  - Scientific notation for intervals endpoints and axes
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              var.c = LargeC, fitline = TRUE, 
              adj.breaks = FALSE, use.scientific = TRUE,
              data = MktDATA)
distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
              breaks.x = 10, var.c = LargeC, 
              adj.breaks = FALSE, use.scientific = TRUE,
              data = MktDATA)

}
\seealso{
\code{\link{distr.table.xy}()} for tabulating a bivariate
distribution.

\code{\link{distr.table.x}()} for tabulating a univariate
distribution.

\code{\link{distr.plot.x}()} for plotting a univariate
distribution.
}
\author{
Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
}
