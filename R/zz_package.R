#' UBStats: a package for teaching and learning introductory statistics
#'
#' @keywords internal
#' _PACKAGE
#'
#' @name UBStats-package
#' @aliases UBStats-pkg
#' @aliases UBStats-p
#' @aliases UBStats
#'
#' @description
#' The \pkg{UBStats} package has been developed to support
#' instructors and students involved in basic statistics courses
#' at Bocconi University (Milan, Italy). It contains a limited
#' number of functions, with very similar and coherent syntax,
#' to easily perform descriptive and inferential statistical analyses.
#'
#' The main functions included in the package are:
#' * \code{\link{distr.table.x}()} and \code{\link{distr.plot.x}()} to
#'   tabulate and plot the distribution of a single variable of any type
#' * \code{\link{distr.table.xy}()} and \code{\link{distr.plot.xy}()}
#'   to tabulate and plot the joint and conditional distributions for
#'   two variables and to build scatterplots
#' * \code{\link{distr.summary.x}()} and \code{\link{summaries.plot.x}()}
#'   to calculate summaries of univariate distributions and/or conditional
#'   summaries and to plot conditional location measures (means, medians
#'   or quantiles)
#' * \code{\link{CI.mean}()} and \code{\link{TEST.mean}()} to build
#'   confidence intervals and to test hypotheses on the mean
#' * \code{\link{CI.prop}()} and \code{\link{TEST.prop}()} to build
#'   confidence intervals and to test hypotheses on the proportion
#' * \code{\link{CI.diffmean}()} and \code{\link{TEST.diffmean}()} to
#'   build confidence intervals and to test hypotheses on the difference
#'   between means
#' * \code{\link{CI.diffprop}()} and \code{\link{TEST.diffprop}()} to
#'   build confidence intervals and to test hypotheses on the difference
#'   between proportions
#' * \code{\link{TEST.diffvar}()} to test hypotheses on the equality of
#'   two variances
#'
#' @details
#' The package has some distinctive features making it a convenient support
#' to students, attending courses on statistics using RStudio, approaching
#' the software for the first time.
#' 
#' Descriptive statistical analyses can be performed for any type of variables,
#' with easy and intuitive procedures to tabulate also numerical variables
#' classified into intervals and/or variables measured in intervals. Collections
#' of univariate summary statistics can be easily obtained, also for factors.
#' Univariate and bivariate distributions can be graphically displayed using two
#' functions only, with a quite rich set of (basic) options.
#' 
#' Inferential statistical analyses can be easily performed based on a reduced
#' set of functions with very similar syntax.
#' 
#' A peculiar characteristic of the package is that in the case of errors the
#' procedure is not immediately interrupted. The functions collect all the
#' possible warnings and error and print a list of all the encountered problems
#' before the possible interruption due to unmanageable errors. This makes the
#' package very convenient for students who approach the study of statistics
#' using RStudio for the first time.
#' 
#' To make suggestions about further developments please feel free to write
#' to \email{raffaella.piccarreta@unibocconi.it}.
#'
#' @import graphics
#' @import grDevices
#' @import stats
#' @import utils
#' 
#' @keywords internal
NULL
