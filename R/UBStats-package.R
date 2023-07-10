#' Model-Based Clustering of Several Dissimilarity Matrices.
#'
#' @docType package
#'
#' @name UBStats-package
#' @aliases UBStats-pkg
#' @aliases UBStats-p
#'
#' @description
#' The \pkg{UBStats} package implements a Bayesian algorithm for clustering a set
#' of dissimilarity matrices within a model-based framework. In particular,
#' we consider the case where \emph{S} matrices are available, each
#' describing the dissimilarities among \emph{n} objects, possibly expressed by
#' \emph{S} subjects (judges), or measured under different experimental conditions,
#' or with reference to different characteristics of the objects them- selves.
#' Specifically, we focus on binary dissimilarities, taking values 0 or 1
#' depending on whether or not two objects are deemed as similar, with the goal
#' of analyzing such data using multidimensional scaling (MDS). Differently
#' from the standard MDS algorithms, we are interested in partitioning the
#' dissimilarity matrices into clusters and, simultaneously, to extract a
#' specific MDS configuration for each cluster. The parameter estimates
#' are derived using a hybrid Metropolis-Gibbs Markov Chain Monte Carlo
#' algorithm. We also include a BIC-like criterion for jointly selecting the
#' optimal number of clusters and latent space dimensions.
#'
#' For efficiency reasons, the core computations in the package are implemented
#' using the \code{C} programming language and the \pkg{RcppArmadillo} package.
#'
#' The \pkg{UBStats} package also supports the simulation of multiple chains
#' through the support of the \pkg{parallel} package.
#'
#' Plotting functionalities are imported from the nice \pkg{bayesplot} package.
#' Currently, the package includes methods for binary data only. In future
#' releases routines will be added specifically for continuous (i.e. normal),
#' multinomial and count data.
#'
#' @section \pkg{UBStats} classes:
#' The \pkg{UBStats} package defines the following new classes:
#' \itemize{
#'   \item{\code{\link{UBStats_data}}: }{defines the data to use in a DMBC model.}
#'   \item{\code{\link{UBStats_model}}: }{defines a DMBC model.}
#'   \item{\code{\link{UBStats_fit}}: }{defines the results of a DMBC analysis
#'     for a single MCMC chain.}
#'   \item{\code{\link{UBStats_fit_list}}: }{defines the results of a DMBC analysis
#'     for multiple MCMC chains.}
#'   \item{\code{\link{UBStats_ic}}: }{defines the results of the computation of
#'     the information criterion for a DMBC analysis.}
#'   \item{\code{\link{UBStats_config}}: }{defines the estimate of the latent
#'     configuration for a DMBC analysis.}
#' }
#' The package includes \code{print}, \code{summary} and \code{plot} methods
#'   for each one of these classes.
#'
#' @section Resources:
#' \itemize{
#'  \item{\strong{Bug reports}:}{
#'  If you have noticed a bug that needs to be fixed, please let us know at the
#'  \pkg{UBStats} issue tracker on GitHub:
#'
#'  \url{https://github.com/raffaellapiccarreta/UBStats/issues/}.
#'  }
#'  \item{\strong{General questions and help}:}{
#'  To ask a question about \pkg{UBStats} send an email to:
#'
#'  \email{sergio.venturini@unicatt.it}.
#' }
#' }
#'
#' @seealso \code{\link[bayesplot]{theme_default}} for the default ggplot theme
#'  used by \pkg{bayesplot}.
#' @seealso \code{\link[bayesplot]{bayesplot-colors}} to set or view the color
#'  scheme used for plotting with \pkg{bayesplot}.
#' @seealso \code{\link[ggplot2]{ggsave}} in \pkg{ggplot2} for saving plots.
#'
#' @references
#'   Venturini, S., Piccarreta, R. (2021), "A Bayesian Approach for Model-Based
#'   Clustering of Several Binary Dissimilarity Matrices: the \pkg{UBStats}
#'   Package in \code{R}", Journal of Statistical Software, 100, 16, 1--35, <10.18637/jss.v100.i16>.
#'
#' @import graphics
#' @import stats
NULL

#' List of binary dissimilarity matrices among 15 MktDATA terms.
#'
#' @description{
#' Rosenberg and Kim (1975) designed an experiment to analyze the perceived
#'   similarities of 15 MktDATA terms.
#'
#'   Here, we consider the data relative to 85 females made available in
#'   Rosenberg (1982). Each subject was asked to group the MktDATA terms
#'   according to the perceived similarity. Thus, \emph{S} = 85 binary
#'   dissimilarity matrices are available whose elements (0 or 1) indicate
#'   whether or not two MktDATA terms were grouped together by each individual.
#' }
#'
#' @usage data(MktDATA)
#'
#' @format{
#'   A \code{\link{UBStats_data}} object whose \code{diss} element is a list of 85
#'   binary dissimilarity matrices. Each matrix is defined as a \code{dist}
#'   object measuring whether each pair of the 15 MktDATA terms is judged as
#'   similar (1) or not (0).
#'
#'   The \code{dist} objects have rows and columns that are named as follows:
#'   \describe{
#'     \item{GrF}{grandfather}
#'     \item{GrM}{grandmother}
#'     \item{GrD}{granddaughter}
#'     \item{GrS}{grandson}
#'     \item{Bro}{brother}
#'     \item{Sis}{sister}
#'     \item{Fat}{father}
#'     \item{Mot}{mother}
#'     \item{Dau}{daughter}
#'     \item{Son}{son}
#'     \item{Nep}{nephew}
#'     \item{Nie}{niece}
#'     \item{Cou}{cousin}
#'     \item{Aun}{aunt}
#'     \item{Unc}{uncle}
#'   }
#' }
#'
#' @references{
#'   Rosenberg, S. (1982). The method of sorting in multivariate research with
#'     applications selected from cognitive psychology and person perception. In
#'     N Hirschberg, LG Humphreys (eds.), Multivariate Applications in the Social
#'     Sciences, pp. 117–142. Erlbaum., Hillsdale, NJ.
#'
#'   Rosenberg, S., Kim, M. P. (1975). The method of sorting as a data-gathering
#'     procedure in multivariate research. Multivariate Behavioral Research, 10.
#' }
#'
#' @examples
#' data(MktDATA)
#' cols <- color_scheme_set("mix-red-blue")
#' plot(MktDATA, colors = unlist(cols)[c(1, 6)], font = 1, cex.font = 0.75)
"MktDATA"
