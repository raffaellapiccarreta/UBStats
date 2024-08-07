% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UBStats_Main_Visible_ALL_202406.R
\name{CI.diffprop}
\alias{CI.diffprop}
\title{Confidence intervals for the difference between proportions}
\usage{
CI.diffprop(
  x,
  y,
  success.x = NULL,
  success.y = NULL,
  conf.level = 0.95,
  by,
  digits = 2,
  force.digits = FALSE,
  use.scientific = FALSE,
  data,
  ...
)
}
\arguments{
\item{x, y}{Unquoted strings identifying the variables of
interest. \code{x} and \code{y} can be the
names of vectors or factors in the workspace or the
names of columns in the data frame
specified in the \code{data} argument.
It is possible to use a mixed specification
(e.g, one vector and one column in data).}

\item{success.x, success.y}{If \code{x,y} are factors, character
vectors, or numeric non-binary vectors, success must be used to indicate
the category/value corresponding to success in the populations. These
arguments can be omitted (\code{NULL}, default) if \code{x,y} are binary
numeric vectors (taking values 0 or 1 only; in this case success is
assumed to correspond to 1) or a logical vector (in these cases success
is assumed to correspond to \code{TRUE}).}

\item{conf.level}{Numeric value specifying the required
confidence level; default to 0.95.}

\item{by}{Optional unquoted string identifying a variable
(of any type), defined same way as \code{x},
taking only \bold{two} values used to split
\code{x} into two independent samples. Given the two
\emph{ordered} values taken by \code{by}
(alphabetical or numerical order,
or order of the levels for factors), say \emph{by1} and \emph{by2},
the confidence interval is built for the difference between the
populations proportions in the \emph{by1}- and  in the \emph{by2}-group.
Note that only \bold{one} between \code{y} and \code{by} can be
specified.}

\item{digits}{Integer value specifying the number of
decimals used to round statistics; default to 2. If the chosen rounding formats some
non-zero values as zero, the number of decimals is increased
so that all values have at least one significant digit, unless the argument
\code{force.digits} is set to \code{TRUE}.}

\item{force.digits}{Logical value indicating whether reported values
should be forcedly rounded to the number of decimals specified in
\code{digits} even if non-zero values are
rounded to zero (default to \code{FALSE}).}

\item{use.scientific}{Logical value indicating whether numbers
in tables should be displayed using
scientific notation (\code{TRUE}); default to \code{FALSE}.}

\item{data}{An optional data frame containing \code{x} and/or
\code{y}. If not found in \code{data}, the variables
are taken from the environment
from which \code{CI.diffprop()} is called.}

\item{...}{Additional arguments to be passed to low level functions.}
}
\value{
A table reporting the confidence intervals for the difference
between the proportions of successes in two independent populations.
}
\description{
\code{CI.diffprop()} builds confidence intervals for the difference
between the proportion of successes in two independent populations.
}
\examples{
data(MktDATA, package = "UBStats")

# Proportions of success defined on non-binary and 
#  non-logical vectors; 'success' coded same way
#  for both vectors
#  - Using x,y: build vectors with data on the two groups
WouldSuggest_F <- MktDATA$WouldSuggest[MktDATA$Gender == "F"]
WouldSuggest_M <- MktDATA$WouldSuggest[MktDATA$Gender == "M"]
CI.diffprop(x = WouldSuggest_M, y = WouldSuggest_F, 
            success.x = "Yes")

PastCampaigns_F<-MktDATA$PastCampaigns[MktDATA$Gender=="F"]
PastCampaigns_M<-MktDATA$PastCampaigns[MktDATA$Gender=="M"]
CI.diffprop(x = PastCampaigns_M, y = PastCampaigns_F,
            success.x = 0, conf.level = 0.99)
            
#  - Using x,by: groups identified by ordered levels of by
CI.diffprop(x = PastCampaigns, by = Gender,
            success.x=0, conf.level = 0.99, 
            data = MktDATA)
#    Since order is F, M, CI is for prop(F) - prop(M)
#    To get the interval for prop(M) - prop(F)
Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
CI.diffprop(x = PastCampaigns, by = Gender.R,
            success.x=0, conf.level = 0.99, data = MktDATA)
 
# Proportions of success defined based on 
#  binary or logical vectors; 'success'
#  coded same way for both vectors
#  - Binary variable (success=1): based on x,y
LastCampaign_F<-MktDATA$LastCampaign[MktDATA$Gender=="F"]
LastCampaign_M<-MktDATA$LastCampaign[MktDATA$Gender=="M"]
CI.diffprop(x = LastCampaign_M, y = LastCampaign_F)
#  - Binary variable (success=1): based on x,y
#    see above for recoding of levels of Gender
Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
CI.diffprop(x = LastCampaign, by = Gender.R, data = MktDATA)
#  - Logical variable (success=TRUE): based on x,y
Deals_w_child <- MktDATA$Deals.ge50[MktDATA$Children>0]
Deals_no_child <- MktDATA$Deals.ge50[MktDATA$Children==0]
CI.diffprop(x = Deals_w_child, y = Deals_no_child, conf.level = 0.9)

# Proportions defined on 
#  non-binary and non-logical vectors, with 'success'
#  coded differently (only specification x,y is reasonable here)
WouldSuggest_Other<-c(rep("OK",310),rep("KO",650-310))
CI.diffprop(x = WouldSuggest, y = WouldSuggest_Other, 
            success.x = "Yes", success.y = "OK",
            data = MktDATA)

# Proportions based on combined conditions
# - Build logical vector/s indicating whether a condition 
#   is satisfied
IsTop<-MktDATA$AOV>80
IsTop_OK<-IsTop[MktDATA$WouldSuggest == "Yes"]
IsTop_KO<-IsTop[MktDATA$WouldSuggest == "No"]
CI.diffprop(x = IsTop_OK, y = IsTop_KO, conf.level = 0.9)

Deals<-MktDATA$NDeals>=5
Deals_Married <- Deals[MktDATA$Marital_Status=="Married" & 
                         MktDATA$Children==0] 
Deals_Single <- Deals[MktDATA$Marital_Status=="Single"] 
CI.diffprop(x = Deals_Married, y = Deals_Single, conf.level = 0.9)

# Output results           
Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
out.ci_diffP<-CI.diffprop(x = PastCampaigns, by = Gender.R,
                          success.x=0, conf.level = 0.99, 
                          data = MktDATA)

# Arguments force.digits and use.scientific
#  An input variable taking very low values
HighAOV <- MktDATA$AOV>150
# - Default: manages possible excess of rounding
CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
            y = HighAOV[MktDATA$Gender=="F"])
#  - Force to the exact number of digits (default, 2)
CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
            y = HighAOV[MktDATA$Gender=="F"],
            force.digits = TRUE)
#  - Allow scientific notation
CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
            y = HighAOV[MktDATA$Gender=="F"],
            use.scientific = TRUE)

}
\seealso{
\code{\link{TEST.diffprop}()} to test hypotheses on the difference
between the proportions of successes in two populations.
}
\author{
Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
}
