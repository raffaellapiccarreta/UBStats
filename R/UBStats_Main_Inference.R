#' Confidence intervals for the mean
#'
#' \code{CI.mean()} computes confidence intervals for the mean of a population.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param sigma An optional numeric value representing the population standard
#'   deviation. If \code{NULL} it estimates the population standard using the
#'   data.
#' @param conf.level Numeric value corresponding to the confidence level
#'   required. Default to 0.95.
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{CI.mean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the population mean.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.mean}()} for computing a test on the population
#'   mean.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # CI for the mean with KNOWN variance with default options
#' CI.mean(AOV, sigma = 30, data = MktDATA)
#' 
#' # CI for the mean with UNKNOWN variance,  confidence level 0.99
#' CI.mean(AOV, conf.level = 0.99, data = MktDATA)
#'
#' @export
CI.mean<-function(x,sigma = NULL,conf.level = 0.95, digits = 2,
                  data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  
  Err.list.input<-as.list("\nErrors found in the definition of input vector:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x ; Err.list.input<-check.x$err.list
  x<-check.x$vec.x
  
  # Check specifications para
  Err.list.para<-chkpar.sigma(value=sigma,err.list=Err.list.para)
  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for intervals
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  if (isTRUE(!is.null(sigma))){
    output<-ci.mean.known(x,sigma = sigma,conf.level = conf.level,
                          digits=digits,type.print=type.print) }
  if (isTRUE(is.null(sigma))){
    output<-ci.mean.unknown(x,conf.level = conf.level,
                            digits=digits,type.print=type.print)}
  out<-output
}

#' Confidence intervals for the proportion
#'
#' \code{CI.prop()} computes confidence intervals for the proportion of
#'   successes for a population.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param success If \code{x} is a factor, a character vector, or a numeric
#'   non-binary vector, success must be used to indicate the category/value
#'   corresponding to success. The argument can be omitted (\code{NULL},
#'   default) if \code{x} is a binary numeric vector (takes values 0 or 1 only;
#'   in this case success is assumed to be 1) or a logical vector (in these
#'   cases success is assumed to be \code{TRUE}).
#' @param conf.level Numeric value corresponding to the confidence level
#'   required. Default to 0.95.
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{CI.prop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the population
#'   proportion of successes.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.prop}()} for computing a test on the proportion
#'   of successes.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Character: a specific value
#' CI.prop(WouldSuggest, success = "Yes", data = MktDATA)
#' 
#' # Factor: a specific level
#' CI.prop(Education, success = "Post-Grad", conf.level = 0.9,
#'   digits = 4, data = MktDATA)
#' 
#' # Numeric vector: a specific value
#' CI.prop(Children, success = 2, conf.level = 0.99, data = MktDATA)
#' 
#' # CI for the proportion based on a binary variable (taking values 0/1)
#' CI.prop(LastCampaign, conf.level = 0.9, digits = 3, data = MktDATA)
#'
#' # Build a (logical) vector indicating whether a condition is satisfied
#' IsTop <- MktDATA$CustClass == "Gold" | MktDATA$CustClass == "Platinum"
#' CI.prop(IsTop, conf.level = 0.9, digits = 3, data = MktDATA)
#'
#' @export
CI.prop<-function(x, success=NULL,conf.level = 0.95, digits = 2,
                  data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of input vector:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  Err.list.input<-check.x$err.list
  x<-check.x$vec.x ; exist.x<-check.x$exist.x 
  
  # check x consistency
  if(is.null(success) && exist.x && 
      #(!is.numeric(x) |
      # !all(unique(na.omit(x)) %in% c(0,1)))){
     (is.character(x) |
       (is.numeric(x) & !all(unique(na.omit(x)) %in% c(0,1))))){
     Err.list.input<-c(Err.list.input,paste0("When no 'success' is specified, ",
                                            "'x' should be logical or a binary (0/1) vector!"))
  }
  # Check specifications para
  Err.list.para<-chkpar.success(value=success,x=x,err.list=Err.list.para)
  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for intervals
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  
  output<-ci.prop(x,success=success,conf.level = conf.level,
                  digits=digits,type.print=type.print)
  out<-output
}

#' Confidence intervals for the mean difference
#'
#' \code{CI.diffmean()} computes confidence intervals for the difference
#'   between the means of two populations.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Unquoted string identifying the variable used to
#'   identify the two samples whose means have to be compared. Note that
#'   only \bold{one} between \code{y} and \code{by} can be specified. More
#'   specifically:
#'   * \code{x}, \code{y} should be specified when data on the two sub-samples
#'     are stored in two distinct vectors,
#'   * \code{x}, \code{by} should be specified when data on the two sub-samples
#'     should be obtained by splitting \code{x} into two groups based on the two
#'     values of the by vector; in this case, \code{x} and \code{by} must have
#'     the same length, \code{x} must be numeric, and \code{by} can take only
#'     two values. Note that this option is available only for \bold{independent}
#'     samples.
#' @param type A length-one character vector specifying the type of samples.
#'   Allowed values are \code{"independent"} or \code{"paired"}.
#' @param sigma.x,sigma.y Optional numeric values representing the population
#'   standard deviations for the two population. If \code{NULL} they estimate
#'   the populations standard deviations using the data.
#' @param sigma.by An optional numeric value used to specify the possibly known
#'   standard deviations for the two independent samples identified via
#'   \code{by}. \code{sigma.by} can be a single value indicating the same
#'   standard deviation in the two by-groups, or a vector with two values,
#'   specifying the standard deviations in the two by-groups. To avoid errors,
#'   in the latter case the vector should be named, with names coinciding with
#'   the two levels of \code{by}.
#' @param sigma.d An optional numeric value that specify the possibly known
#'   standard deviation of the difference in \bold{paired} samples.
#' @param conf.level Numeric value corresponding to the confidence level
#'   required. Default to 0.95.
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param var.test Logical value indicating whether to run a test on the
#'   equality of variance in two (\bold{independent}) samples or not (default).
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{CI.diffmean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the population mean
#'   difference.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.diffmean}()} for computing a test on the difference
#'   between two population means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # CI for the difference between means in groups of males and females
#' # based on x, y: build vectors with data on the two groups
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' CI.diffmean(x = AOV_M, y = AOV_F, type = "independent")
#' 
#' # Same as above but using x, by: x split based on the by's (2) levels
#' MktDATA$Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' CI.diffmean(x = AOV, by = Gender.R, type = "independent", data = MktDATA)
#'
#' @export
CI.diffmean<-function(x,y,type="independent",sigma.x=NULL,sigma.y=NULL,sigma.by=NULL,
                      sigma.d=NULL,conf.level=0.95,digits=2,by,
                      var.test = FALSE,data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list; 
  x<-check.x$vec.x

  # Check required type
  check.type<-chkpar.option(value=type,allowed=c("paired","independent"),onlyone = TRUE,
                            err.list=Err.list.input,warn.list=Warn.list)
  Err.list.input<-check.type$err.list
  Warn.list<-check.type$warn.list
  exist.t<-check.type$exist
  type<-check.type$value

  # Check if 'y' or 'by' exist and are properly specified
  name.y<-deparse1(substitute(y))
  name.by<-deparse1(substitute(by))
  check.yby<-chkcon.diff(type="mean",err.list=Err.list.input,
                         type.s=type,
                         x=switch(exist.x,T=x,F=NULL),
                         y=y,by=by,data=data,name.y=name.y,name.by=name.by,
                         name.data=deparse1(substitute(data)),
                         sigma.d=sigma.d,sigma.x=sigma.x,
                         sigma.y=sigma.y,sigma.by=sigma.by)
  exist.y<-check.yby$exist.y; exist.by<-check.yby$exist.by
  y<-check.yby$y; by<-check.yby$by
  Err.list.input<-check.yby$err.list
  
  # check only properly stated variances
  if(exist.t && type=="paired"){
    Err.list.para<-chkpar.sigma(value=sigma.d,err.list=Err.list.para)
  }
  if(exist.t && type=="independent"){
    if(!exist.by){
      Err.list.para<-chkpar.sigma(value=sigma.x,err.list=Err.list.para)
      Err.list.para<-chkpar.sigma(value=sigma.y,err.list=Err.list.para)
    }
    if(exist.by){
      Err.list.para<-chkpar.sigma(value=sigma.by,onlyone = FALSE,err.list=Err.list.para)
    }
  }
  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)

  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }

    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # last.check
  if(!exist.x | (!exist.y & !exist.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  if(!is.null(sigma.d) & (!is.null(sigma.x) | !is.null(sigma.y)) & 
     !is.null(sigma.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  
  # Final steps for building ci
  known.var <- FALSE
  if(exist.y){
    use.x<-x; use.y<-y
    names.xy<-c("x"=name.x,"y"=name.y)
    if(!is.null(sigma.x) & !is.null(sigma.y)){known.var <- TRUE}
    if(is.null(sigma.y) & !is.null(sigma.x)){
      Warn.list<-c(Warn.list,"Only 'sigma.x' is specified: 'sigma.y' set equal to 'sigma.x'")
      sigma.y<-sigma.x ;  known.var <- TRUE
    }

    if(!is.null(sigma.y) & is.null(sigma.x)){
      Warn.list<-c(Warn.list,"Only 'sigma.y' is specified; 'sigma.x' set equal to 'sigma.y'")
      sigma.x<-sigma.y ;  known.var <- TRUE
    }
  }

  if(exist.by){
    use.by<-factor(by)
    use.x<-x[use.by==levels(use.by)[1]]
    use.y<-x[use.by==levels(use.by)[2]]
    names.xy<-c("x"=paste0(name.x,"|",name.by,"=",levels(use.by)[1]),
                "y"=paste0(name.x,"|",name.by,"=",levels(use.by)[2]))
    if(!is.null(sigma.by) && length(sigma.by)==2){
      known.var <- TRUE
      sigma.x<-sigma.by[levels(use.by)[1]] ; 
      sigma.y<-sigma.by[levels(use.by)[2]]
    }
    if(!is.null(sigma.by) && length(sigma.by)==1){
      known.var <- TRUE
      Warn.list<-c(Warn.list,"'sigma.by' has one element: equal variances assumed in 'by'-groups")
      sigma.x<-sigma.y<-sigma.by
    }
  }
  
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  # Ready for intervals
  if(type=="paired" & !is.null(sigma.d)){
    output<-ci.diff.paired_known(x=use.x,y=use.y,names.xy=names.xy,sigma.d=sigma.d,
                                 conf.level = conf.level,
                                 digits=digits,type.print=type.print) }
  if(type=="paired" & is.null(sigma.d)){
    output<-ci.diff.paired_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                   conf.level = conf.level,
                                   digits=digits,type.print=type.print) }
  if(type=="independent" & known.var){
    output<-ci.diff.indep_known(x=use.x,y=use.y,names.xy=names.xy,
                                sigma.x=sigma.x,sigma.y=sigma.y,
                                conf.level = conf.level,
                                digits=digits,type.print=type.print) }
  if(type=="independent" & !known.var){
    output<-ci.diff.indep_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                  conf.level = conf.level,
                                  digits=digits,var.test=var.test,
                                  type.print=type.print) }
}

#' Confidence intervals for the proportion difference
#'
#' \code{CI.diffprop()} computes confidence intervals for the difference
#'   between the proportion of successes of two populations.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Unquoted string identifying the variable used to
#'   identify the two samples whose means have to be compared. Note that
#'   only \bold{one} between \code{y} and \code{by} can be specified. More
#'   specifically:
#'   * \code{x}, \code{y} should be specified when data on the two sub-samples
#'     are stored in two distinct vectors,
#'   * \code{x}, \code{by} should be specified when data on the two sub-samples
#'     should be obtained by splitting \code{x} into two groups based on the two
#'     values of the by vector; in this case, \code{x} and \code{by} must have
#'     the same length, \code{x} must be numeric, and \code{by} can take only
#'     two values. Note that this option is available only for \bold{independent}
#'     samples.
#' @param success.x,success.y If \code{x,y} are factors, character
#'   vectors, or numeric non-binary vectors, success must be used to indicate
#'   the category/value corresponding to success in the populations. These
#'   arguments can be omitted (\code{NULL}, default) if \code{x,y} are binary
#'   numeric vectors (taking values 0 or 1 only; in this case success is
#'   assumed to correspond to 1) or a logical vector (in these cases success
#'   is assumed to correspond to \code{TRUE}).
#' @param conf.level Numeric value corresponding to the confidence level
#'   required. Default to 0.95.
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{CI.diffprop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the population
#'   proportion difference.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.diffprop}()} for computing a test on the difference
#'   between two population proportions of success.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Based on x,y: build vectors with data on the two groups
#' WouldSuggest_F <- MktDATA$WouldSuggest[MktDATA$Gender == "F"]
#' WouldSuggest_M <- MktDATA$WouldSuggest[MktDATA$Gender == "M"]
#' CI.diffprop(x = WouldSuggest_M, y = WouldSuggest_F, success.x = "Yes",
#'   conf.level = 0.99, digits = 2)
#' 
#' # CI for difference between proportion based on logical variable
#' NoChildren <- (MktDATA$Children == 0)
#' Deals_w_child <- MktDATA$Deals.ge50[!NoChildren]
#' Deals_no_child <- MktDATA$Deals.ge50[NoChildren]
#' CI.diffprop(x = Deals_w_child, y = Deals_no_child, conf.level = 0.9,
#'   digits = 3)
#'
#' @export
CI.diffprop<-function(x,y,success.x=NULL,success.y=NULL,
                      conf.level=0.95,digits=2,by,
                      data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list; 
  x<-check.x$vec.x
  
  # Check if 'y' or 'by' exist and are properly specified
  name.y<-deparse1(substitute(y))
  name.by<-deparse1(substitute(by))
  check.yby<-chkcon.diff(type="prop",err.list=Err.list.input,
                         x=switch(exist.x,T=x,F=NULL),
                         y=y,by=by,data=data,name.y=name.y,name.by=name.by,
                         name.data=deparse1(substitute(data)))
  exist.y<-check.yby$exist.y; exist.by<-check.yby$exist.by
  y<-check.yby$y; by<-check.yby$by
  Err.list.input<-check.yby$err.list
  
  
  # check x consistency
  if(is.null(success.x) && exist.x && 
     (is.character(x) |
      (is.numeric(x) & !all(unique(na.omit(x)) %in% c(0,1))))){
    Err.list.input<-c(Err.list.input,paste0("When no 'success.x' is specified, ",
                                            "'x' should be logical or a binary (0/1) vector!"))
  }
  if(is.null(success.y) && is.null(success.x) && exist.y && 
     (is.character(y) |
      (is.numeric(y) & !all(unique(na.omit(y)) %in% c(0,1))))){
    Err.list.input<-c(Err.list.input,paste0("When no 'success.y' or 'success.x' is specified, ",
                                            "'y' should be logical or a binary (0/1) vector!"))
  }
  if(!is.null(success.y) && !exist.y && exist.by){
    Err.list.input<-c(Err.list.input,"'success.y' not allowed with 'by'!")
  }
  
  # Check specifications para
  Err.list.para<-chkpar.success(value=success.x,x=x,err.list=Err.list.para)
  if(is.null(success.y) && !is.null(success.x) && exist.y && 
     (!is.numeric(y) | !all(unique(y) %in% c(0,1)))){
    Warn.list<-c(Warn.list,"Only 'success.x' is specified; 'success.y' set equal to 'success.x'")
    Err.list.para<-chkpar.success(value=success.x,x=y,err.list=Err.list.para)
  }
  Err.list.para<-chkpar.success(value=success.y,x=y,err.list=Err.list.para)
  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # last.check
  if(!exist.x | (!exist.y & !exist.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  
  # Final steps for building ci
  if(exist.y){
    use.x<-x; use.y<-y
    names.xy<-c("x"=name.x,"y"=name.y,"name.by"="NONE")
    if(is.null(success.y) & !is.null(success.x) && 
       (!is.numeric(y) | !all(unique(y) %in% c(0,1)))){success.y<-success.x}
  }
  if(exist.by){
    use.by<-factor(by)
    use.x<-x[use.by==levels(use.by)[1]]
    use.y<-x[use.by==levels(use.by)[2]]
    names.xy<-c("x"=name.x,"name.by"=name.by,"lev1"=levels(use.by)[1],
                "y"=name.x,"lev2"=levels(use.by)[2])
    if(!is.null(success.x)){success.y<-success.x}
  }
  
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  # Ready for intervals
  output<-ci.diff.prop(x=use.x,y=use.y,names.xy,success.x,success.y,
                       conf.level = conf.level,
                       digits=digits,type.print=type.print) 
}

#' Test for the mean
#'
#' \code{TEST.mean()} performs tests on the mean of a population.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param sigma An optional numeric value representing the population standard
#'   deviation. If \code{NULL} it estimates the population standard using the
#'   data.
#' @param mu0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   population mean differs from \code{mu0}; default), or \code{"less"}
#'   (population mean is lower than \code{mu0}), or \code{"greater"}
#'   (population mean is higher than \code{mu0}).
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{TEST.mean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the test for the population mean.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.mean}()} for computing confidence intervals on the
#'   population mean.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Bilateral test, known variance
#' TEST.mean(NStore_Purch, sigma = 9, mu0 = 5, alternative = "two.sided",
#'   data = MktDATA)
#'
#' # Unilateral test,  unknown variance
#' TEST.mean(TotVal, mu0 = 600, alternative = "less", data = MktDATA)
#'
#' @export
TEST.mean<-function(x,sigma = NULL,mu0=0,alternative="two.sided",
                    digits = 2,
                    data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of input vector:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  if(op.sci==0){options(scipen=10)}
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x ; Err.list.input<-check.x$err.list
  x<-check.x$vec.x
  
  # Check specifications para
  if(!is.numeric(mu0)){
    Err.list.para<-c(Err.list.para,"'mu0' should be a number")}
  Err.list.para<-chkpar.sigma(value=sigma,err.list=Err.list.para)
  check.alternative<-chkpar.option(value=alternative,
                                   allowed=c("two.sided","less","greater"),
                                   onlyone = TRUE,err.list=Err.list.para,
                                   warn.list=Warn.list)
  Err.list.para<-check.alternative$err.list
  Warn.list<-check.alternative$warn.list
  exist.a<-check.alternative$exist
  alternative<-check.alternative$value
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for test
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  if (isTRUE(!is.null(sigma))){
    output<-hyp.mean.known(x,sigma = sigma,mu0=mu0,alternative=alternative,
                           digits=digits,type.print=type.print) }
  if (isTRUE(is.null(sigma))){
    output<-hyp.mean.unknown(x,mu0=mu0,alternative=alternative,
                             digits=digits,type.print=type.print)}
  out<-output
}

#' Test for the proportion
#'
#' \code{TEST.prop()} performs tests on the proportion of successes for a
#'   population.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param success If \code{x} is a factor, a character vector, or a numeric
#'   non-binary vector, success must be used to indicate the category/value
#'   corresponding to success. The argument can be omitted (\code{NULL},
#'   default) if \code{x} is a binary numeric vector (takes values 0 or 1 only;
#'   in this case success is assumed to be 1) or a logical vector (in these
#'   cases success is assumed to be \code{TRUE}).
#' @param p0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   population mean differs from \code{p0}; default), or \code{"less"}
#'   (population mean is lower than \code{p0}), or \code{"greater"}
#'   (population mean is higher than \code{p0}).
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{TEST.prop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the tests for the population proportion of
#'   successes.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.prop}()} for computing confidence intervals on the
#'   proportion of successes.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Bilateral test (character or factor: specific value/level)
#' TEST.prop(WouldSuggest, success = "Yes", p0 = 0.7, data = MktDATA)
#' 
#' # Unilateral test (binary variable - takes values 0/1)
#' TEST.prop(LastCampaign, p0 = 0.2, alternative = "less", digits = 3,
#'   data = MktDATA)
#' 
#' # Unilateral test (logical variable,  taking values TRUE/FALSE)
#' TEST.prop(Deals.ge50, p0 = 0.13, alternative = "greater", digits = 3,
#'   data = MktDATA)
#'
#' @export
TEST.prop<-function(x, success=NULL,p0=0.5,
                    alternative = "two.sided", digits = 2,
                    data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of input vector:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  if(op.sci==0){options(scipen=10)}
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  Err.list.input<-check.x$err.list
  x<-check.x$vec.x ; exist.x<-check.x$exist.x 
  
  # check x consistency
  # if(is.null(success) && exist.x && 
  #    (!is.numeric(x) | !all(unique(na.omit(x)) %in% c(0,1)))){
  #   Err.list.input<-c(Err.list.input,paste0("When no 'success' is specified, ",
  #                                           "'x' should be a vector of 0s and 1s!"))
  # }

  if(is.null(success) && exist.x && 
     (is.character(x) |
      (is.numeric(x) & !all(unique(na.omit(x)) %in% c(0,1))))){
    Err.list.input<-c(Err.list.input,paste0("When no 'success' is specified, ",
                                            "'x' should be logical or a binary (0/1) vector!"))
  }  
  # Check specifications para
  if(!is.numeric(p0)){
    Err.list.para<-c(Err.list.para,"'p0' should be a number")
  } else if(is.numeric(p0) && (p0<=0 | p0>=1) ){
    Err.list.para<-c(Err.list.para,"'p0' should be a number between 0 and 1")
  }
  Err.list.para<-chkpar.success(value=success,x=x,err.list=Err.list.para)
  check.alternative<-chkpar.option(value=alternative,
                                   allowed=c("two.sided","less","greater"),
                                   onlyone = TRUE,
                                   err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.alternative$err.list
  Warn.list<-check.alternative$warn.list
  exist.a<-check.alternative$exist
  alternative<-check.alternative$value
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for intervals
  
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  output<-hyp.prop(x,success,p0=p0,alternative=alternative,
                   digits=digits,type.print=type.print)
  out<-output
}

#' Tests for the mean difference
#'
#' \code{TEST.diffmean()} computes tests for the difference between the
#'   means of two populations.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Unquoted string identifying the variable used to
#'   identify the two samples whose means have to be compared. Note that
#'   only \bold{one} between \code{y} and \code{by} can be specified. More
#'   specifically:
#'   * \code{x}, \code{y} should be specified when data on the two sub-samples
#'     are stored in two distinct vectors,
#'   * \code{x}, \code{by} should be specified when data on the two sub-samples
#'     should be obtained by splitting \code{x} into two groups based on the two
#'     values of the by vector; in this case, \code{x} and \code{by} must have
#'     the same length, \code{x} must be numeric, and \code{by} can take only
#'     two values. Note that this option is available only for \bold{independent}
#'     samples.
#' @param type A length-one character vector specifying the type of samples.
#'   Allowed values are \code{"independent"} or \code{"paired"}.
#' @param sigma.x,sigma.y Optional numeric values representing the population
#'   standard deviations for the two population. If \code{NULL} they estimate
#'   the populations standard deviations using the data.
#' @param sigma.by An optional numeric value used to specify the possibly known
#'   standard deviations for the two independent samples identified via
#'   \code{by}. \code{sigma.by} can be a single value indicating the same standard
#'   deviation in the two by-groups, or a vector with two values, specifying the
#'   standard deviations in the two by-groups. To avoid errors, in the latter case
#'   the vector should be named, with names coinciding with the two levels of
#'   \code{by}.
#' @param sigma.d An optional numeric value that specify the possibly known
#'   standard deviation of the difference in \bold{paired} samples.
#' @param mdiff0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   population mean differs from \code{mdiff0}; default), or \code{"less"}
#'   (population mean is lower than \code{mdiff0}), or \code{"greater"}
#'   (population mean is higher than \code{mdiff0}).
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param var.test Logical value indicating whether to run a test on the
#'   equality of variance in two (\bold{independent}) samples or not (default).
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{TEST.diffmean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the tests for the population mean difference.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffmean}()} for computing confidence intervals on
#'   the difference between two population means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Unilateral test for the mean difference in groups of males and females
#' # based on x, y: build vectors with data on the two groups
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' TEST.diffmean(x = AOV_M, y = AOV_F, type = "independent", mdiff0 = 30,
#'   alternative = "two.sided", var.test = TRUE)
#' 
#' # Same as above but using x, by: x split based on the by's (2) levels
#' MktDATA$Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffmean(x = AOV, by = Gender.R, type = "independent", mdiff0 = 30,
#'   alternative = "two.sided", var.test = TRUE, data = MktDATA)
#'
#' @export
TEST.diffmean<-function(x,y,type="independent",mdiff0=0,sigma.x=NULL,
                        sigma.y=NULL,sigma.by=NULL,sigma.d=NULL,
                        alternative="two.sided",digits=2,by,
                        var.test = FALSE,data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  if(op.sci==0){options(scipen=10)}
  
  # All requests checked for coherency before stopping if one is not ok
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list; 
  x<-check.x$vec.x
  
  # Check required type
  check.type<-chkpar.option(value=type,allowed=c("paired","independent"),onlyone = TRUE,
                            err.list=Err.list.input,warn.list=Warn.list)
  Err.list.input<-check.type$err.list
  Warn.list<-check.type$warn.list
  exist.t<-check.type$exist
  type<-check.type$value
  
  # Check if 'y' or 'by' exist and are properly specified
  name.y<-deparse1(substitute(y))
  name.by<-deparse1(substitute(by))
  check.yby<-chkcon.diff(type="mean",err.list=Err.list.input,
                         type.s=type,
                         x=switch(exist.x,T=x,F=NULL),
                         y=y,by=by,data=data,name.y=name.y,name.by=name.by,
                         name.data=deparse1(substitute(data)),
                         sigma.d=sigma.d,sigma.x=sigma.x,
                         sigma.y=sigma.y,sigma.by=sigma.by)
  exist.y<-check.yby$exist.y; exist.by<-check.yby$exist.by
  y<-check.yby$y; by<-check.yby$by
  Err.list.input<-check.yby$err.list
  
  # Check specifications para
  # check only properly stated variances
  if(!is.numeric(mdiff0)){
    Err.list.para<-c(Err.list.para,"'mdiff0' should be a number")
  }
  if(exist.t && type=="paired"){
    Err.list.para<-chkpar.sigma(value=sigma.d,err.list=Err.list.para)
  }
  if(exist.t && type=="independent"){
    if(!exist.by){
      Err.list.para<-chkpar.sigma(value=sigma.x,err.list=Err.list.para)
      Err.list.para<-chkpar.sigma(value=sigma.y,err.list=Err.list.para)
    }
    if(exist.by){
      Err.list.para<-chkpar.sigma(value=sigma.by,onlyone = FALSE,err.list=Err.list.para)
    }
  }
  check.alternative<-chkpar.option(value=alternative,
                                   allowed=c("two.sided","less","greater"),
                                   onlyone = TRUE,err.list=Err.list.para,
                                   warn.list=Warn.list)
  Err.list.para<-check.alternative$err.list
  Warn.list<-check.alternative$warn.list
  exist.a<-check.alternative$exist
  alternative<-check.alternative$value
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # last.check
  if(!exist.x | (!exist.y & !exist.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  if(!is.null(sigma.d) & (!is.null(sigma.x) | !is.null(sigma.y)) & 
     !is.null(sigma.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  
  # Final steps for building ci
  known.var <- FALSE
  if(exist.y){
    use.x<-x; use.y<-y
    names.xy<-c("x"=name.x,"y"=name.y)
    if(!is.null(sigma.x) & !is.null(sigma.y)){known.var <- TRUE}
    if(is.null(sigma.y) & !is.null(sigma.x)){
      Warn.list<-c(Warn.list,"Only 'sigma.x' is specified: 'sigma.y' set equal to 'sigma.x'")
      sigma.y<-sigma.x
      known.var <- TRUE
    }
    if(!is.null(sigma.y) & is.null(sigma.x)){
      Warn.list<-c(Warn.list,"Only 'sigma.y' is specified; 'sigma.x' set equal to 'sigma.y'")
      sigma.x<-sigma.y
      known.var <- TRUE
    }
  }
  if(exist.by){
    use.by<-factor(by)
    use.x<-x[use.by==levels(use.by)[1]]
    use.y<-x[use.by==levels(use.by)[2]]
    names.xy<-c("x"=paste0(name.x,"|",name.by,"=",levels(use.by)[1]),
                "y"=paste0(name.x,"|",name.by,"=",levels(use.by)[2]))
    if(!is.null(sigma.by) && length(sigma.by)==2){
      known.var <- TRUE
      sigma.x<-sigma.by[levels(use.by)[1]] ; 
      sigma.y<-sigma.by[levels(use.by)[2]]
    }
    if(!is.null(sigma.by) && length(sigma.by)==1){
      known.var <- TRUE
      Warn.list<-c(Warn.list,"'sigma.by' has one element: equal variances assumed in 'by'-groups")
      sigma.x<-sigma.y<-sigma.by
    }
  }
  
  # if(length(Warn.list)>1){
  #   invisible(lapply(Warn.list[!duplicated(Warn.list)],
  #                    function(x) cat(paste0("\n   ",x),file=stderr())))  
  #   cat("\n")
  # }
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  # Ready for tests
  if(type=="paired" & !is.null(sigma.d)){
    output<-hyp.diff.paired_known(x=use.x,y=use.y,names.xy=names.xy,
                                  mdiff0=mdiff0,sigma.d=sigma.d,
                                  alternative = alternative,
                                  digits=digits,type.print=type.print) }
  if(type=="paired" & is.null(sigma.d)){
    output<-hyp.diff.paired_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                    mdiff0=mdiff0,
                                    alternative=alternative,
                                    digits=digits,type.print=type.print) }
  if(type=="independent" & known.var){
    output<-hyp.diff.indep_known(x=use.x,y=use.y,names.xy=names.xy,
                                 mdiff0=mdiff0,sigma.x=sigma.x,
                                 sigma.y=sigma.y,
                                 alternative = alternative,
                                 digits=digits,type.print=type.print) }
  if(type=="independent" & !known.var){
    output<-hyp.diff.indep_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                   mdiff0=mdiff0,var.test=var.test,
                                   alternative = alternative,
                                   digits=digits,type.print=type.print) }
}

#' Tests for the proportion difference
#'
#' \code{TEST.diffprop()} computes tests for the difference between the
#'   proportion of successes of two populations.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Unquoted string identifying the variable used to
#'   identify the two samples whose means have to be compared. Note that
#'   only \bold{one} between \code{y} and \code{by} can be specified. More
#'   specifically:
#'   * \code{x}, \code{y} should be specified when data on the two sub-samples
#'     are stored in two distinct vectors,
#'   * \code{x}, \code{by} should be specified when data on the two sub-samples
#'     should be obtained by splitting \code{x} into two groups based on the two
#'     values of the by vector; in this case, \code{x} and \code{by} must have
#'     the same length, \code{x} must be numeric, and \code{by} can take only
#'     two values. Note that this option is available only for \bold{independent}
#'     samples.
#' @param success.x,success.y If \code{x,y} are factors, character
#'   vectors, or numeric non-binary vectors, success must be used to indicate
#'   the category/value corresponding to success in the populations. These
#'   arguments can be omitted (\code{NULL}, default) if \code{x,y} are binary
#'   numeric vectors (taking values 0 or 1 only; in this case success is
#'   assumed to correspond to 1) or a logical vector (in these cases success
#'   is assumed to correspond to \code{TRUE}).
#' @param pdiff0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   population mean differs from \code{pdiff0}; default), or \code{"less"}
#'   (population mean is lower than \code{pdiff0}), or \code{"greater"}
#'   (population mean is higher than \code{pdiff0}).
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{TEST.diffprop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the tests for the population proportion difference.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffprop}()} for computing confidence intervals on
#'   the difference between two population proportions.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Input vectors: Numeric, based on x, y
#' PastCampaigns_F <- MktDATA$PastCampaigns[MktDATA$Gender == "F"]
#' PastCampaigns_M <- MktDATA$PastCampaigns[MktDATA$Gender == "M"]
#' TEST.diffprop(x = PastCampaigns_F, y = PastCampaigns_M, success.x = 0,
#'  pdiff0 = 0.1, alternative = "greater")
#' 
#' # Test for the proportion difference based on logical variables
#' NoChildren <- (MktDATA$Children == 0)
#' Deals_w_child <- MktDATA$Deals.ge50[!NoChildren]
#' Deals_no_child <- MktDATA$Deals.ge50[NoChildren]
#' TEST.diffprop(x = Deals_w_child, y = Deals_no_child, pdiff0 = 0.2,
#'   alternative = "less", digits = 3)
#'
#' @export
TEST.diffprop<-function(x,y,success.x=NULL,success.y=NULL,pdiff0=0,
                        alternative="two.sided",digits=2,by,
                        data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  if(op.sci==0){options(scipen=10)}
  
  # All requests checked for coherency before stopping if one is not ok
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list; 
  x<-check.x$vec.x
  
  # Check if 'y' or 'by' exist and are properly specified
  name.y<-deparse1(substitute(y))
  name.by<-deparse1(substitute(by))
  check.yby<-chkcon.diff(type="prop",err.list=Err.list.input,
                         x=switch(exist.x,T=x,F=NULL),
                         y=y,by=by,data=data,name.y=name.y,name.by=name.by,
                         name.data=deparse1(substitute(data)))
  exist.y<-check.yby$exist.y; exist.by<-check.yby$exist.by
  y<-check.yby$y; by<-check.yby$by
  Err.list.input<-check.yby$err.list
  
  # check x consistency
  if(is.null(success.x) && exist.x && 
    (is.character(x) |
       (is.numeric(x) & !all(unique(na.omit(x)) %in% c(0,1))))){
    Err.list.input<-c(Err.list.input,paste0("When no 'success.x' is specified, ",
                                            "'x' should be logical or a binary (0/1) vector!"))
  }
  if(is.null(success.y) && is.null(success.x) && exist.y && 
     (is.character(y) |
      (is.numeric(y) & !all(unique(na.omit(y)) %in% c(0,1))))){
    Err.list.input<-c(Err.list.input,paste0("When no 'success.y' or 'success.x' is specified, ",
                                            "'y' should be logical or a binary (0/1) vector!"))
  }
  if(!is.null(success.y) && !exist.y && exist.by){
    Err.list.input<-c(Err.list.input,"'success.y' not allowed with 'by'!")
  }
  
  # Check specifications para
  if(!is.numeric(pdiff0)){
    Err.list.para<-c(Err.list.para,"'pdiff0' should be a number")
  } else if(is.numeric(pdiff0) && abs(pdiff0)>1){
    Err.list.para<-c(Err.list.para,"'pdiff0' should be a number between -1 and 1")
  }
  Err.list.para<-chkpar.success(value=success.x,x=x,err.list=Err.list.para)
  if(is.null(success.y) && !is.null(success.x) && exist.y && 
     (!is.numeric(y) | !all(unique(y) %in% c(0,1)))){
    Warn.list<-c(Warn.list,"Only 'success.x' is specified; 'success.y' set equal to 'success.x'")
    Err.list.para<-chkpar.success(value=success.x,x=y,err.list=Err.list.para)
  }
  Err.list.para<-chkpar.success(value=success.y,x=y,err.list=Err.list.para)
  check.alternative<-chkpar.option(value=alternative,
                                   allowed=c("two.sided","less","greater"),
                                   onlyone = TRUE,
                                   err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.alternative$err.list
  Warn.list<-check.alternative$warn.list
  exist.a<-check.alternative$exist
  alternative<-check.alternative$value
  
  # If there are errors, print errors and stop
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # last.check
  if(!exist.x | (!exist.y & !exist.by)){
    cat("\nSomething is wrong: please check the syntax",file=stderr())  
    cat("\nThe procedure is interrupted",file=stderr())  
    stop_quietly()}
  
  # Final steps for building ci
  if(exist.y){
    use.x<-x; use.y<-y
    names.xy<-c("x"=name.x,"y"=name.y,"name.by"="NONE")
    if(is.null(success.y) & !is.null(success.x) && 
       (!is.numeric(y) | !all(unique(y) %in% c(0,1)))){success.y<-success.x}
  }
  if(exist.by){
    use.by<-factor(by)
    use.x<-x[use.by==levels(use.by)[1]]
    use.y<-x[use.by==levels(use.by)[2]]
    names.xy<-c("x"=name.x,"name.by"=name.by,"lev1"=levels(use.by)[1],
                "y"=name.x,"lev2"=levels(use.by)[2])
    if(!is.null(success.x)){success.y<-success.x}
  }
  
  # if(length(Warn.list)>1){
  #   invisible(lapply(Warn.list[!duplicated(Warn.list)],
  #                    function(x) cat(paste0("\n   ",x),file=stderr())))  
  #   cat("\n")
  # }
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  # Ready for test
  output<-hyp.diff.prop(x=use.x,y=use.y,names.xy,pdiff0=pdiff0,success.x,success.y,
                        alternative=alternative,
                        digits=digits,type.print=type.print) 
}

#' Test on the equality of variances
#'
#' \code{TEST.diffvar()} allows testing the equality between the variances
#'   of two independent samples.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Unquoted string identifying the variable used to
#'   identify the two samples whose means have to be compared. Note that
#'   only \bold{one} between \code{y} and \code{by} can be specified. More
#'   specifically:
#'   * \code{x}, \code{y} should be specified when data on the two sub-samples
#'     are stored in two distinct vectors,
#'   * \code{x}, \code{by} should be specified when data on the two sub-samples
#'     should be obtained by splitting \code{x} into two groups based on the two
#'     values of the by vector; in this case, \code{x} and \code{by} must have
#'     the same length, \code{x} must be numeric, and \code{by} can take only
#'     two values. Note that this option is available only for \bold{independent}
#'     samples.
#' @param digits Integer value specifying the number of decimals used
#'   to round statistics; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{TEST.diffvar()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the tests for the population mean difference.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffmean}()} for computing confidence intervals on
#'   the difference between two population means.
#' @seealso \code{\link{TEST.diffmean}()} for computing tests on the difference
#'   between two population means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' MktDATA$Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffvar(x = AOV, by = Gender.R, data = MktDATA)
#'
#' @export
TEST.diffvar<-function(x,y,by,digits=2,data,...){
  type="independent"
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
  Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  if(op.sci==0){options(scipen=10)}
  
  # All requests checked for coherency before stopping if one is not ok
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list; 
  x<-check.x$vec.x
  
  # Check required type
  check.type<-chkpar.option(value=type,allowed=c("paired","independent"),onlyone = TRUE,
                            err.list=Err.list.input,warn.list=Warn.list)
  Err.list.input<-check.type$err.list
  Warn.list<-check.type$warn.list
  exist.t<-check.type$exist
  type<-check.type$value
  
  # Check if 'y' or 'by' exist and are properly specified
  name.y<-deparse1(substitute(y))
  name.by<-deparse1(substitute(by))
  check.yby<-chkcon.diff(type="mean",err.list=Err.list.input,
                         type.s=type,
                         x=switch(exist.x,T=x,F=NULL),
                         y=y,by=by,data=data,name.y=name.y,name.by=name.by,
                         name.data=deparse1(substitute(data)),
                         sigma.d=NULL,sigma.x=NULL,
                         sigma.y=NULL,sigma.by=NULL)
  exist.y<-check.yby$exist.y; exist.by<-check.yby$exist.by
  y<-check.yby$y; by<-check.yby$by
  Err.list.input<-check.yby$err.list
  
  # Check specifications para
  # check only properly stated variances
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # last.check

  # Final steps for building ci
  known.var <- FALSE
  
  if(exist.y){
    use.x<-x; use.y<-y
    names.xy<-c("x"=name.x,"y"=name.y)
  }
  if(exist.by){
    use.by<-factor(by)
    use.x<-x[use.by==levels(use.by)[1]]
    use.y<-x[use.by==levels(use.by)[2]]
    names.xy<-c("x"=paste0(name.x,"|",name.by,"=",levels(use.by)[1]),
                "y"=paste0(name.x,"|",name.by,"=",levels(use.by)[2]))
  }
  
  # Ready for tests
  my.p.list(paste0("Test hypotheses on variances", 
                   "\n   x=",names.xy["x"],
                   "\n   y=",names.xy["y"]),type.print=type.print)
  out.var<-hyp.diff.var(use.x,use.y,type="levene",
                        digits=digits,type.print=type.print)
}
