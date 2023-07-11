#' Analysis of a univariate distribution using tables
#'
#' \code{distr.table.x()} computes the frequency table of a vector or a factor.
#'
#' @param x A quoted or unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param freq A string providing the type of frequencies to be displayed.
#'   Allowed values (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"}, \code{"proportions"}, \code{"densities"}
#'   (only for variables classified into intervals), and \code{"cumulative"}.
#'   If no frequency is specified, \code{"counts"} and \code{"proportions"}
#'   are displayed by default if only \code{"cumulative"} is requested,
#'   counts and proportions will also be displayed, with their respective
#'   cumulative frequencies.
#' @param total Logical value indicating whether the sum of the requested
#'   frequencies should be added to the table; default to \code{TRUE}.
#' @param breaks Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param adj.breaks Logical value indicating whether numbers displayed
#'   should avoid using scientific notation; default to \code{TRUE}.
#' @param interval Logical value indicating whether \code{x} is a variable
#'   measured in intervals (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param f.digits Integer value specifying the number of decimals used
#'   to round proportions; default to 2.
#' @param p.digits Integer value specifying the number of decimals used
#'   to round percentages; default to 0.
#' @param d.digits Integer value specifying the number of decimals used
#'   to round densities; default to 5.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.table.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table listing the values taken by the variable arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, order of levels
#'   for factors), and the frequencies requested.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @seealso \code{\link{distr.table.xy}()} for computing a bivariate
#'   distribution.
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' distr.table.x(Education, data = MktDATA)
#'
#' distr.table.x(Children, data = MktDATA, freq = c("count", "prop", "cum"),
#'   f.digits = 4)
#'
#' distr.table.x(AOV, breaks = 6, freq = c("Count", "Perc", "Cum"),
#'   p.digits = 2, data = MktDATA)
#'
#' distr.table.x(AOV, breaks = c(0, 20, 30, 50, 100, 180),
#'   freq = c("Count", "Perc", "Cum", "Densities"), p.digits = 2,
#'   data = MktDATA)
#'
#' @export
distr.table.x<-function(x,freq=c("counts","proportions"),total=TRUE,
                        breaks,adj.breaks=TRUE,interval=FALSE,
                        f.digits=2,p.digits=0,d.digits=5,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Err.list.options<-as.list("\nErrors found in the definition of options:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok

  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x

  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages","cumulative","densities"),
                            onlyone=F,listall=T,err.list=Err.list.para,
                            warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)

  # Create a list with all the info needed to build tables
  if(exist.x==T){
    all.info<-build.Xlist(x,breaks,interval,
                          adj.breaks=adj.breaks,consistency=F,
                          err.list=Err.list.options,warn.list=Warn.list,
                          list.print=NULL)
    Err.list.options<-all.info$err.list
    Warn.list<-all.info$warn.list
    List.print<-all.info$list.print
    Xlist<-all.info$Vlist
  }

  ## Interrupt the procedure if there are errors
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)  }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[duplicated(Err.list.options)==F],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }

  # Ready for tables!
  if(("densities" %in% freq) && Xlist$class=="standard"){
    Warn.list<-c(Warn.list,"Densities allowed only for classified data")
    freq<-freq[freq != "densities"]
  }
  # Print all the warnings
  if(length(Warn.list)>1 | length(List.print)>0){
    # if(msg.p==T & markd==F){
    #   if(length(Warn.list)>1){
    #   invisible(lapply(Warn.list[duplicated(Warn.list)==F],
    #                    function(x) cat(paste0("\n   ",x),file=stderr())))  }
    # if(length(List.print)>0){
    #   invisible(lapply(List.print,
    #                    function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
    #                    } else if(is.data.frame(x)){print(x)}))
    # }
    # cat("\n")
    # }

    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],type.print=type.print)
      }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }
  # adjust digits
  use.digits=c("Prop"=f.digits,"Percent"=p.digits,
               "Density"=d.digits,"Count"=0)
  # capitalize freq
  substr(freq,1,1)<-toupper(substr(freq,1,1))

  # if only Cumulative requested, add also freq (counts or prop) to cum
  if(length(freq)==1 & ("Cumulative" %in% freq)){
    freq<-c("Counts","Proportions","Cumulative")}

  out<-data.frame(table(Xlist$V.f))
  colnames(out)<-c(name.x,"Count")
  if("Proportions" %in% freq){
    out$Prop<-out$Count/(sum(out$Count))}
  if("Percentages" %in% freq){
    out$Percent<-100*out$Count/sum(out$Count)}
  if("Densities" %in% freq & Xlist$class=="breaks"){
    widths<-diff(Xlist$breaks)
    out$Density<-out$Count/(sum(out$Count))
    out$Density<-out$Density/widths
  }
  if("Densities" %in% freq & Xlist$class=="interval"){
    widths<-(Xlist$info.int$tab$Up)-Xlist$info.int$tab$Low
    out$Density<-out$Count/(sum(out$Count))
    out$Density<-out$Density/widths
  }

  if("Cumulative" %in% freq){
    for(w.c in (colnames(out)[2:ncol(out)])){
      out[[paste0("Cum.",w.c)]]<-cumsum(out[[w.c]])
    }
  }
  if(!("Counts" %in% freq)){out$Count<-out$Cum.Count<-NULL}
  if(("Densities" %in% freq)){out$Cum.Density<-NULL}

  out[,1]<-as.character(out[,1])
  if(total==T){
    out[nrow(out)+1,]<-rep(NA,ncol(out))
    out[nrow(out),1]<-"TOTAL"
    no.cum<-!(substr(colnames(out),1,3) %in% c("Cum","Den") |
                colnames(out)==name.x)
    out[nrow(out),no.cum]<-apply(out[,no.cum,drop=F],2,"sum",na.rm=T)
  }
  rownames(out)[nrow(out)]<-"Sum"
  out.print<-out
  for(k in c("Prop","Percent","Count","Density")){
    sel<-colnames(out.print) %in% c(k,paste0("Cum.",k))
    if(sum(sel)>0){
      out.print[,sel]<-round(out.print[,sel],use.digits[k])}
  }
  op.sci<-getOption("scipen")
  if(op.sci==0){options(scipen=10)}
  out.print.p<-as.matrix(out.print)
  out.print.p[is.na(out.print.p)]<-""
  print(as.data.frame(out.print.p),row.names=F,quote=F,right=T)
  options(scipen=op.sci)
  output<-out
}

#' Analysis of a bivariate distribution using tables
#'
#' \code{distr.table.xy()} allows displaying tables of joint or conditional
#'   distributions.
#'
#' @param x,y Quoted or unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   Note that in the table \code{x} is displayed on the \emph{rows} and
#'   \code{y} on the \emph{columns}.
#' @param freq A string providing the frequencies to be displayed. Allowed
#'   values (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"} and \code{"proportions"}.
#' @param freq.type A length-one character vector providing the type of
#'   frequencies to be displayed. Allowed options are \code{joint} (default) 
#'   for joint frequencies, \code{x|y} (or \code{column}) for the distributions
#'   of \code{x} conditioned to \code{y}, and \code{y|x} (or \code{row}) for
#'   the distributions of \code{y} conditioned to \code{x}.
#' @param total Logical value indicating whether the sum of the requested
#'   frequencies should be added to the table; default to \code{TRUE}.
#' @param breaks.x Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param breaks.y Allows to classify a \emph{numerical} variable \code{y} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{y}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{y} range.
#' @param adj.breaks Logical value indicating whether numbers displayed
#'   should avoid using scientific notation; default to \code{TRUE}.
#' @param interval.x Logical value indicating whether \code{x} is a variable
#'   measured in classes (default \code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param interval.y Logical value indicating whether \code{y} is a variable
#'   measured in classes (default \code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param f.digits Integer value specifying the number of decimals used
#'   to round proportions; default to 2.
#' @param p.digits Integer value specifying the number of decimals used
#'   to round percentages; default to 0.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.table.xy()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table listing the values taken by the variables arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, order of levels
#'   for factors), and the frequencies requested.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @seealso \code{\link{distr.table.x}()} for computing a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Character vectors, factors, and discrete numeric vectors
#' distr.table.xy(LikeMost, Children, data = MktDATA) # default: joint counts
#' 
#' # Conditional distribution (proportions) of x|y
#' distr.table.xy(LikeMost, Education, freq = "Prop", freq.type = "x|y",
#'   data = MktDATA)
#' 
#' # Conditional distribution (proportions) of x|y
#' distr.table.xy(LikeMost, Education, freq = "Prop", freq.type = "x|y",
#'   data = MktDATA)
#' 
#' # Conditional distribution (%) of y|x (columns-conditionals)
#' distr.table.xy(CustClass, Children, freq = "Percentages", freq.type = "row",
#'   data = MktDATA)
#' 
#' # A numerical variable classified into intervals and a factor
#' distr.table.xy(CustClass, TotPurch, breaks.y = c(0, 5, 10, 15, 20, 35),
#'   freq = "Prop", freq.type = "y|x", data = MktDATA)
#' 
#' # A numerical variable classified into intervals and a factor
#' distr.table.xy(CustClass, TotPurch, breaks.y = c(0, 5, 10, 15, 20, 35),
#'   freq = "Prop", freq.type = "y|x", data = MktDATA,
#'   msg.control = list(err = FALSE, warn = FALSE, msg = TRUE))
#' 
#' # Two numerical  variables classified into intervals
#' distr.table.xy(Income.S, TotPurch, interval.x = TRUE,
#'   breaks.y = c(0, 5, 10, 15, 20, 35),
#'   freq = "Counts", freq.type = "row", data = MktDATA)
#' 
#' # Two numerical classified variables
#' distr.table.xy(Income.S, TotPurch, interval.x = TRUE,
#'   breaks.y = c(0, 5, 10, 15, 20, 35),
#'   freq = "Counts", freq.type = "row", data = MktDATA,
#'   msg.control = list(err = FALSE, warn = FALSE, msg = TRUE))
#'
#' @export
distr.table.xy<-function(x,y,freq="Counts",freq.type="joint",total=TRUE,
                         breaks.x,breaks.y,adj.breaks=TRUE,interval.x=FALSE,
                         interval.y=FALSE,f.digits=2,p.digits=0,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Err.list.options<-as.list("\nErrors found in the definition of options:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  # All requests checked for coherency before stopping if one is not ok

  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x

  # Check if 'y' exists and if it is coherent (not missing)
  name.y<-deparse1(substitute(y))
  check.y<-chk.data(y,data,deparse1(substitute(data)),
                    name.y,missing=T,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.y<-check.y$exist.x ;
  Err.list.input<-check.y$err.list
  Warn.list<-check.y$warn.list
  y<-check.y$vec.x

  # check if x and y have the same length
  if(exist.x==T && exist.y==T && (length(x) != length(y))){
    Err.list.input<-c(Err.list.input,"'x' and 'y' should have the same length")
  }

  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages"),
                            onlyone=F,listall=T,
                            err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)

  # Check required freq.type
  check.ftype<-chkpar.option(value=freq.type,
                             allowed=c("joint","x|y","y|x","rows","columns"),
                             onlyone=F,listall=T,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.ftype$err.list ; Warn.list<-check.ftype$warn.list
  exist.t<-check.ftype$exist;  type.tab<-unique(check.ftype$value)
  type.tab<-as.character(factor(type.tab,levels=c("joint","x|y","y|x","rows","columns"),
                                labels=c("Joint","x_y","y_x","row","col")))

  # Create lists with all the info needed to build tables
  List.print<-NULL
  if(exist.x==T){
    all.infoX<-build.Xlist(x,breaks.x,interval.x,adj.breaks=adj.breaks,consistency=F,
                           err.list=Err.list.options,warn.list=Warn.list,
                           list.print=List.print,suffix=T)
    Err.list.options<-all.infoX$err.list
    Warn.list<-all.infoX$warn.list
    List.print<-all.infoX$list.print
    Xlist<-all.infoX$Vlist
  }
  if(exist.y==T){
    all.infoY<-build.Xlist(y,breaks.y,interval.y,adj.breaks=adj.breaks,consistency=F,
                           err.list=Err.list.options,warn.list=Warn.list,
                           list.print=List.print,
                           suffix=T)
    Err.list.options<-all.infoY$err.list
    Warn.list<-all.infoY$warn.list
    List.print<-all.infoY$list.print
    Ylist<-all.infoY$Vlist
  }

  ## Interrupt the procedure if there are errors
  # if(length(Err.list.input)>1 | length(Err.list.para)>1 |
  #    length(Err.list.options)>1){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.input)>1){
  #     invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.para)>1){
  #     invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.options)>1){
  #     invisible(lapply(Err.list.options,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\nThe procedure is interrupted",file=stderr())
  #   stop_quietly()}

  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print) }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)  }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[duplicated(Err.list.options)==F],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }

  # Ready for tables!
  # Print all the warnings
  # if(length(Warn.list)>1 | length(List.print)>0){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))  }
  #   if(length(List.print)>0){
  #     invisible(lapply(List.print,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\n") }

  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],type.print=type.print)
      }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
        my.p.list(List.print,type.print=type.print)
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }


  # adjust digits
  use.digits=c("Proportions"=f.digits,"Percentages"=p.digits,
               "Counts"=0)
  # capitalize freq
  substr(freq,1,1)<-toupper(substr(freq,1,1))

  # Ready for tables!
  out<-list()
  tab.c<-data.frame(Xlist$V.f,Ylist$V.f)
  colnames(tab.c)<-c(name.x,name.y)
  tab.c<-table(tab.c[,c(name.x,name.y)])
  count.tab<-0
  for(ftype.i in type.tab){ # joint, x|y or y|x
    for(freq.i in freq){ # Counts,Proportions,Percentages
      tit<-switch(ftype.i,Joint=paste0("Joint ",tolower(freq.i)),
                  x_y=paste0("x|y: ",freq.i),y_x=paste0("y|x: ",freq.i),
                  row=paste0("Row ",freq.i),col=paste0("Column ",freq.i))
      if(ftype.i=="Joint"){
        tit<-paste0("Joint ",tolower(freq.i))
        tab<-switch(freq.i,Counts=tab.c,Proportions=prop.table(tab.c),
                    Percentages=prop.table(tab.c)*100)
        if(total==T){tab<-addmargins(tab)}
        out[[tit]]<-tab # save the table
        colnames(tab)[colnames(tab)=="Sum"]<-"TOTAL"
        rownames(tab)[rownames(tab)=="Sum"]<-"TOTAL"
        #cat(paste0("\n","   \n",tit,"\n"))
        if(count.tab==0){cat(paste0(tit,"\n"))
        } else {cat(paste0("   \n",tit,"\n"))}
        print(round(tab,use.digits[freq.i]))
        count.tab<-count.tab+1
      }
      if(ftype.i %in% c("x_y","y_x","row","col")){
        use.m<-switch(ftype.i,x_y=c(2,1),y_x=c(1,2),col=c(2,1),row=c(1,2))
        tab<-switch(freq.i,Counts=tab.c,
Proportions=prop.table(tab.c,margin=use.m[1]),
                    Percentages=prop.table(tab.c,margin=use.m[1])*100)
        if(total==T){tab<-addmargins(tab,use.m[2])}
        out[[tit]]<-tab # save the table
        colnames(tab)[colnames(tab)=="Sum"]<-"TOTAL"
        rownames(tab)[rownames(tab)=="Sum"]<-"TOTAL"
        #cat(paste0("\n","   \n",tit,"\n"))
        if(count.tab==0){cat(paste0(tit,"\n"))
        } else {cat(paste0("   \n",tit,"\n"))}
        print(round(tab,use.digits[freq.i]))
        count.tab<-count.tab+1
      }
    }
  }
  Out<-out
}

#' Analysis of a univariate distribution using plots
#'
#' \code{distr.plot.x()} generates plots of a univariate distribution.
#'
#' @param x A quoted or unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param freq A length-one character vector providing the frequencies to be
#'   displayed. Allowed values (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"}, \code{"proportions"}, \code{"densities"}
#'   (for histograms and density plots).
#' @param plot.type A length-one character vector providing the plot to generate.
#'   Allowed values are \code{"pie"}, \code{"bars"}, \code{"spike"},
#'   \code{"histogram"}, \code{"density"}, \code{"boxplot"}, and
#'   \code{"cumulative"}.
#' @param ord.freq A length-one character vector that is allowed only when
#'   \code{plot.type = "pie"} or \code{plot.type = "bars"}. It specifies
#'   whether the levels of \code{x} should be displayed in a standard order
#'   (\code{ord.freq = "none"}) or in an increasing or decreasing order
#'   of the levels (\code{ord.freq = "increasing"} or
#'   \code{ord.freq = "decreasing"}).
#' @param breaks Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param adj.breaks Logical value indicating whether numbers displayed
#'   should avoid using scientific notation; default to \code{TRUE}.
#' @param interval Logical value indicating whether \code{x} is a variable
#'   measured in intervals (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param bw Logical value indicating whether plots are coloured using a standard palette
#'   (\code{FALSE}) rather than in scale of greys.
#' @param color String vector indicating the specific colors to use in the plot, or
#'   \code{NULL} (defualt) which indicates to use a standard palette.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.plot.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.table.x}()} for computing a univariate
#'   distribution.
#' @seealso \code{\link{distr.table.xy}()} for computing a bivariate
#'   distribution.
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Pie charts
#' distr.plot.x(x = LikeMost, plot.type = "pie", bw = TRUE, data = MktDATA)
#' 
#' # Bar charts
#' distr.plot.x(x = Education, plot.type = "bars", freq = "percentage",
#'   ord.freq = "dec", data = MktDATA)
#' 
#' # Spike charts
#' distr.plot.x(x = NPickUp_Purch, plot.type = "spike", freq = "prop", data = MktDATA)
#' 
#' # Histograms
#' #   - no breaks provided
#' distr.plot.x(x = AOV, plot.type = "histogram", data = MktDATA)
#' #   - 10 equal width intervals
#' distr.plot.x(x = AOV, plot.type = "histogram", breaks = 10, data = MktDATA)
#' #   - with breaks
#' distr.plot.x(AOV, plot.type = "histogram", breaks = c(0, 20, 40, 60, 80, 100, 180), data = MktDATA)
#' distr.plot.x(Income, plot.type = "histogram", interval = TRUE, data = MktDATA)
#' 
#' # Density plot
#' distr.plot.x(Income, plot.type = "density", interval = TRUE, data = MktDATA)
#' 
#' # Boxplots
#' distr.plot.x(x = TotVal, plot.type = "boxplot", data = MktDATA)
#' 
#' # Cumulative distribution plots
#' distr.plot.x(x = Children, plot.type = "cum", freq = "percent", data = MktDATA)
#' distr.plot.x(Income, plot.type = "cum", interval = TRUE, data = MktDATA)
#'
#' @export
distr.plot.x<-function(x,freq="Counts",plot.type,ord.freq="none",
                       breaks,adj.breaks=TRUE,interval=FALSE,
                       bw=FALSE,color=NULL,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Err.list.options<-as.list("\nErrors found in the definition of options:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg

  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input,warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x

  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages","densities"),
                            onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)

  check.order<-chkpar.option(value=ord.freq,
                             allowed=c("none","increasing","decreasing"),
                             onlyone=T,listall=T,
                             err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.order$err.list ; Warn.list<-check.order$warn.list
  exist.o<-check.order$exist;  ord.freq<-unique(check.order$value)

  # Check plots are correctly specified / same procedure as before
  if(isTRUE(missing(plot.type))){
    print("passa da qui")
    Err.list.para<-c(Err.list.para,"'plot.type' must be specified")
    exist.plt<-F} else {
      if(!is.character(plot.type)){plot.type<-deparse1(substitute(plot.type))}
      check.plt<-chkpar.option(value=plot.type,
                               allowed=c("pie","bars","spike","cumulative",
                                         "histogram","boxplot","density"),
                               onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
      Err.list.para<-check.plt$err.list ; Warn.list<-check.plt$warn.list
      exist.plt<-check.plt$exist;  type.plt<-unique(check.plt$value)
    }
  # Check coherency between plots and types of variables
  if(exist.f==T && exist.plt==T && freq=="densities" &&
     type.plt=="cumulative"){
    Err.list.options<-c(Err.list.options,"Cumulative plots cannot be built based on densities")
    exist.plt<-F }

  if(exist.x==T && exist.plt==T){
    isnum<-is.numeric(x) # check x numeric
    isfac<-is.factor(x) # check x is a factor
    if((isFALSE(missing(breaks)) | interval==T) && type.plt=="boxplot"){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
      exist.plt<-F }
    if(isTRUE(missing(breaks)) && interval==F && !is.numeric(x) &&
       type.plt %in% c("boxplot","histogram","density")){
      Err.list.para<-c(Err.list.para,paste0("'",name.x,"' is character or factor: the required plot cannot be built"))
      exist.plt<-F }
  }
  # Create a list with all the info needed to build plots
  # if plots for numerical vars, endpoints must be consistent
  if(exist.plt==F & (isFALSE(missing(breaks)) | interval==T)){
    Warn.list<-c(Warn.list,paste0("Consistency of breaks and interval cannot be tested",
                                  "\n   "," because of mis-specified/missing 'plot.type'"))
  }
  if(exist.x==T && exist.plt==T){
    if(type.plt %in% c("boxplot","histogram","density")){
      all.infoX<-build.Xlist(x,breaks,interval,adj.breaks=adj.breaks,consistency=T,
                             err.list=Err.list.options,warn.list=Warn.list,list.print=NULL)
      Err.list.options<-all.infoX$err.list
      Warn.list<-all.infoX$warn.list
      List.print<-all.infoX$list.print
      Xlist<-all.infoX$Vlist
    } else {
      all.infoX<-build.Xlist(x,breaks,interval,adj.breaks=adj.breaks,consistency=F,
                             err.list=Err.list.options,warn.list=Warn.list,list.print=NULL)
      Err.list.options<-all.infoX$err.list
      Warn.list<-all.infoX$warn.list
      List.print<-all.infoX$list.print
      Xlist<-all.infoX$Vlist
    }
  }

  ## Interrupt the procedure if there are errors
  # if(length(Err.list.input)>1 | length(Err.list.para)>1 |
  #    length(Err.list.options)>1){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.input)>1){
  #     invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.para)>1){
  #     invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.options)>1){
  #     invisible(lapply(Err.list.options,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\nThe procedure is interrupted",file=stderr())
  #   stop_quietly()}

  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[duplicated(Err.list.options)==F],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }

  # Ready for plots!
  # Print all the warnings
  if(type.plt=="density" & Xlist$class=="breaks"){
    Warn.list<-c(Warn.list,"'breaks' ignored in density plots")
  }

  # if(length(Warn.list)>1 | length(List.print)>0){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))  }
  #   if(length(List.print)>0){
  #     invisible(lapply(List.print,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x))
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\n") }

  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],type.print=type.print)
      }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }


  # tabulate x depending on the value of 'freq'
  substr(freq,1,1)<-toupper(substr(freq,1,1))
  Xlist$tab.x<-switch(freq,Counts=table(Xlist$V.f),
                      Proportions=prop.table(table(Xlist$V.f)),
                      Percentages=prop.table(table(Xlist$V.f))*100)
  if(type.plt %in% c("bars","pie")){
    if(ord.freq=="increasing"){
      Xlist$tab.x<-Xlist$tab.x[order(Xlist$tab.x)]
    }
    if(ord.freq=="decreasing"){
      Xlist$tab.x<-Xlist$tab.x[rev(order(Xlist$tab.x))]
    }
  }
  # Plots!
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  par(mar=c(3.5,3.5,3,2.1),tck=(-0.01),tcl=NA,las=1,
      mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  if(type.plt=="pie"){
    plt.x.pie(Xlist$tab.x,bw=bw,color=color,name.x,freq)}
  if(type.plt=="bars"){
    plt.x.bars(Xlist$tab.x,bw=bw,color=color,name.x,freq)}
  if(type.plt=="spike"){
    plt.x.spike(Xlist,color=color,name.x,freq)}
  if(type.plt=="cumulative"){
    plt.x.cum(Xlist,color=color,name.x,freq,adj.breaks=adj.breaks)}
  if(type.plt=="histogram"){
    plt.x.hist(Xlist,bw=bw,color=color,name.x,freq,adj.breaks=adj.breaks)}
  if(type.plt=="density"){
    plt.x.density(Xlist,bw=bw,color=color,name.x,freq="Density",adj.breaks=adj.breaks)}
  if(type.plt=="boxplot"){
    plt.x.boxplot(Xlist,bw=bw,color=color,name.x,freq,adj.breaks=adj.breaks)
  }
  par(pardef)
}

#' Analysis of a bivariate distribution using plots
#'
#' \code{distr.plot.xy()} generates plots of a bivariate distribution.
#'
#' @param x,y Quoted or unquoted strings identifying the variables whose
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   Note that in the output \code{x} represents the \emph{row} variable
#'   while \code{y} corresponds to the \emph{column} variable.
#' @param freq A string providing the frequencies to be displayed. Allowed
#'   values (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"} and \code{"proportions"}.
#' @param freq.type A length-one character vector providing the type of
#'   frequencies to be displayed. Allowed options are \code{joint} (default) 
#'   for joint frequencies, \code{x|y} (or \code{column}) for the distributions
#'   of \code{x} conditioned to \code{y}, and \code{y|x} (or \code{row}) for
#'   the distributions of \code{y} conditioned to \code{x}.
#' @param plot.type A length-one character vector providing the plot to generate.
#'   Allowed values are \code{"bars"}, \code{"scatter"}, and
#'   \code{"boxplot"}.
#' @param bar.type A length-one character vector indicating whether in a bar plot,
#'   stacked (\code{bar.type = "stacked"}, default) or side-by-side
#'   (\code{bar.type = "beside"}) bars should be displayed.
#' @param breaks.x Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param breaks.y Allows to classify a \emph{numerical} variable \code{y} into
#'   intervals. It can be an integer specifying the number of intervals of
#'   equal width used to classify \code{y}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   he entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{y} range.
#' @param adj.breaks Logical value indicating whether numbers displayed
#'   should avoid using scientific notation; default to \code{TRUE}.
#' @param interval.x Logical value indicating whether \code{x} is a variable
#'   measured in classes (default \code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param interval.y Logical value indicating whether \code{y} is a variable
#'   measured in classes (default \code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent.
#' @param fitline Logical value indicating whether the line of best fit (also
#'   called trendline or regression line) should be added to a scatterplot
#'   (\code{fitline = TRUE}) or not (\code{fitline = FALSE}; default).
#' @param legend Logical value indicating whether a legend should be displayed
#'   in the plot (\code{legend = TRUE}; default) or not (\code{legend = FALSE}).
#' @param bw Logical value indicating whether plots are coloured using a standard palette
#'   (\code{FALSE}) rather than in scale of greys.
#' @param color String vector indicating the specific colors to use in the plot, or
#'   \code{NULL} (defualt) which indicates to use a standard palette.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.plot.xy()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.table.xy}()} for computing a bivariate
#'   distribution.
#' @seealso \code{\link{distr.table.x}()} for computing a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Bivariate bar plots
#' distr.plot.xy(CustClass, Children, freq = "Counts", freq.type = "joint",
#'   plot.type = "bars", data = MktDATA)
#' distr.plot.xy(TotPurch, Income, freq = "Percentages", freq.type = "x|y",
#'   plot.type = "bars", breaks.x = c(0, 5, 10, 15, 20, 35),
#'   interval.y = TRUE, data = MktDATA)
#' 
#' # Side-by-side boxplots
#' distr.plot.xy(x = AOV, y = Education, plot.type = "boxplot",
#'   freq.type = "y|x", data = MktDATA)
#' 
#' # Scatterplots
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", fitline = TRUE,
#'   data = MktDATA)
#' 
#' # Bubble plots
#' #  - two characters/factors
#' distr.plot.xy(Education, LikeMost, plot.type = "scatter", data = MktDATA)
#' #  - classified variables (i.e. not properly numerical)
#' distr.plot.xy(Income.S, TotPurch, interval.x = TRUE,
#'   breaks.y = c(0, 5, 10, 15, 20, 35), plot.type = "scatter", data = MktDATA)
#'
#' @export
distr.plot.xy<-function(x,y,freq="Counts",freq.type="joint",plot.type,
                        bar.type="stacked",fitline=FALSE,legend=TRUE,
                        breaks.x,breaks.y,adj.breaks=TRUE,
                        interval.x=FALSE,interval.y=FALSE,
                        bw=FALSE,color=NULL,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Err.list.options<-as.list("\nErrors found in the definition of options:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg

  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input,warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x

  # Check if 'y' exists and if it is coherent (not missing)
  name.y<-deparse1(substitute(y))
  check.y<-chk.data(y,data,deparse1(substitute(data)),
                    name.y,missing=T,err.list=Err.list.input,warn.list=Warn.list)
  exist.y<-check.y$exist.x ;
  Err.list.input<-check.y$err.list
  Warn.list<-check.y$warn.list
  y<-check.y$vec.x

  # check if x and y have the same length
  if(exist.x==T && exist.y==T && (length(x) != length(y))){
    Err.list.input<-c(Err.list.input,"'x' and 'y' should have the same length")
  }

  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages"),
                            onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)

  # Check plots are correctly specified / same procedure as before
  if(isTRUE(missing(plot.type))){
    Err.list.para<-c(Err.list.para,"'plot.type' must be specified")
    exist.plt<-F} else {
      if(!is.character(plot.type)){plot.type<-deparse1(substitute(plot.type))}
      check.plt<-chkpar.option(value=plot.type,
                               allowed=c("bars","scatter","boxplot"),
                               onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
      Err.list.para<-check.plt$err.list ; Warn.list<-check.plt$warn.list
      exist.plt<-check.plt$exist;  type.plt<-unique(check.plt$value)
    }

  # Check required freq.type
  check.ftype<-chkpar.option(value=freq.type,
                             allowed=c("joint","x|y","y|x"),
                             onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.ftype$err.list ; Warn.list<-check.ftype$warn.list
  exist.t<-check.ftype$exist;  type.tab<-unique(check.ftype$value)

  # Check required freq.type
  check.bartype<-chkpar.option(value=bar.type,
                               allowed=c("stacked","beside"),
                               onlyone=F,listall=T,
                               err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.bartype$err.list ;
  Warn.list<-check.bartype$warn.list
  exist.bart<-check.bartype$exist;
  bar.type<-unique(check.bartype$value)

  if(exist.x==T & exist.y==T && is.numeric(x)==F && is.numeric(y)==F
     && exist.plt==T && type.plt=="boxplot"){
    Err.list.para<-c(Err.list.para,"To build boxplots at least one of 'x' and 'y' must be numeric")
  }

  # Create lists with all the info needed to build tables
  if(exist.x==T & exist.y==T){
    both.in<-complete.cases(data.frame(x,y))
    x<-x[both.in] ; y<-y[both.in]
  }

  List.print<-NULL
  if(exist.x==T){
    all.infoX<-build.Xlist(x,breaks.x,interval.x,adj.breaks=adj.breaks,consistency=F,
                           err.list=Err.list.options,
                           warn.list=Warn.list,list.print=List.print,suffix=T)
    Err.list.options<-all.infoX$err.list
    Warn.list<-all.infoX$warn.list
    List.print<-all.infoX$list.print
    Xlist<-all.infoX$Vlist
  }
  if(exist.y==T){
    all.infoY<-build.Xlist(y,breaks.y,interval.y,adj.breaks=adj.breaks,consistency=F,
                           err.list=Err.list.options,warn.list=Warn.list,list.print=List.print,
                           suffix=T)
    Err.list.options<-all.infoY$err.list
    Warn.list<-all.infoY$warn.list
    List.print<-all.infoY$list.print
    Ylist<-all.infoY$Vlist
  }
  if(exist.x==T && exist.y==T && exist.plt==T && type.plt=="boxplot"){
    if(Xlist$class=="standard" & Xlist$isnum==F & (Ylist$class %in% c("interval","breaks"))){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
    }
    if(Ylist$class=="standard" & Ylist$isnum==F & (Xlist$class %in% c("interval","breaks"))){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
    }
  }
  if(exist.x==T && exist.y==T && exist.plt==T && type.plt=="scatter"
     & fitline==T){
    if((Xlist$class=="standard" & Xlist$isnum==F) |
       (Ylist$class=="standard" & Ylist$isnum==F)){
      Warn.list<-c(Warn.list,
                   "Fitline can be added only when x and y are both numeric")
      fitline<-F
    }
    if(Xlist$class=="breaks" | Xlist$class=="interval" |
       Ylist$class=="breaks" | Ylist$class=="interval"){
      Warn.list<-c(Warn.list,
                   "Fitline can be added only when x and y are both numeric")
      fitline<-F
    }
  }
  if(exist.x==T && exist.y==T && exist.plt==T && type.plt!="scatter"
     & fitline==T){
    Warn.list<-c(Warn.list,
                 "Fitline can be added only to scatterplots")
    fitline<-F
  }
  if(exist.x==T && exist.y==T && exist.plt==T && type.plt=="bars"){
    if((Xlist$class=="standard" & Xlist$isnum==T &
        length(unique(Xlist$V.f))>20) |
       (Ylist$class=="standard" & Ylist$isnum==T &
        length(unique(Ylist$V.f))>20)){
      Err.list.input<-c(Err.list.input,
                        paste0("x and/or y are/is numeric with too many levels",
                               "\n   "," -> to force the procedure transform the variable/s into factor/s"))
    }
  }
  ## Interrupt the procedure if there are errors
  # if(length(Err.list.input)>1 | length(Err.list.para)>1 |
  #    length(Err.list.options)>1){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.input)>1){
  #     invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.para)>1){
  #     invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.options)>1){
  #     invisible(lapply(Err.list.options,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\nThe procedure is interrupted",file=stderr())
  #   stop_quietly()}

  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[duplicated(Err.list.options)==F],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }


  # Ready for plots!
  # Print all the warnings
  # if(length(Warn.list)>1 | length(List.print)>0){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))  }
  #   if(length(List.print)>0){
  #     invisible(lapply(List.print,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x))
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\n") }

  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],type.print=type.print)
      }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }

  # Plots!
  # capitalize freq
  substr(freq,1,1)<-toupper(substr(freq,1,1))
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  par(mar=c(3.5,3.5,3,2.1),tck=(-0.01),tcl=NA,las=1,
      mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  par.my<-list(mar=c(3.5,3.5,3,2.1),tck=(-0.01),tcl=NA,las=1,
               mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)

  if(bar.type=="beside"){beside<-T} else{beside<-F}

  if(type.plt=="bars"){
    switch.xy<-F
    tab.ini<-table(Ylist$V.f,Xlist$V.f)
    use.nx<-name.x; use.ny<-name.y
    if(type.tab=="joint"){
      use.tit<-paste0(name.x," , ",name.y)
      tab<-switch(freq,Counts=tab.ini,
                  Proportions=prop.table(tab.ini),
                  Percentages=prop.table(tab.ini)*100)  }
    if(type.tab=="y|x"){
      if(freq=="Counts"){freq<-"Proportions"}
      use.tit<-paste0(name.y," | ",name.x)
      tab<-switch(freq,Counts=prop.table(tab.ini,margin=2),
                  Proportions=prop.table(tab.ini,margin=2),
                  Percentages=prop.table(tab.ini,margin=2)*100)  }
    if(type.tab=="x|y"){
      use.tit<-paste0(name.x," | ",name.y)
      if(freq=="Counts"){freq<-"Proportions"}
      # the table is transposed
      tab<-switch(freq,Counts=prop.table(t(tab.ini),margin=2),
                  Proportions=prop.table(t(tab.ini),margin=2),
                  Percentages=prop.table(t(tab.ini),margin=2)*100)
      # change the names of the variables too
      #use.nx<-name.y; use.ny<-name.x
      switch.xy<-T
    }
    plt.xy.crossbars(tab,bw=bw,color=color,use.nx,use.ny,freq,
                     legend=legend,beside=beside,use.tit=use.tit,
                     switch.xy=switch.xy,use.par=par.my)
  }
  if(type.plt=="scatter"){
    plt.xy.scatter(Xlist,Ylist,bw=bw,color=color,name.x,name.y,
                   adj.breaks=adj.breaks,fitline=fitline,
                   use.par=par.my)
  }
  if(type.plt=="boxplot"){
    # if(Xlist$isnum==F & Ylist$isnum==F){
    #   cat("\nErrors in the definition of options:",file=stderr())
    #   cat("\n  To build boxplots at least one of 'x' and 'y' must be numeric",file=stderr())
    #   cat("\n    -> It is not possible to proceed with the required analysis",file=stderr())
    #   stop_quietly()
    # }
    switch.xy<-F
    if(type.tab=="x|y"){switch.xy<-T}
    plt.xy.boxplot(Xlist,Ylist,bw=bw,color=color,name.x,name.y,
                   adj.breaks=adj.breaks,switch.xy=switch.xy,
                   use.par=par.my)
  }
  par(pardef)
}

## Functions to plot by ------

distr.plot.xby<-function(x,by,plot.type,overlay=FALSE,legend=TRUE,
                         breaks.x,breaks.by,interval.x=FALSE,interval.by=FALSE,
                         bw=TRUE,color=NULL,nrows=NULL,ncols=NULL,square=TRUE,
                         adj.breaks=FALSE,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
  add.dots<-list(...)
  if(length(add.dots)>0){
    if("markd" %in% names(add.dots)){type.print<-"print"}
    if("msg.control" %in% names(add.dots)){
      msg.p<-add.dots[["msg.control"]]}
  }
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Err.list.options<-as.list("\nErrors found in the definition of options:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg

  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input,warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x

  # Check if 'by' is specified and is coherent
  name.by<-deparse1(substitute(by))
  check.by<-chk.data(by,data,deparse1(substitute(data)),
                     name.by,missing=T,err.list=Err.list.input,warn.list=Warn.list)
  exist.by<-check.by$exist.x ;
  Err.list.input<-check.by$err.list
  Warn.list<-check.by$warn.list
  by<-check.by$vec.x

  # check if x and by have the same length
  if(exist.x==T && exist.by==T && (length(x) != length(by))){
    Err.list.input<-c(Err.list.input,"'x' and 'by' should have the same length")
  }

  # Check plots are correctly specified / same procedure as before
  if(isTRUE(missing(plot.type))){
    Err.list.para<-c(Err.list.para,"'plot.type' must be specified")
    exist.plt<-F} else {
      if(!is.character(plot.type)){plot.type<-deparse1(substitute(plot.type))}
      check.plt<-chkpar.option(value=plot.type,
                               allowed=c("histogram","density","boxplot"),
                               onlyone=T,listall=T,err.list=Err.list.para,warn.list=Warn.list)
      Err.list.para<-check.plt$err.list ; Warn.list<-check.plt$warn.list
      exist.plt<-check.plt$exist;  type.plt<-unique(check.plt$value)
    }
  # Check coherency between plots and types of variables
  if(exist.x==T && exist.plt==T){
    isnum<-is.numeric(x) # check x numeric
    isfac<-is.factor(x) # check x is a factor
    if((isFALSE(missing(breaks.x)) | interval.x==T) && type.plt=="boxplot"){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
      exist.plt<-F }
    if(isTRUE(missing(breaks.x)) && interval.x==F && !is.numeric(x) &&
       type.plt %in% c("boxplot","histogram","density")){
      Err.list.para<-c(Err.list.para,paste0("'",name.x,"' is character or factor: the required plot cannot be built"))
      exist.plt<-F }
  }

  # Create lists with all the info needed to build tables
  if(exist.x==T && exist.by==T){
    both.in<-complete.cases(data.frame(x,by))
    x<-x[both.in] ; by<-by[both.in]
  }

  List.print<-NULL
  if(exist.x==T){
    all.infoX<-build.Xlist(x,breaks.x,interval.x,adj.breaks=adj.breaks,consistency=F,
                           err.list=Err.list.options,warn.list=Warn.list,
                           list.print=List.print,suffix=T)
    Err.list.options<-all.infoX$err.list
    Warn.list<-all.infoX$warn.list
    List.print<-all.infoX$list.print
    Xlist<-all.infoX$Vlist
  }
  if(exist.by==T){
    all.infobY<-build.Xlist(by,breaks.by,interval.by,adj.breaks=adj.breaks,consistency=F,
                            err.list=Err.list.options,warn.list=Warn.list,list.print=List.print,
                            suffix=T)
    Err.list.options<-all.infobY$err.list
    Warn.list<-all.infobY$warn.list
    List.print<-all.infobY$list.print
    bYlist<-all.infobY$Vlist
  }

  # adjust the by variable if numeric -> factor
  if(exist.by==T && bYlist$class=="standard" && bYlist$isnum==T){
    num.levels<-unique(bYlist$V.f)
    num.levels<-num.levels[order(num.levels)]
    bYlist$V.f<-factor(bYlist$V.f,levels=num.levels)
  }

  # check consistency of required plots:
  if(exist.plt && (plot.type=="density" | plot.type=="histogram")){
    # if(Xlist$class=="standard" & Xlist$isnum==F){
    #   Err.list.para<-c(Err.list.para,"To build the required plot 'x' must be numeric")
    # }
    if(exist.by==T && bYlist$class=="standard" &&
       bYlist$isnum==T && length(unique(bYlist$V.f))>20){
      Err.list.para<-c(Err.list.para,paste0("'by' is numeric and has too many levels",
                                            "\n   "," -> to force the procedure transform 'by' into a factor"))
    }
  }

  # define the grid layout
  if(exist.by==T){
    dim.grid<-c(1,1)
    n.plots=length(levels(bYlist$V.f))
    if(n.plots>=2){
      dim.grid<-find.layout(n.plots=n.plots,nrows=nrows,ncols=ncols,
                            square=square,err.list=Err.list.options,warn.list=Warn.list)
      Err.list.options<-dim.grid$err.list
      Warn.list<-dim.grid$warn.list
      dim.grid<-dim.grid$dim.grid
    }
  }

  ## Interrupt the procedure if there are errors
  # if(length(Err.list.input)>1 | length(Err.list.para)>1 |
  #    length(Err.list.options)>1){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.input)>1){
  #     invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.para)>1){
  #     invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.options)>1){
  #     invisible(lapply(Err.list.options,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x),file=stderr())
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\nThe procedure is interrupted",file=stderr())
  #   stop_quietly()}

  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[duplicated(Err.list.options)==F],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }


  # Ready for plots!
  # Print all the warnings
  # if(length(Warn.list)>1 | length(List.print)>0){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))  }
  #   if(length(List.print)>0){
  #     invisible(lapply(List.print,
  #                      function(x) if(is.character(x)){cat(paste0("\n   ",x))
  #                      } else if(is.data.frame(x)){print(x)}))
  #   }
  #   cat("\n") }

  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],type.print=type.print)
      }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }

  # Plots!
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  par(cex=0.88,cex.axis=0.88)

  if(plot.type=="density"){
    plt.xby.dens(Xlist,bYlist,name.x,name.by,bw=bw,color=color,
                 dim.grid=dim.grid,square=square,overlay=overlay,
                 legend=legend)
  }
  if(plot.type=="histogram"){
    plt.xby.hist(Xlist,bYlist,name.x,name.by,bw=bw,color=color,
                 dim.grid=dim.grid,square=square,legend=legend)
  }
  par(pardef)
}

# Functions to create and plot summaries         ------

build.summaries<-function(x,by1=NULL,by2=NULL,name.1=NULL,name.2=NULL,
                          stats,digits=2,f.digits=4){
  # create the by list
  list.by<-list()
  if(!is.null(by1)){list.by[[name.1]]<-by1}
  if(!is.null(by2)){list.by[[name.2]]<-by2}
  if(length(list.by)==0){
    list.by$toberemoved<-rep(1,length(x))
  }

  # build the function to calculate stats
  pip<-function(x,stats,digits=digits,f.digits=f.digits){
    use.f<-F
    if(is.factor(x)){use.f<-T}
    x.na<-sum(is.na(x));  x<-na.omit(x)
    cs<-c("n"=length(x),"n.a"=x.na)

    # functions for modes and their properties
    f.mode <- function(x,use.f){
      tab.x<-tabulate(match(x, unique(x)))
      mode.s<-(unique(x)[which.max(tab.x)])
      if(use.f==T){mode.s<-as.character(mode.s)}
      return(mode.s)}
    f.n_mode <- function(x,use.f){
      tab.x<-tabulate(match(x, unique(x)))
      n.m<-sum(tab.x==max(tab.x))
      #if(use.f==T){n.m<-as.character(n.m)}
      return(n.m)}
    f.p_mode <- function(x,use.f,f.digits=f.digits){
      p.m<-max(prop.table(tabulate(match(x, unique(x)))))
      p.m<-round(p.m,f.digits)
      #if(use.f==T){p.m<-as.character(p.m)}
      return(p.m)}
    # functions for quartiles, adjusted for factors
    f.q<-function(x,f=F,p){
      if(f==F){qq<-quantile(x,p)} else {qq<-as.character(quantile(x,type=3,p))}
      return(qq)}

    cso<-NULL
    for(k in tolower(stats)){
      if(k=="mode"){cso<-c(cso,f.mode(x,use.f))}
      if(k=="n.modes"){cso<-c(cso,f.n_mode(x,use.f))}
      if(k=="mode%"){cso<-c(cso,f.p_mode(x,use.f,f.digits=f.digits))}
      #if(k=="min"){cso<-c(cso,min(x))}
      #if(k=="min"){cso<-c(cso,quantile(x,type=type.q,probs=0))}
      if(k=="min"){cso<-c(cso,f.q(x,f=use.f,p=0))}
      if(k=="median"){cso<-c(cso,f.q(x,f=use.f,p=0.5))}
      #if(k=="median"){cso<-c(cso,quantile(x,type=type.q,probs=0.5))}
      #if(k=="q1"){cso<-c(cso,quantile(x,type=type.q,probs=0.25))}
      #if(k=="q2"){cso<-c(cso,quantile(x,type=type.q,probs=0.5))}
      #if(k=="q3"){cso<-c(cso,quantile(x,type=type.q,probs=0.75))}
      if(k=="q1"){cso<-c(cso,f.q(x,f=use.f,p=0.25))}
      if(k=="q2"){cso<-c(cso,f.q(x,f=use.f,p=0.5))}
      if(k=="q3"){cso<-c(cso,f.q(x,f=use.f,p=0.75))}
      if(k=="mean"){cso<-c(cso,mean(x))}
      #if(k=="max"){cso<-c(cso,max(x))}
      #if(k=="max"){cso<-c(cso,quantile(x,type=type.q,probs=1))}
      if(k=="max"){cso<-c(cso,f.q(x,f=use.f,p=1))}
      if(k=="sd"){cso<-c(cso,sd(x))}
      if(k=="var"){cso<-c(cso,var(x))}
      if(k=="cv"){cso<-c(cso,(sd(x)/abs(mean(x))))}
      if(k=="range"){cso<-c(cso,(max(x)-min(x)))}
      if(k=="iqrange"){cso<-c(cso,(f.q(x,f=use.f,p=0.75)-
                                     f.q(x,f=use.f,p=0.25)))}
      for(j in 0:100){
        if(k==paste0("p",j)){
          cso<-c(cso,f.q(x,f=use.f,p=(j/100)))}
      }
    }
    names(cso)<-stats
    if(is.numeric(x)){cso<-c(cs,cso) } else {cso<-c(as.character(cs),cso)
    names(cso)[1:2]<-c("n","n.a")}
    cso
  }
  out<-aggregate(x,by=list.by,
                 FUN=function(x) pip(x,stats,digits=digits,f.digits=f.digits))
  out<-data.frame(out[,1:length(list.by)],out$x,check.names = F)
  names(out)[1:length(list.by)]<-names(list.by)
  for(k in c("n","n.a","n.modes")){
    if(k %in% colnames(out)){
      out[,k]<-round(as.numeric(out[,k]),0)}  }
  for(k in c("mode%")){
    if(k %in% colnames(out)){
      out[,k]<-round(as.numeric(out[,k]),f.digits)}
  }
  if(is.numeric(x)){
    for(k in stats[!(stats %in% c("n","n.a","n.modes","mode%"))]){
      if(k %in% colnames(out)){
        out[,k]<-round(as.numeric(out[,k]),digits)} }
  }
  out$toberemoved<-NULL
  myout<-out
}

#' Summary statistics for a single variable
#'
#' \code{distr.summary.x()} computes the frequency table of a vector or a factor.
#'
#' @param x A quoted or unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param by1,by2 Quoted or unquoted strings identifying optional variables
#'   (typically taking few values/levels) used to build conditional summaries,
#'   that can be defined same way as x.
#' @param stats String vector providing the summary statistics to compute.
#'   The following options are available:
#'   * \code{"summary"} returns min, q1 , median, mean, q3, max, sd, var,
#'   * \code{"central"} returns central tendency measures only,
#'   * \code{"dispersion"} returns measures of dispersion only,
#'   * \code{"fivenumbers"} returns the five number summary,
#'   * \code{"quartiles"}, \code{"quintiles"}, \code{"deciles"},
#'     \code{"percentiles"} return the corresponding quantities.
#'
#'   It is also possible to list the following specific statistics:
#'   \code{"q1"}, \code{"q2"}, \code{"q3"}, \code{"mean"}, \code{"median"},
#'   \code{"mode"} (which returns the mode, the number of modes and the
#'   proportion of cases with modal value respectively), \code{"min"},
#'   \code{"max"}, \code{"sd"}, \code{"var"}, \code{"cv"} (coefficient of
#'   variation), \code{"range"}, \code{"IQrange"}, or \code{"p1"},
#'   \code{"p2"},..., \code{"p100"} (i.e. specific percentiles).
#' @param digits Integer value specifying the number of decimals used
#'   to round summary statistics; default to 4.
#' @param f.digits Integer value specifying the number of decimals used
#'   to round frequencies; default to 2.
#' @param data An optional data frame containing the variable to be analysed.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.summary.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table listing the values taken by the variable arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, order of levels
#'   for factors), and the frequencies requested.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.table.x}()} for computing a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Default summaries
#' distr.summary.x(x = AOV, data = MktDATA)
#' distr.summary.x(x = AOV, stats = c("central", "dispersion", "fivenumbers"),
#'   data = MktDATA)
#' distr.summary.x(x = AOV, stats = c("mode", "mean", "sd", "cv", "fivenumbers"),
#'   data = MktDATA)
#' 
#' # Conditional summaries
#' distr.summary.x(x = TotVal, by1 = Gender,
#'   stats = c("p5", "p10", "p25", "p50", "p75", "p90", "p95"),
#'   digits = 1, data = MktDATA)
#' distr.summary.x(x = AOV, by1 = Gender, by2 = Kids,
#'   stats = "fivenumbers", data = MktDATA)
#'
#' @export
distr.summary.x<-function(x,by1,by2,stats="summary",digits=2,
                          f.digits=4,data,...){
  type.print<-"cat"
  msg.p<-list(err=T,warn=T,msg=T)
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
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing=T,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list;
  x<-check.x$vec.x

  # Check if 'y' or 'by' exist and are properly specified
  name.by1<-deparse1(substitute(by1))
  name.by2<-deparse1(substitute(by2))
  check.bys<-chkcon.des(err.list=Err.list.input,
                        x=switch(exist.x,T=x,F=NULL),
                        y1=by1,y2=by2,data=data,name.1=name.by1,name.2=name.by2,
                        name.data=deparse1(substitute(data)))
  exist.by1<-check.bys$exist.1; exist.by2<-check.bys$exist.2
  by1<-check.bys$l1; by2<-check.bys$l2
  Err.list.input<-check.bys$err.list
  if(is.null(by1)){name.by1<-NULL}
  if(is.null(by2)){name.by2<-NULL}

  # Check specifications para
  check.stats<-chkpar.option(value=stats,
                             allowed=c("summary","central","dispersion",
                                       "fivenumbers","quartiles","quintiles",
                                       "deciles","percentiles","q1","q2","q3",
                                       "mean","median","mode","min",
                                       "max","sd","var","cv","range",
                                       "IQrange",paste0("p",1:100)),
                             onlyone=F,listall=F,
                             err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.stats$err.list
  Warn.list<-check.stats$warn.list
  exist.s<-check.stats$exist;  stats<-unique(check.stats$value)

  list.stats<-list.tit<-list()
  if("summary" %in% stats){
    list.stats$summary<-c("min","q1","median","mean","q3","max","sd","var")
    list.tit$summary<-"Summary measures"}
  if("central" %in% stats){
    list.stats$central<-c("mode","n.modes","mode%","median","mean")
    list.tit$central<-"Central tendency measures"}
  if("dispersion" %in% stats){
    list.stats$dispersion<-c("range","IQrange","sd","var","cv")
    list.tit$dispersion<-"Measures of dispersion"}
  if("percentiles" %in% stats){
    list.stats$percentiles<-c("min",paste0("p",seq(1,99,by=1)),"max")
    list.tit$percentiles<-"Percentiles"}
  if("deciles" %in% stats){
    list.stats$deciles<-c("min",paste0("p",seq(10,90,by=10)),"max")
    list.tit$deciles<-"Deciles"}
  if("quintiles" %in% stats){
    list.stats$quintiles<-c("min","p20","p40","p60","p80","max")
    list.tit$quintiles<-"Quintiles"}
  if("fivenumbers" %in% stats){
    list.stats$fivenum<-c("min","q1","median","q3","max")
    list.tit$fivenum<-"Five number summary"}
  if("quartiles" %in% stats){
    list.stats$quartiles<-c("min","p25","p50","p75","max")
    list.tit$quartiles<-"Quartiles"}
  free.stats<-stats[!(stats %in% c("summary","central","dispersion",
                                   "fivenumbers","quartiles",
                                   "quintiles","deciles","percentiles"))]
  if("mode" %in% free.stats){
    free.stats<-na.omit(unique(c(free.stats[1:(which(free.stats=="mode"))],
                                 "n.modes","mode%",
                                 free.stats[(which(free.stats=="mode")+1):length(free.stats)])))}
  if(length(free.stats)>0){
    list.stats$statistics<-free.stats
    list.tit$statistics<-"Requested statistics"}

  unique.stats<-na.omit(unique(unlist(list.stats)))
  char.stats<-c("mode","n.modes","mode%")
  fact.stats<-c("min","max","median","mode","n.modes","mode%",paste0("p",1:100))
  num.stats<-c("mean","sd","var","cv","range","IQrange")

  if(exist.x && exist.s && is.character(x) &&
     sum(unique.stats %in% char.stats)==0){
    Err.list.input<-c(Err.list.input,
                      "None of the required 'stats' can be calculated when 'x' is character")
  }
  if(exist.x && exist.s && is.character(x) && sum(unique.stats %in% char.stats)>0 &&
     sum(unique.stats %in% unique(c(fact.stats,num.stats)))>0){
    Warn.list<-c(Warn.list,
                 "Some of the required 'stats' cannot be calculated when 'x' is character")
    list.stats<-lapply(1:length(list.stats),
                       function(x) list.stats[[x]][(list.stats[[x]] %in% char.stats)])
    to.rem<-which(sapply(list.stats, length) ==0)
    if(length(to.rem)>0){list.stats<-list.stats[-to.rem];
    list.tit<-list.tit[-to.rem] }
  }
  if(exist.x && exist.s && is.factor(x) &&
     sum(unique.stats %in% fact.stats)==0){
    Err.list.input<-c(Err.list.input,
                      "None of the required 'stats' can be calculated when 'x' is factor")}
  if(exist.x && exist.s && is.factor(x) &&
     sum(unique.stats %in% fact.stats)>0 &&
     sum(unique.stats %in% num.stats)>0){
    Warn.list<-c(Warn.list,
                 "Some of the required 'stats' cannot be calculated when 'x' is factor")
    list.stats<-lapply(1:length(list.stats),
                       function(x) list.stats[[x]][(list.stats[[x]] %in% fact.stats)])
    to.rem<-which(sapply(list.stats, length) ==0)
    if(length(to.rem)>0){list.stats<-list.stats[-to.rem]; list.tit<-list.tit[-to.rem] }
  }

  # if(length(Err.list.input)>1 | length(Err.list.para)>1){
  #   if(length(Warn.list)>1){
  #     invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.input)>1){
  #     invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   if(length(Err.list.para)>1){
  #     invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
  #                      function(x) cat(paste0("\n   ",x),file=stderr())))    }
  #   cat("\nThe procedure is interrupted",file=stderr())
  #   stop_quietly()}

  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[duplicated(Err.list.input)==F],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[duplicated(Err.list.para)==F],
                  type.print=type.print)    }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }

  # Ready for calculations
  unique.stats<-na.omit(unique(unlist(list.stats)))

  #if(is.character(x)){unique.stats<-"mode"}
  if(is.factor(x)){
    x<-as.ordered(x)
    Warn.list<-c(Warn.list,paste0("The required 'stats' will be calculated",
                                  " using the actual order of 'x' levels",
                                  "\n   ","  -> ",paste0(levels(x),collapse=" < ")))
  }

  # if(length(Warn.list)>1){
  #   invisible(lapply(Warn.list[duplicated(Warn.list)==F],
  #                    function(x) cat(paste0("\n   ",x),file=stderr())))
  #   cat("\n")}

  if(length(Warn.list)>1){
    if(msg.p$warn==T){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[duplicated(Warn.list)==F],
                  type.print=type.print)  }
      cat("\n")
    }
  }

  out<-build.summaries(x=x,by1=by1,name.1=name.by1,by2=by2,name.2=name.by2,
                       stats=unique.stats,digits=digits,f.digits=f.digits)

  tit1<-paste0("\nSummary measures for ",name.x)
  tit2<-c(name.by1,name.by2)
  if(!is.null(tit2) && length(tit2)>1){tit2<-paste0(tit2,collapse=",")}
  if(!is.null(tit2)){tit1<-paste0(tit1," | ",tit2)}
  #cat(paste0(tit1,"\n"))
  my.p.list(tit1,type.print=type.print)

  list.out<-list()
  to.keep<-colnames(out)[!(colnames(out) %in% unique.stats)]
  for(k in 1:length(list.stats)){
    #cat(paste0("\n",list.tit[[k]],"\n"),file=stderr())
    my.p.list(list.tit[[k]],type.print=type.print)
    list.out[[k]]<-out[,c(to.keep,list.stats[[k]])]
    print(list.out[[k]],row.names = F)
  }
}

summary.plot.x<-function(x,by1,by2,stats="mean",plot.type="bars",conf.level=0.95,
                         bw=F,color=NULL,legend=T,data,print.stats=F){
  Err.list.input<-as.list("\nErrors found in the definition of inputs:")
  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  Warn.list<-as.list("\nWarning:") # list to collect warn msg

  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num=T,missing=T,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list;
  x<-check.x$vec.x

  # Check specifications para
  check.stats<-chkpar.option(value=stats,
                             allowed=c("mean","median","ci.mean","quartiles",
                                       "quintiles","deciles","percentiles"),onlyone=T,
                             listall=F,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.stats$err.list
  Warn.list<-check.stats$warn.list
  exist.s<-check.stats$exist;  stats<-unique(check.stats$value)

  check.plot<-chkpar.option(value=plot.type,
                            allowed=c("bars","lines","points"),onlyone=T,
                            listall=T,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.plot$err.list
  Warn.list<-check.plot$warn.list
  exist.p<-check.plot$exist;  type<-check.plot$value

  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)

  # Check if 'y' or 'by' exist and are properly specified
  if(isTRUE(missing(by1) & isTRUE(missing(by2)))){
    Err.list.input<-c(Err.list.input,"To obtain plots for summaries by1 or by2 should be specified")
    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))    }
    if(length(Err.list.input)>1){
      invisible(lapply(Err.list.input[duplicated(Err.list.input)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))    }
    if(length(Err.list.para)>1){
      invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))    }
    cat("\nThe procedure is interrupted",file=stderr())
    stop_quietly()}


  Err.list.para<-as.list("\nErrors found in the definition of parameters:")
  name.by1<-deparse1(substitute(by1))
  name.by2<-deparse1(substitute(by2))
  check.bys<-chkcon.des(err.list=Err.list.para,x=switch(exist.x,T=x,F=NULL),
                        y1=by1,y2=by2,data=data,name.1=name.by1,name.2=name.by2,
                        name.data=deparse1(substitute(data)))
  exist.by1<-check.bys$exist.1; exist.by2<-check.bys$exist.2
  by1<-check.bys$l1; by2<-check.bys$l2
  Err.list.para<-check.bys$err.list
  if(is.null(by1)){name.by1<-NULL}
  if(is.null(by2)){name.by2<-NULL}

  if((type=="bars" | type=="points") && (stats %in% c("quartiles","quintiles","deciles","percentiles"))){
    Warn.list<-c(Warn.list,"Only 'lines' allowed for quantiles: 'bars'/'points' ignored")
    type<-"lines"
  }
  if(type=="bars" && !is.null(by1) && !is.null(by2) && (stats %in% c("mean"))){
    Warn.list<-c(Warn.list,"For means only 'lines'/'points' allowed with 2 layers: 'bars' ignored")
    type<-"lines"
  }
  if(type=="bars" && !is.null(by1) && !is.null(by2) && (stats %in% c("median"))){
    Warn.list<-c(Warn.list,"For medians only 'lines'/'points' allowed with 2 layers: 'bars' ignored")
    type<-"lines"
  }
  if(!is.null(by1) && !is.null(by2) &&
     (stats %in% c("quartiles","quintiles","deciles","percentiles"))){
    Err.list.para<-c(Err.list.para,"Quantiles can be displayed only by a single layer")
  }
  if(!is.null(by1) && !is.null(by2) && (stats %in% c("ci.mean"))){
    Err.list.para<-c(Err.list.para,"Means CI can be displayed only by a single layer")
  }
  # not useful, added in case more options are allowed in the future
  if(is.character(x)){
    Err.list.para<-c(Err.list.para,"The available plots cannot be built for a character variable")
  }
  if(!is.numeric(x) & stats %in% c("mean","median","ci.mean")){
    Err.list.para<-c(Err.list.para,"The requested plots cannot be built for a non numeric variable")
  }

  if(length(Err.list.para)>1){
    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))    }
    if(length(Err.list.para)>1){
      invisible(lapply(Err.list.para[duplicated(Err.list.para)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))    }
    cat("\nThe procedure is interrupted",file=stderr())
    stop_quietly()}


  list.stats<-list.tit<-NULL
  if("mean" %in% stats){list.stats<-"mean" ; list.tit<-"Means"}
  if("median" %in% stats){list.stats<-"median"; list.tit<-"Medians"}
  if("ci.mean" %in% stats){list.stats<-c("mean","sd")
  list.tit<-paste0("Means with ",conf.level*100,"% CI")}
  if("percentiles" %in% stats){list.tit<-"Percentiles"
  list.stats<-c("min",paste0("p",seq(1,99,by=1)),"max")}
  if("deciles" %in% stats){list.tit<-"Deciles"
  list.stats<-c("min",paste0("p",seq(10,90,by=10)),"max")}
  if("quintiles" %in% stats){list.tit<-"Quintiles"
  list.stats<-c("min","p20","p40","p60","p80","max")}
  if("fivenumbers" %in% stats){list.tit<-"Five number summary"
  list.stats<-c("min","q1","median","q3","max")}
  if("quartiles" %in% stats){list.tit<-"Quartiles"
  list.stats<-c("min","p25","p50","p75","max")}

  unique.stats<-na.omit(unique(unlist(list.stats)))

  # Ready for calculations
  if(length(Warn.list)>1){
    invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                     function(x) cat(paste0("\n   ",x),file=stderr())))    }

  out<-build.summaries(x=x,by1=by1,name.1=name.by1,by2=by2,name.2=name.by2,
                       stats=unique.stats,digits=10,f.digits=10)

  out<-build.summary.plt(out,name.x,list.stats,list.tit,by1=by1,by2=by2,
                         name.1=name.by1,name.2=name.by2,stats=stats,plot.type=type,
                         bw=bw,color=color,legend=legend,conf.level=conf.level)
  if(print.stats==T){print(out)}
  myout<-out
}

#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices gray.colors
#' @importFrom grDevices rainbow
build.summary.plt<-function(out,name.x,list.stats,list.tit,by1=NULL,by2=NULL,name.1=NULL,name.2=NULL,
                            stats,plot.type,bw=F,color=NULL,legend=T,conf.level=0.95){
  Warn.list<-as.list("\nWarning:") # list to collect warn msg
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  par(mar=c(3.5,3.5,3,2.1),tck=(-0.01),tcl=NA,las=1,mgp=c(3, 0.3, 0),cex=0.88,
      cex.axis=0.88)
  # create the by list
  list.by<-list()
  if(!is.null(by1)){list.by[[name.1]]<-by1}
  if(!is.null(by2)){list.by[[name.2]]<-by2}

  if(stats %in% c("mean","median") & length(list.by)==1){
    if(max(nchar(as.character(max(round(out[[stats]],0)))))<5){mylas=1} else{mylas=0}
    par(las=mylas)
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))
    if(plot.type=="bars"){
      if(bw==T){use.color <- "grey"}
      if(bw==F & is.null(color)){use.color <- "skyblue"}
      if(!(is.null(color))){use.color<-color[1]}

      if(is.numeric(out[[names(list.by)]])){
        Warn.list<-c(Warn.list,"With 'bars' the by-var is treated as a factor")}

      barplot(out[[stats]],names.arg=out[[names(list.by)]],
              col=use.color,main=tit.stats)}
    if(plot.type=="lines"){
      use.color<-c("black")
      if(!(is.null(color))){use.color<-color[1]}

      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}

      plot(y=out[[stats]],x=x.n,type="l",col=use.color,main=tit.stats,
           axes=F,xlab="",ylab="",lwd=2)
      points(y=out[[stats]],x=x.n,pch=16,col=use.color)
      box()
      axis(2)
      axis(1,at=x.n,labels=x.c)}
    if(plot.type=="points"){
      use.color<-c("black")
      if(!(is.null(color))){use.color<-color[1]}
      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}
      plot(y=out[[stats]],x=x.n,pch=16,col=use.color,main=tit.stats,
           axes=F,xlab="",ylab="",lwd=2)
      box()
      axis(2)
      axis(1,at=x.n,labels=x.c)}

    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = 2.3,las=0)
    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))
      cat("\n")}
    par(pardef)
    return(out)
  }


  if(stats %in% c("mean","median") & length(list.by)==2){
    tit.stats<-paste0(name.x," | ",names(list.by)[1]," by ",names(list.by)[2])
    use.by2<-unique(out[[names(list.by)[2]]])

    if(bw==T){use.color <- grDevices::gray.colors(length(use.by2))}
    if(bw==F & is.null(color)){use.color <- grDevices::rainbow(length(use.by2))}
    if(!(is.null(color))){use.color<-color
    if(length(use.color)<length(use.by2)){
      use.color <- grDevices::rainbow(length(use.by2))}}
    names(use.color)[1:length(use.by2)]<-use.by2

    if(is.numeric(out[[names(list.by)[1]]])){x.c<-x.n<-out[[names(list.by)[1]]] }
    if(!is.numeric(out[[names(list.by)[1]]])){
      x.c<-factor(out[[names(list.by)[1]]],levels=out[[names(list.by)[1]]])
      x.n<-as.numeric(x.c)}

    mymin.y<-min(out[[stats]],na.rm=T);  mymax.y<-max(out[[stats]],na.rm=T)
    mymin.x<-min(x.n,na.rm=T); mymax.x<-max(x.n,na.rm=T)

    if(nchar(as.character(round(mymax.y,0)))<5){mylas=1} else{mylas=0}
    par(las=mylas)

    if(legend==T){
      by2.legend<-use.by2
      col.legend<-use.color
      #length.leg<-0.98+0.1735453*max((nchar(as.character(by2.legend)))-1)
      length.leg<-0.3+0.1735453*max((nchar(as.character(by2.legend)))-1)
      prop.leg<-min(length.leg/10.36,0.5)

      layout(matrix(c(1,2), nrow=1, byrow=TRUE),widths = c(1-prop.leg,prop.leg))
      par(mar=c(3.5,3.5,3,0),tck=(-0.01),tcl=NA,las=1,mgp=c(3, 0.3, 0),
          cex=0.88,cex.axis=0.88,las=mylas)
    }
    if(is.numeric(out[[names(list.by)[1]]])){
      plot(c(mymin.x,mymax.x),c(mymin.y,mymax.y),#axes=F,
           type="n",xlim=c(mymin.x,mymax.x),ylab="",xlab="",main=tit.stats)
    } else if(!is.numeric(out[[names(list.by)[1]]])){
      plot(c(mymin.x,mymax.x),c(mymin.y,mymax.y),axes=F,
           type="n",xlim=c(mymin.x,mymax.x),ylab="",xlab="",main=tit.stats)
    }
    for(k in use.by2){
      sel<-out[[names(list.by)[2]]]==k
      points(x.n[sel],out[[stats]][sel],type="l",col=use.color[k],lwd=2)
      points(x.n[sel],out[[stats]][sel],pch=16,col=use.color[k],cex=0.6)}

    if(!is.numeric(out[[names(list.by)[1]]])){
      box()
      axis(2)
      axis(1,at=x.n,labels=x.c)    }
    mtext(side = 1, names(list.by)[1], line = 2)
    mtext(side = 2, list.tit, line = 2.3,las=0)

    if(legend==T){
      par(mar=c(0,0,3,0))
      plot(1:10,1:10,type="n",axes=F)
      legend("topleft", legend=by2.legend,
             pch=21,pt.bg=col.legend,pt.cex=1,bty="n",x.intersp = 0.5,
             cex=0.9)    }

    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))
      cat("\n")}
    par(pardef)
    return(out)
  }

  if(stats %in% c("ci.mean") & length(list.by)==1){
    myout<-out
    t.q<-0
    t.q[out$n>1]<-qt((0.5+conf.level/2),df=(out$n[out$n>1] -1))
    myout[,paste0("low.",conf.level*100)]<-out$mean-t.q*(out$sd/sqrt(out$n))
    myout[,paste0("up.",conf.level*100)]<-out$mean+t.q*(out$sd/sqrt(out$n))
    out$marg.err<-t.q*(out$sd/sqrt(out$n))
    out$marg.err[is.na(out$marg.err)]<-0
    sel<-out$marg.err>0
    if(any(sel==F)){
      Warn.list<-c(Warn.list,"CI could not be calculated for by-groups with one case only")}
    mymax<-max(out$mean+out$marg.err)
    if(max(nchar(mymax))<5){mylas=1} else{mylas=0}
    par(las=mylas)
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))
    if(plot.type=="bars"){
      if(is.numeric(out[[names(list.by)]])){
        Warn.list<-c(Warn.list,"With 'bars' the by-var is treated as a factor")}
      mymin=min(0,min(out$mean-out$marg.err))
      if(bw==T){use.color <- "grey"}
      if(bw==F & is.null(color)){use.color <- "skyblue"}
      if(!(is.null(color))){use.color<-color[1]}
      orig.plt<-barplot(out$mean,names.arg=out[[names(list.by)]],
                        col=use.color,main=tit.stats,ylim=c(mymin,mymax),plot=F)
      barplot(out$mean,names.arg=out[[names(list.by)]],
              col=use.color,main=tit.stats,ylim=c(mymin,mymax))
      points(y=out$mean[sel],x=orig.plt[sel],pch=16,col="black")
      arrows(x0=orig.plt[sel], y0=(out$mean[sel]-out$marg.err[sel]),
             x1=orig.plt[sel], y1=(out$mean[sel]+out$marg.err[sel]),
             code=3, angle=90, col="black",lwd=1.5,
             length=min(0.1,(0.05*15/length(out$mean))))
      if(min(out$mean,na.rm=T)<0){
        segments(x0=0,y0=0,x1=max(orig.plt),y1=0)}
    }
    if(plot.type=="lines"){
      use.color<-c("black")
      if(!(is.null(color))){use.color<-color[1]}
      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}
      mymin=min(out$mean-out$marg.err,na.rm=T)

      plot(y=out$mean,x=x.n,type="l",col=use.color,main=tit.stats,
           axes=F,xlab="",ylab="",lwd=2,ylim=c(mymin,mymax))
      points(y=out$mean[sel],x=x.n[sel],pch=16,col=use.color)
      points(y=out$mean[sel==F],x=x.n[sel==F],pch=8,col=use.color)
      box()
      axis(2)
      axis(1,at=x.n,labels=x.c)
      arrows(x0=x.n[sel], y0=(out$mean[sel]-out$marg.err[sel]),
             x1=x.n[sel], y1=(out$mean[sel]+out$marg.err[sel]),
             code=3, angle=90, col="black",lwd=1.5,
             length=min(0.1,(0.05*15/length(out$mean))))
    }
    if(plot.type=="points"){
      use.color<-c("black")
      if(!(is.null(color))){use.color<-color[1]}
      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}
      mymin=min(out$mean-out$marg.err,na.rm=T)

      plot(y=out$mean,x=x.n,type="n",col=use.color,main=tit.stats,
           axes=F,xlab="",ylab="",lwd=2,ylim=c(mymin,mymax))
      points(y=out$mean[sel],x=x.n[sel],pch=16,col=use.color)
      points(y=out$mean[sel==F],x=x.n[sel==F],pch=8,col=use.color)
      box()
      axis(2)
      axis(1,at=x.n,labels=x.c)
      arrows(x0=x.n[sel], y0=(out$mean[sel]-out$marg.err[sel]),
             x1=x.n[sel], y1=(out$mean[sel]+out$marg.err[sel]),
             code=3, angle=90, col="black",lwd=1.5,
             length=min(0.1,(0.05*15/length(out$mean))))
    }

    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = 2.3,las=0)
    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))
      cat("\n")
    }
    par(pardef)
    return(myout)
  } # closes ci.mean

  if(stats %in% c("quartiles","quintiles","deciles","percentiles") &
     length(list.by)==1){
    pctiles<-switch(stats,
                    quartiles=c("min","p25","p50","p75","max"),
                    quintiles=c("min","p20","p40","p60","p80","max"),
                    deciles=c("min",paste0("p",seq(10,90,by=10)),"max"),
                    percentiles=c("min",paste0("p",seq(1,99,by=1)),"max"))
    if(is.numeric(out[[names(list.by)]])){
      x.c<-x.n<-out[[names(list.by)]] }
    if(!is.numeric(out[[names(list.by)]])){
      x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
      x.n<-as.numeric(x.c)}
    mymin.y<-min(out$min);  mymax.y<-max(out$max)
    mymin.x<-min(x.n); mymax.x<-max(x.n)
    if(max(nchar(as.character(max(round(mymax.y,0)))))<5){mylas=1} else{mylas=0}
    par(las=mylas)
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))

    colramp.pct <- grDevices::colorRampPalette(c("red", "darkorange","gold",
                                           "green","darkgreen"))
                                           if(bw==T){use.color <- grDevices::gray.colors(length(pctiles))}
    if(bw==F & is.null(color)){use.color <- colramp.pct(length(pctiles))}
    if(!(is.null(color))){use.color<-color
    if(length(use.color)<length(pctiles)){
      use.color <- colramp.pct(length(pctiles))}}
    names(use.color)[1:length(pctiles)]<-pctiles

    if(legend==T){
      pct.legend<-pctiles
      col.legend<-use.color
      if(stats=="percentiles"){
        pctles.sel<-pctiles %in% c("min",paste0("p",seq(10,90,by=10)),"max")
        pct.legend<-pctiles[pctles.sel]
        col.legend<-use.color[pctles.sel]    }
      #length.leg<-0.98+0.1735453*max((nchar(as.character(pct.legend)))-1)
      length.leg<-0.8+0.1735453*max((nchar(as.character(pct.legend)))-1)
      prop.leg<-min(length.leg/10.36,0.5)

      layout(matrix(c(1,2), nrow=1, byrow=TRUE),widths = c(1-prop.leg,prop.leg))
      par(mar=c(3.5,3.5,3,0),tck=(-0.01),tcl=NA,las=1,mgp=c(3, 0.3, 0),cex=0.88,
          cex.axis=0.88,las=mylas)
    }
    plot(c(mymin.x,mymax.x),c(mymin.y,mymax.y),#axes=F,
         type="n",xlim=c(mymin.x,mymax.x),ylab="",xlab="",main=tit.stats)
    for(k in pctiles){
      points(x.n,out[[k]],type="l",col=use.color[k],lwd=2)
      points(x.n,out[[k]],pch=16,col=use.color[k],cex=0.6)
    }
    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = 2.3,las=0)
    if(legend==T){
      par(mar=c(0,0,3,0))
      plot(1:10,1:10,type="n",axes=F)
      legend("topleft", legend=pct.legend,
             pch=21,pt.bg=col.legend,pt.cex=1,bty="n",x.intersp = 0.5,
             cex=0.9)    }

    if(length(Warn.list)>1){
      invisible(lapply(Warn.list[duplicated(Warn.list)==F],
                       function(x) cat(paste0("\n   ",x),file=stderr())))
      cat("\n")    }
    par(pardef)
    return(out)
  } # closes quantiles
}
