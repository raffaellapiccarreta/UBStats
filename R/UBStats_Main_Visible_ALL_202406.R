## ---------------------------------------## 
## ------- Main (visible) functions ------## 
## ---------------------------------------## 
# Tables ------
## Univariate distributions -----
#' Analysis of a univariate distribution using frequency tables
#'
#' \code{distr.table.x()} computes the frequency table of a vector or a factor.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param freq A character vector specifying the set of frequencies to be 
#'   displayed (more options are allowed). Allowed options (possibly abbreviated)
#'   are \code{"counts"}, \code{"percentages"}, \code{"proportions"},
#'   \code{"densities"} (only for variables classified into intervals
#'   or measured in classes), and \code{"cumulative"}. 
#'   If no frequency is specified, \code{"counts"}
#'   and \code{"proportions"} are displayed by default.
#'   If only \code{"cumulative"} is specified, counts and proportions 
#'   are displayed too, with their respective cumulative frequencies.
#' @param total Logical value indicating whether the sum of the requested
#'   frequencies should be added to the table; default to \code{TRUE}.
#' @param breaks Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer indicating the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   the entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of a numerical variable \code{x} when classified 
#'   into intervals should be displayed avoiding scientific notation; 
#'   default to \code{TRUE}.
#' @param interval Logical value indicating whether \code{x} is a variable
#'   measured in intervals (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is tabulated as it is, even if
#'   results are not necessarily consistent; default to \code{FALSE}.
#' @param f.digits,p.digits,d.digits Integer values specifying the number of 
#'   decimals used to round respectively proportions (default: \code{f.digits=2}),  
#'   percentages (default: \code{p.digits=0}), and densities 
#'   (default: \code{d.digits=5}). If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether frequencies and
#'   densities should be forcedly rounded to the number of decimals specified in
#'   \code{f.digits}, \code{p.digits}, and \code{d.digits} even if non-zero 
#'   values are rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables (typically densities) should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the environment
#'   from which \code{distr.table.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table (converted to dataframe) listing the 
#'   values taken by the variable, arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, 
#'   order of levels for factors, ordered intervals for classified
#'   variables or for variables measured in classes), and the requested set 
#'   of frequencies.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @seealso \code{\link{distr.table.xy}()} for tabulating a bivariate
#'   distribution.
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Character vectors, factors, and discrete numeric vectors
#' distr.table.x(Education, data = MktDATA)
#' 
#' distr.table.x(Children, freq = c("count","prop","cum"),
#'               data = MktDATA)
#' 
#' # Numerical variable classified into intervals
#' # - Classes of equal width
#' distr.table.x(AOV, breaks = 6, freq = c("Count","Prop","Perc","Cum"),
#'               p.digits = 2, data = MktDATA)
#' # - Classes with specified endpoints
#' distr.table.x(AOV, breaks = c(0,20,30,50,100,180),
#'               freq = c("Count","Perc","Cum","Densities"), 
#'               p.digits = 2, data = MktDATA)
#' # Numerical variable measured in classes
#' # - Variable measured in classes
#' distr.table.x(Income, freq = c("count","prop","cum","dens"),
#'               interval = TRUE, data = MktDATA)
#' # - An example of non-consistent intervals. 
#' #   Densities are not calculated
#' x.inconsistent <- c(rep("0;10",30),rep("10;20",25),rep("25;8",25),
#'                     rep("15;31",15),rep("20;45",16),rep("30;40",18))
#' distr.table.x(x.inconsistent, freq = c("count","prop","cum","dens"),
#'               interval = TRUE)
#' 
#' # Arguments adj.breaks, use.scientific, and force.digits
#' #  A variable with a very wide range (very small densities)
#' LargeX <- MktDATA$AOV*5000000 
#' # - Default: manages possible excess of rounding
#' distr.table.x(LargeX, breaks = 5, 
#'               freq = c("count","percent","densities"))
#' # - Forcing digits to the default values 
#' distr.table.x(LargeX, breaks = 5,
#'               freq=c("count","percent","dens"),
#'               force.digits = TRUE)
#' #  - Scientific notation for frequencies/densities 
#' distr.table.x(LargeX, breaks = 5,
#'               freq = c("count","percent","dens"),
#'               use.scientific = TRUE)
#' #  - Scientific notation both for intervals’ endpoints 
#' #    and for frequencies/densities
#' distr.table.x(LargeX, breaks = 5, adj.breaks = FALSE,
#'               freq = c("count","percent","dens"),
#'               use.scientific = TRUE)
#' 
#' # Output a dataframe with the table
#' table.AOV<-distr.table.x(AOV, breaks = c(0,20,30,50,100,180),
#'                          freq = c("Count","Perc","Cum","Dens"), 
#'                          data = MktDATA)
#'                          
#' @export
distr.table.x<-function(x, freq = c("counts","proportions"), total = TRUE,
                        breaks, adj.breaks = TRUE, interval = FALSE,
                        f.digits = 2, p.digits = 0, d.digits = 5,
                        force.digits = FALSE,
                        use.scientific=FALSE,data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x
  
  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages","cumulative","densities"),
                            onlyone = FALSE,listall = TRUE,err.list=Err.list.para,
                            warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)
  
  # Create a list with all the info needed to build tables
  if(exist.x){
    all.info<-build.Xlist(x,name.x,breaks,interval,
                          adj.breaks=adj.breaks,consistency = FALSE,
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
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
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
  if(!is.numeric(f.digits)){
    Warn.list<-c(Warn.list,"'f.digits' should be a number. The default value (2) is used")
    f.digits<-2
  }
  if(!is.numeric(p.digits)){
    Warn.list<-c(Warn.list,"'p.digits' should be a number. The default value (0) is used")
    p.digits<-0
  }
  if(!is.numeric(d.digits)){
    Warn.list<-c(Warn.list,"'d.digits' should be a number. The default value (5) is used")
    d.digits<-5
  }
  
  # Print all the warnings
  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)
      }
      cat("\n")
    }
    if(msg.p$msg){
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }
  # adjust digits
  use.digits=c("Prop"=f.digits,"Percent"=p.digits,
               "Density"=d.digits,"Count"=0)
  
  # Build the tables
  out<-Signal.Error(build.table.x(Xlist,name.x,freq,total,use.digits,
                                  force.digits,
                                  use.scientific=use.scientific),
                    type.print=type.print)
}

## Bivariate distributions -----
#' Analysis of a bivariate distribution using cross-tables
#'
#' \code{distr.table.xy()} displays tables of joint or conditional
#'   distributions.
#'
#' @param x,y Unquoted strings identifying the variables whose joint
#'   distribution has to be analysed. \code{x} and \code{y} can be the
#'   name of a vector or a factor in the workspace or the name of one of
#'   the columns in the data frame specified in the \code{data} argument.
#'   Note that in the table \code{x} is displayed on the \emph{rows} and
#'   \code{y} on the \emph{columns}.
#' @param freq A character vector specifying the set of frequencies 
#'   to be displayed (more options are allowed). Allowed options 
#'   (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"} and \code{"proportions"}.
#' @param freq.type A character vector specifying the types of
#'   frequencies to be displayed (more types are allowed). 
#'   Allowed options are \code{joint} (default) 
#'   for joint frequencies, \code{x|y} (or \code{column}) for the distributions
#'   of \code{x} conditioned to \code{y}, and \code{y|x} (or \code{row}) for
#'   the distributions of \code{y} conditioned to \code{x}.
#' @param total Logical value indicating whether the sum of the requested
#'   frequencies should be added to the table; default to \code{TRUE}.
#' @param breaks.x,breaks.y Allow to classify the variables \code{x} 
#'   and/or \code{y}, if \emph{numerical}, into intervals. 
#'   They can be integers indicating the number of intervals of
#'   equal width used to classify \code{x} and/or \code{y}, or 
#'   vectors of increasing numeric values defining the endpoints of 
#'   the intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). 
#'   To cover the entire range of values taken by one variable, 
#'   the maximum and the minimum values should be included between 
#'   the first and the last break. 
#'   It is possible to specify a set of breaks covering only a portion 
#'   of the variable's range.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of a numerical variable (\code{x} or \code{y}) 
#'   when classified into intervals should be displayed avoiding 
#'   scientific notation; default to \code{TRUE}.
#' @param interval.x,interval.y Logical values indicating whether 
#'   \code{x} and/or \code{y} are variables measured in classes 
#'   (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with 
#'   upper endpoint higher than the lower one), the variable is 
#'   tabulated as it is, even if results are not necessarily 
#'   consistent; default to \code{FALSE}.
#' @param f.digits,p.digits Integer values specifying the number of 
#'   decimals used to round respectively proportions 
#'   (default: \code{f.digits=2}) and percentages (default: \code{p.digits=0}). 
#'   If the chosen rounding
#'   formats some non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether proportions and
#'   percentages should be forcedly rounded to the number of decimals specified in
#'   \code{f.digits} and \code{p.digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param data An optional data frame containing \code{x} and/or \code{y}.
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.table.xy()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A list whose elements are the requested tables 
#'   (converted  to dataframes) listing the values taken 
#'   by the two variables arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, 
#'   order of levels for factors, ordered intervals for classified
#'   variables or for variables measured in classes)
#'   and the specified joint or conditional types of frequencies.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @seealso \code{\link{distr.table.x}()} for tabulating a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Character vectors, factors, and discrete numeric vectors
#' # - Default: joint counts
#' distr.table.xy(LikeMost, Children, data = MktDATA) 
#'
#' # - Joint and conditional distribution of x|y
#' #   counts and proportions, no totals
#' distr.table.xy(LikeMost, Education, freq = c("counts","Prop"), 
#'                freq.type = c("joint","x|y"), total = FALSE,
#'                data = MktDATA)
#' # - Joint and conditional row and column distributions (%) 
#' distr.table.xy(CustClass, Children, freq = "Percentages", 
#'                freq.type = c("joint","row","column"),
#'                data = MktDATA)
#' 
#' # Numerical variables classified or measured in classes
#' # - A numerical variable classified into intervals 
#' #   and a factor
#' distr.table.xy(CustClass, TotPurch, 
#'                breaks.y = c(0,5,10,15,20,35),
#'                freq = c("Counts","Prop"), freq.type = "y|x", 
#'                data = MktDATA)
#' 
#' # - Two numerical variables, one measured in classes
#' #   and the other classified into intervals 
#' distr.table.xy(Income.S, TotPurch, interval.x = TRUE,
#'                breaks.y = c(0,5,10,15,20,35),
#'                freq = c("Counts","Prop"), 
#'                freq.type = c("row","col"), data = MktDATA)
#' 
#' # Argument force.digits
#' # - Default: manages possible excess of rounding
#' distr.table.xy(CustClass, Children, freq = "Percentages", 
#'                freq.type = c("x|y"),data = MktDATA)
#' # - Force to the required rounding
#' distr.table.xy(CustClass, Children, freq = "Percentages", 
#'                freq.type = c("x|y"), 
#'                force.digits = TRUE, data = MktDATA)
#' 
#' # Output the list with the requested tables
#' tables.xy<-distr.table.xy(Income.S, TotPurch, 
#'                           interval.x = TRUE,
#'                           breaks.y = c(0,5,10,15,20,35),
#'                           freq = c("Counts","Prop"), 
#'                           freq.type = c("joint","row","col"), 
#'                           data = MktDATA)
#' 
#' @export
distr.table.xy<-function(x, y, freq = "counts", freq.type = "joint",
                         total = TRUE,
                         breaks.x, breaks.y, adj.breaks = TRUE,
                         interval.x = FALSE, interval.y = FALSE,
                         f.digits = 2, p.digits = 0, 
                         force.digits = FALSE,
                         data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x
  
  # Check if 'y' exists and if it is coherent (not missing)
  name.y<-deparse1(substitute(y))
  check.y<-chk.data(y,data,deparse1(substitute(data)),
                    name.y,missing = TRUE,err.list=Err.list.input,
                    warn.list=Warn.list)
  exist.y<-check.y$exist.x ;
  Err.list.input<-check.y$err.list
  Warn.list<-check.y$warn.list
  y<-check.y$vec.x
  
  # check if x and y have the same length
  if(exist.x && exist.y && (length(x) != length(y))){
    Err.list.input<-c(Err.list.input,"'x' and 'y' should have the same length")
  }
  
  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages"),
                            onlyone = FALSE,listall = TRUE,
                            err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)
  
  # Check required freq.type
  check.ftype<-chkpar.option(value=freq.type,
                             allowed=c("joint","x|y","y|x","rows","columns"),
                             onlyone = FALSE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.ftype$err.list ; Warn.list<-check.ftype$warn.list
  exist.t<-check.ftype$exist;  type.tab<-unique(check.ftype$value)
  type.tab<-as.character(factor(type.tab,levels=c("joint","x|y","y|x","rows","columns"),
                                labels=c("Joint","x_y","y_x","row","col")))
  
  # Create lists with all the info needed to build tables
  List.print<-NULL
  if(exist.x){
    all.infoX<-build.Xlist(x,name.x,breaks.x,interval.x,adj.breaks=adj.breaks,consistency = FALSE,
                           err.list=Err.list.options,warn.list=Warn.list,
                           list.print=List.print,suffix = TRUE)
    Err.list.options<-all.infoX$err.list
    Warn.list<-all.infoX$warn.list
    List.print<-all.infoX$list.print
    Xlist<-all.infoX$Vlist
  }
  if(exist.y){
    all.infoY<-build.Xlist(y,name.y,breaks.y,interval.y,adj.breaks=adj.breaks,consistency = FALSE,
                           err.list=Err.list.options,warn.list=Warn.list,
                           list.print=List.print,
                           suffix = TRUE)
    Err.list.options<-all.infoY$err.list
    Warn.list<-all.infoY$warn.list
    List.print<-all.infoY$list.print
    Ylist<-all.infoY$Vlist
  }
  
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print) }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)  }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  if(!is.numeric(f.digits)){
    Warn.list<-c(Warn.list,"'f.digits' should be a number. The default value (2) is used")
    f.digits<-2
  }
  if(!is.numeric(p.digits)){
    Warn.list<-c(Warn.list,"'p.digits' should be a number. The default value (0) is used")
    p.digits<-0
  }
  # Ready for tables!
  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)
      }
      cat("\n")
    }
    if(msg.p$msg){
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      cat("\n")
    }
  }
  # adjust digits
  use.digits=c("Proportions"=f.digits,"Percentages"=p.digits,
               "Counts"=0)
  # Build the tables
  out<-Signal.Error(build.table.xy(Xlist,Ylist,name.x,name.y,
                                   type.tab,freq,total,use.digits,
                                   force.digits),
                    type.print=type.print)
}

# Plots --------
## Univariate distributions -----
#' Analysis of a univariate distribution using plots
#'
#' \code{distr.plot.x()} generates plots of a univariate distribution.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be analysed. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param freq A single character specifying the frequencies to be
#'   displayed. Allowed options (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"}, \code{"proportions"}, \code{"densities"}
#'   (for histograms and density plots).
#' @param plot.type A single character specifying the type of plot to build.
#'   Allowed options are \code{"pie"}, \code{"bars"}, \code{"spike"},
#'   \code{"histogram"}, \code{"density"}, \code{"boxplot"}, and
#'   \code{"cumulative"}.
#' @param ord.freq A single character vector that can be specified when
#'   \code{plot.type = "pie"} or \code{plot.type = "bars"}. It indicates
#'   whether the levels of \code{x} should be displayed in a standard order
#'   (\code{ord.freq = "none"}, the default) or in an increasing 
#'   or decreasing order (\code{ord.freq = "increasing"} or 
#'   \code{ord.freq = "decreasing"}).
#' @param breaks Allows to classify a \emph{numerical} variable \code{x} into
#'   intervals. It can be an integer indicating the number of intervals of
#'   equal width used to classify \code{x}, or a vector of increasing numeric
#'   values defining the endpoints of intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). To cover
#'   the entire range of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to specify
#'   a set of breaks covering only a portion of the \code{x} range.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of a numerical variable \code{x} when classified 
#'   into intervals should be displayed avoiding scientific notation; 
#'   default to \code{TRUE}.
#' @param interval Logical value indicating whether \code{x} is a variable
#'   measured in intervals (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with upper endpoint
#'   higher than the lower one), the variable is analyzed as it is, even if
#'   results are not necessarily consistent; default to 
#'   \code{FALSE}.
#' @param bw Logical value indicating whether plots should be colored 
#'   in scale of greys (\code{TRUE}) rather than using a standard 
#'   palette (\code{FALSE}, default).
#' @param color Optional string vector allowing to specify colors 
#'   to use in the plot rather than a standard palette  
#'   (\code{NULL}, default).
#' @param use.scientific Logical value indicating whether numbers on 
#'   axes should be displayed using scientific notation 
#'   (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the environment
#'   from which \code{distr.plot.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.table.x}()} for tabulating a univariate
#'   distribution.
#' @seealso \code{\link{distr.table.xy}()} for tabulating a bivariate
#'   distribution.
#' @seealso \code{\link{distr.plot.xy}()} for plotting a bivariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Pie charts 
#' # - A character variable: grey scale
#' distr.plot.x(x = LikeMost, plot.type = "pie", bw = TRUE, data = MktDATA)
#' # - A discrete numeric variable: user-defined palette
#' distr.plot.x(x = Children, plot.type = "pie", 
#'              color=c("red","gold","green","forestgreen"),
#'              data = MktDATA)
#' 
#' # Bar charts 
#' # - A factor: standard order of levels 
#' distr.plot.x(x = Education, plot.type = "bars", 
#'              freq = "percentage", data = MktDATA)
#' # - A factor: levels arranged by decreasing percentage 
#' distr.plot.x(x = Education, plot.type = "bars", 
#'              freq = "perc", ord.freq = "dec", data = MktDATA)
#' # - A discrete variable (note: distance between values
#' #   not taken into account)
#' distr.plot.x(x = NPickUp_Purch, plot.type = "bars",
#'              freq = "percentage", data = MktDATA)
#' 
#' # Spike plots 
#' # - A discrete variable
#' distr.plot.x(x = NPickUp_Purch, plot.type = "spike", 
#'              freq = "percent", data = MktDATA)
#' # - A factor (levels placed at the same distance)
#' distr.plot.x(x = Education, plot.type = "spike", 
#'              freq = "prop",data = MktDATA)
#' # - A variable measured in classes (levels placed at the 
#' #   same distance)
#' distr.plot.x(x = Income.S, interval = TRUE,
#'              plot.type = "spike", 
#'              freq = "prop",data = MktDATA)
#' # - A numeric variable classified into intervals
#' #   (levels placed at the same distance)
#' distr.plot.x(x = AOV, breaks = 5, plot.type = "spike", 
#'              data = MktDATA)
#' 
#' # Cumulative distribution plots
#' # - A discrete variable
#' distr.plot.x(x = Children, plot.type = "cum", data = MktDATA)
#' # - A continuous numerical variable 
#' distr.plot.x(x = AOV, plot.type = "cum", 
#'              freq = "perc", data = MktDATA)
#' # - A numeric variable classified into intervals
#' distr.plot.x(AOV, plot.type = "cum", 
#'              breaks = c(0,20,40,60,80,100,180), data = MktDATA)
#' # - A variable measured in classes
#' distr.plot.x(Income, plot.type = "cum", interval = TRUE, 
#'              freq = "percent", data = MktDATA)
#' # - A factor
#' distr.plot.x(x = Education, plot.type = "cum", 
#'              freq = "prop",data = MktDATA)
#' 
#' # Histograms 
#' # - A continuous numerical variable: no breaks provided
#' #    default classes built by R
#' distr.plot.x(x = AOV, plot.type = "histogram", data = MktDATA)
#' # - A continuous numerical variable: equal width intervals
#' distr.plot.x(x = AOV, plot.type = "histogram", 
#'              breaks = 10, data = MktDATA)
#' # - A continuous numerical variable: specified breaks
#' distr.plot.x(AOV, plot.type = "histogram", 
#'              breaks = c(0,20,40,60,80,100,180), 
#'              data = MktDATA)
#' # - A variable measured in classes
#' distr.plot.x(Income, plot.type = "histogram", 
#'              interval = TRUE, data = MktDATA)
#' 
#' # Density plots 
#' # - A  numerical variable
#' distr.plot.x(x = AOV, plot.type = "density", data = MktDATA)
#' # - A  numerical variable: breaks are ignored
#' distr.plot.x(AOV, plot.type = "density", 
#'              breaks = c(0,20,40,60,80,100,180), 
#'              data = MktDATA)
#' # - A variable measured in classes
#' distr.plot.x(Income, plot.type = "density", 
#'              interval = TRUE, data = MktDATA)
#' 
#' # Boxplots (only for numerical unclassified variables)
#' # - A  numerical variable
#' distr.plot.x(x = TotVal, plot.type = "boxplot", data = MktDATA)
#' # - A  numerical variable: with specified breaks
#' #   the plot is not built
#' \dontrun{distr.plot.x(AOV, plot.type = "boxplot", 
#'                       breaks = c(0,20,40,60,80,100,180), 
#'                       data = MktDATA)}
#' 
#' # Arguments adj.breaks, use.scientific
#' #  A variable with a very wide range (very small densities)
#' LargeX<-MktDATA$AOV*5000000 
#' #  - Default formatting for intervals' endpoints
#' distr.plot.x(LargeX, breaks = 5, plot.type = "spike")
#' #  - Scientific notation for intervals' endpoints
#' distr.plot.x(LargeX, breaks = 5,plot.type = "spike",
#'              adj.breaks = FALSE)
#' #  - Default formatting for axes
#' distr.plot.x(LargeX, breaks = 5,plot.type = "histogram",
#'              freq = "densities")
#' #  - Scientific notation for axes
#' distr.plot.x(LargeX, breaks = 5,plot.type = "histogram",
#'              freq = "densities",use.scientific = TRUE)
#' 
#' @export
distr.plot.x<-function(x,freq="counts",plot.type,ord.freq="none",
                       breaks,adj.breaks = TRUE,interval = FALSE,
                       bw = FALSE,color=NULL,use.scientific=FALSE,
                       data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input,warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x
  
  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages","densities"),
                            onlyone = TRUE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)
  
  check.order<-chkpar.option(value=ord.freq,
                             allowed=c("none","increasing","decreasing"),
                             onlyone = TRUE,listall = TRUE,
                             err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.order$err.list ; Warn.list<-check.order$warn.list
  exist.o<-check.order$exist;  ord.freq<-unique(check.order$value)
  
  # Check plots are correctly specified / same procedure as before
  if(isTRUE(missing(plot.type))){
    Err.list.para<-c(Err.list.para,"'plot.type' must be specified")
    exist.plt <- FALSE} else {
      if(!is.character(plot.type)){plot.type<-deparse1(substitute(plot.type))}
      check.plt<-chkpar.option(value=plot.type,
                               allowed=c("pie","bars","spike","cumulative",
                                         "histogram","boxplot","density"),
                               onlyone = TRUE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
      Err.list.para<-check.plt$err.list ; Warn.list<-check.plt$warn.list
      exist.plt<-check.plt$exist;  type.plt<-unique(check.plt$value)
    }
  # Check coherency between plots and types of variables
  if(exist.f && exist.plt && freq=="densities" &&
     type.plt=="cumulative"){
    Err.list.options<-c(Err.list.options,"Cumulative plots cannot be built based on densities")
    exist.plt <- FALSE }
  
  if(exist.x && exist.plt){
    isnum<-is.numeric(x) # check x numeric
    isfac<-is.factor(x) # check x is a factor
    if((isFALSE(missing(breaks)) | interval) && type.plt=="boxplot"){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
      exist.plt <- FALSE }
    if(isTRUE(missing(breaks)) && !interval && !is.numeric(x) &&
       type.plt %in% c("boxplot","histogram","density")){
      Err.list.para<-c(Err.list.para,paste0("'",name.x,"' is character or factor: the required plot cannot be built"))
      exist.plt <- FALSE }
  }
  # Create a list with all the info needed to build plots
  # if plots for numerical vars, endpoints must be consistent
  if(!exist.plt & (isFALSE(missing(breaks)) | interval)){
    Warn.list<-c(Warn.list,paste0("Consistency of breaks and interval cannot be tested",
                                  "\n   "," because of mis-specified/missing 'plot.type'"))
  }
  if(exist.x && exist.plt){
    if(type.plt %in% c("boxplot","histogram","density")){
      all.infoX<-build.Xlist(x,name.x,breaks,interval,adj.breaks=adj.breaks,consistency = TRUE,
                             err.list=Err.list.options,warn.list=Warn.list,list.print=NULL)
      Err.list.options<-all.infoX$err.list
      Warn.list<-all.infoX$warn.list
      List.print<-all.infoX$list.print
      Xlist<-all.infoX$Vlist
    } else {
      all.infoX<-build.Xlist(x,name.x,breaks,interval,adj.breaks=adj.breaks,consistency = FALSE,
                             err.list=Err.list.options,warn.list=Warn.list,list.print=NULL)
      Err.list.options<-all.infoX$err.list
      Warn.list<-all.infoX$warn.list
      List.print<-all.infoX$list.print
      Xlist<-all.infoX$Vlist
    }
  }
  
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
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
  
  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)
      }
      cat("\n")
    }
    if(msg.p$msg){
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
  par(mar=c(3.5,3.7,3,2.1),tck=(-0.01),tcl=NA,las=1,
      mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  if(type.plt=="pie"){
    Signal.Error(plt.x.pie(Xlist$tab.x,bw=bw,color=color,name.x,freq),
                 type.print=type.print)
  }
  if(type.plt=="bars"){
    Signal.Error(plt.x.bars(Xlist$tab.x,bw=bw,color=color,name.x,freq),
                 type.print=type.print)
  }
  if(type.plt=="spike"){
    Signal.Error(plt.x.spike(Xlist,bw=bw,color=color,name.x,freq),
                 type.print=type.print)
  }
  if(type.plt=="cumulative"){
    Signal.Error(plt.x.cum(Xlist,bw=bw,color=color,name.x,freq,
                           adj.breaks=as.logical(1-use.scientific)),
                 type.print=type.print)
  }
  if(type.plt=="histogram"){
    Signal.Error(plt.x.hist(Xlist,bw=bw,color=color,name.x,
                            freq,adj.breaks=as.logical(1-use.scientific)),
                 type.print=type.print)
  }
  if(type.plt=="density"){
    Signal.Error(plt.x.density(Xlist,bw=bw,color=color,name.x,
                               freq="Density",
                               adj.breaks=as.logical(1-use.scientific)),
                 type.print=type.print)
  }
  if(type.plt=="boxplot"){
    Signal.Error(plt.x.boxplot(Xlist,bw=bw,color=color,name.x,
                               freq,adj.breaks=as.logical(1-use.scientific)),
                 type.print=type.print)
  }
  par(pardef)
}

## Bivariate distributions -----
#' Analysis of a bivariate distribution using plots
#'
#' \code{distr.plot.xy()} generates plots of a bivariate distribution.
#'
#' @param x,y Unquoted strings identifying the variables whose
#'   distribution has to be graphically displayed. \code{x} and 
#'   \code{y} can be the name of a vector or a factor in the 
#'   workspace or the name of one of the columns in the data frame 
#'   specified in the \code{data} argument.
#'   Note that in the plot \code{x} is reported on the 
#'   \emph{horizontal} axis while \code{y} is reported on 
#'   the \emph{vertical} axis.
#' @param plot.type A single character specifying the type of plot to build.
#'   Allowed options are \code{"bars"}, \code{"scatter"}, and
#'   \code{"boxplot"}. If both \code{x} and \code{y} are character vectors
#'   or factors and \code{bar.type = "scatter"} a bubble plot is 
#'   built, with dots having a size proportional to the joint frequency of
#'   each pair of observed values. If \code{bar.type = "boxplot"}, at least
#'   one input variable must be numeric; when both the variables are numeric
#'   the conditional distributions of \code{y|x} are displayed, unless
#'   otherwise specified using \code{freq.type="x|y"}.
#' @param bar.type A single character indicating whether in a bar plot
#'   stacked (\code{bar.type = "stacked"}, default) or side-by-side
#'   (\code{bar.type = "beside"}) bars should be displayed.
#' @param freq A single character specifying the frequencies 
#'   to be displayed when a bar plot is requested (\code{plot.type="bars"}). 
#'   Allowed options (possibly abbreviated) are \code{"counts"},
#'   \code{"percentages"} and \code{"proportions"}.
#' @param freq.type A single character specifying the type of
#'   frequencies to be displayed when a bar plot is requested 
#'   (\code{plot.type="bars"}). Allowed options are \code{joint} (default) 
#'   for joint frequencies, \code{x|y} for the distributions
#'   of \code{x} conditioned to \code{y}, and \code{y|x} for
#'   the distributions of \code{y} conditioned to \code{x}. The option
#'   \code{x|y} can also be used when \code{plot.type="boxplot"}.
#' @param breaks.x,breaks.y Allow to classify the variables \code{x} 
#'   and/or \code{y}, if \emph{numerical}, into intervals. 
#'   They can be integers indicating the number of intervals of
#'   equal width used to classify \code{x} and/or \code{y}, or 
#'   vectors of increasing numeric values defining the endpoints of 
#'   the intervals (closed on the left and open
#'   on the right; the last interval is closed on the right too). 
#'   To cover the entire range of values taken by one variable, 
#'   the maximum and the minimum values should be included between 
#'   the first and the last break. 
#'   It is possible to specify a set of breaks covering only a portion 
#'   of the variable's range.
#' @param interval.x,interval.y Logical values indicating whether 
#'   \code{x} and/or \code{y} are variables measured in classes 
#'   (\code{TRUE}). If the detected intervals are not
#'   consistent (e.g. overlapping intervals, or intervals with 
#'   upper endpoint higher than the lower one), the variable is 
#'   analyzed as it is, even if results are not necessarily 
#'   consistent; default to \code{FALSE}.
#' @param bw Logical value indicating whether plots should be colored 
#'   in scale of greys (\code{TRUE}) rather than using a standard 
#'   palette (\code{FALSE}, default).
#' @param color Optional string vector allowing to specify colors 
#'   to use in the plot rather than a standard palette  
#'   (\code{NULL}, default).
#' @param var.c An optional unquoted string identifying one variable 
#'   used to color points in a scatter plot (\code{plot.type="scatter"}), 
#'   that can be defined same way as \code{x}. This is allowed only when 
#'   at least one of the input variables \code{x} and \code{y} is numeric.
#' @param breaks.c Allows to classify the variable \code{var.c}, if
#'   \emph{numerical}, into intervals. It can be defined as \code{breaks.x}.
#' @param interval.c Logical value indicating whether \code{var.c} is a variable
#'   measured in intervals (\code{TRUE}) or not, as described for
#'   \code{interval.x}; default to \code{FALSE}.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of a numerical variable (\code{x}, or \code{y}, 
#'   or \code{var.c}) when classified into intervals should be 
#'   displayed avoiding scientific notation; default to \code{TRUE}.
#' @param fitline Logical value indicating whether the line of best fit (also
#'   called trend line or regression line) should be added to a scatter plot
#'   (\code{fitline = TRUE}) or not (\code{fitline = FALSE}; default).
#' @param legend Logical value indicating whether a legend should be displayed
#'   in the plot (\code{legend = TRUE}; default) or not (\code{legend = FALSE}).
#' @param use.scientific Logical value indicating whether numbers on 
#'   axes should be displayed using scientific notation 
#'   (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or \code{y} 
#'   and/or \code{var.c} (the variable used to color points in scatter plots). 
#'   If not found in \code{data}, the variables are taken from the environment
#'   from which \code{distr.plot.xy()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.table.xy}()} for tabulating a bivariate
#'   distribution.
#' @seealso \code{\link{distr.table.x}()} for tabulating a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Bivariate bar plots
#' # - Two discrete variables (factor or vector with few levels)
#' #   Joint counts
#' distr.plot.xy(CustClass, Children,plot.type = "bars", 
#'               freq = "Counts", freq.type = "joint",
#'               data = MktDATA)
#' # - Two discrete variables (factor or vector with few levels)
#' #   Joint percentages, side-by-side bars
#' #   User-defined colors
#' distr.plot.xy(Children,CustClass, plot.type = "bars", 
#'               bar.type = "beside",
#'               freq = "percent", freq.type = "joint",
#'               color = c("red","gold","green","forestgreen"),
#'               data = MktDATA)
#' # - One numeric variable classified into intervals
#' #   and one variable measured in classes
#' #   Conditional percentages of x|y 
#' distr.plot.xy(TotPurch, Income, plot.type = "bars", 
#'               freq = "percent",freq.type = "x|y",
#'               breaks.x = c(0,5,10,15,20,35),
#'               interval.y = TRUE, data = MktDATA)
#' #   Conditional percentages of y|x 
#' distr.plot.xy(TotPurch, Income, plot.type = "bars", 
#'               freq = "percent",freq.type = "y|x",
#'               breaks.x = c(0,5,10,15,20,35),
#'               interval.y = TRUE, data = MktDATA)
#' 
#' # Side-by-side boxplots
#' # - A continuous variable conditioned to a factor, 
#' #   a character, or a classified variable
#' #   The distributions of the numeric variable conditioned
#' #   to the factor (or character) are displayed
#' distr.plot.xy(x = AOV, y = Education, plot.type = "boxplot",
#'               data = MktDATA)
#' distr.plot.xy(x = Income.S, y = AOV, plot.type = "boxplot",
#'               interval.x = TRUE, data = MktDATA)
#' distr.plot.xy(x = Baseline, y = TotPurch, plot.type = "boxplot",
#'               breaks.y = c(0,5,10,15,20,35),
#'               data = MktDATA)
#' # - Two numerical variables. By default distributions 
#' #   of y|x are displayed unless differently 
#' #   specified in freq.type
#' distr.plot.xy(x = NPickUp_Purch, y = NWeb_Purch,
#'               plot.type = "boxplot", data = MktDATA)
#' distr.plot.xy(x = NPickUp_Purch, y = NWeb_Purch,
#'               plot.type = "boxplot",freq.type = "x|y",
#'               data = MktDATA)
#' 
#' # Scatter plots
#' # - Two numerical variables: default options
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
#'               fitline = TRUE, data = MktDATA)
#' # - Two numerical variables: colors based on discrete var 
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
#'               var.c = Marital_Status,  
#'               fitline = TRUE, data = MktDATA)
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
#'               var.c = Income, interval.c = TRUE, 
#'               fitline = TRUE, data = MktDATA)
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
#'               var.c = TotPurch, breaks.c = 10, 
#'               fitline = TRUE, data = MktDATA)
#' # - Two numerical variables: colors based 
#' #   on a continuous numerical variable
#' distr.plot.xy(Baseline, TotVal, plot.type = "scatter", 
#'               var.c = AOV, fitline = TRUE, data = MktDATA)
#' 
#' # - One numerical variable and one factor or character 
#' distr.plot.xy(Baseline, Marital_Status, plot.type = "scatter", 
#'               fitline = TRUE, data = MktDATA)
#' distr.plot.xy(Income.S, Baseline, plot.type = "scatter", 
#'               interval.x = TRUE,
#'               fitline = TRUE, data = MktDATA)
#' #   color based on a third variable
#' distr.plot.xy(TotPurch, TotVal, plot.type = "scatter", 
#'               breaks.x = c(0,5,10,15,20,35),
#'               var.c = AOV,
#'               fitline = TRUE, data = MktDATA)
#' 
#' # - Two factors or character vectors: bubble plots
#' distr.plot.xy(Education, LikeMost, plot.type = "scatter", 
#'               data = MktDATA)
#' # - Two classified variables (i.e. not properly numerical): 
#' #   bubble plots, changed color
#' distr.plot.xy(Income.S, TotPurch, plot.type = "scatter",
#'               interval.x = TRUE,
#'               breaks.y = c(0,5,10,15,20,35),
#'               color = "orchid", data = MktDATA)
#' 
#' # Arguments adj.breaks and use.scientific 
#' #  Variable with very wide ranges
#' LargeC<-MktDATA$AOV*5000000 
#' LargeX<-MktDATA$Baseline*1000000 
#' LargeY<-MktDATA$TotVal*1000000
#' #  - Default: no scientific notation
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               var.c = LargeC, data = MktDATA)
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               breaks.x = 10, var.c = LargeC, 
#'               data = MktDATA)
#' #  - Scientific notation for axes 
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               breaks.x = 10, var.c = LargeC, 
#'               use.scientific = TRUE,
#'               data = MktDATA)
#' #  - Scientific notation for intervals' endpoints
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               breaks.x = 10, var.c = LargeC, 
#'               adj.breaks = FALSE,
#'               data = MktDATA)
#' #  - Scientific notation for intervals endpoints and axes
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               var.c = LargeC, fitline = TRUE, 
#'               adj.breaks = FALSE, use.scientific = TRUE,
#'               data = MktDATA)
#' distr.plot.xy(LargeX, LargeY, plot.type = "scatter", 
#'               breaks.x = 10, var.c = LargeC, 
#'               adj.breaks = FALSE, use.scientific = TRUE,
#'               data = MktDATA)
#'
#' @export
distr.plot.xy<-function(x,y,plot.type,
                        bar.type="stacked",
                        freq="counts",freq.type="joint",
                        breaks.x,breaks.y,
                        interval.x = FALSE,interval.y = FALSE,
                        bw = FALSE,color=NULL,var.c,
                        breaks.c,interval.c=FALSE,
                        adj.breaks = TRUE,
                        fitline = FALSE,legend = TRUE,
                        use.scientific=FALSE,data,...){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input,warn.list=Warn.list)
  exist.x<-check.x$exist.x ;
  Err.list.input<-check.x$err.list
  Warn.list<-check.x$warn.list
  x<-check.x$vec.x
  if(exist.x){or.n.x<-length(x)}
  
  # Check if 'y' exists and if it is coherent (not missing)
  name.y<-deparse1(substitute(y))
  check.y<-chk.data(y,data,deparse1(substitute(data)),
                    name.y,missing = TRUE,err.list=Err.list.input,warn.list=Warn.list)
  exist.y<-check.y$exist.x ;
  Err.list.input<-check.y$err.list
  Warn.list<-check.y$warn.list
  y<-check.y$vec.x
  
  # check if x and y have the same length
  if(exist.x && exist.y && (length(x) != length(y))){
    Err.list.input<-c(Err.list.input,"'x' and 'y' should have the same length")
  }
  # Check required frequencies
  check.freq<-chkpar.option(value=freq,
                            allowed=c("counts","proportions","percentages"),
                            onlyone = TRUE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.freq$err.list ; Warn.list<-check.freq$warn.list
  exist.f<-check.freq$exist;  freq<-unique(check.freq$value)
  
  # Check plots are correctly specified / same procedure as before
  if(isTRUE(missing(plot.type))){
    Err.list.para<-c(Err.list.para,"'plot.type' must be specified")
    exist.plt <- FALSE} else {
      if(!is.character(plot.type)){plot.type<-deparse1(substitute(plot.type))}
      check.plt<-chkpar.option(value=plot.type,
                               allowed=c("bars","scatter","boxplot"),
                               onlyone = TRUE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
      Err.list.para<-check.plt$err.list ; Warn.list<-check.plt$warn.list
      exist.plt<-check.plt$exist;  type.plt<-unique(check.plt$value)
    }
  # Check required freq.type
  check.ftype<-chkpar.option(value=freq.type,
                             allowed=c("joint","x|y","y|x"),
                             onlyone = TRUE,listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.ftype$err.list ; Warn.list<-check.ftype$warn.list
  exist.t<-check.ftype$exist;  type.tab<-unique(check.ftype$value)
  
  # Check required freq.type
  check.bartype<-chkpar.option(value=bar.type,
                               allowed=c("stacked","beside"),
                               onlyone = FALSE,listall = TRUE,
                               err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.bartype$err.list ;
  Warn.list<-check.bartype$warn.list
  exist.bart<-check.bartype$exist;
  bar.type<-unique(check.bartype$value)
  
  if(exist.x & exist.y && !is.numeric(x) && !is.numeric(y)
     && exist.plt && type.plt=="boxplot"){
    Err.list.para<-c(Err.list.para,"To build boxplots at least one of 'x' and 'y' must be numeric")
  }
  
  # Create lists with all the info needed to build tables
  if(exist.x && exist.y && (length(x) == length(y))){
    both.in<-complete.cases(data.frame(x,y))
    x<-x[both.in] ; y<-y[both.in]
  }
  
  List.print<-NULL
  if(exist.x){
    all.infoX<-build.Xlist(x,name.x,breaks.x,interval.x,adj.breaks=adj.breaks,consistency = FALSE,
                           err.list=Err.list.options,
                           warn.list=Warn.list,list.print=List.print,suffix = TRUE)
    Err.list.options<-all.infoX$err.list
    Warn.list<-all.infoX$warn.list
    List.print<-all.infoX$list.print
    Xlist<-all.infoX$Vlist
  }
  if(exist.y){
    all.infoY<-build.Xlist(y,name.y,breaks.y,interval.y,adj.breaks=adj.breaks,consistency = FALSE,
                           err.list=Err.list.options,warn.list=Warn.list,list.print=List.print,
                           suffix = TRUE)
    Err.list.options<-all.infoY$err.list
    Warn.list<-all.infoY$warn.list
    List.print<-all.infoY$list.print
    Ylist<-all.infoY$Vlist
  }
  if(exist.x && exist.y && exist.plt && type.plt=="boxplot"){
    if(Xlist$class=="standard" & !Xlist$isnum & (Ylist$class %in% c("interval","breaks"))){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
    }
    if(Ylist$class=="standard" & !Ylist$isnum & (Xlist$class %in% c("interval","breaks"))){
      Err.list.para<-c(Err.list.para,"Boxplot cannot be built for a variable classified in intervals")
    }
  }
  if(exist.x && exist.y && exist.plt && type.plt=="scatter"
     & fitline){
    if((Xlist$class=="standard" & !Xlist$isnum) |
       (Ylist$class=="standard" & !Ylist$isnum)){
      Warn.list<-c(Warn.list,
                   "Fitline can be added only when x and y are both numeric")
      fitline <- FALSE
    }
    if(Xlist$class=="breaks" | Xlist$class=="interval" |
       Ylist$class=="breaks" | Ylist$class=="interval"){
      Warn.list<-c(Warn.list,
                   "Fitline can be added only when x and y are both numeric")
      fitline <- FALSE
    }
  }
  if(exist.x && exist.y && exist.plt && type.plt!="scatter"
     & fitline){
    Warn.list<-c(Warn.list,
                 "Fitline can be added only to scatterplots")
    fitline <- FALSE
  }
  
  ### added 202405: color scatter by var 
  exist.Vcol<-FALSE
  if(exist.x && exist.y && exist.plt && type.plt=="scatter"){
    if(isFALSE(missing(var.c))){
      name.Vcol<-deparse1(substitute(var.c))
      check.Vcol<-chk.data(var.c,data,
                           deparse1(substitute(data)),
                           name.Vcol,missing = TRUE,
                           err.list=Err.list.input,
                           warn.list=Warn.list)
      exist.Vcol<-check.Vcol$exist.x ;
      var.c<-check.Vcol$vec.x 
      add.err<-check.Vcol$err.list[!(unlist(check.Vcol$err.list) %in% unlist(Err.list.input))]
      Warn.list<-check.Vcol$warn.list
      err.Vcol<-0
      if(length(add.err)>0){
        Warn.list<-c(Warn.list,add.err)
        err.Vcol<-100}
      if(exist.Vcol && (length(var.c) != or.n.x)){
        Warn.list<-c(Warn.list,"'x' and 'var.c' should have the same length")
        err.Vcol<-err.Vcol+20}
      if(err.Vcol==0){
        var.c<-var.c[both.in] 
        all.infoC<-build.Xlist(var.c,name.Vcol,breaks.c,interval.c,
                               adj.breaks=adj.breaks,
                               consistency = FALSE,
                               err.list=Err.list.options,
                               warn.list=Warn.list,
                               list.print=List.print,
                               suffix = TRUE)
        Warn.list<-all.infoC$warn.list
        List.print<-all.infoC$list.print
        add.err<-all.infoC$err.list[!(unlist(all.infoC$err.list) %in% unlist(Err.list.options))]
        #add.list<-all.infoC$list.print[!(unlist(all.infoC$list.print) %in% unlist(List.print))]
        if(length(add.err)>0){
          Warn.list<-c(Warn.list,add.err)
        }
      }
      if(err.Vcol>0){
        Warn.list<-c(Warn.list,
                     paste0('var.c=',name.Vcol," cannot be used to color the scatterplot"))
        exist.Vcol<-FALSE}
      if(err.Vcol==0){
        Clist<-all.infoC$Vlist
      }
    }
  }
  ## end added color scatter by var  
  
  if(exist.x && exist.y && exist.plt && type.plt=="bars"){
    if((Xlist$class=="standard" & Xlist$isnum &
        length(unique(Xlist$V.f))>20) |
       (Ylist$class=="standard" & Ylist$isnum &
        length(unique(Ylist$V.f))>20)){
      Err.list.input<-c(Err.list.input,
                        paste0("x and/or y are/is numeric with too many levels",
                               "\n   "," -> to force the procedure transform the variable/s into factor/s"))
    }
  }
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for plots!
  if(length(Warn.list)>1 | length(List.print)>0){
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)
      }
      cat("\n")
    }
    if(msg.p$msg){
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
  par(mar=c(3.5,3.7,3,2.1),tck=(-0.01),tcl=NA,las=1,
      mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  par.my<-list(mar=c(3.5,3.7,3,2.1),tck=(-0.01),tcl=NA,las=1,
               mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  
  if(bar.type=="beside"){beside <- TRUE} else{beside <- FALSE}
  
  if(type.plt=="bars"){
    switch.xy <- FALSE
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
      switch.xy <- TRUE
    }
    Signal.Error(plt.xy.crossbars(tab,bw=bw,color=color,use.nx,
                                  use.ny,freq,
                                  legend=legend,beside=beside,use.tit=use.tit,
                                  switch.xy=switch.xy,use.par=par.my),
                 type.print=type.print)
  }
  
  if(type.plt=="scatter"){
    if(!exist.Vcol){
      Signal.Error(plt.xy.scatter(Xlist,Ylist,bw=bw,color=color,name.x,name.y,
                                  legend=FALSE,
                                  adj.breaks=as.logical(1-use.scientific),
                                  fitline=fitline,
                                  use.par=par.my,type.print=type.print,
                                  msg.p=msg.p),
                   type.print=type.print)
    } else {
      Clist$name.v<-name.Vcol
      Signal.Error(plt.xy.scatter(Xlist,Ylist,bw=bw,color=color,name.x,name.y,
                                  legend=legend,
                                  adj.breaks=as.logical(1-use.scientific),
                                  fitline=fitline,
                                  use.par=par.my,type.print=type.print,
                                  msg.p=msg.p,clist=Clist),
                   type.print=type.print)
    }
  }
  if(type.plt=="boxplot"){
    switch.xy <- FALSE
    if(type.tab=="x|y"){switch.xy <- TRUE}
    Signal.Error(plt.xy.boxplot(Xlist,Ylist,bw=bw,color=color,name.x,name.y,
                                adj.breaks=as.logical(1-use.scientific),
                                switch.xy=switch.xy,use.par=par.my),
                 type.print=type.print)
  }
  par(pardef)
}

# Summaries ------
## Tables ------
#' Summary statistics for a single variable
#'
#' \code{distr.summary.x()} computes summary statistics of a vector or a factor.
#'
#' @param x An unquoted string identifying the variable whose
#'   distribution has to be summarized. \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param stats A character vector specifying the summary statistics 
#'   to compute (more summaries can be specified).
#'   Specific types of summaries can be requested with the following 
#'   options:
#'   * \code{"summary"}: min, q1, median, mean, q3, max, sd, var;
#'   * \code{"central"}: central tendency measures;
#'   * \code{"dispersion"}: measures of dispersion;
#'   * \code{"fivenumbers"}: five-number summary;
#'   * \code{"quartiles"}, \code{"quintiles"}, \code{"deciles"},
#'     \code{"percentiles"}: set of quantiles.
#'
#'   It is also possible to request the following statistics:
#'   \code{"q1"}, \code{"q2"}, \code{"q3"}, \code{"mean"}, \code{"median"},
#'   \code{"mode"} (which returns the mode, the number of modes and the
#'   proportion of cases with modal value respectively), \code{"min"},
#'   \code{"max"}, \code{"sd"}, \code{"var"}, \code{"cv"} (coefficient of
#'   variation), \code{"range"}, \code{"IQrange"} (interquartile range), 
#'   and \code{"p1"}, \code{"p2"},..., \code{"p100"} (i.e. specific 
#'   percentiles).
#' @param by1,by2 Unquoted strings identifying optional variables
#'   (typically taking few values/levels) used to build conditional 
#'   summaries, that can be defined same way as \code{x}.
#' @param breaks.by1,breaks.by2 Allow classifying the variables \code{by1} 
#'   and/or \code{by2}, if \emph{numerical}, into intervals.
#'   They can be integers indicating the number of intervals of
#'   equal width used to classify \code{by1} and/or \code{by2}, 
#'   or vectors of increasing numeric values defining the endpoints 
#'   of intervals (closed on the left and open on the right; the last 
#'   interval is closed on the right too). To cover the entire range 
#'   of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to 
#'   specify a set of breaks covering only a portion of the range 
#'   of \code{by1} and/or \code{by2}.
#' @param interval.by1,interval.by2 Logical values indicating 
#'   whether \code{by1} and/or \code{by2} are variables
#'   measured in classes (\code{TRUE}). If the intervals for 
#'   one variable are not consistent (e.g. overlapping intervals, 
#'   or intervals with upper endpoint higher than the lower one), 
#'   the variable is analysed as it is, even if
#'   results are not necessarily consistent; default to \code{FALSE}.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of the numerical variables \code{by1} or \code{by2}, 
#'   when classified into intervals, should be displayed avoiding 
#'   scientific notation; default to \code{TRUE}.
#' @param digits,f.digits Integer values specifying the number of 
#'   decimals used to round respectively summary statistics 
#'   (default: \code{digits=4}) and proportions   
#'   percentages (default: \code{f.digits=2}). If the chosen rounding 
#'   formats some non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether the
#'   requested summaries should be forcedly rounded to the number of decimals 
#'   specified in \code{digits} and \code{f.digits} even if non-zero 
#'   values are rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}
#'   and/or the variables specifying the layers, \code{by1} and \code{by2}.
#'   If not found in \code{data}, the variables are taken from 
#'   the environment from which \code{distr.summary.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A list whose elements are tables 
#'   (converted  to dataframes) with the requested summaries, possibly
#'   conditioned to \code{by1} and/or \code{by2}. The values taken
#'   by the conditioning variables are arranged in standard
#'   order (logical, alphabetical or numerical order for vectors, 
#'   order of levels for factors, ordered intervals for classified
#'   variables or for variables measured in classes).
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{summaries.plot.x}()} to graphically display
#'   conditioned tendency summaries of a univariate distribution.
#' @seealso \code{\link{distr.table.x}()} for tabulating a univariate
#'   distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Marginal summaries
#' # - Numerical variable: Default summaries
#' distr.summary.x(x = AOV, data = MktDATA)
#' # - Numerical variable: More summaries
#' distr.summary.x(x = AOV, 
#'                 stats = c("central","dispersion","fivenum"),
#'                 data = MktDATA)
#' distr.summary.x(x = AOV, stats = c("mode","mean","sd","cv","fivenum"),
#'                 data = MktDATA)
#' # - Character or factor (only proper statistics calculated)
#' distr.summary.x(x = LikeMost, stats = c("mode","mean","sd","cv","fivenum"),
#'                 data = MktDATA)
#' distr.summary.x(x = Education, stats = c("mode","mean","sd","cv","fivenum"),
#'                 data = MktDATA)
#' 
#' # Measures conditioned to a single variable
#' # - Numerical variable by a character vector
#' distr.summary.x(x = TotVal, 
#'                 stats = c("p5","p10","p25","p50","p75","p90","p95"),
#'                 by1 = Gender, digits = 1, data = MktDATA)
#' # - Numerical variable by a numerical variable
#' #   classified into intervals
#' distr.summary.x(x = TotVal, 
#'                 stats = c("central","dispersion"),
#'                 by1 = AOV, breaks.by1 = 5,
#'                 digits = 1, data = MktDATA)
#' # - Numerical variable by a variable measured in classes
#' distr.summary.x(x = TotVal, 
#'                 stats = c("central","dispersion"),
#'                 by1 = Income.S, 
#'                 interval.by1 = TRUE,
#'                 digits = 1, data = MktDATA)
#' 
#' # Measures conditioned to two variables
#' distr.summary.x(x = TotVal, stats = "fivenumbers", 
#'                 by1 = Gender, by2 = Kids, data = MktDATA)
#' distr.summary.x(x = TotVal, stats = "fivenumbers", 
#'                 by1 = Income.S, by2 = Gender,
#'                 interval.by1 = TRUE, data = MktDATA)
#' distr.summary.x(x = TotVal, stats = "fivenumbers",
#'                 by1 = Gender, by2 = AOV,
#'                 breaks.by2 = 5, data = MktDATA)
#' 
#' # Arguments adj.breaks and use.scientific
#' #  Variables with a very wide range
#' LargeX<-MktDATA$TotVal*1000000
#' LargeBY<-MktDATA$AOV*5000000 
#' #  - Default: no scientific notation
#' distr.summary.x(LargeX, by1=LargeBY, breaks.by1 = 5, 
#'                 data = MktDATA)
#' #  - Scientific notation for summaries 
#' distr.summary.x(LargeX, by1=LargeBY, breaks.by1 = 5, 
#'                 use.scientific = TRUE, data = MktDATA)
#' #  - Scientific notation for intervals endpoints
#' distr.summary.x(LargeX, by1=LargeBY, breaks.by1 = 5, 
#'                 adj.breaks = FALSE, data = MktDATA)
#' #  - Scientific notation for intervals endpoints and summaries
#' distr.summary.x(LargeX, by1=LargeBY, breaks.by1 = 5, 
#'                 adj.breaks = FALSE, use.scientific = TRUE,
#'                 data = MktDATA)
#' 
#' # Output the list with the requested summaries
#' Out_TotVal<-distr.summary.x(x = TotVal, 
#'                             by1 = Income.S, by2 = Gender,
#'                             interval.by1 = TRUE,
#'                             stats = c("central","fivenum","dispersion"),
#'                             data = MktDATA)
#'
#' @export
distr.summary.x<-function(x,stats=c("summary"),
                          by1,by2,
                          breaks.by1,interval.by1=FALSE,
                          breaks.by2,interval.by2=FALSE,
                          adj.breaks=TRUE,
                          digits=2,f.digits=4,
                          force.digits = FALSE,
                          use.scientific=FALSE,data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  List.print<-NULL
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list;
  x<-check.x$vec.x
  
  List.by<-list()
  if(isFALSE(missing(by1))){
    name.by1<-deparse1(substitute(by1))
    check.by1<-chk.data(by1,data,deparse1(substitute(data)),
                        name.by1,missing = TRUE,err.list=Err.list.input,
                        warn.list=Warn.list)
    Err.list.input<-check.by1$err.list
    Warn.list<-check.by1$warn.list
    if(check.by1$exist.x){
      List.by[[length(List.by)+1]]<-list(exist=check.by1$exist.x,
                                         name=name.by1,by=check.by1$vec.x,
                                         id="by1")
    }
  }
  if(isFALSE(missing(by2))){
    name.by2<-deparse1(substitute(by2))
    check.by2<-chk.data(by2,data,deparse1(substitute(data)),
                        name.by2,missing = TRUE,err.list=Err.list.input,
                        warn.list=Warn.list)
    Err.list.input<-check.by2$err.list
    Warn.list<-check.by2$warn.list
    if(check.by2$exist.x){
      List.by[[length(List.by)+1]]<-list(exist=check.by2$exist.x,
                                         name=name.by2,by=check.by2$vec.x,
                                         id="by2")
    }
  }
  if(length(List.by)>0 && List.by[[1]]$id=="by2"){
    Warn.list<-c(Warn.list,"Only 'by2' has been specified")
  }
  
  # check if by1 and/or by2 are interval or breaks
  if(length(List.by)>0){
    for(k in 1:length(List.by)){
      if(List.by[[k]]$id=="by1"){
        name.by1<-List.by[[k]]$name
        all.infoby1<-build.Xlist(List.by[[k]]$by,name.by1,
                                 breaks.by1,interval.by1,
                                 adj.breaks=adj.breaks,
                                 consistency = FALSE,
                                 err.list=Err.list.options,
                                 warn.list=Warn.list,
                                 list.print=List.print,
                                 suffix = TRUE)
      }
      if(List.by[[k]]$id=="by2"){
        name.by2<-List.by[[k]]$name
        all.infoby1<-build.Xlist(List.by[[k]]$by,name.by2,
                                 breaks.by2,interval.by2,
                                 adj.breaks=adj.breaks,
                                 consistency = FALSE,
                                 err.list=Err.list.options,
                                 warn.list=Warn.list,
                                 list.print=List.print,
                                 suffix = TRUE)
      }
      Err.list.options<-all.infoby1$err.list
      Warn.list<-all.infoby1$warn.list
      List.print<-all.infoby1$list.print
      List.by[[k]]$Bylist<-all.infoby1$Vlist
      
      if(List.by[[k]]$Bylist$class=="standard" & List.by[[k]]$Bylist$isnum &
         length(unique(List.by[[k]]$Bylist$V.f))>20){
        Err.list.input<-c(Err.list.input,
                          paste0("'",List.by[[k]]$id,"' is numeric with too many levels",
                                 "\n   "," -> to force the procedure transform the variable/s into factor/s"))
      }
    }
  }
  
  kept<-rep(TRUE,length(List.by))
  if(length(List.by)>0 && exist.x){
    for(k in 1:length(List.by)){
      if(length(x) != length(List.by[[k]]$by)){
        Err.list.input<-c(Err.list.input,paste0("'x' and '",List.by[[k]]$id,"' should have the same length"))
        kept[k]<-FALSE}
    }
    if(sum(kept)>0){List.by<-List.by[kept]}
  }
  
  if(exist.x && length(List.by)>0){
    not.mis<-x
    for(k in 1:length(List.by)){
      if(length(x) == length(List.by[[k]]$by)){
        not.mis<-cbind(not.mis,List.by[[k]]$by)
      }
    }
    both.in<-(complete.cases(not.mis))
    if(sum(both.in)<2){
      Err.list.input<-c(Err.list.input,
                        "Cases with non missing values on input vectors should be at least 2")
    }
  }
  
  # Check specifications para
  check.stats<-chkpar.option(value=stats,
                             allowed=c("summary","central","dispersion",
                                       "fivenumbers","quartiles","quintiles",
                                       "deciles","percentiles","q1","q2","q3",
                                       "mean","median","mode","min",
                                       "max","sd","var","cv","range",
                                       "IQrange",paste0("p",1:100)),
                             onlyone = FALSE,listall = FALSE,
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
  fact.stats<-c("min","max","median","mode","n.modes","mode%","q1","q3",paste0("p",1:100))
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
  
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Ready for calculations
  # change options only if not previously modified by the user
  if(!use.scientific){if(op.sci==0){options(scipen=10)}}
  unique.stats<-na.omit(unique(unlist(list.stats)))
  if(!is.numeric(digits)){
    Warn.list<-c(Warn.list,"'digits' should be a number. The default value (2) is used")
    digits<-2  }
  if(!is.numeric(f.digits)){
    Warn.list<-c(Warn.list,"'f.digits' should be a number. The default value (4) is used")
    f.digits<-4  }
  if(is.factor(x)){
    x<-as.ordered(x)
    Warn.list<-c(Warn.list,paste0("The required 'stats' will be calculated",
                                  " using the actual order of 'x' levels",
                                  "\n   ","  -> ",paste0(levels(x),collapse=" < ")))
  }
  if(length(Warn.list)>1){
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      cat("\n")
    }
  }
  
  out<-Signal.Error(build.summaries(x=x,ListBy=List.by,
                                    stats=unique.stats,
                                    digits=digits,f.digits=f.digits,
                                    force.digits=force.digits,
                                    use.scientific=use.scientific),
                    type.print=type.print)
  
  if(type.print=="cat"){
    tit1<-paste0("\nSummary measures for ",name.x)
  }
  if(type.print=="print"){
    tit1<-paste0("Summary measures for ",name.x)
  }
  tit2<-NULL
  if(length(List.by)>=1){
    for(k in 1:length(List.by)){
      tit2<-c(tit2,List.by[[k]]$name)}
  }
  if(!is.null(tit2) && length(tit2)>1){
    tit2<-paste0(tit2,collapse=",")}
  if(!is.null(tit2)){tit1<-paste0(tit1," | ",tit2)}
  my.p.list(tit1,type.print=type.print)
  
  list.out<-list()
  myout<-out$tab
  myout.print<-out$tab.print
  to.keep<-colnames(myout)[!(colnames(myout) %in% unique.stats)]
  for(k in 1:length(list.stats)){
    my.p.list(list.tit[[k]],type.print=type.print)
    list.out[[list.tit[[k]]]]<-myout[,c(to.keep,list.stats[[k]])]
    print.k<-myout.print[,c(to.keep,list.stats[[k]])]
    print(print.k,row.names = F)
  }
  options(scipen=op.sci)
  output<-list.out
}

## Plots -------
#' Plot of central and non-central conditional tendency measures for 
#' a single numeric variable
#'
#' \code{summaries.plot.x()} plots location statistics for a 
#' numeric vector conditioned to the levels of one or more variables.
#'
#' @param x An unquoted string identifying a \emph{numerical} variable whose
#'   tendency measures have to be graphically displayed. 
#'   \code{x} can be the name of a vector in the workspace or the 
#'   name of one of the columns in the data frame specified in the 
#'   \code{data} argument.
#' @param stats A single character specifying the conditioned 
#'   tendency measure/s to 
#'   display in the plot. The available options are \code{"mean"}, 
#'   \code{"median"}, \code{"ci.mean"} (to plot the means and the 
#'   confidence intervals for the means), and specific sets of
#'   quantiles, namely \code{"quartiles"}, \code{"quintiles"}, 
#'   \code{"deciles"}, and \code{"percentiles"} (note that for quantiles
#'   only one single layer can be specified).
#' @param plot.type A single character specifying the type of plot
#'   used to compare the requested measures conditioned to the levels
#'   of one variable, \code{by1}, possibly broken down by the 
#'   levels of a second variable, \code{by2}, if specified. 
#'   The available options are:
#'   *  \code{"bars"}: Available only when \code{stats} is \code{"mean"}, 
#'   \code{"median"}, or \code{"ci.mean"} and one single layer (\code{by1}) 
#'   is specified. For each level of \code{by1} a bar is built whose 
#'   height coincides with the conditional mean or median. Confidence 
#'   intervals for the means are reported when \code{stats = "ci.mean"}.
#'   *  \code{"points"}: Available only when \code{stats} is \code{"mean"}, 
#'   \code{"median"}, and \code{"ci.mean"}. Confidence 
#'   intervals for the means are reported when \code{stats = "ci.mean"} and
#'   one single layer is specified.
#'   *  \code{"lines"}: Points joined by lines; this is the unique option 
#'   available for quantiles.
#' @param conf.level A number between 0 and 1 indicating the 
#'   confidence level of the intervals for the conditional means 
#'   when \code{stats = "ci.mean"} is specified (default to 0.95).
#' @param by1,by2 Unquoted strings identifying variables
#'   (typically taking few values/levels) used to build conditional 
#'   summaries, that can be defined same way as \code{x}. At least one
#'   layer has to be specified. The conditional measures are plotted
#'   against the values of \code{by1}, broken down by the levels 
#'   of \code{by2}, if specified.
#' @param breaks.by1,breaks.by2 Allow classifying the variables 
#'   \code{by1}  and/or \code{by2}, if \emph{numerical}, into intervals.
#'   They can be integers indicating the number of intervals of
#'   equal width used to classify \code{by1} and/or \code{by2}, 
#'   or vectors of increasing numeric values defining the endpoints 
#'   of intervals (closed on the left and open on the right; the last 
#'   interval is closed on the right too). To cover the entire range 
#'   of values the maximum and the minimum values should be
#'   included between the first and the last break. It is possible to 
#'   specify a set of breaks covering only a portion of the range 
#'   of \code{by1} and/or \code{by2}.
#' @param interval.by1,interval.by2 Logical values indicating 
#'   whether \code{by1} and/or \code{by2} are variables
#'   measured in classes (\code{TRUE}). If the intervals for 
#'   one variable are not consistent (e.g. overlapping intervals, 
#'   or intervals with upper endpoint higher than the lower one), 
#'   the variable is analysed as it is, even if
#'   results are not necessarily consistent; default to \code{FALSE}.
#' @param adj.breaks Logical value indicating whether the endpoints of
#'   intervals of the numerical variables \code{by1} or \code{by2}, 
#'   when classified into intervals, should be displayed avoiding 
#'   scientific notation; default to \code{TRUE}.
#' @param bw Logical value indicating whether plots should be colored 
#'   in scale of greys (\code{TRUE}) rather than using a standard 
#'   palette (\code{FALSE}, default).
#' @param color Optional string vector to specify colors 
#'   to use in the plot rather than a standard palette  
#'   (\code{NULL}, default).
#' @param legend Logical value indicating whether a legend should be displayed
#'   in the plot (\code{legend = TRUE}; default) or not 
#'   (\code{legend = FALSE}).
#' @param use.scientific Logical value indicating whether numbers on 
#'   axes should be displayed using scientific notation 
#'   (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} 
#'   and/or the variables specifying the layers, \code{by1} and \code{by2}.
#'   If not found in \code{data}, the variables are taken from 
#'   the environment from which \code{distr.summary.x()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table (converted to dataframe) reporting the requested
#'   statistics conditioned to the levels of the specified layers.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{distr.summary.x}()} for tabulating summary
#'   measures of a univariate distribution.
#' @seealso \code{\link{distr.plot.x}()} for plotting a univariate
#'   distribution.
#' @seealso \code{\link{distr.table.x}()} for tabulating a univariate
#'   distribution.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Means (and their CI) or medians by a single variable
#' # - Barplot of means (default) by a character 
#' summaries.plot.x(x = TotVal, stats = "mean",  
#'                by1 = Gender, data = MktDATA)
#' # - Barplot of medians by a numerical variable
#' #   classified into intervals: user-defined color
#' summaries.plot.x(x = TotVal, stats = "median", 
#'                by1 = AOV, breaks.by1 = 5, 
#'                color = "purple", data = MktDATA)
#' # - Lineplot of means and their CI by a variable 
#' #   measured in classes
#' summaries.plot.x(x = TotVal, 
#'                stats = "ci.mean", plot.type = "lines",
#'                by1 = Income.S, interval.by1 = TRUE,
#'                data = MktDATA)
#' # - Barplot of means and their CI by a 
#' #   numerical variable; change the confidence level
#' summaries.plot.x(x = TotVal, 
#'                stats = "ci.mean", conf.level = 0.90,
#'                plot.type = "bars", 
#'                by1 = NWeb_Purch, data = MktDATA)
#' # - Note: no plot built for a variable with 
#' #   too many levels (>20)
#' \dontrun{summaries.plot.x(x = TotVal, 
#'                         stats = "ci.mean", plot.type = "lines",
#'                         by1 = AOV, data = MktDATA)}
#' 
#' # Quantiles by a single variable
#' # - Only lines plots allowed for quantiles
#' summaries.plot.x(x = Baseline, 
#'                stats = "deciles", plot.type = "lines",
#'                by1 = NDeals, data = MktDATA)
#' summaries.plot.x(x = Baseline, 
#'                stats = "quartiles", plot.type = "lines",
#'                by1 = Marital_Status, data = MktDATA)
#' 
#' # Means and medians by two variables
#' # - Default: only lines allowed
#' summaries.plot.x(x = TotVal, stats = "mean", 
#'                by1 = Education, by2 = Kids, 
#'                data = MktDATA)
#' summaries.plot.x(x = TotVal, stats = "median", 
#'                by1 = Income.S, by2 = Gender,
#'                interval.by1 = TRUE,
#'                data = MktDATA)
#' summaries.plot.x(x = Baseline, stats = "mean", 
#'                by1 = CustClass, by2 = AOV,
#'                breaks.by2 = 5, data = MktDATA)
#' # - "ci.mean" not allowed with two layers
#' CustClass_Kids<-paste0(MktDATA$CustClass,"-",MktDATA$Kids)
#' summaries.plot.x(x = Baseline, stats = "ci.mean", 
#'                conf.level = 0.99, by1 = CustClass_Kids,
#'                color = "gold", data = MktDATA)
#' 
#' # Arguments adj.breaks and use.scientific
#' #  Variables with a very wide range
#' LargeX<-MktDATA$TotVal*1000000
#' LargeBY<-MktDATA$AOV*5000000 
#' #  - Default: no scientific notation
#' summaries.plot.x(LargeX, plot.type = "bars",
#'                by1=LargeBY, breaks.by1 = 5, data = MktDATA)
#' #  - Scientific notation for summaries (axes) 
#' summaries.plot.x(LargeX, plot.type = "lines",
#'                by1=LargeBY, breaks.by1 = 5, 
#'                use.scientific = TRUE, data = MktDATA)
#' #  - Scientific notation for intervals endpoints
#' summaries.plot.x(LargeX, stats = "ci.mean",
#'                plot.type = "lines", 
#'                by1=LargeBY, breaks.by1 = 5, 
#'                adj.breaks = FALSE, data = MktDATA)
#' #  - Scientific notation for intervals endpoints and summaries
#' summaries.plot.x(LargeX, stats = "quartiles",
#'                plot.type = "lines", 
#'                by1=LargeBY, breaks.by1 = 5, 
#'                adj.breaks = FALSE, use.scientific = TRUE,
#'                data = MktDATA)
#' 
#' # Output the table with the requested summaries 
#' Out_TotVal<-summaries.plot.x(x = TotVal, stats = "ci.mean", 
#'                            by1 = Education, data = MktDATA)
#'
#' @export
summaries.plot.x<-function(x,stats="mean",
                         plot.type="bars",conf.level=0.95,
                         by1,by2,
                         breaks.by1,interval.by1=FALSE,
                         breaks.by2,interval.by2=FALSE,
                         adj.breaks = TRUE,
                         bw = FALSE,color=NULL,legend = TRUE,
                         use.scientific=FALSE,
                         data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  # change options only if not previously modified by the user
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  List.print<-NULL
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Err.list.options<-as.list("\nErrors found in the definition of options:")
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Err.list.options<-as.list("Errors found in the definition of options:")
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,num = TRUE,missing = TRUE,err.list=Err.list.input)
  exist.x<-check.x$exist.x
  Err.list.input<-check.x$err.list;
  x<-check.x$vec.x
  
  # Check specifications para
  check.stats<-chkpar.option(value=stats,
                             allowed=c("mean","median","ci.mean","quartiles",
                                       "quintiles","deciles","percentiles"),onlyone = TRUE,
                             listall = FALSE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.stats$err.list
  Warn.list<-check.stats$warn.list
  exist.s<-check.stats$exist;  stats<-unique(check.stats$value)
  
  check.plot<-chkpar.option(value=plot.type,
                            allowed=c("bars","lines","points"),onlyone = TRUE,
                            listall = TRUE,err.list=Err.list.para,warn.list=Warn.list)
  Err.list.para<-check.plot$err.list
  Warn.list<-check.plot$warn.list
  exist.p<-check.plot$exist;  type<-check.plot$value
  
  Err.list.para<-chkpar.conf(value=conf.level,err.list=Err.list.para)
  
  # Check if 'y' or 'by' exist and are properly specified
  if(isTRUE(missing(by1) & isTRUE(missing(by2)))){
    Err.list.input<-c(Err.list.input,"To obtain plots for summaries by1 or by2 should be specified")
  }
  
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
                  type.print=type.print)    }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  List.by<-list()
  if(isFALSE(missing(by1))){
    name.by1<-deparse1(substitute(by1))
    check.by1<-chk.data(by1,data,deparse1(substitute(data)),
                        name.by1,missing = TRUE,err.list=Err.list.input,
                        warn.list=Warn.list)
    Err.list.input<-check.by1$err.list
    Warn.list<-check.by1$warn.list
    if(check.by1$exist.x){
      List.by[[length(List.by)+1]]<-list(exist=check.by1$exist.x,
                                         name=name.by1,by=check.by1$vec.x,
                                         id="by1")
    }
  }
  if(isFALSE(missing(by2))){
    name.by2<-deparse1(substitute(by2))
    check.by2<-chk.data(by2,data,deparse1(substitute(data)),
                        name.by2,missing = TRUE,err.list=Err.list.input,
                        warn.list=Warn.list)
    Err.list.input<-check.by2$err.list
    Warn.list<-check.by2$warn.list
    if(check.by2$exist.x){
      List.by[[length(List.by)+1]]<-list(exist=check.by2$exist.x,
                                         name=name.by2,by=check.by2$vec.x,
                                         id="by2")
    }
  }
  if(length(List.by)>0 && List.by[[1]]$id=="by2"){
    Warn.list<-c(Warn.list,"Only 'by2' has been specified")
  }
  
  # check if by1 and/or by2 are interval or breaks
  if(length(List.by)>0){
    for(k in 1:length(List.by)){
      if(List.by[[k]]$id=="by1"){
        name.by1<-List.by[[k]]$name
        all.infoby1<-build.Xlist(List.by[[k]]$by,name.by1,
                                 breaks.by1,interval.by1,
                                 adj.breaks=adj.breaks,
                                 consistency = FALSE,
                                 err.list=Err.list.options,
                                 warn.list=Warn.list,
                                 list.print=List.print,
                                 suffix = TRUE)
      }
      if(List.by[[k]]$id=="by2"){
        name.by2<-List.by[[k]]$name
        all.infoby1<-build.Xlist(List.by[[k]]$by,name.by2,
                                 breaks.by2,interval.by2,
                                 adj.breaks=adj.breaks,
                                 consistency = FALSE,
                                 err.list=Err.list.options,
                                 warn.list=Warn.list,
                                 list.print=List.print,
                                 suffix = TRUE)
      }
      Err.list.options<-all.infoby1$err.list
      Warn.list<-all.infoby1$warn.list
      List.print<-all.infoby1$list.print
      List.by[[k]]$Bylist<-all.infoby1$Vlist
      
      if(List.by[[k]]$Bylist$class=="standard" & List.by[[k]]$Bylist$isnum &
         length(unique(List.by[[k]]$Bylist$V.f))>20){
        Err.list.input<-c(Err.list.input,
                          paste0("'",List.by[[k]]$id,"' is numeric with too many levels",
                                 "\n   "," -> to force the procedure transform the variable/s into factor/s"))
      }
    }
  }
  
  kept<-rep(TRUE,length(List.by))
  if(length(List.by)>0 && exist.x){
    for(k in 1:length(List.by)){
      if(length(x) != length(List.by[[k]]$by)){
        Err.list.input<-c(Err.list.input,paste0("'x' and '",List.by[[k]]$id,"' should have the same length"))
        kept[k]<-FALSE}
    }
    if(sum(kept)>0){List.by<-List.by[kept]}
  }
  if(exist.x && length(List.by)>0){
    not.mis<-x
    for(k in 1:length(List.by)){
      if(length(x) == length(List.by[[k]]$by)){
        not.mis<-cbind(not.mis,List.by[[k]]$by)
      }
    }
    both.in<-(complete.cases(not.mis))
    if(sum(both.in)<2){
      Err.list.input<-c(Err.list.input,
                        "Cases with non missing values on input vectors should be at least 2")
    }
  }
  
  
  if(length(Err.list.input)>1 | length(Err.list.para)>1 |
     length(Err.list.options)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)    }
      if(length(Err.list.options)>1){
        my.p.list(Err.list.options[!duplicated(Err.list.options)],
                  type.print=type.print)  }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # Verify parameters 
  
  if((type=="bars" | type=="points") && 
     (stats %in% c("quartiles","quintiles","deciles",
                   "percentiles"))){
    Warn.list<-c(Warn.list,"Only 'lines' allowed for quantiles: 'bars'/'points' ignored")
    type<-"lines"
  }
  if(type=="bars" && length(List.by)==2 && (stats %in% c("mean"))){
    Warn.list<-c(Warn.list,"For means only 'lines'/'points' allowed with 2 layers: 'bars' ignored")
    type<-"lines"
  }
  if(type=="bars" && length(List.by)==2  && (stats %in% c("median"))){
    Warn.list<-c(Warn.list,"For medians only 'lines'/'points' allowed with 2 layers: 'bars' ignored")
    type<-"lines"
  }
  if(length(List.by)==2 &&
     (stats %in% c("quartiles","quintiles","deciles","percentiles"))){
    Err.list.para<-c(Err.list.para,"Quantiles can be displayed only by a single layer")
  }
  if(length(List.by)==2 && (stats %in% c("ci.mean"))){
    Err.list.para<-c(Err.list.para,"Means CI can be displayed only by a single layer")
  }
  # not useful, added in case more options are allowed in the future
  if(is.character(x)){
    Err.list.para<-c(Err.list.para,"The available plots cannot be built for a character variable")
  }
  if(!is.numeric(x) & stats %in% c("mean","median","ci.mean")){
    Err.list.para<-c(Err.list.para,"The requested plots cannot be built for a non numeric variable")
  }
  
  if(length(Err.list.input)>1 | length(Err.list.para)>1){
    if(msg.p$err){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      if(length(List.print)>0){
        my.p.list(List.print,type.print=type.print)
      }
      if(length(Err.list.input)>1){
        my.p.list(Err.list.input[!duplicated(Err.list.input)],
                  type.print=type.print)  }
      if(length(Err.list.para)>1){
        my.p.list(Err.list.para[!duplicated(Err.list.para)],
                  type.print=type.print)    }
    }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  for(k in 1:length(List.by)){
    if(List.by[[k]]$Bylist$class=="interval" | List.by[[k]]$Bylist$class=="breaks"){
      List.by[[k]]$by <- List.by[[k]]$Bylist$V.f  }
  }
  
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
  if(msg.p$warn){
    if(length(Warn.list)>1){
      my.p.list(Warn.list[!duplicated(Warn.list)],
                type.print=type.print)  }
    if(length(List.print)>0){
      my.p.list(List.print,type.print=type.print)
    }
    if(length(Err.list.para)>1){
      my.p.list(Err.list.para[!duplicated(Err.list.para)],
                type.print=type.print)    }
  }
  
  out<-Signal.Error(build.summaries(x=x,ListBy=List.by,
                                    stats=unique.stats,
                                    digits=100,f.digits=100,
                                    force.digits=FALSE,
                                    use.scientific=FALSE),
                    type.print=type.print)
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  plt.out<-Signal.Error(build.summary.plt(out$tab,name.x,
                                          list.stats,list.tit,
                                          ListBy=out$list.by,
                                          stats=stats,plot.type=type,
                                          bw=bw,color=color,
                                          legend=legend,conf.level=conf.level,
                                          adj.breaks=as.logical(1-use.scientific),
                                          msg.p=msg.p,
                                          type.print=type.print),
                        type.print=type.print)
  # removed printout of the table
  par(pardef)
  plt.out
}

# Confidence intervals ------
#' Confidence intervals for the mean
#'
#' \code{CI.mean()} builds confidence intervals for the mean of a population.
#'
#' @param x An unquoted string identifying the \emph{numeric}
#'   variable whose mean is of interest. \code{x} can be the 
#'   name of a vector in the workspace or the name of one of 
#'   the columns in the data frame specified in the \code{data} argument.
#' @param sigma An optional numeric value specifying the 
#'   population standard deviation. If \code{NULL} (default) 
#'   the population standard deviation is estimated using the
#'   data.
#' @param conf.level Numeric value specifying the required 
#'   confidence level; default to 0.95.
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the 
#'   environment from which \code{CI.mean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence interval for the 
#'   population mean. If the variance is unknown, the interval is 
#'   built using percentiles from
#'   both the normal and the Student's t distribution.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.mean}()} to test hypotheses on a population
#'   mean.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # CI for the mean with KNOWN variance; default options
#' CI.mean(AOV, sigma = 30, data = MktDATA)
#' 
#' # CI for the mean with UNKNOWN variance;  
#' # - change digits and confidence level 0.99
#' CI.mean(AOV, conf.level = 0.99, digits = 3, data = MktDATA)
#' 
#' # Arguments force.digits and use.scientific
#' #  A variable taking very small values
#' SmallX<-MktDATA$AOV/5000 
#' #  - Default: manages possible excess of rounding
#' CI.mean(SmallX)
#' #  - Forcing digits to the default values (2)
#' CI.mean(SmallX, force.digits = TRUE)
#' #  - Allow scientific notation
#' CI.mean(SmallX, use.scientific = TRUE)
#' 
#' # Output the table with the requested interval
#' out.ci_mean<-CI.mean(AOV, data = MktDATA)
#'
#' @export
CI.mean<-function(x,sigma = NULL,conf.level = 0.95, 
                  digits = 2, force.digits = FALSE,
                  use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                          digits=digits,force.digits=force.digits,
                          use.scientific=use.scientific,
                          type.print=type.print) }
  if (isTRUE(is.null(sigma))){
    output<-ci.mean.unknown(x,conf.level = conf.level,
                            digits=digits,force.digits=force.digits,
                            use.scientific=use.scientific,
                            type.print=type.print) }
  out<-output
}

#' Confidence intervals for the proportion
#'
#' \code{CI.prop()} builds confidence intervals for the proportion of
#'   successes in a population.
#'
#' @param x An unquoted string identifying the variable of interest. 
#'   \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param success If \code{x} is a factor, a character vector, or a numeric
#'   non-binary vector, \code{success} must be used to indicate the category/value
#'   corresponding to success. The argument can be omitted (\code{NULL},
#'   default) if \code{x} is a binary numeric vector (takes values 0 or 1 only;
#'   in this case success is assumed to be 1) or a logical vector (in these
#'   cases success is assumed to be \code{TRUE}).
#' @param conf.level Numeric value specifying the required 
#'   confidence level; default to 0.95.
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the environment
#'   from which \code{CI.prop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for 
#'   the population proportion of successes.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.prop}()} to test hypotheses on the  
#'   proportion of successes in a population.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Success = one value of a character vector or factor
#' CI.prop(WouldSuggest, success = "Yes", data = MktDATA)
#' 
#' # - change confidence level and rounding
#' CI.prop(Education, success = "Post-Grad", 
#'         conf.level = 0.9, digits = 4, 
#'         data = MktDATA)
#' 
#' # Success = numeric value
#' CI.prop(Children, success = 2, data = MktDATA)
#' 
#' # Binary variable ('success' is 1 by default)
#' CI.prop(LastCampaign, digits = 3, data = MktDATA)
#'
#' # Logical variable ('success' is TRUE by default)
#' CI.prop(RespCampaign, conf.level = 0.9, digits = 3, data = MktDATA)
#' 
#' # Success based on combined conditions
#' # - Build a (logical) vector indicating whether a condition is satisfied
#' IsTop <- MktDATA$CustClass == "Gold" | MktDATA$CustClass == "Platinum"
#' CI.prop(IsTop, conf.level = 0.9)
#' # - A very rare event
#' HighAOV <- MktDATA$AOV>150
#' CI.prop(HighAOV, conf.level = 0.9)
#' 
#' # Arguments force.digits, use.scientific
#' # - Default: manages possible excess of rounding
#' CI.prop(HighAOV)
#' # - Forcing digits to the default values (2)
#' CI.prop(HighAOV, force.digits = TRUE)
#' # - Allow scientific notation
#' CI.prop(HighAOV, use.scientific = TRUE)
#' 
#' # Output results
#' out_ci_prop<-CI.prop(HighAOV)
#'
#' @export
CI.prop<-function(x, success = NULL, conf.level = 0.95, 
                  digits = 2, force.digits = FALSE,
                  use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # All requests checked for coherency before stopping if one is not ok
  # Check if 'x' exists and if it is coherent (not missing)
  name.x<-deparse1(substitute(x))
  check.x<-chk.data(x,data,deparse1(substitute(data)),
                    name.x,missing = TRUE,err.list=Err.list.input)
  Err.list.input<-check.x$err.list
  x<-check.x$vec.x ; exist.x<-check.x$exist.x 
  
  # check x consistency
  if(is.null(success) && exist.x && 
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
                  digits=digits,force.digits=force.digits,
                  use.scientific=use.scientific,type.print=type.print)
  out<-output
}

#' Confidence intervals for the difference between means
#'
#' \code{CI.diffmean()} builds confidence intervals for the difference
#'   between the means of two independent or paired populations.
#'
#' @param x,y Unquoted strings identifying two \emph{numeric} 
#'   variables with the same length whose means have to be compared. 
#'   \code{x} and \code{y} can be the names of vectors in the workspace 
#'   or the names of columns in the data frame 
#'   specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param type A length-one character vector specifying the type of samples.
#'   Allowed values are \code{"independent"} or \code{"paired"}.
#' @param conf.level Numeric value specifying the required 
#'   confidence level; default to 0.95.
#' @param sigma.x,sigma.y Optional numeric values specifying 
#'   the possibly known populations' standard deviations 
#'   (when \code{x} and \code{y} are specified). If \code{NULL} (default)
#'   standard deviations are estimated using the data.
#' @param by Optional unquoted string, available only when
#'   \code{type = "independent"}, identifying a variable 
#'   (of any type), defined same way as \code{x},
#'   taking only \bold{two} values used to split 
#'   \code{x} into two \bold{independent samples}. Given the two 
#'   \emph{ordered} values taken by \code{by} 
#'   (alphabetical or numerical order, 
#'   or order of the levels for factors), say \emph{by1} and \emph{by2}, 
#'   the confidence interval is built for the difference between the 
#'   populations means in the \emph{by1}- and  in the \emph{by2}-group.
#'   Note that only \bold{one} between \code{y} and \code{by} can be 
#'   specified. 
#' @param sigma.by Optional numeric value specifying the possibly known
#'   standard deviations for the two \emph{independent} samples identified via
#'   \code{by} (when \code{x} and \code{by} are specified). 
#'   \code{sigma.by} can be a single value indicating the same
#'   standard deviation in the two by-groups, or a vector with two values,
#'   specifying the standard deviations in the two by-groups. To avoid errors,
#'   in the latter case the vector should be named, with names coinciding 
#'   with the two levels of \code{by}.
#' @param sigma.d Optional numeric value specifying the possibly known
#'   standard deviation of the difference when samples are \bold{paired}.
#' @param var.test Logical value indicating whether to run a test on the
#'   equality of variance for two (\bold{independent}) samples or not 
#'   (default).
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or
#'   \code{y}. If not found in \code{data}, the variables 
#'   are taken from the environment
#'   from which \code{CI.diffmean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the difference
#'   between the populations' means. For \emph{independent} samples in the case
#'   of unknown variances, the intervals are built both under the
#'   assumption that the variances are equal and under the assumption that
#'   they differ, using percentiles from both the normal and the 
#'   Student's t distribution. If \code{}
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.diffmean}()} to test hypotheses on the 
#'   difference between two populations' means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Independent samples (default type), UNKNOWN variances
#' #  CI for the difference between means of males and females
#' #  - Using x,y: build vectors with data on the two groups
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' CI.diffmean(x = AOV_M, y = AOV_F)
#' #  - Change confidence level
#' CI.diffmean(x = AOV_M, y = AOV_F, conf.level = 0.99)
#' #  - Using x,by: groups identified by ordered levels of by
#' CI.diffmean(x = AOV, by = Gender, conf.level = 0.99, data = MktDATA)
#' #    Since order is F, M, CI is for mean(F) - mean(M)
#' #    To get the interval for mean(M) - mean(F)
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' CI.diffmean(x = AOV, by = Gender.R, conf.level = 0.99,  
#'             data = MktDATA)
#' #  - Testing hypotheses on equality of unknown variances
#' CI.diffmean(x = AOV_M, y = AOV_F, conf.level = 0.99, 
#'             var.test = TRUE)
#' 
#' #  - Output results: only information on the CI
#' out.ci_diffM<-CI.diffmean(x = AOV_M, y = AOV_F)
#' #  - Output results: list with information on CI and test on var
#' out.ci_diffM.V<-CI.diffmean(x = AOV_M, y = AOV_F, var.test = TRUE)
#' 
#' # Independent samples (default type), KNOWN variances
#' #  CI for the difference between means of males and females
#' #  - Using x,y: build vectors with data on the two groups
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' CI.diffmean(x = AOV_M, y = AOV_F, 
#'             sigma.x = 10, sigma.y = 20)
#' #  - Using x,by: groups identified by ordered levels of by
#' CI.diffmean(x = AOV, by = Gender, 
#'             sigma.by = c("M" = 10, "F"=20), data = MktDATA)
#' #    To change the sign, order levels as desired
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' CI.diffmean(x = AOV, by = Gender.R, 
#'             sigma.by = c("M" = 10, "F"=20), data = MktDATA)
#' #  - Output results 
#' out.ci_diffM<-CI.diffmean(x = AOV_M, y = AOV_F, 
#'                           sigma.x = 10, sigma.y = 20)
#' 
#' # Paired samples: UNKNOWN variances
#' # - Default settings
#' CI.diffmean(x = NStore_Purch, y = NWeb_Purch,
#'             type = "paired", data=MktDATA)
#' # - Change confidence level
#' CI.diffmean(x = NStore_Purch, y = NWeb_Purch,
#'             type = "paired", conf.level = 0.9, data = MktDATA)
#' # Paired: KNOWN variances
#' CI.diffmean(x = NStore_Purch, y = NWeb_Purch,
#'             type = "paired", conf.level = 0.9, 
#'             sigma.d = 2, data = MktDATA)
#' #  - Output results 
#' out.ci_diffM<-CI.diffmean(x = NStore_Purch, y = NWeb_Purch,
#'                           type = "paired", conf.level = 0.9, 
#'                           sigma.d = 2, data = MktDATA)
#' 
#' # Arguments force.digits and use.scientific
#' #  An input variable taking very low values
#' SmallX<-MktDATA$AOV/5000
#' SmallX_M <- SmallX[MktDATA$Gender == "M"]
#' SmallX_F <- SmallX[MktDATA$Gender == "F"]
#' # - Default: manages possible excess of rounding
#' CI.diffmean(x = SmallX_M, y = SmallX_F)
#' # - Force to the requested nr of digits (default, 2)
#' CI.diffmean(x = SmallX_M, y = SmallX_F,
#'             force.digits = TRUE)
#' # - Allow scientific notation
#' CI.diffmean(x = SmallX_M, y = SmallX_F, 
#'             use.scientific = TRUE)
#'
#' @export
CI.diffmean<-function(x, y, type = "independent",
                      sigma.x = NULL, sigma.y = NULL,
                      conf.level = 0.95,
                      by, sigma.by = NULL, sigma.d = NULL,
                      var.test = FALSE,
                      digits = 2, force.digits = FALSE,
                      use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                                 digits=digits,force.digits=force.digits,
                                 use.scientific=use.scientific,
                                 type.print=type.print) }
  if(type=="paired" & is.null(sigma.d)){
    output<-ci.diff.paired_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                   conf.level = conf.level,
                                   digits=digits,force.digits=force.digits,
                                   use.scientific=use.scientific,
                                   type.print=type.print) }
  if(type=="independent" & known.var){
    output<-ci.diff.indep_known(x=use.x,y=use.y,names.xy=names.xy,
                                sigma.x=sigma.x,sigma.y=sigma.y,
                                conf.level = conf.level,
                                digits=digits,force.digits=force.digits,
                                use.scientific=use.scientific,
                                type.print=type.print) }
  if(type=="independent" & !known.var){
    output<-ci.diff.indep_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                  conf.level = conf.level,
                                  digits=digits,var.test=var.test,
                                  force.digits=force.digits,
                                  use.scientific=use.scientific,
                                  type.print=type.print) }
  invisible(output)
}

#' Confidence intervals for the difference between proportions
#'
#' \code{CI.diffprop()} builds confidence intervals for the difference
#'   between the proportion of successes in two independent populations.
#'
#' @param x,y Unquoted strings identifying the variables of 
#'   interest. \code{x} and \code{y} can be the
#'   names of vectors or factors in the workspace or the 
#'   names of columns in the data frame 
#'   specified in the \code{data} argument.
#'   It is possible to use a mixed specification 
#'   (e.g, one vector and one column in data).
#' @param success.x,success.y If \code{x,y} are factors, character
#'   vectors, or numeric non-binary vectors, success must be used to indicate
#'   the category/value corresponding to success in the populations. These
#'   arguments can be omitted (\code{NULL}, default) if \code{x,y} are binary
#'   numeric vectors (taking values 0 or 1 only; in this case success is
#'   assumed to correspond to 1) or a logical vector (in these cases success
#'   is assumed to correspond to \code{TRUE}).
#' @param conf.level Numeric value specifying the required 
#'   confidence level; default to 0.95.
#' @param by Optional unquoted string identifying a variable 
#'   (of any type), defined same way as \code{x},
#'   taking only \bold{two} values used to split 
#'   \code{x} into two independent samples. Given the two 
#'   \emph{ordered} values taken by \code{by} 
#'   (alphabetical or numerical order, 
#'   or order of the levels for factors), say \emph{by1} and \emph{by2}, 
#'   the confidence interval is built for the difference between the 
#'   populations proportions in the \emph{by1}- and  in the \emph{by2}-group.
#'   Note that only \bold{one} between \code{y} and \code{by} can be 
#'   specified. 
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or
#'   \code{y}. If not found in \code{data}, the variables 
#'   are taken from the environment
#'   from which \code{CI.diffprop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the confidence intervals for the difference
#'   between the proportions of successes in two independent populations.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{TEST.diffprop}()} to test hypotheses on the difference
#'   between the proportions of successes in two populations.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Proportions of success defined on non-binary and 
#' #  non-logical vectors; 'success' coded same way
#' #  for both vectors
#' #  - Using x,y: build vectors with data on the two groups
#' WouldSuggest_F <- MktDATA$WouldSuggest[MktDATA$Gender == "F"]
#' WouldSuggest_M <- MktDATA$WouldSuggest[MktDATA$Gender == "M"]
#' CI.diffprop(x = WouldSuggest_M, y = WouldSuggest_F, 
#'             success.x = "Yes")
#' 
#' PastCampaigns_F<-MktDATA$PastCampaigns[MktDATA$Gender=="F"]
#' PastCampaigns_M<-MktDATA$PastCampaigns[MktDATA$Gender=="M"]
#' CI.diffprop(x = PastCampaigns_M, y = PastCampaigns_F,
#'             success.x = 0, conf.level = 0.99)
#'             
#' #  - Using x,by: groups identified by ordered levels of by
#' CI.diffprop(x = PastCampaigns, by = Gender,
#'             success.x=0, conf.level = 0.99, 
#'             data = MktDATA)
#' #    Since order is F, M, CI is for prop(F) - prop(M)
#' #    To get the interval for prop(M) - prop(F)
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' CI.diffprop(x = PastCampaigns, by = Gender.R,
#'             success.x=0, conf.level = 0.99, data = MktDATA)
#'  
#' # Proportions of success defined based on 
#' #  binary or logical vectors; 'success'
#' #  coded same way for both vectors
#' #  - Binary variable (success=1): based on x,y
#' LastCampaign_F<-MktDATA$LastCampaign[MktDATA$Gender=="F"]
#' LastCampaign_M<-MktDATA$LastCampaign[MktDATA$Gender=="M"]
#' CI.diffprop(x = LastCampaign_M, y = LastCampaign_F)
#' #  - Binary variable (success=1): based on x,y
#' #    see above for recoding of levels of Gender
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' CI.diffprop(x = LastCampaign, by = Gender.R, data = MktDATA)
#' #  - Logical variable (success=TRUE): based on x,y
#' Deals_w_child <- MktDATA$Deals.ge50[MktDATA$Children>0]
#' Deals_no_child <- MktDATA$Deals.ge50[MktDATA$Children==0]
#' CI.diffprop(x = Deals_w_child, y = Deals_no_child, conf.level = 0.9)
#' 
#' # Proportions defined on 
#' #  non-binary and non-logical vectors, with 'success'
#' #  coded differently (only specification x,y is reasonable here)
#' WouldSuggest_Other<-c(rep("OK",310),rep("KO",650-310))
#' CI.diffprop(x = WouldSuggest, y = WouldSuggest_Other, 
#'             success.x = "Yes", success.y = "OK",
#'             data = MktDATA)
#' 
#' # Proportions based on combined conditions
#' # - Build logical vector/s indicating whether a condition 
#' #   is satisfied
#' IsTop<-MktDATA$AOV>80
#' IsTop_OK<-IsTop[MktDATA$WouldSuggest == "Yes"]
#' IsTop_KO<-IsTop[MktDATA$WouldSuggest == "No"]
#' CI.diffprop(x = IsTop_OK, y = IsTop_KO, conf.level = 0.9)
#' 
#' Deals<-MktDATA$NDeals>=5
#' Deals_Married <- Deals[MktDATA$Marital_Status=="Married" & 
#'                          MktDATA$Children==0] 
#' Deals_Single <- Deals[MktDATA$Marital_Status=="Single"] 
#' CI.diffprop(x = Deals_Married, y = Deals_Single, conf.level = 0.9)
#' 
#' # Output results           
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' out.ci_diffP<-CI.diffprop(x = PastCampaigns, by = Gender.R,
#'                           success.x=0, conf.level = 0.99, 
#'                           data = MktDATA)
#' 
#' # Arguments force.digits and use.scientific
#' #  An input variable taking very low values
#' HighAOV <- MktDATA$AOV>150
#' # - Default: manages possible excess of rounding
#' CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'             y = HighAOV[MktDATA$Gender=="F"])
#' #  - Force to the exact number of digits (default, 2)
#' CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'             y = HighAOV[MktDATA$Gender=="F"],
#'             force.digits = TRUE)
#' #  - Allow scientific notation
#' CI.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'             y = HighAOV[MktDATA$Gender=="F"],
#'             use.scientific = TRUE)
#'
#' @export
CI.diffprop<-function(x, y, success.x = NULL, success.y = NULL,
                      conf.level=0.95, by,
                      digits = 2, force.digits = FALSE, 
                      use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                       digits=digits,force.digits=force.digits,
                       use.scientific=use.scientific,
                       type.print=type.print) 
}

# Tests  ------
#' Test on the mean
#'
#' \code{TEST.mean()}  tests hypotheses on the mean of a population.
#'
#' @param x An unquoted string identifying the \emph{numeric}
#'   variable whose mean is of interest. \code{x} can be the 
#'   name of a vector in the workspace or the name of one of 
#'   the columns in the data frame specified in the \code{data} argument.
#' @param sigma An optional numeric value specifying the 
#'   population standard deviation. If \code{NULL} (default) 
#'   the population standard deviation is estimated using the
#'   data.
#' @param mu0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   (population mean differs from \code{mu0}; default), or \code{"less"}
#'   (population mean is lower than \code{mu0}), or \code{"greater"}
#'   (population mean is higher than \code{mu0}).
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the environment
#'   from which \code{TEST.mean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the results of the test on the population mean.
#'   If the variance is unknown, the test is run using percentiles from
#'   both the normal and the Student's t distribution.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.mean}()} to build confidence intervals for the
#'   population mean.
#' @examples
#' data(MktDATA, package = "UBStats")
#' 
#' # Test on the mean; KNOWN variance
#' # - Bilateral test
#' TEST.mean(NStore_Purch, sigma = 9, mu0 = 5, 
#'           alternative = "two.sided", data = MktDATA)
#' # - Unilateral test
#' TEST.mean(NStore_Purch, sigma = 9,mu0 = 5,
#'           alternative = "greater", data = MktDATA)
#' 
#' # Test on the mean; UNKNOWN variance;
#' # - Unilateral test
#' TEST.mean(TotVal, mu0 = 600, alternative = "less",
#'           data = MktDATA)
#' 
#' # Arguments force.digits and use.scientific
#' # An input variable taking very low values
#' SmallX<-MktDATA$AOV/500
#' # Default output
#' TEST.mean(SmallX, mu0 = 0.1)
#' # Request to use the exact number of digits (default, 2)
#' TEST.mean(SmallX, mu0 = 0.1,force.digits=TRUE)
#' # Request to allow scientific notation
#' TEST.mean(SmallX, mu0 = 0.1,use.scientific=TRUE)
#' 
#' # Output results
#' out.test_mean<-TEST.mean(TotVal, mu0 = 600, alternative = "less",
#'                          data = MktDATA)
#'
#' @export
TEST.mean<-function(x, sigma = NULL, 
                    mu0 = 0, alternative = "two.sided",
                    digits = 2, force.digits = FALSE,
                    use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                           digits=digits,force.digits=force.digits,
                           use.scientific=use.scientific,type.print=type.print) }
  if (isTRUE(is.null(sigma))){
    output<-hyp.mean.unknown(x,mu0=mu0,alternative=alternative,
                             digits=digits,force.digits=force.digits,
                             use.scientific=use.scientific,type.print=type.print) }
  out<-output
}

#' Test on the proportion
#'
#' \code{TEST.prop()}  tests hypotheses on the proportion of successes in a
#'   population.
#'
#' @param x An unquoted string identifying the variable of interest. 
#'   \code{x} can be the name of a vector
#'   or a factor in the workspace or the name of one of the columns in the
#'   data frame specified in the \code{data} argument.
#' @param success If \code{x} is a factor, a character vector, or a numeric
#'   non-binary vector, \code{success} must be used to indicate the category/value
#'   corresponding to success. The argument can be omitted (\code{NULL},
#'   default) if \code{x} is a binary numeric vector (takes values 0 or 1 only;
#'   in this case success is assumed to be 1) or a logical vector (in these
#'   cases success is assumed to be \code{TRUE}).
#' @param p0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   (population proportion differs from \code{p0}; default), or \code{"less"}
#'   (population proportion is lower than \code{p0}), or \code{"greater"}
#'   (population proportion is higher than \code{p0}).
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x}.
#'   If not found in \code{data}, \code{x} is taken from the environment
#'   from which \code{TEST.prop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the results of the test on the population 
#'   proportion of successes.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.prop}()} to build confidence intervals for the
#'   population proportion of successes.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Success = one value of a character vector or factor
#' # - Bilateral test
#' TEST.prop(WouldSuggest, success = "Yes", p0 = 0.7, 
#'          data = MktDATA)
#' # - Unilateral test, change digits
#' TEST.prop(Education, success = "Post-Grad", p0 = 0.3, 
#'           alternative = "less", digits = 4,data = MktDATA)
#' 
#' # Success = numeric value; bilateral test
#' TEST.prop(Children, success = 2, p0 = 0.3, data = MktDATA)
#' 
#' # Binary variable (success = 1 by default); unilateral
#' TEST.prop(LastCampaign, p0 = 0.1, alternative = "greater", 
#'           digits = 3, data = MktDATA)
#' 
#' # Logical variable (success = TRUE by default); unilateral test
#' TEST.prop(Deals.ge50, p0 = 0.13, alternative = "greater", 
#'           digits = 3, data = MktDATA)
#' 
#' # Success based on combined conditions
#' # - Build a (logical) vector 
#' IsTop <- MktDATA$CustClass == "Gold" |
#'   MktDATA$CustClass == "Platinum"
#' TEST.prop(IsTop, p0 = 0.2, data = MktDATA)
#' 
# Arguments force.digits, use.scientific
# - Default: manages possible excess of rounding
#' HighAOV <- MktDATA$AOV>150
#' TEST.prop(HighAOV, p0 = 0.1)
# - Forcing digits to the default values (2)
#' TEST.prop(HighAOV, p0 = 0.1, force.digits = TRUE)
# - Allow scientific notation
#' TEST.prop(HighAOV, p0 = 0.1, use.scientific = TRUE)
#' 
#' # Output results
#' out_test_prop<-TEST.prop(IsTop, p0 = 0.2, data = MktDATA)
#'
#' @export
TEST.prop<-function(x, success = NULL,
                    p0 = 0.5, alternative = "two.sided", 
                    digits = 2, force.digits = FALSE, 
                    use.scientific = FALSE,data,...){
  
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                   digits=digits,
                   force.digits=force.digits,
                   use.scientific=use.scientific,
                   type.print=type.print)
  out<-output
}

#' Tests on the difference between means
#'
#' \code{TEST.diffmean()}  tests hypotheses on the difference between the
#'   means of two independent or paired populations.
#'
#' @param x,y Unquoted strings identifying the \emph{numeric} 
#'   variables with the same length whose means have to be compared. \code{x} and 
#'   \code{y} can be the names of vectors in the workspace 
#'   or the names of columns in the data frame 
#'   specified in the \code{data} argument.
#'   It is possible to use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param type A length-one character vector specifying the type of samples.
#'   Allowed values are \code{"independent"} or \code{"paired"}.
#' @param mdiff0 Numeric value that specifies the null hypothesis to test for
#'   (default is 0).
#' @param alternative A length-one character vector specifying the direction
#'   of the alternative hypothesis. Allowed values are \code{"two.sided"} 
#'   (difference between populations' means differs from \code{mdiff0}; default), or \code{"less"}
#'   (difference between populations' means is lower than \code{mdiff0}), or \code{"greater"}
#'   (difference between populations' means is higher than \code{mdiff0}).
#' @param sigma.x,sigma.y Optional numeric values specifying 
#'   the possibly known populations' standard deviations 
#'   (when \code{x} and \code{y} are specified). If \code{NULL} (default)
#'   standard deviations are estimated using the data.
#' @param by Optional unquoted string, available only when
#'   \code{type = "independent"}, identifying a variable 
#'   (of any type), defined same way as \code{x},
#'   taking only \bold{two} values used to split 
#'   \code{x} into two \bold{independent samples}. Given the two 
#'   \emph{ordered} values taken by \code{by} 
#'   (alphabetical or numerical order, 
#'   or order of the levels for factors), say \emph{by1} and \emph{by2}, 
#'   hypotheses are tested on the difference between the 
#'   populations means in the \emph{by1}- and  in the \emph{by2}-group.
#'   Note that only \bold{one} between \code{y} and \code{by} can be 
#'   specified. 
#' @param sigma.by Optional numeric value specifying the possibly known
#'   standard deviations for the two \emph{independent} samples identified via
#'   \code{by} (when \code{x} and \code{by} are specified). 
#'   \code{sigma.by} can be a single value indicating the same
#'   standard deviation in the two by-groups, or a vector with two values,
#'   specifying the standard deviations in the two by-groups. To avoid errors,
#'   in the latter case the vector should be named, with names coinciding 
#'   with the two levels of \code{by}.
#' @param sigma.d Optional numeric value specifying the possibly known
#'   standard deviation of the difference when samples are \bold{paired}.
#' @param var.test Logical value indicating whether to run a test on the
#'   equality of variance for two (\bold{independent}) samples or not 
#'   (default).
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or
#'   \code{y} or \code{by}. If not found in \code{data}, the variables 
#'   are taken from the environment
#'   from which \code{TEST.diffmean()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the results of the test on the difference
#'   between the populations' means. For independent samples in the case
#'   of unknown variances the test is run both under the
#'   assumption that the variances are equal and under the assumption that
#'   they differ, using percentiles from both the normal and the 
#'   Student's t distribution.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffmean}()} to build confidence intervals for
#'   the difference between two populations' means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Independent samples (default type), UNKNOWN variances
#' #  Bilateral test on difference between means of males and females
#' #  - Using x,y: build vectors with data on the two groups
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' TEST.diffmean(x = AOV_M, y = AOV_F, mdiff0 = 0)
#' #  - Using x,by: groups identified by ordered levels of by
#' TEST.diffmean(x = AOV, by = Gender, mdiff0 = 0, data = MktDATA)
#' #    Since order is F, M, hypothesis are on mean(F) - mean(M)
#' #    To test hypotheses on mean(M) - mean(F)
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffmean(x = AOV, by = Gender.R , mdiff0 = 0, 
#'               data = MktDATA)
#' #  - Testing also hypotheses on equality of unknown variances
#' TEST.diffmean(x = AOV_M, y = AOV_F, mdiff0 = 0, 
#'               var.test = TRUE)
#' 
#' #  - Output results: test on differences
#' out.test_diffM<-TEST.diffmean(x = AOV_M, y = AOV_F)
#' #  - Output results: list with both test on means and variances
#' out.test_diffM.V<-TEST.diffmean(x = AOV_M, y = AOV_F, var.test = TRUE)
#' 
#' # Independent samples (default type), KNOWN variances
#' #  Test hypotheses on the difference between means of males and females
#' #  - Using x,y: build vectors with data on the two groups
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' TEST.diffmean(x = AOV_M, y = AOV_F, mdiff0 = 10, 
#'               alternative = "greater", sigma.x = 10, sigma.y = 20)
#' #  - Using x,by: groups identified by ordered levels of by
#' #    Adjust considering the ordering of levels
#' TEST.diffmean(x = AOV, by = Gender, mdiff0 = -10,
#'               alternative = "less",
#'               sigma.by = c("M" = 10, "F"=20), data = MktDATA)
#' #    To change the sign, order levels as desired
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffmean(x = AOV, by = Gender.R, mdiff0 = 10,
#'               alternative = "greater",
#'               sigma.by = c("M" = 10, "F"=20), data = MktDATA)
#' #  - Output results 
#' out.test_diffM<-TEST.diffmean(x = AOV_M, y = AOV_F, mdiff0 = 10, 
#'                               alternative = "greater", 
#'                               sigma.x = 10, sigma.y = 20)
#' 
#' # Paired samples: UNKNOWN variances
#' # - Default settings
#' TEST.diffmean(x = NStore_Purch, y = NWeb_Purch, 
#'               type = "paired", 
#'               mdiff0 = 1.5, alternative = "greater", data=MktDATA)
#' # Paired: KNOWN variances
#' TEST.diffmean(x = NStore_Purch, y = NWeb_Purch,
#'               type = "paired", mdiff0 = 1.5, alternative = "greater",
#'               sigma.d = 2, data = MktDATA)
#' #  - Output results 
#' out.test_diffM<-TEST.diffmean(x = NStore_Purch, 
#'                               y = NWeb_Purch,
#'                               type = "paired", mdiff0 = 1.5, alternative = "greater",
#'                               sigma.d = 2, data = MktDATA)
#' 
#' # Arguments force.digits and use.scientific
#' #  An input variable taking very low values
#' SmallX<-MktDATA$AOV/50000
#' SmallX_M <- SmallX[MktDATA$Gender == "M"]
#' SmallX_F <- SmallX[MktDATA$Gender == "F"]
#' #  - Default output
#' TEST.diffmean(x = SmallX_M, y = SmallX_F)
#' #  - Request to use the exact number of digits (default, 2)
#' TEST.diffmean(x = SmallX_M, y = SmallX_F,
#'               force.digits = TRUE)
#' #  - Request to allow scientific notation
#' TEST.diffmean(x = SmallX_M, y = SmallX_F, 
#'               use.scientific = TRUE)
#'
#' @export
TEST.diffmean<-function(x, y, type = "independent", 
                        mdiff0 = 0, alternative = "two.sided",
                        sigma.x = NULL, sigma.y = NULL,
                        by, sigma.by = NULL, sigma.d = NULL,
                        var.test = FALSE,
                        digits = 2,force.digits = FALSE,
                        use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                                  digits=digits,
                                  force.digits = force.digits,
                                  use.scientific = use.scientific,
                                  type.print=type.print) }
  if(type=="paired" & is.null(sigma.d)){
    output<-hyp.diff.paired_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                    mdiff0=mdiff0,
                                    alternative=alternative,
                                    digits=digits,
                                    force.digits = force.digits,
                                    use.scientific = use.scientific,
                                    type.print=type.print) }
  if(type=="independent" & known.var){
    output<-hyp.diff.indep_known(x=use.x,y=use.y,names.xy=names.xy,
                                 mdiff0=mdiff0,sigma.x=sigma.x,
                                 sigma.y=sigma.y,
                                 alternative = alternative,
                                 digits=digits,
                                 force.digits = force.digits,
                                 use.scientific = use.scientific,
                                 type.print=type.print) }
  if(type=="independent" & !known.var){
    output<-hyp.diff.indep_unknown(x=use.x,y=use.y,names.xy=names.xy,
                                   mdiff0=mdiff0,var.test=var.test,
                                   alternative = alternative,
                                   digits=digits,
                                   force.digits = force.digits,
                                   use.scientific = use.scientific,
                                   type.print=type.print) }
  invisible(output)
}

#' Tests on the difference between proportions
#'
#' \code{TEST.diffprop()} tests hypotheses on the difference between the
#'   proportion of successes in two independent populations.
#'
#' @param x,y Unquoted strings identifying the variables of 
#'   interest. \code{x} and \code{y} can be the
#'   names of vectors or factors in the workspace or the 
#'   names of columns in the data frame 
#'   specified in the \code{data} argument.
#'   It is possible to use a mixed specification 
#'   (e.g, one vector and one column in data).
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
#'   (difference between populations' proportions differs from \code{pdiff0}; default), or \code{"less"}
#'   (difference between populations' proportions is lower than \code{pdiff0}), or \code{"greater"}
#'   (difference between populations' proportions is higher than \code{pdiff0}).
#' @param by Optional unquoted string identifying a variable 
#'   (of any type), defined same way as \code{x},
#'   taking only \bold{two} values used to split 
#'   \code{x} into two independent samples. Given the two 
#'   \emph{ordered} values taken by \code{by} 
#'   (alphabetical or numerical order, 
#'   or order of the levels for factors), say \emph{by1} and \emph{by2}, 
#'   hypotheses are tested on the difference between the 
#'   populations proportions in the \emph{by1}- and  in the \emph{by2}-group.
#'   Note that only \bold{one} between \code{y} and \code{by} can be 
#'   specified. 
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or
#'   \code{y} or \code{by}. If not found in \code{data}, the variables 
#'   are taken from the environment
#'   from which \code{TEST.diffprop()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the results of the test on the difference
#'   between the proportions of successes in two independent populations.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffprop}()} to build confidence intervals for
#'   the difference between two populations' proportions of successes.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Proportions of success defined on non-binary and 
#' #  non-logical vectors; 'success' coded same way
#' #  for both vectors
#' #  - Using x,y: build vectors with data on the two groups
#' WouldSuggest_F <- MktDATA$WouldSuggest[MktDATA$Gender == "F"]
#' WouldSuggest_M <- MktDATA$WouldSuggest[MktDATA$Gender == "M"]
#' TEST.diffprop(x = WouldSuggest_M, y = WouldSuggest_F, 
#'               success.x = "Yes", pdiff0 = 0.1, alternative = "less")
#' 
#' PastCampaigns_F<-MktDATA$PastCampaigns[MktDATA$Gender=="F"]
#' PastCampaigns_M<-MktDATA$PastCampaigns[MktDATA$Gender=="M"]
#' TEST.diffprop(x = PastCampaigns_M, y = PastCampaigns_F,
#'               success.x = 0, pdiff0 = 0.2)
#' 
#' #  - Using x,by: groups identified by ordered levels of by
#' TEST.diffprop(x = PastCampaigns, by = Gender,
#'               success.x=0, pdiff0 = 0.2, data = MktDATA)
#' #    Since order is F, M, test is on prop(F) - prop(M)
#' #    To get the interval for prop(M) - prop(F)
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffprop(x = PastCampaigns, by = Gender.R,
#'               success.x=0, pdiff0 = 0.2, data = MktDATA)
#' 
#' # Proportions of success defined based on 
#' #  binary or logical vectors; 'success'
#' #  coded same way for both vectors
#' #  - Binary variable (success=1): based on x,y
#' LastCampaign_F<-MktDATA$LastCampaign[MktDATA$Gender=="F"]
#' LastCampaign_M<-MktDATA$LastCampaign[MktDATA$Gender=="M"]
#' TEST.diffprop(x = LastCampaign_M, y = LastCampaign_F)
#' #  - Binary variable (success=1): based on x,y
#' #    see above for recoding of levels of Gender
#' TEST.diffprop(x = LastCampaign, by = Gender, data = MktDATA)
#' Gender.R <- factor(MktDATA$Gender, levels = c("M", "F"))
#' TEST.diffprop(x = LastCampaign, by = Gender.R, data = MktDATA)
#' #  - Logical variable (success=TRUE): based on x,y
#' Deals_w_child <- MktDATA$Deals.ge50[MktDATA$Children>0]
#' Deals_no_child <- MktDATA$Deals.ge50[MktDATA$Children==0]
#' TEST.diffprop(x = Deals_w_child, y = Deals_no_child, 
#'               pdiff0 = 0.2, alternative = "less",)
#' # Proportions defined on 
#' #  non-binary and non-logical vectors, with 'success'
#' #  coded differently (only specification x,y is reasonable here)
#' WouldSuggest_Other<-c(rep("OK",310),rep("KO",650-310))
#' TEST.diffprop(x = WouldSuggest, y = WouldSuggest_Other, 
#'               success.x = "Yes", success.y = "OK",
#'               pdiff0 = 0.1, alternative = "greater",
#'               data = MktDATA)
#' 
#' # Proportions based on combined conditions
#' # - Build logical vector/s indicating whether a condition 
#' #   is satisfied
#' IsTop<-MktDATA$AOV>80
#' IsTop_OK<-IsTop[MktDATA$WouldSuggest == "Yes"]
#' IsTop_KO<-IsTop[MktDATA$WouldSuggest == "No"]
#' TEST.diffprop(x = IsTop_OK, y = IsTop_KO, pdiff0 = 0.05,
#'               alternative = "greater")
#' 
#' Deals<-MktDATA$NDeals>=5
#' Deals_Married <- Deals[MktDATA$Marital_Status=="Married" & 
#'                          MktDATA$Children==0] 
#' Deals_Single <- Deals[MktDATA$Marital_Status=="Single"] 
#' TEST.diffprop(x = Deals_Married, y = Deals_Single,
#'               alternative = "less")
#' 
#' # Output results           
#' out.test_diffP<-TEST.diffprop(x = Deals_Married, y = Deals_Single,
#'                               alternative = "less")
#' 
#' # Arguments force.digits and use.scientific
#' #  An input variable taking very low values
#' HighAOV <- MktDATA$AOV>150
#' # - Default: manages possible excess of rounding
#' TEST.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'               y = HighAOV[MktDATA$Gender=="F"])
#' #  - Force to the exact number of digits (default, 2)
#' TEST.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'               y = HighAOV[MktDATA$Gender=="F"],
#'               force.digits = TRUE)
#' #  - Allow scientific notation
#' TEST.diffprop(x = HighAOV[MktDATA$Gender=="M"], 
#'               y = HighAOV[MktDATA$Gender=="F"],
#'               use.scientific = TRUE)
#'
#' @export
TEST.diffprop<-function(x, y, success.x = NULL, success.y = NULL,
                        pdiff0 = 0, alternative = "two.sided",
                        by, 
                        digits = 2, force.digits = FALSE, 
                        use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
  
  if(length(Warn.list)>1){ # not needed, leave in case added warnings
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],type.print=type.print)  
      }
      cat("\n") 
    }
  }
  
  # Ready for test
  output<-hyp.diff.prop(x=use.x,y=use.y,names.xy,
                        pdiff0=pdiff0,success.x,success.y,
                        alternative=alternative,
                        digits=digits,
                        force.digits = force.digits,
                        use.scientific = use.scientific,
                        type.print=type.print) 
}

#' Tests on variances
#'
#' \code{TEST.diffvar()} tests the hypothesis of equality between the 
#'   variances of two independent populations.
#'
#' @param x,y Unquoted strings identifying the \emph{numeric} 
#'   variables with the same length whose variances have to be compared. 
#'   \code{x} and \code{y} can be the names of vectors in the workspace 
#'   or the names of columns in the data frame 
#'   specified in the \code{data} argument. It is possible to 
#'   use a mixed specification (e.g, one vector and one
#'   column in data).
#' @param by Optional unquoted string identifying a variable 
#'   (of any type), defined same way as \code{x},
#'   taking only \bold{two} values used to split 
#'   \code{x} into two independent samples. Since the
#'   null hypothesis of equal variances is tested against the
#'   bilateral alternative only, the order of the levels of
#'   \code{by} is irrelevant (differently from what holds
#'   for functions building confidence intervals or testing 
#'   hypotheses on the differences between means or proportions).
#'   Note that only \bold{one} between \code{y} and \code{by} can be 
#'   specified. 
#' @param digits Integer value specifying the number of 
#'   decimals used to round statistics; default to 2. If the chosen rounding formats some 
#'   non-zero values as zero, the number of decimals is increased 
#'   so that all values have at least one significant digit, unless the argument  
#'   \code{force.digits} is set to \code{TRUE}.
#' @param force.digits Logical value indicating whether reported values
#'   should be forcedly rounded to the number of decimals specified in
#'   \code{digits} even if non-zero values are
#'   rounded to zero (default to \code{FALSE}).
#' @param use.scientific Logical value indicating whether numbers 
#'   in tables should be displayed using 
#'   scientific notation (\code{TRUE}); default to \code{FALSE}.
#' @param data An optional data frame containing \code{x} and/or
#'   \code{y}. If not found in \code{data}, the variables 
#'   are taken from the environment
#'   from which \code{TEST.diffvar()} is called.
#' @param ... Additional arguments to be passed to low level functions.
#' @return A table reporting the results of the test on the
#'   difference between the variances of two independent populations.
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @seealso \code{\link{CI.diffmean}()} to build confidence intervals for 
#'   the difference between two populations' means.
#' @seealso \code{\link{TEST.diffmean}()} to test hypotheses on the difference
#'   between two populations' means.
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Using x,y: build vectors with data on the two groups
#' AOV_M <- MktDATA$AOV[MktDATA$Gender == "M"]
#' AOV_F <- MktDATA$AOV[MktDATA$Gender == "F"]
#' TEST.diffvar(x = AOV_M, y = AOV_F)
#' TEST.diffvar(x = AOV_F, y = AOV_M) # same
#' 
#' # Using x,by: groups identified by ordered levels of by
#' TEST.diffvar(x = AOV, by = Gender, data=MktDATA)
#' 
#' # Output results
#' out_test.diffV<-TEST.diffvar(x = AOV_M, y = AOV_F)
#' 
#' # Arguments force.digits and use.scientific
#' #  An input variable taking very low values
#' SmallX<-MktDATA$AOV/50000
#' SmallX_M <- SmallX[MktDATA$Gender == "M"]
#' SmallX_F <- SmallX[MktDATA$Gender == "F"]
#' #  - Default output
#' TEST.diffvar(x = SmallX_M, y = SmallX_F)
#' #  - Request to use the exact number of digits (default, 2)
#' TEST.diffvar(x = SmallX_M, y = SmallX_F,
#'              force.digits = TRUE)
#' #  - Request to allow scientific notation
#' TEST.diffvar(x = SmallX_M, y = SmallX_F, 
#'              use.scientific = TRUE)
#'
#' @export
TEST.diffvar<-function(x, y, by, digits = 2,
                       force.digits = FALSE, 
                       use.scientific = FALSE, data,...){
  op.sci<-getOption("scipen")
  on.exit(options(scipen=op.sci))
  type="independent"
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  # change outlet if rmarkdown (private, not usable)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Err.list.para<-as.list("\nErrors found in the definition of parameters:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of inputs:") 
    Err.list.para<-as.list("Errors found in the definition of parameters:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
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
                        digits=digits,force.digits=force.digits,
                        use.scientific=use.scientific,
                        type.print=type.print)
}

# LM reorganisation -----
#' Extract Model Residuals and other Regression Diagnostics 
#'
#' \code{LM.output()}  Provides fitted values, residuals and other basic
#' quantities used to check the quality of regression fits. 
#' @param object An object returned by function lm.
#' @param data An optional data frame containing the data 
#'   frame possibly specified in the call of function lm.
#' @return A dataframe containing the variables in the model
#'   and the model's fitted values, residuals 
#'   and influence statistics, merged with the dataframe
#'   specified in the call of function lm, or with the
#'   dataframe possibly specified in \code{data} 
#'   (if it is consistent with the model's output)
#' @author Raffaella Piccarreta \email{raffaella.piccarreta@unibocconi.it}
#' @examples
#' data(MktDATA, package = "UBStats")
#'
#' # Model and output based on a given dataframe
#' mod1 <- lm(TotVal ~ Baseline + Kids + Age, data = MktDATA)
#' # Equivalent calls (since data is specified in lm() 
#' mod1_out <- LM.output(mod1, data = MktDATA)
#' dim(mod1_out)
#' mod1_out <- LM.output(mod1)
#' dim(mod1_out) # same as above
#' 
#' # Model based on a dataframe's columns
#' mod2 <- lm(MktDATA$TotVal ~ MktDATA$Baseline + 
#'            MktDATA$Kids + MktDATA$Age)
#' mod2_out <- LM.output(mod2)
#' # note: colnames in mod2_out
#' colnames(mod2_out)
#' # note that the dataframe in 'data' is not considered
#' # as compatible, because the names of columns differ
#' mod2_out <- LM.output(mod2, data = MktDATA)
#' 
#' @importFrom stats rstandard
#' @importFrom stats rstudent
#' @importFrom stats hatvalues
#' @importFrom stats cooks.distance
#' @export
LM.output<-function(object,data){
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  .rp_outlet<-.do_rpic_set_markdown()
  type.print<-.rp_outlet$type.print
  msg.p<-.rp_outlet$msg.p
  
  if(type.print=="cat"){
    Err.list.input<-as.list("\nErrors found in the definition of inputs:") 
    Warn.list<-as.list("\nWarning:") # list to collect warn msg
  }
  if(type.print=="print"){
    Err.list.input<-as.list("Errors found in the definition of input vector:") 
    Warn.list<-as.list("Warning:") # list to collect warn msg
  }
  
  # if object not specified -> error  
  if(isTRUE(missing(object))){
    Err.list.input<-c(Err.list.input,paste0("'object' is not specified"))  
  } 
  if(isFALSE(missing(object))){
    name.mod<-deparse1(substitute(object))
    mod.exist<-FALSE
    # check existence
    ge <- (ls(name=.GlobalEnv))
    if(!(name.mod %in% ge)){
      Err.list.input<-c(Err.list.input,paste0("object ",name.mod," does not exist"))
    }
    if((name.mod %in% ge)){
      if(!inherits(object, "lm")){
        Err.list.input<-c(Err.list.input,paste0(name.mod," is not the output of function lm()"))
      }
      if(inherits(object, "lm")){
        mod.exist<-TRUE
      }
    }
  }
  if(length(Err.list.input)>1){
    if(msg.p$err){
      my.p.list(Err.list.input[!duplicated(Err.list.input)],
                type.print=type.print)  }
    my.p.list("\nThe procedure is interrupted",type.print=type.print)
    stop_quietly()
  }
  
  # create a dataframe with all needed info
  mod.data<-object$model
  mod.vars<-colnames(mod.data)
  # add info to mod.data
  mod.data$fitted.values<-object$fitted.values
  mod.data$residuals<-object$residuals
  mod.data$rstandard<-rstandard(object)
  mod.data$rstudent<-rstudent(object)
  mod.data$leverage<-hatvalues(object)
  mod.data$cook<-cooks.distance(object)
  added.mod<-colnames(mod.data)[!colnames(mod.data) %in% mod.vars]
  
  # check if data is specified
  data.exist<-FALSE
  type.check<-0
  if(isTRUE(missing(data))){
    usedata<-eval(object$call$data)
    if(is.data.frame(usedata)){
      if(msg.p$err){
        my.p.list(paste0("\nNo data has been specified, but ",name.mod," was based on a dataframe",
                         "\n","  --> The ouput dataframe contains both lm results and source data"),type.print=type.print)
      }
      data.exist<-TRUE
    }
  }
  if(isFALSE(missing(data)) && !data.exist){
    name.data<-deparse1(substitute(data))
    # check existence
    ge <- (ls(name=.GlobalEnv))
    if(!(name.data %in% ge)){
      Warn.list<-c(Warn.list,paste0("data ",name.data," does not exist"))
      type.check<-1
    }
    if((name.data %in% ge)){
      if(!(is.data.frame(data))){
        Warn.list<-c(Warn.list,paste0(name.data," is not a dataframe"))
        type.check<-1
      }
      if(is.data.frame(data)){
        check<-0
        check<-abs(sum(mod.vars %in% colnames(data))-length(mod.vars))
        if(check>0){
          Warn.list<-c(Warn.list,paste0("One or more variables used in ",name.mod," are not included in ",name.data))
          type.check<-2
        }
        if(check==0){
          check<-as.numeric(nrow(mod.data)>nrow(data))
          if(check>0){
            Warn.list<-c(Warn.list,paste0("The number of cases used in ",name.mod," and those in ",name.data," are not compatible"))
            type.check<-2
          }
        }
        if(check==0){
          in.data<-(1:nrow(data))[complete.cases(data[,mod.vars])]
          notin.data<-(1:nrow(data))[!complete.cases(data[,mod.vars])]
          data.compare<-data[in.data,]
          check<-abs(nrow(mod.data)-nrow(data.compare))
          if(check>0){
            Warn.list<-c(Warn.list,paste0("The number of cases used in ",name.mod," and those in ",name.data," are not compatible"))
            type.check<-2
          }
        }
        if(check==0){
          for(k in mod.vars){
            uu<-sum(mod.data[,k]!=data.compare[,k])
            check<-check+uu
          }
          if(check>0){
            Warn.list<-c(Warn.list,paste0("The number of cases used in ",name.mod," and those in ",name.data," are not compatible"))
            type.check<-2
          }
        }
        if(check==0){
          data.exist<-TRUE
          usedata<-data
          rm(data)
        }
      }
    }
  }
  if(length(Warn.list)>1){
    if(msg.p$err){
      my.p.list(Warn.list[!duplicated(Warn.list)],
                type.print=type.print)  
      if(type.check==1){
        my.p.list(paste0("\n","  --> The ouput dataframe contains only data used in ",name.mod),type.print=type.print)
      }
      if(type.check==2){
        my.p.list(paste0("\n",name.mod," and ",name.data," are not compatible",
                         "\n","  --> The ouput dataframe contains only data used in ",name.mod),type.print=type.print)
      }
    }
  }
  
  if(data.exist){
    in.data<-(1:nrow(usedata))[complete.cases(usedata[,mod.vars])]
    notin.data<-(1:nrow(usedata))[!complete.cases(usedata[,mod.vars])]
    
    long<-nchar(colnames(usedata))
    long<-colnames(usedata)[long==max(long)][1]
    longer<-paste0(long,"xxxxx")
    mod.data[,longer]<-in.data
    usedata[,longer]<-1:nrow(usedata)
    mod.data<-merge(usedata,mod.data[,c(added.mod,longer)],
                    by=longer,all=T)
    mod.data<-mod.data[order(mod.data[,longer]),]
    mod.data[,longer]<-NULL
    rownames(mod.data)<-rownames(usedata)
  }
  
  out<-mod.data
} # close function





