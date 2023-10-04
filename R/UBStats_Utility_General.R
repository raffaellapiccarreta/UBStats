# Utility functions 

# General functions used everywhere -------------
## Function to stop quietly
stop_quietly <- function() {
  opt<-getOption("show.error.messages")
  on.exit(options(show.error.messages = opt))  
  options(show.error.messages = F)  
  stop()
}

# Function to printout in markdown
#' @importFrom utils write.table
my.p.err <- function(m){
  utils::write.table(format(m, justify="left"),
              row.names = FALSE, col.names = FALSE, quote = FALSE)}

# Function to printout list with messages and df
my.p.list<-function(use.xx,type.print="print"){
  pp.c<-NULL
  c.pp<-0
  for(k in 1:length(use.xx)){
    if(inherits(use.xx[[k]],"data.frame")){
      if(!is.null(pp.c)){
        if(type.print=="print"){my.p.err(c(pp.c," "))}
        if(type.print=="cat"){cat(c(pp.c," "),sep="\n",file=stderr())}
        pp.c<-NULL}
      print(use.xx[[k]])
      my.p.err(c(" "))
    }
    if(inherits(use.xx[[k]],"character")){
      pp.c<-c(pp.c,use.xx[[k]])
    }
    if(k==length(use.xx) & !is.null(pp.c)){
      if(type.print=="print"){my.p.err(pp.c)}
      if(type.print=="cat"){cat(c(pp.c),sep="\n",file=stderr())}
    }
  }
}

# Function to verify whether an input exists or not
chk.data.old<-function(x,data,name.data,name.x,num = FALSE,missing = FALSE,
                   err.list=NULL,warn.list=NULL,required="vector"){
  exist.x <- FALSE; vec.x<-NULL;  class.x<-"none"; bad.request <- FALSE
  if(!is.list(err.list)){err.list<-list()}
  ini.err<-length(err.list)
  which<-deparse1(substitute(x))
  if(isTRUE(missing(x))){
    err.list<-c(err.list,paste0("'",which,"' is not specified"))  } 
  if(isFALSE(missing(x))){
    copyx<-function(x){y<-x ; return(y)}
    check.x<-tryCatch(y<-x, error=function(e){chk<-"Error" })
    check.dim<-dim(check.x) ; check.len<-length(check.x)
    if(is.null(check.dim) && check.len>1){
      if(is.atomic(x)){
        exist.x <- TRUE ;  vec.x<-x; class.x<-"vector"
      } else {
        err.list<-c(err.list,paste0("'",which,"' should be vector, not ",class(x))) } 
    }
    if(!is.null(check.dim) & required=="vector"){
      err.list<-c(err.list,paste0("'",which,"' should be vector, not ",class(x))) } 
    if(!is.null(check.dim) & required=="data" && (inherits(x,"matrix") | inherits(x,"data.frame"))){
      exist.x <- TRUE; vec.x<-x; class.x<-"data"
    }
    # if(exist.x && isFALSE(missing(data))){
    #   warn.list<-c(warn.list,"'data' ignored because the input/s exist")
    # }
    if(!exist.x & length(err.list)==ini.err){
      if(isTRUE(missing(data))){
        if(is.null(check.dim) & check.len<2){
          err.list<-c(err.list,paste0(name.x," does not exist or is not consistent")) } 
        # if(!is.null(check.dim) ){
        #   err.list<-c(err.list,paste0("'",which,"' should be a vector, not a ",class(x))) } 
      } else if(isFALSE(missing(data))){
        ge <- (ls(name=.GlobalEnv))
        if(!(name.data %in% ge)){
          err.list<-c(err.list,paste0("data ",name.data," does not exist"))}
        if(name.data %in% ge){
          if(!(is.data.frame(data) | is.matrix(data))){
            err.list<-c(err.list,paste0(name.data," is not a dataframe or a matrix"))}
          if(is.data.frame(data) | is.matrix(data)){
            if(name.x %in% colnames(data)){
              vec.x<-data[[name.x]]; exist.x <- TRUE; class.x<-"vector"} 
            if(!(name.x %in% colnames(data))){
              n.name.x<-gsub("\"", "", name.x)
              if(n.name.x %in% colnames(data)){
                vec.x<-data[,n.name.x]; exist.x <- TRUE; class.x<-"vector"} 
              if(!(n.name.x %in% colnames(data))){
                err.list<-c(err.list,paste0("Variable ",name.x," is not included in ",name.data))
              }
            }
          }
        }
      }
    }
  }
  if(exist.x && class.x=="vector" && missing){
    if(sum(is.na(vec.x))==length(vec.x)){
      err.list<-c(err.list,paste0("All the elements in '",which,"' are missing!"))
      exist.x <- FALSE}
    if(exist.x && sum(!is.na(vec.x))<2){
      err.list<-c(err.list,paste0("Cases with non missing values in '",which,"' should be at least 2!"))
      exist.x <- FALSE}
    if(exist.x && length(unique(vec.x))==1){
      err.list<-c(err.list,paste0("'",which,"' is constant (takes only one value)!"))
      exist.x <- FALSE}
  }
  if(exist.x & class.x=="vector" & num){
    if(!is.numeric(vec.x)){
      err.list<-c(err.list,paste0("'",which,"' should be numeric"))
    }
  }
  
  if(exist.x & class.x=="data" & missing){
    if(sum(complete.cases(vec.x))==0){
      err.list<-c(err.list,paste0("All the elements in '",which,"' are missing!"))
    }
    if(sum(complete.cases(vec.x))<2){
      err.list<-c(err.list,paste0("Cases with non missing values in'",which,"' should be at least 2!"))
    }
  }
  out=list(exist.x=exist.x,vec.x=vec.x,class.x=class.x,
           bad.request=bad.request,err.list=err.list,warn.list=warn.list)
}

chk.data<-function(x,data,name.data,name.x,num = FALSE,missing = FALSE,
                       err.list=NULL,warn.list=NULL,required="vector"){
  exist.x <- FALSE; vec.x<-NULL;  class.x<-"none"; 
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  ini.err<-length(err.list)
  which<-deparse1(substitute(x))
  
  # x not given
  if(isTRUE(missing(x))){err.list<-c(err.list,paste0("'",which,"' is not specified"))  } 
  
  check.x<-function(){
    x.err.list<-list()
    x.exist.x <- FALSE; x.vec.x<-NULL;  x.class.x<-"none"; 
    # x can be an object in the environment, or a column in a dataframe or an element in a list
    # -> try to create a copy of x to verify whether it exists in env or in a df/matrix
    copyx<-function(x){y<-x ; return(y)}
    check.x<-tryCatch(y<-x, error=function(e){chk<-"Error" })
    check.dim<-dim(check.x) ; check.len<-length(check.x)
    # if no dimension and has length x is a vector
    if(is.null(check.dim) && check.len>1){
      if(is.atomic(x)){
        x.exist.x <- TRUE ;  x.vec.x<-x; x.class.x<-"vector"
      } else {
        x.err.list<-c(x.err.list,paste0("'",which,"' should be vector, not ",class(x))) } 
    }
    if(is.null(check.dim) && check.len<2){
      x.err.list<-c(x.err.list,
                    paste0(name.x," does not exist or is not consistent")) } 
    
    if(!is.null(check.dim) & required=="vector"){
      x.err.list<-c(x.err.list,paste0("'",which,"' should be vector, not ",class(x))) } 
    if(!is.null(check.dim) & required=="data" && (inherits(x,"matrix") | inherits(x,"data.frame"))){
      x.exist.x <- TRUE; x.vec.x<-x; x.class.x<-"data"
    }
    
    list(exist.x=x.exist.x,vec.x=x.vec.x,class.x=x.class.x,
         check.dim=check.dim,check.len=check.len,x.err.list=x.err.list)
  }
  
  check.data<-function(){
    d.err.list<-list()
    d.exist.x <- FALSE; d.vec.x<-NULL;  d.class.x<-"none"; 
    ge <- (ls(name=.GlobalEnv))
    if(!(name.data %in% ge)){
      d.err.list<-c(d.err.list,paste0("data ",name.data," does not exist"))}
    if(name.data %in% ge){
      if(!(is.data.frame(data) | is.matrix(data))){
        d.err.list<-c(d.err.list,paste0(name.data," is not a dataframe or a matrix"))}
      if(is.data.frame(data) | is.matrix(data)){
        if(name.x %in% colnames(data)){
          d.vec.x<-data[[name.x]]; d.exist.x <- TRUE; d.class.x<-"vector"} 
        if(!(name.x %in% colnames(data))){
          n.name.x<-gsub("\"", "", name.x)
          if(n.name.x %in% colnames(data)){
            d.vec.x<-data[,n.name.x]; d.exist.x <- TRUE; d.class.x<-"vector"} 
          if(!(n.name.x %in% colnames(data))){
            d.err.list<-c(d.err.list,paste0("Variable ",name.x," is not included in ",name.data))
          }
        }
      }
    }
    list(exist.x=d.exist.x,vec.x=d.vec.x,class.x=d.class.x,
         d.err.list=d.err.list)
  }
  
  
  check.solo<-check.indata <- FALSE
  x.solo<-x.data<-NULL
  if(isFALSE(missing(x))){x.solo<-check.x(); check.solo = TRUE}
  if(isFALSE(missing(data))){x.data<-check.data(); check.indata = TRUE}
  
  # only x given
  if(check.solo & !check.indata){
    exist.x<-x.solo$exist.x; vec.x<-x.solo$vec.x;  
    class.x<-x.solo$class.x
    err.list<-c(err.list,x.solo$x.err.list)
  }
  # both x and data given
  if(check.solo & check.indata){
    # x exists & not a column of data
    if(x.solo$exist.x & !x.data$exist.x){
      exist.x<-x.solo$exist.x; vec.x<-x.solo$vec.x;  
      class.x<-x.solo$class.x
      err.list<-c(err.list,x.solo$x.err.list)
    }
    # x does not exist & x is a column of data
    if(!x.solo$exist.x & x.data$exist.x){
      exist.x<-x.data$exist.x; vec.x<-x.data$vec.x;  
      class.x<-x.data$class.x
      err.list<-c(err.list,x.data$d.err.list)
    }
    # x does not exist & x is not a column of data
    if(!x.solo$exist.x & !x.data$exist.x){
      err.list<-c(err.list,x.solo$x.err.list)
      err.list<-c(err.list,x.data$d.err.list)
    }
    # x exists & x is a column of data
    if(x.solo$exist.x & x.data$exist.x){
      warn.list<-c(warn.list,
                   paste0(name.x," is both a vector/factor in the environment",
                          " and a column in ",name.data,
                          "\n   "," -> The latter will be considered"))
      exist.x<-x.data$exist.x; vec.x<-x.data$vec.x;  
      class.x<-x.data$class.x
      err.list<-c(err.list,x.data$d.err.list)
    }
  }
  
  # consistency checks for x:
  if(exist.x && class.x=="vector" && missing){
    if(sum(is.na(vec.x))==length(vec.x)){
      err.list<-c(err.list,paste0("All the elements in '",which,"' are missing!"))
      exist.x <- FALSE}
    if(exist.x && sum(!is.na(vec.x))<2){
      err.list<-c(err.list,paste0("Cases with non missing values in '",which,"' should be at least 2!"))
      exist.x <- FALSE}
    if(exist.x && length(unique(vec.x))==1){
      err.list<-c(err.list,paste0("'",which,"' is constant (takes only one value)!"))
      exist.x <- FALSE}
  }
  if(exist.x & class.x=="vector" & num){
    if(!is.numeric(vec.x)){
      err.list<-c(err.list,paste0("'",which,"' should be numeric"))
    }
  }
  
  if(exist.x & class.x=="data" & missing){
    if(sum(complete.cases(vec.x))==0){
      err.list<-c(err.list,paste0("All the elements in '",which,"' are missing!"))
    }
    if(sum(complete.cases(vec.x))<2){
      err.list<-c(err.list,paste0("Cases with non missing values in'",which,"' should be at least 2!"))
    }
  }
  out=list(exist.x=exist.x,vec.x=vec.x,class.x=class.x,
           err.list=err.list,warn.list=warn.list)
}

chkpar.option<-function(value,allowed,onlyone = TRUE,listall = TRUE,
                        err.list=NULL,warn.list=NULL){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  what<-deparse1(substitute(value))
  exist.char <- FALSE; fin.val<-"none"
  ok.value<-(complete.cases(value))
  if(sum(ok.value)==0){
    err.list<-c(err.list,paste0("All the elements of '",what,"' are missing!"))
  } else if(sum(ok.value)>0){
    if(!inherits(value,"character")){
      err.list<-c(err.list,paste0("'",what,"' cannot be a ",class(value)[1]))
    } 
    if(inherits(value,"character")){
      value<-value[ok.value]
      is.inchar<-pmatch(toupper(value),toupper(allowed))
      valid<-!is.na(is.inchar)
      not.valid<-is.na(is.inchar)
      if(sum(valid)==0){
        if(listall){
          err.list<-c(err.list,paste0("Invalid '",what,"' argument (allowed are ",
                                      paste0(allowed,collapse="/"),")"))}
        if(!listall){
          err.list<-c(err.list,paste0("Invalid '",what,"' argument"))}
      } 
      if(sum(not.valid)>=1 & sum(valid)>0){
        warn.list<-c(warn.list,paste0("Wrong specification/s for '",what,"': ",
                                      paste0(value[not.valid],collapse=", ")))
      }
      if(sum(valid)>=1){
        exist.char <- TRUE
        fin.val<-allowed[is.inchar[valid]]
        if(onlyone & sum(valid)>1){
          warn.list<-c(warn.list,paste0("More valid options defined for '",what,
                                        "'; only the first is considered"))
          fin.val<-fin.val[1]
        }
      } 
    }
  }
  out=list(err.list=err.list,exist=exist.char,value=fin.val,warn.list=warn.list)
  return(out)
}

