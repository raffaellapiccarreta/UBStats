## --------------------------------## 
## ------- Utility functions ------## 
## --------------------------------## 

# General functions used everywhere -------------
## Secret function to use to create 
#    a "bad" output to be modified in .tex
#    after markdown
.do_rpic_set_markdown<-function(){
  Outlet<-"Console"
  type.print<-"cat"
  msg.p<-list(err = TRUE,warn = TRUE,msg = TRUE)
  check.mkd<-tryCatch(y<-.private_creator_markdown_piccarreta, 
                      error=function(e){chk<-"Error" })
  if(exists(".private_creator_markdown_piccarreta",
            envir = globalenv())){
    Outlet<-"Markdown"
    type.print<-"print"
    if(!is.null(.private_creator_markdown_piccarreta$msg.p)){
      msg.p<-.private_creator_markdown_piccarreta$msg.p
    }
  }
  list(type.print=type.print,msg.p=msg.p)
}

## Function to stop quietly
stop_quietly <- function() {
  opt<-getOption("show.error.messages")
  on.exit(options(show.error.messages = opt))  
  options(show.error.messages = FALSE)  
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

# Function to signal error and stop when
# unmanaged errors are encountered
Signal.Error<-function(x,type.print){
  tryCatch(x,error=function(e){
    my.p.list(c("\nThere is an unmanaged error in the syntax.
    The procedure is interrupted"),type.print=type.print)
  })
}

# Function to verify whether an input exists or not
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

# Function to check whether para are ok
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

# Functions specific for x and xy tables and plots -----
## General utilities for tables and plots -----
# function to define a nice maximum
pretty_max<-function(tab,type){
  if(isTRUE(missing(type))){
    use.max.y<-max(pretty(max(tab,na.rm = TRUE)))
  } else {
    if(type=="Counts" & max(tab)>100){
      use.max.y<-max(pretty(max(tab)))}
    if(type=="Percentages" | 
       (type=="Counts" & max(tab)<=100)){
      if(abs(max(tab,na.rm = TRUE)-100)<0.9){use.max.y<-100
      } else {
        use.max.y<-min(seq(0,100,by=5)[(seq(0,100,by=5)>=max(tab,na.rm = TRUE))])}
    }
    if(type=="Proportions"){
      use.max.y<-min(seq(0,1,by=0.05)[(seq(0,1,by=0.05)>=max(tab,na.rm = TRUE))])}
    if(type %in% c("Densities","Density")){
      p<-signif(max(tab),1)
      use.max.y<-seq(0,2*p,by=p/100)
      use.max.y<-min(use.max.y[use.max.y>=max(tab,na.rm = TRUE)])
    }
  }
  use.max.y
}

# function to verify whether two objects exist
# check if used 
chkcon.des<-function(type=NULL,err.list=NULL,
                     warn.list=NULL,x=NULL,
                     y1,y2,data,name.1,name.2,name.data){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  exist.y1<-exist.y2 <- FALSE
  vec.y1<-vec.y2<-NULL
  what1<-deparse1(substitute(y1))
  what2<-deparse1(substitute(y2))
  
  if(isFALSE(missing(y1))){
    check.y1<-chk.data(y1,data,name.data,name.1,
                       missing = TRUE,err.list=err.list)
    err.list<-check.y1$err.list
    exist.y1<-check.y1$exist.x; 
    vec.y1<-check.y1$vec.x
    
    if(exist.y1 && is.numeric(vec.y1) & length(unique(vec.y1))>20){
      err.list<-c(err.list,paste0("'",what1,"' is numeric and has too many levels",
                                  "\n   "," -> to force the procedure transform '",what1,"' into a factor"))
      exist.y1 <- FALSE}
    if(!is.null(x) && exist.y1 && length(x) != length(vec.y1)){
      err.list<-c(err.list,paste0("'x' and '",what1,"' should have the same length"))
      exist.y1 <- FALSE}
  }
  
  if(isFALSE(missing(y2))){
    check.y2<-chk.data(y2,data,name.data,name.2,
                       missing = TRUE,err.list=err.list)
    err.list<-check.y2$err.list
    exist.y2<-check.y2$exist.x; 
    vec.y2<-check.y2$vec.x
    
    if(exist.y2 && is.numeric(vec.y2) & length(unique(vec.y2))>20){
      err.list<-c(err.list,paste0("'",what2,"' is numeric and has too many levels",
                                  "\n   "," -> to force the procedure transform '",what2,"' into a factor"))
      exist.y2 <- FALSE}
    if(!is.null(x) && exist.y2 && length(x) != length(vec.y2)){
      err.list<-c(err.list,paste0("'x' and '",what2,"' should have the same length"))
      exist.y2 <- FALSE}
  }
  if(!is.null(x) && exist.y1 && !exist.y2 && length(x) == length(vec.y1) && 
     sum(!(is.na(x)) & !(is.na(vec.y1)))<2){
    err.list<-c(err.list,"Cases with non missing values on input vectors should be at least 2")
    exist.y1 <- FALSE
  }
  if(!is.null(x) && !exist.y1 && exist.y2 && length(x) == length(vec.y2) && 
     sum(!(is.na(x)) & !(is.na(vec.y2)))<2){
    err.list<-c(err.list,"Cases with non missing values on input vectors should be at least 2")
    exist.y2 <- FALSE
  }
  if(!is.null(x) && exist.y1 && exist.y2 && 
     length(x) == length(vec.y1) &&  length(x) == length(vec.y2) && 
     sum(!(is.na(x)) & !(is.na(vec.y1)) & !(is.na(vec.y2)))<2){
    err.list<-c(err.list,"Cases with non missing values on input vectors should be at least 2")
    exist.y1<-exist.y2 <- FALSE
  }
  out<-list(exist.1=exist.y1,exist.2=exist.y2,l1=vec.y1,l2=vec.y2,err.list=err.list)
}

## Function to extract endpoints for a variable classified in intervals
chk.interval<-function(obs.x,name.x,consistency = FALSE,warn.list=NULL,
                       err.list=NULL,list.print=NULL,suffix = FALSE){
  if(!is.list(warn.list)){warn.list<-list()}
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(list.print)){list.print<-list()}
  
  separators<-c(",",";",":","-","/")
  endpoints<-list(low=c("(","["),up=c(")","]"))
  
  # extract the classes
  copylow<-copyup<-obs.classes<-unique(na.omit(obs.x))
  # extract endpoints from classes using different separators and delimiters
  is.ok<-"NO" # will be set to ok when numeric endpoints are found
  for(k in separators){ 
    if(is.ok=="NO"){ 
      for(j in endpoints[["low"]]){ 
        copylow<-sub(paste0("\\",j,"(.+)",k,".*"), "\\1", copylow)  }
      copylow<-sub(paste0("\\",k,".*"), "\\1", copylow) 
      
      for(j in endpoints[["up"]]){
        copyup<-sub(paste0("[^,]*",k,"([^]]*)\\",j), "\\1", copyup)   }
      copyup<-sub(paste0("[^,]*",k,"([^]]*)"), "\\1", copyup)
      
      # check that all the extremes have been identified
      check.k<-suppressWarnings(as.numeric(unique(c(copylow,copyup))))
      # if all elements are numeric we probably found the classes
      if(sum(is.na(check.k))==0){
        is.ok<-"OK" # num. endpoints found
        df.ends<-data.frame(Obs=obs.classes,
                            Low=as.numeric(copylow),Up=as.numeric(copyup))
        df.ends<-df.ends[order(df.ends$Low),]
        
        # check inconsistencies in endpoints
        df.ends$Overlap<-df.ends$WrongEnds<-"OK"
        # check up<low
        up_low_end<-(df.ends$Up-df.ends$Low)
        if(any(up_low_end<=0)){
          is.ok<-"TABLE" # var can just be tabulated as it is
          df.ends$WrongEnds[up_low_end==0]<-"Low=Up"
          df.ends$WrongEnds[up_low_end<0]<-"Low>Up"}
        # check overlapping
        diff.cl<-(as.double(diff(as.vector(t(cbind(df.ends$Low,df.ends$Up))))))
        diff.cl<-matrix(c(0,diff.cl),byrow = TRUE,ncol=2)
        is.overl<-(diff.cl[,1]<0 | diff.cl[,2]<0)
        if(any(is.overl)){
          is.ok<-"TABLE" # var can just be tabulated as it is
          df.ends$Overlap[is.overl]<-"Overlay"}
      }
    } # close ok=NO
  } # close k
  if(is.ok=="OK"){
    list.print[[length(list.print)+1]]<-paste0("Intervals endpoints detected and used for '",name.x,"'")
    if(consistency){
      df.ends.d<-as.matrix(t(df.ends[,c("Low","Up")]))
      colnames(df.ends.d)<-df.ends$Obs
      list.print[[length(list.print)+1]]<-data.frame(df.ends.d,check.names = FALSE)
      list.print[[length(list.print)+1]]<-"\n"
    }
    out=list(isinterval.x = TRUE,x=factor(obs.x,levels=df.ends$Obs),
             tab=df.ends,level.consistency="Numeric",err.list=err.list,
             warn.list=warn.list,list.print=list.print)
    return(out)}
  if(is.ok=="TABLE"){
    if(!consistency){
      warn.list<-c(warn.list,paste0("Intervals endpoints detected for '",name.x,"' are inconsistent!",
                                    "\n   ","  ->'",name.x,"' is analyzed as it is,", 
                                    " but results might be meaningless"))
      out=list(isinterval.x = TRUE,x=factor(obs.x,levels=df.ends$Obs),tab=df.ends,
               level.consistency="Factor",warn.list=warn.list,err.list=err.list,
               list.print=list.print)
      return(out)}
    if(consistency){
      err.list[[length(err.list)+1]]<-paste0("Intervals endpoints detected for '",name.x,"' are inconsistent\n")
      err.list[[length(err.list)+1]]<-df.ends
      out=list(isinterval.x = FALSE,x=NULL,tab=NULL,level.consistency="none",warn.list=warn.list,
               err.list=err.list,list.print=list.print)
      return(out)}
  }
  if(is.ok=="NO"){
    if(!consistency){
      warn.list<-c(warn.list,paste0("Intervals endpoints for '",name.x,"' could not be detected",
                                    "\n   ","  ->'",name.x,"' is analyzed as it is,", 
                                    " but results might be meaningless"))
      out=list(isinterval.x = FALSE,level.consistency="Factor",warn.list=warn.list,
               err.list=err.list,list.print=list.print)
      return(out)}
    if(consistency){
      err.list<-c(err.list,paste0("Intervals endpoints for '",name.x,"' could not be detected"))
      out=list(isinterval.x = FALSE,x=NULL,tab=NULL,level.consistency="none",
               warn.list=warn.list,err.list=err.list,list.print=list.print)
      return(out)}
  }
}

## Function to classify a variable into intervals
chk.breaks<-function(x,name.x,breaks,adj.breaks,consistency = TRUE,
                     err.list=NULL,warn.list=NULL,nn=NULL){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  
  name.x<-deparse1(substitute(x))
  name.breaks<-"breaks"
  if(!is.null(nn)){name.breaks<-nn}
  ok.breaks <- TRUE
  i.x<-NULL
  
  #check breaks
  isna.x<-sum(is.na(x))
  if(length(breaks)==1){
    r.min<-min(x,na.rm = TRUE)
    r.max<-max(x,na.rm = TRUE)
    r.by<-(r.max-r.min)/breaks
    brk<-seq(r.min,r.max,by=r.by)
    i.x<-cut(x,breaks=brk,include.lowest = TRUE,right = FALSE)
    levels(i.x)<-paste0(rep("[",length(brk)-1),
                        round(brk[-length(brk)],5),",",round(brk[-1],5),
                        c(rep(")",length(brk)-2),"]"))
    breaks<-brk
  } else if(length(breaks)>1){
    if(any(diff(breaks)<=0)){
      err.list<-c(err.list,paste0("'",name.breaks,"' are not strictly increasing"))
      ok.breaks <- FALSE
    } else {
      i.x<-cut(x,breaks=breaks,include.lowest = TRUE,right = FALSE)
      levels(i.x)<-paste0(rep("[",length(breaks)-1),
                          round(breaks[-length(breaks)],5),",",
                          round(breaks[-1],5),
                          c(rep(")",length(breaks)-2),"]"))
      isna.newx<-sum(is.na(i.x))
      # Breaks are not ok?
      if(isna.newx != isna.x){
        warn.list<-c(warn.list,
                     paste0("Some values of '",name.x,"' not counted!",
                            "\n   ","  -> Selected '",name.breaks,"' might not span '",name.x,"'s range",
                            "\n   ","  -> Be sure this is actually what you want"))
      }
    }
  }
  # if required, avoid scientific notation (only to a certain extent)
  if(adj.breaks & ok.breaks){
    op.sci<-getOption("scipen")
    # change options only if not previously modified by the user
    if(op.sci==0){options(scipen=10)}
    labs<-levels(i.x)
    lower = ( sub("\\((.+),.*", "\\1", labs) )
    lower = ( sub("\\[(.+),.*", "\\1", lower) )
    upper = ( sub("[^,]*,([^]]*)\\]", "\\1", labs) )   
    upper = ( sub("[^,]*,([^]]*)\\)", "\\1", upper) )   
    for(k in unique(c(lower,upper))){
      labs<-gsub(as.character(k),
                 as.character(as.numeric(k)),labs,fixed = TRUE)}
    levels(i.x)<-labs
    options(scipen=op.sci)
  } # close adjust breaks
  out<-list(ok.breaks=ok.breaks,breaks=breaks,i.x=i.x,
            err.list=err.list,warn.list=warn.list)
}

## Function to create a list with var features
build.Xlist<-function(x,name.x,breaks,interval,adj.breaks,consistency = FALSE,
                      err.list=NULL,warn.list=NULL,
                      list.print=NULL,suffix = FALSE){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  if(!is.list(list.print)){list.print<-list()}
  name.breaks<-deparse1(substitute(breaks))
  name.interv<-deparse1(substitute(interval))
  if(suffix){nn<-name.breaks} else{nn<-NULL}
  
  isnum<-is.numeric(x) # check x numeric
  isfac<-is.factor(x) # check x factor
  
  # Create a list with all the info needed to build tables/plots
  Xlist<-list(class="standard",isnum=isnum,V=x,V.f=x)
  if(!isnum & !isfac){Xlist$V.f<-factor(x)}
  
  if(isTRUE(missing(breaks)) & !interval){
    out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
              list.print=list.print)
    return(out)
  }
  # check breaks and intervals
  ok.check.breaks<-ok.check.interval <- TRUE
  
  if(isFALSE(missing(breaks)) & interval){
    if(isnum){ok.check.interval <- FALSE
    warn.list<-c(warn.list,paste0("'",name.interv,"' ignored because '",name.x,"' is numeric"))}
    if(!isnum){ok.check.breaks <- FALSE
    warn.list<-c(warn.list,paste0("'",name.breaks,"' ignored because '",name.x,"' is not numeric"))}
  }
  
  ## Breaks: allowed only for numbers
  if(isFALSE(missing(breaks)) & ok.check.breaks){
    if(!isnum){ # The var is not numeric
      err.list<-c(err.list,paste0("'",name.x,"' is not numeric: '",name.breaks,"' not allowed!"))
      ok.check.breaks <- FALSE
      out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
                list.print=list.print)
      return(out)
    } else { 
      check.breaks<-chk.breaks(x=x,name.x=name.x,breaks=breaks,adj.breaks=adj.breaks,
                               consistency=consistency,
                               warn.list=warn.list,err.list=err.list,nn=nn)
      # update the list including info on breaks
      Xlist<-list(class="breaks",isnum = TRUE,breaks=check.breaks$breaks,V=x,
                  V.f=check.breaks$i.x)
      warn.list<-check.breaks$warn.list
      err.list<-check.breaks$err.list
      out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
                list.print=list.print)
      return(out)
    }
  } # close breaks
  
  # Deal with interval variables
  if(interval & ok.check.interval){
    if(isnum){ # The var cannot be numeric
      err.list<-c(err.list,paste0("'",name.x,"' is a numeric and not an interval variable!",
                                  "\n   ","   -> Consider using '",name.breaks,
                                  "' if you want to classify '",name.x,"'"))
      ok.check.interval <- FALSE
      out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
                list.print=list.print)
      return(out)
    } else {
      # check if intervals endpoints can be found & are consistent
      check.int<-chk.interval(obs.x=x,name.x=name.x,consistency=consistency,
                              warn.list=warn.list,
                              err.list=err.list,list.print=list.print)
      warn.list<-check.int$warn.list
      err.list<-check.int$err.list
      list.print<-check.int$list.print
      if(!check.int$isinterval.x){
        out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,list.print=list.print)
        return(out)
      }
      # If intervals correctly found, modify the object
      if(check.int$isinterval.x & check.int$level.consistency=="Numeric"){
        Xlist<-list(class="interval",isnum = TRUE,info.int=check.int,
                    V=check.int$x,V.f=check.int$x)
        out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
                  list.print=list.print)
        return(out)
      }
      # if endpoint are inconsistent, results used only to
      # correctly order the intervals
      if(check.int$isinterval.x & check.int$level.consistency=="Factor"){
        Xlist<-list(class="standard",isnum = FALSE,V=check.int$x,V.f=check.int$x)
        out<-list(Vlist=Xlist,warn.list=warn.list,err.list=err.list,
                  list.print=list.print)
        return(out)
      }
    }
  } # closes interval
}

## Function to create univariate tables
build.table.x<-function(Xlist,name.x,freq,total,
                        use.digits,force.digits=FALSE,
                        use.scientific=FALSE){
  op.sci<-getOption("scipen")
  # capitalize freq
  substr(freq,1,1)<-toupper(substr(freq,1,1))
  # if only Cumulative requested, add also freq (counts or prop) to cum
  if(length(freq)==1 & ("Cumulative" %in% freq)){
    freq<-c("Counts","Proportions","Cumulative")}
  if(("Cumulative" %in% freq) & 
     sum(c("Counts","Proportions","Percentages") %in% freq)==0){
    freq<-c(freq,"Counts","Proportions")}
  
  out<-data.frame(table(Xlist$V.f))
  colnames(out)<-c(name.x,"Count")
  if("Proportions" %in% freq){out$Prop<-out$Count/(sum(out$Count))}
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
  if(total){
    out[nrow(out)+1,]<-rep(NA,ncol(out))
    out[nrow(out),1]<-"TOTAL"
    no.cum<-!(substr(colnames(out),1,3) %in% c("Cum","Den") |
                colnames(out)==name.x)
    out[nrow(out),no.cum]<-apply(out[,no.cum,drop = FALSE],2,"sum",na.rm = TRUE)
  }
  rownames(out)[nrow(out)]<-"Sum"
  
  use.digits["Cum.Prop"]<-use.digits["Prop"]
  use.digits["Cum.Percent"]<-use.digits["Percent"]
  
  list.out<-list()
  for(k in c("Prop","Percent","Density")){
    if(k %in% colnames(out)){
      to.use<-out[,k]; n.use<-rep(k,length(to.use))
      if(paste0("Cum.",k) %in% colnames(out)){
        to.use<-c(to.use,out[,paste0("Cum.",k)]); 
        n.use<-c(n.use,rep(paste0("Cum.",k),length(n.use)))
      }
      round.k<-round(to.use,use.digits[k])
      if(k=="Prop"){
        n.dec.k<-nchar(sub("^\\d+\\.","",sub("0+$","",
                                             as.character(round.k))))
        n.dec.k<-max(n.dec.k,na.rm=T)
        round.k<-format(round.k,nsmall=n.dec.k)
      }
      if(k=="Percent" & use.digits["Percent"]>0){
        n.dec.k<-nchar(sub("^\\d+\\.","",as.character(round.k[round.k<100])))
        n.dec.k<-max(n.dec.k,na.rm=T)
        round.k<-format(round.k,nsmall=n.dec.k)
      }
      list.out[[k]]$original<-to.use
      list.out[[k]]$r.values<-round.k
      list.out[[k]]$names<-n.use
    }
  }
  if(!force.digits){
    for(k in names(list.out)){
      use.values<-list.out[[k]]$original
      min.nozero<-min(use.values[use.values>0],na.rm=T)
      min.round<-round(min.nozero,use.digits[k])
      if(min.round==0){
        list.out[[k]]$r.values<-format(use.values,digits=1,
                                       scientific=use.scientific)
      }
      if(is.numeric(list.out[[k]]$r.values)){
        list.out[[k]]$r.values<-as.character(list.out[[k]]$r.values)
      }
      is.NA<-grep("NA",list.out[[k]]$r.values)
      if(length(is.NA)>0){list.out[[k]]$r.values[is.NA]<-""}
    }
  }
  out.print<-data.frame(as.character(out[,1]))
  colnames(out.print)<-colnames(out)[1]
  for(k in colnames(out)[2:ncol(out)]){
    if(k %in% c("Count","Cum.Count")){
      out.print[,k]<-as.character(out[,k])}
    if(k %in% c("Prop","Percent","Density")){
      out.print[,k]<-list.out[[k]]$r.values[list.out[[k]]$names==k]}
    if(k %in% c("Cum.Prop")){
      out.print[,k]<-list.out[["Prop"]]$r.values[list.out[["Prop"]]$names==k]}
    if(k %in% c("Cum.Percent")){
      out.print[,k]<-list.out[["Percent"]]$r.values[list.out[["Percent"]]$names==k]}
  }
  if(!use.scientific){options(scipen=10)}
  out.print.p<-as.matrix(out.print)
  out.print.p[is.na(out)]<-""
  print(as.data.frame(out.print.p),row.names = FALSE,quote = FALSE,
        right = TRUE)
  output<-out
}

## Function to create bivariate tables
build.table.xy<-function(Xlist,Ylist,name.x,name.y,
                         type.tab,freq,total,use.digits,force.digits=FALSE){
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
        if(total){tab<-addmargins(tab)}
        colnames(tab)[colnames(tab)=="Sum"]<-"TOTAL"
        rownames(tab)[rownames(tab)=="Sum"]<-"TOTAL"
        out[[tit]]<-as.data.frame.matrix(tab) # save the table
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
        if(total){tab<-addmargins(tab,use.m[2])}
        colnames(tab)[colnames(tab)=="Sum"]<-"TOTAL"
        rownames(tab)[rownames(tab)=="Sum"]<-"TOTAL"
        out[[tit]]<-as.data.frame.matrix(tab) # save the table
        
        if(count.tab==0){cat(paste0(tit,"\n"))
        } else {cat(paste0("   \n",tit,"\n"))}
        
        # added
        min.nozero<-min(tab[tab>0],na.rm=T)
        min.round<-round(min.nozero,use.digits[freq.i])
        if(min.round==0 & !force.digits){
          print(tab,digits=1,quote=F,right=T)
        } else {print(round(tab,use.digits[freq.i]),quote=F,right=T)
        }
        count.tab<-count.tab+1
      }
    }
  }
  Out<-out
}




# Added 202405
## Function to build colors for plots -----
#' @importFrom grDevices gray.colors
#' @importFrom grDevices rgb
#' @importFrom grDevices hcl.colors
#' @importFrom grDevices colorRampPalette
#' @importFrom stats dnorm
# Function to create the default palette
build.palette<-function(n,add=0.15,mod.b=TRUE,
                        palette="rgb"){
  if (n <= 0){return(character(0))}
  if(n==1){col<-palette}
  # Adapted from rich.colors in gplots
  if(n>1){
    if(palette=="rgb"){
      x <- seq(0, 1, length = n)
      r <- 1/(1 + exp(20 - 35 * x))
      g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
      b <- stats::dnorm(x, 0.25, 0.15)/max(stats::dnorm(x, 0.25, 0.15))
      # fix greens (too dark blue)
      tofix<-(1:length(g))[g<g[length(x)] & x<(0.5)]
      if(length(tofix)==1){g[tofix]<-add}
      if(length(tofix)>1){
        slope.g<-(g[length(x)]-add)/x[length(tofix)]
        g[tofix]<-add+slope.g*x[tofix]
      }
      # fix blues (too dark blue)
      if(mod.b){
        is.max<-(1:length(b))[b==max(b)][1]
        b[1:is.max]<-b[1:is.max]+0.2
        b[b>1]<-1
      }
      col <- mapply(grDevices::rgb, r, g, b, 1)
    }
    if(palette=="spectral"){
      col<-grDevices::hcl.colors(n,palette = "Spectral")
    }
    if(palette=="bw"){col=grDevices::gray.colors(n)}
    if(palette=="ramp_red_green"){
      col<-grDevices::colorRampPalette(c("darkgreen","green",
                                         "gold","darkorange","red"))
      col<-col(n)
    }
  }
  col=rev(col)
  return(col)
}

# Function to create colors
build.colors<-function(n,bw=TRUE,color=NULL,
                       bw.default="black",
                       col.default="rgb"){
  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)})
  }
  if(n==1){
    if(bw){use.color<-bw.default}
    if(!(is.null(color))){
      good.colors<-color[areColors(color)==TRUE]
      if(length(good.colors)>=1){use.color<-good.colors[1]
      } else {use.color <- col.default}
    }
    if(!bw & is.null(color)){use.color <- col.default} 
  }
  if(n>1){
    if(bw){
      use.color <- build.palette(n,palette="bw")} 
    if(!bw & is.null(color)){
      use.color <- build.palette(n,palette=col.default)} 
    if(!(is.null(color))){
      good.colors<-color[areColors(color)==TRUE]
      if(length(good.colors)>=n){use.color<-good.colors[1:n]
      } else {
        use.color <- build.palette(n,palette=col.default)}
    }
  }
  return(use.color)
}

# Function to create colors for continuous var
#' @importFrom grDevices gray
#' @importFrom grDevices rgb
#' @importFrom stats dnorm
build.varcolors<-function(var,bw=TRUE,color=NULL,
                          add=0.15,mod.b=TRUE){
  create.rgb<-function(){
    r <- 1/(1 + exp(20 - 35 * vec))
    g <- pmin(pmax(0, -0.8 + 6 * vec - 5 * vec^2), 1)
    b <- stats::dnorm(vec, 0.25, 0.15)/max(stats::dnorm(vec, 0.25, 0.15))
    # fix greens (too dark blue)
    # 0.2=g(1)
    tofix<-(1:length(g))[g<0.2 & vec<(0.5)]
    if(length(tofix)==1){g[tofix]<-add}
    if(length(tofix)>1){
      slope.g<-(0.2-add)/vec[length(tofix)]
      g[tofix]<-add+slope.g*vec[tofix]
    }
    # fix blues (too dark blue)
    if(mod.b){
      is.max<-(1:length(b))[b==max(b)][1]
      b[1:is.max]<-b[1:is.max]+0.2
      b[b>1]<-1
    }
    col <- mapply(grDevices::rgb, r, g, b, 1)
    return(col)
  }
  mis.color<-"white"
  
  pretty.d<-pretty(var)
  pretty.min<-min(pretty.d)
  pretty.max<-max(pretty.d)
  var.N<-(var-pretty.min)/(pretty.max-pretty.min)
  legend.p<-c(pretty.d,
              (pretty.d[-length(pretty.d)] + pretty.d[-1L])/2)
  legend.p<-legend.p[order(legend.p)]
  legend.N<-(legend.p-pretty.min)/(pretty.max-pretty.min)
  
  list.rgb<-list()
  x.orig<-data.frame(d=var,vec=var.N,row=1:length(var.N))
  pres.mis<-sum(is.na(var.N))
  vec <-1-var.N[!is.na(var.N)]
  row<-x.orig$row[!is.na(var.N)]
  if(bw){col.x<-(gray(vec)); mis.color<-"pink"}
  if(!bw & is.null(color)){col.x<-create.rgb()}
  if(!is.null(color)){col.x<-create.rgb()}
  data.m <- data.frame(vec=vec,col=col.x,row=row)
  data.m<-merge(data.m,x.orig,by="row",all=TRUE,sort=FALSE)
  data.m$col[is.na(data.m$vec)]<-mis.color
  data.m<-data.m[order(data.m$row),]
  list.rgb$data<-data.m
  
  x.orig<-data.frame(d=legend.p,vec=legend.N,row=1:length(legend.N))
  vec <-1-legend.N
  row<-x.orig$row
  if(bw){col.x<-(gray(vec)); mis.color<-"pink"}
  if(!bw & is.null(color)){col.x<-create.rgb()}
  if(!is.null(color)){col.x<-create.rgb()}
  data.m <- data.frame(vec=vec,col=col.x,row=row)
  data.m<-merge(data.m,x.orig,by="row",all=TRUE,sort=FALSE)
  data.m<-data.m[order(data.m$row),]
  data.m$col[is.na(data.m$vec)]<-mis.color
  
  data.m$select<-data.m$d %in% pretty.d
  data.m$label.legend<-""
  data.m$label.legend[data.m$select]<-data.m$d[data.m$select]
  if(pres.mis>0){
    add.mis<-rep(NA,ncol(data.m))
    names(add.mis)<-colnames(data.m)
    add.mis["col"]<-"white"
    data.m<-rbind(data.m,add.mis,add.mis)
    data.m$label.legend[nrow(data.m)]<-"NA"
    data.m$col[nrow(data.m)]<-mis.color
  }
  data.m$border<-"black"
  data.m$border[!is.na(data.m$col) & data.m$col=="white" &
                  is.na(data.m$label.legend)]<-"white"
  list.rgb$legend<-data.m
  return(list.rgb)
}


## Functions to obtain plots of a single variable ------
# Modified 202405
plt.x.pie<-function(tab,bw = TRUE,color=NULL,name.x,freq){
  use.color<-build.colors(n=length(tab),bw,color)
  mai.p<-par("mai")
  par(mai=c(0,0,0.1,0))
  pie(tab,col=use.color,clockwise = TRUE)
  mtext(side=3,paste0("Pie chart: ",name.x),font=2,
        cex=par("cex.main"),line=-1)
  par(mai=mai.p)
}
plt.x.bars<-function(tab,bw = TRUE,color=NULL,name.x,
                     freq){
  pos.tit2<-2.6
  use.color<-build.colors(n=1,bw,color,bw.default="grey",
                          col.default="skyblue")
  use.max.y<-pretty_max(tab,freq)
  if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  barplot(tab,col=use.color,main=paste0("Bar plot: ",name.x),
          ylim=c(0,use.max.y))
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = pos.tit2,las=0)
  box()
}
plt.x.bars<-function(tab,bw = TRUE,color=NULL,name.x,
                     freq,tit=NA){
  pos.tit2<-2.6
  use.color<-build.colors(n=1,bw,color,bw.default="grey",
                          col.default="skyblue")
  use.max.y<-pretty_max(tab,freq)
  if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  if(is.na(tit)){
    barplot(tab,col=use.color,main=paste0("Bar plot: ",name.x),
            ylim=c(0,use.max.y))
  }
  if(!is.na(tit)){
    barplot(tab,col=use.color,
            main=tit,ylim=c(0,use.max.y))
  }
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = pos.tit2,las=0)
  box()
}
plt.x.spike<-function(xlist,bw=TRUE,color=NULL,name.x,freq){
  pos.tit2<-2.6
  use.color<-build.colors(n=1,bw,color,bw.default="black",
                          col.default="black")
  tab<-xlist$tab.x
  use.max.y<-pretty_max(tab,freq)
  if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  if(xlist$isnum & xlist$class=="standard"){
    x.values<-as.numeric(names(tab))
    plot(x.values,y=as.vector(tab),type="p",
         main=paste0("Spike plot: ",name.x),
         ylim=c(0,use.max.y),xlab="",ylab="",lwd=2,pch=19,
         cex=0.01,
         xlim=c(min(x.values),max(x.values)),col=use.color) 
    lines(tab)
    points(x.values,y=as.vector(tab),pch=21,
           bg=use.color,cex=1.2,
           ylim=c(0,use.max.y),xlab="",ylab="",lwd=2) 
    box()
  } else {
    x.values<-factor(names(tab),levels=names(tab))
    plot(tab,main=paste0("Spike plot: ",name.x),type="p",
         pch=19,cex=0.01,
         ylim=c(0,use.max.y),xlab="",ylab="",lwd=2) 
    lines(tab)
    points(1:length(x.values),tab,pch=21,
           bg=use.color,cex=1.2,
           ylim=c(0,use.max.y),xlab="",ylab="",lwd=2) 
    box()
  }
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = pos.tit2,las=0)
}
plt.x.cum<-function(xlist,bw=TRUE,color=NULL,name.x,freq,adj.breaks){
  pos.tit2<-2.6
  use.color<-build.colors(n=1,bw,color,bw.default="black",
                          col.default="black")
  
  if(xlist$class=="interval" | xlist$class=="breaks"){
    if(xlist$class=="interval"){
      check.int<-xlist$info.int$tab
      x<-xlist$V
      # Adjust possible missing intervals
      all.ends<-unique(c(check.int$Low,check.int$Up))
      all.ends<-all.ends[order(all.ends)]
      # reshape x assigning upper ends to intervals
      intm.x<-factor(x,levels=check.int$Obs,labels=check.int$Up)
      intm.x<-as.numeric(as.character(intm.x))
      # reshape using all the endpoints as levels.
      # Not observed endpoints are added but will not contribute to freqs
      intm.x<-factor(intm.x,levels=all.ends)
      x.values<-all.ends
      tab<-switch(freq,Counts=table(intm.x),
                  Proportions=prop.table(table(intm.x)),
                  Percentages=prop.table(table(intm.x))*100)
      Cum.freq<-cumsum(tab)
    }
    if(xlist$class=="breaks"){
      # if breaks specified, create a factor
      # assigning to x the endpoints of specified intervals
      x.f<-cut(xlist$V,breaks=xlist$breaks,
               right = FALSE,include.lowest = TRUE,labels=xlist$breaks[-1])
      x.values<-c(xlist$breaks[1],as.numeric(levels(x.f)))
      tab<-switch(freq,Counts=table(x.f),
                  Proportions=prop.table(table(x.f)),
                  Percentages=prop.table(table(x.f))*100)
      Cum.freq<-c(0,cumsum(tab))
    }
    use.max.y<-switch(freq,Counts=pretty_max(Cum.freq,freq),
                      Proportions=as.integer(1),
                      Percentages=as.integer(100))
    mylas<-1
    if(freq=="Counts"){
      if(!adj.breaks){
        if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
      if(adj.breaks){
        if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
          mylas=1} else{mylas=0}}
    }
    par(las=mylas)
    if(!adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),
           col="black")
      # add dots only when reasonable
      if(length(x.values)<100){
        points(x=x.values,y=Cum.freq,pch=21,bg=use.color)}}
    if(adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),
           col="black",axes = FALSE)
      if(length(x.values)<100){
        points(x=x.values,y=Cum.freq,pch=21,bg=use.color)}
      if(freq!="Counts"){
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        axis(2, at=aty)
      }
      if(freq=="Counts"){
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        axis(2, at=aty, labels=format(aty, scientific = FALSE)) 
      }
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = pos.tit2,las=0)
  }
  if(xlist$class=="standard"){
    tab<-xlist$tab.x
    Cum.freq=as.numeric(cumsum(tab))
    use.max.y<-switch(freq,Counts=pretty_max(Cum.freq,freq),
                      Proportions=as.integer(1),
                      Percentages=as.integer(100))
    mylas<-1
    if(freq=="Counts"){
      if(max(nchar(as.character(use.max.y)))>5){mylas=0}
    }
    par(las=mylas)
    if(!xlist$isnum){
      plot(x=factor(names(tab),levels=names(tab)),
           y=Cum.freq,type="l",
           xlab="",ylab="",lwd=1,
           main=paste0("Cumulative freq: ",name.x),
           ylim=c(0,use.max.y),col=use.color)
    } else {
      x.values<-as.numeric(names(tab))
      dr <- max(0.08 * (max(x.values)-min(x.values)), 
                median(diff(x.values)))
      use.xlim <- c(min(x.values)-dr,max(x.values)+ dr)
      use.x.values<-c(min(x.values)-dr,x.values,max(x.values)+dr)
      use.Cum.freq<-c(0,Cum.freq,max(Cum.freq))
      if(!adj.breaks){
        plot(x=c(min(x.values),x.values),y=c(0,Cum.freq),
             type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,col="black")
        # add points only if limited
        if(length(x.values)<100){
          points(x=x.values,y=Cum.freq,pch=21,bg=use.color)}
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col="grey50",lty =2, lwd =1.5)
      }
      if(adj.breaks){
        plot(x=c(min(x.values),x.values),
             y=c(0,Cum.freq),type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,
             axes = FALSE,col="black")
        if(length(x.values)<100){
          points(x=x.values,y=Cum.freq,pch=21,bg=use.color)}
        # use.color substituted by grey
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col="grey50",lty =2, lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col="grey50",lty =2, lwd =1.5)
        if(freq!="Counts"){
          p.yaxp<-par("yaxp")
          aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
          axis(2, at=aty)
        }
        if(freq=="Counts"){
          p.yaxp<-par("yaxp")
          aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
          axis(2, at=aty, labels=format(aty, scientific = FALSE)) 
        }
        p.xaxp<-par("xaxp")
        atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
        use.labs<-format(atx, scientific = FALSE)
        use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
        axis(1, at=atx, labels=use.labs)
      }
      box()
    }
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = pos.tit2,las=0)
  }
}
plt.x.hist<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  pos.tit2<-2.6
  
  use.color<-build.colors(n=1,bw,color,bw.default="grey",
                          col.default="skyblue")
  x<-xlist$V[!(is.na(xlist$V))]
  if(xlist$class=="interval"){
    check.int<-xlist$info.int$tab
    # Adjust possible missing intervals
    all.ends<-unique(c(check.int$Low,check.int$Up))
    all.ends<-all.ends[order(all.ends)]
    # reshape x assigning upper ends to intervals
    intm.x<-factor(x,levels=check.int$Obs,
                   labels=((check.int$Low+check.int$Up)/2))
    intm.x<-as.numeric(as.character(intm.x))
    # save the histogram
    h<-hist(intm.x,breaks=all.ends,include.lowest = TRUE,right = FALSE,plot = FALSE)
  } # close interval = TRUE
  if(xlist$class=="standard"){h<-hist(x,include.lowest = TRUE,right = FALSE,plot = FALSE)}
  if(xlist$class=="breaks"){
    x<-x[x>= min(xlist$breaks) & x<=max(xlist$breaks)]
    h<-hist(x,breaks=xlist$breaks,include.lowest = TRUE,right = FALSE,plot = FALSE)}
  # if classes have equal width one can choose what to display
  if(h$equidist){
    h$density<-switch(freq,Counts=h$counts,
                      Percentages= h$counts/sum(h$counts)*100,
                      Proportions=h$counts/sum(h$counts),
                      Densities=h$density)
  }
  # if classes width unequal h$density is not modified
  # and freq is set to Density
  if(!h$equidist){freq<-"Densities"}
  
  # plot:
  use.max.y<-pretty_max(h$density,freq)
  if(!adj.breaks){
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
  if(adj.breaks){
    if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
      mylas=1} else{mylas=0}}
  par(las=mylas)
  
  # if no adj required:
  if(!adj.breaks){
    plot(h,freq = FALSE,main=paste0("Histogram: ",name.x),
         xlab="",ylab="",ylim=c(0,use.max.y),col=use.color)  }
  if(adj.breaks){
    plot(h,freq = FALSE,main=paste0("Histogram: ",name.x),
         xlab="",ylab="",ylim=c(0,use.max.y),col=use.color,axes = FALSE)
    p.yaxp<-par("yaxp")
    aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
    axis(2, at=aty, labels=format(aty, scientific = FALSE))
    p.xaxp<-par("xaxp")
    atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
    use.labs<-format(atx, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(1, at=atx, labels=use.labs)
  }
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = pos.tit2,las=0)
}
plt.x.density<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  pos.tit2<-2.6
  
  use.color<-build.colors(n=1,bw,color,bw.default="black",
                          col.default="black")
  x<-xlist$V[!(is.na(xlist$V))]
  if(xlist$class=="interval"){
    check.int<-xlist$info.int$tab
    # create a synthetic vector by sampling from
    # a uniform distribution from one endpoint to another
    # as many cases as are those in each interval
    set.seed(100)
    x<-NULL
    for(k in 1:length(levels(xlist$V.f))){
      x<-c(x,runif(sum(!is.na(xlist$V.f) & xlist$V.f== levels(xlist$V.f)[k]),
                   check.int$Low[k],check.int$Up[k]))
    }
  } # close interval = TRUE
  if(xlist$class=="breaks"){
    #cat("\n   'breaks' ignored in density plots",file=stderr())
    x<-x[x>= min(xlist$breaks) & x<=max(xlist$breaks)]
  }
  d<-density(x)
  use.max.y<-pretty_max(d$y,freq)
  if(!adj.breaks){
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
  if(adj.breaks){
    if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
      mylas=1} else{mylas=0}}
  par(las=mylas)
  
  # if no adj required:
  if(!adj.breaks){
    plot(d$x,d$y,type="l",main=paste0("Density plot: ",name.x),
         xlab="",ylab="",ylim=c(0,use.max.y),col=use.color,lwd=2)  }
  if(adj.breaks){
    plot(d$x,d$y,type="l",main=paste0("Density plot: ",name.x),
         xlab="",ylab="",ylim=c(0,use.max.y),col=use.color,axes = FALSE,lwd=2)
    p.yaxp<-par("yaxp")
    aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
    axis(2, at=aty, labels=format(aty, scientific = FALSE))
    
    p.xaxp<-par("xaxp")
    atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
    use.labs<-format(atx, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(1, at=atx, labels=use.labs)
  }
  box()
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = pos.tit2,las=0)
}
plt.x.boxplot<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  pos.tit2<-2.6
  
  use.color<-build.colors(n=1,bw,color,bw.default="grey",
                          col.default="skyblue")
  x<-xlist$V
  if(max(nchar(as.character(max(x,na.rm = TRUE))))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  if(!adj.breaks){
    boxplot(x,main=paste0("Boxplot: ",name.x),
            xlab="",ylab="",col=use.color)  }
  if(adj.breaks){
    boxplot(x,main=paste0("Boxplot: ",name.x),
            xlab="",ylab="",col=use.color,axes = FALSE)  
    p.yaxp<-par("yaxp")
    aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
    use.labs<-format(aty, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(2, at=aty, labels=use.labs)
    box()
  }
  mtext(side = 2, name.x, line = pos.tit2,las=0)
}  

## Functions to obtain plots of two variables ------
plt.xy.crossbars<-function(tab,bw = TRUE,color=NULL,name.x,name.y,freq,legend,
                           beside = FALSE,use.tit=NULL,switch.xy = FALSE,
                           use.par=NULL){
  pos.tit2<-2.6
  use.color<-build.colors(n=nrow(tab),bw,color)
  names(use.color)<-rownames(tab)
  if(beside){
    bar.space=c(0,1)
    use.max.y<-pretty_max(tab,freq)}
  if(!beside){
    bar.space<-0.2
    use.max.y<-pretty_max(max(apply(tab,2,sum,na.rm = TRUE)),freq)
  }
  bar.width=1
  if(!switch.xy){
    tit.use<-paste0("Bars: ",use.tit)  
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    if(!legend){
      barplot(tab,col=use.color,main=tit.use,
              ylim=c(0,use.max.y),beside=beside)
      mtext(side = 1, name.x, line = 2)
      mtext(side = 2, freq, line = pos.tit2,las=0)
    }
    if(legend){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
      leg.text<-max((strwidth(paste0(rownames(tab),"aaA"),units="fig")))
      leg.tit<-(strwidth(name.y,units="fig"))
      length.leg<-max(leg.text,leg.tit)
      prop.leg<-min(length.leg,0.5)
      mypar<-use.par$mar
      mypar[4]<-0.2
      layout(matrix(c(1,2), nrow=1, byrow = TRUE),
             widths = c(1-prop.leg,prop.leg))
      par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      barplot(tab,col=use.color,main=tit.use,
              ylim=c(0,use.max.y),beside=beside)
      mtext(side = 1, name.x, line = 2)
      mtext(side = 2, freq, line = pos.tit2-0.2, las=0)
      
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=use.par$tck,tcl=use.par$tcl,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      plot(1:10,1:10,type="n",axes = FALSE)
      legend("topleft", legend=rownames(tab), 
             pch=21,pt.bg=use.color,pt.cex=1,bty="n",x.intersp = 0.5,
             cex=use.par$cex.axis,title=name.y,
             xjust=0,title.adj = 0.5,title.font=2)    
    }
  }
  if(switch.xy){
    tit.use<-paste0("Bars: ",use.tit)  
    if(!legend){
      mypar<-use.par$mar
      mypar[2]<-1+max(ceiling(strwidth(colnames(tab),units="inc")*5))
      par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      barplot(tab,col=use.color,main=tit.use,
              xlim=c(0,use.max.y),beside=beside,
              horiz = TRUE,cex.names=use.par$cex.axis)
      mtext(side = 1, freq, line = 2)
      mtext(side = 2, name.y, line = mypar[2]-1,las=0)
    }
    if(legend){
      mypar<-use.par$mar
      mypar[2]<-1+max(ceiling(strwidth(colnames(tab),units="inc")*5))
      mypar[4]<-0.2
      leg.text<-max((strwidth(paste0(rownames(tab),"aaA"),units="fig")))
      leg.tit<-(strwidth(name.x,units="fig"))
      length.leg<-max(leg.text,leg.tit)
      prop.leg<-min(length.leg,0.5)
      layout(matrix(c(1,2), nrow=1, byrow = TRUE),
             widths = c(1-prop.leg,prop.leg))
      par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      barplot(tab,col=use.color,main=tit.use,
              xlim=c(0,use.max.y),beside=beside,
              horiz = TRUE,cex.names=use.par$cex.axis)
      mtext(side = 1, freq, line = 2)
      mtext(side = 2, name.y, line = mypar[2]-1,las=0)
      
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=use.par$tck,tcl=use.par$tcl,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      plot(1:10,1:10,type="n",axes = FALSE)
      legend("topleft", legend=rownames(tab), 
             pch=21,pt.bg=use.color,pt.cex=1,bty="n",x.intersp = 0.5,
             cex=use.par$cex.axis,title=name.x,
             xjust=0,title.adj =0.5,title.font = 2)    
    }
  }
}
# Modified 202406
plt.xy.scatter<-function(xlist,ylist,bw = TRUE,color=NULL,
                         name.x,name.y,legend=FALSE,
                         adj.breaks = TRUE,fitline = FALSE,
                         use.par=NULL,type.print,msg.p,clist){
  pos.tit2<-2.6
  
  if(xlist$class=="interval" | xlist$class=="breaks"){
    xlist$isnum <- FALSE  }
  if(ylist$class=="interval" | ylist$class=="breaks"){
    ylist$isnum <- FALSE  }
  use.var<-FALSE
  if(isFALSE(missing(clist)) & !xlist$isnum & !ylist$isnum){
    if(msg.p$warn){
      my.p.list(list(paste0(name.x," and ",name.y," are both discrete",
                            "\n   "," -> Colouring cases by ",clist$name.v," would be meaningless")),
                type.print=type.print)}}
  if(isFALSE(missing(clist)) & (xlist$isnum  | ylist$isnum)){
    use.var<-TRUE
    check.howmany<-sum(!duplicated(data.frame(xlist$V,ylist$V)))
    check.howmany<-round(100*check.howmany/length(xlist$V),1)
    if(msg.p$warn){
      my.p.list(list(paste0("% of distinct combinations: ",check.howmany,"%",
                            "\n   "," -> A too low % makes colouring meaningless")),
                type.print=type.print) }
  }
  # colors fitline
  line.color<-"firebrick"
  if(!bw | !is.null(color)){line.color <- "black"} 
  # colors points in scatter
  if(!use.var){
    use.color<-build.colors(n=1,bw,color,bw.default="black",
                            col.default="skyblue")
    legend<-FALSE
  }
  if(use.var){
    if(clist$class=="interval"){clist$isnum <- FALSE  } 
    if(clist$class=="breaks" & !is.null(clist$V.f)){
      clist$isnum <- FALSE}
    use.gradient<-FALSE
    if(!clist$isnum){
      mycolor<-build.colors(n=length(levels(clist$V.f)),bw,color)
      use.color<-mycolor[as.numeric(clist$V.f)]
      tab<-mycolor
      names(tab)<-levels(clist$V.f)
    }
    if(clist$isnum){
      vv<-clist$V
      if(length(unique(vv))<=20){
        vv.r<-1:length(unique(vv))
        mycolor<-build.colors(n=length(unique(vv)),bw,color)
        tab<-mycolor
        names(tab)<-unique(vv)[order(unique(vv))]
        use.color<-mycolor[vv.r]
      } else {
        use.gradient<-TRUE
        gradient.var<-build.varcolors(vv,bw,color)
        use.color<-gradient.var$data$col
      }
    }
  } # closes use.var
  
  tit.use<-paste0("Scatter: ",name.x," , ",name.y)  
  
  # at least one of the two is numeric  
  if(!(!xlist$isnum & !ylist$isnum)){
    if(ylist$isnum){
      use.max.y<-pretty_max(ylist$V)
      if(!adj.breaks){
        if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
      if(adj.breaks){
        if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
          mylas=1} else{mylas=0}}
      mypar<-use.par$mar
      y.num<-ylist$V
    }
    if(xlist$isnum){x.num<-xlist$V}
    if(!xlist$isnum){x.num<-as.numeric(xlist$V.f)}
    if(!ylist$isnum){
      mypar<-use.par$mar
      mypar[2]<-1.5+max(ceiling(strwidth(levels(ylist$V.f),
                                         units="inc")*5))
      y.num<-as.numeric(ylist$V.f)
      mylas=1
    }
    
    # no legend
    if(!legend){
      
      par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      
      
      plot(x=x.num,y=y.num,main=tit.use,axes=FALSE,
           xlab="",ylab="",pch=21,bg=use.color)
      if(fitline){
        abline(lm(y.num ~ x.num),col=line.color,lwd=2)}
      if(!ylist$isnum){
        axis(2, at=1:length(levels(ylist$V.f)), 
             labels=levels(ylist$V.f),las=1)}
      if(!xlist$isnum){
        axis(1,at=1:length(levels(xlist$V.f)),
             labels=levels(xlist$V.f))
      }
      if(!adj.breaks){
        if(xlist$isnum){axis(1)}
        if(ylist$isnum){axis(2, las=mylas)}
      } # closes !adj breaks
      if(adj.breaks){
        if(ylist$isnum){
          p.yaxp<-par("yaxp")
          aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
          use.labs<-format(aty, scientific = FALSE)
          use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
          axis(2, at=aty, labels=use.labs)}
        if(xlist$isnum){
          p.xaxp<-par("xaxp")
          atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
          use.labs<-format(atx, scientific = FALSE)
          use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
          axis(1, at=atx, labels=use.labs)
        }
      }# closes adj breaks
      box()
      mtext(side = 1, name.x, line = 2)
      if(ylist$isnum){
        mtext(side = 2, name.y, line = pos.tit2,las=0)
      }
      if(!ylist$isnum){
        mtext(side = 2, name.y, line = mypar[2]-1,las=0)
      }
    } # closes !legend
    
    # legend
    if(legend){
      
      if(!use.gradient){
        leg.text<-max((strwidth(paste0(names(tab),"aaA"),
                                units="fig")))}
      if(use.gradient){
        leg.text<-max((strwidth(paste0(gradient.var$legend$label.legend,"aaA"),
                                units="fig")))  }
      leg.tit<-(strwidth(clist$name.v,units="fig"))
      length.leg<-max(leg.text,leg.tit)
      prop.leg<-min(length.leg,0.5)
      mypar[4]<-0.2
      layout(matrix(c(1,2), nrow=1, byrow = TRUE),
             widths = c(1-prop.leg,prop.leg))
      par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis)
      
      
      plot(x=x.num,y=y.num,main=tit.use,axes=FALSE,
           xlab="",ylab="",pch=21,bg=use.color)
      if(fitline){
        abline(lm(y.num ~ x.num),col=line.color,lwd=2)}
      if(!ylist$isnum){
        axis(2, at=1:length(levels(ylist$V.f)), 
             labels=levels(ylist$V.f),las=1)}
      if(!xlist$isnum){
        axis(1,at=1:length(levels(xlist$V.f)),
             labels=levels(xlist$V.f))}
      
      if(!adj.breaks){
        if(xlist$isnum){axis(1)}
        if(ylist$isnum){axis(2, las=mylas)}
      } # closes !adj breaks
      if(adj.breaks){
        if(ylist$isnum){
          p.yaxp<-par("yaxp")
          aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
          use.labs<-format(aty, scientific = FALSE)
          use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
          axis(2, at=aty, labels=use.labs)}
        if(xlist$isnum){
          p.xaxp<-par("xaxp")
          atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
          use.labs<-format(atx, scientific = FALSE)
          use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
          axis(1, at=atx, labels=use.labs)
        }
      }# closes adj breaks
      box()
      mtext(side = 1, name.x, line = 2)
      if(ylist$isnum){
        mtext(side = 2, name.y, line = pos.tit2,las=0)
      }
      if(!ylist$isnum){
        mtext(side = 2, name.y, line = mypar[2]-1,las=0)
      }
      
      # PLACE THE LEGEND
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=use.par$tck,tcl=use.par$tcl,
          mgp=use.par$mgp,cex=use.par$cex,
          cex.axis=use.par$cex.axis) 
      if(!use.gradient){ 
        plot(1:10,1:10,type="n",axes = FALSE,)
        legend("topleft", legend=names(tab), 
               pch=21,pt.bg=tab,pt.cex=1,bty="n",
               x.intersp = 0.5,
               cex=use.par$cex.axis,title=clist$name.v,
               xjust=0,title.adj =0.5,title.font = 2)    
      }
      if(use.gradient){
        plot(1:10,1:10,type="n",axes = FALSE)
        legend("topleft",pch=22, pt.cex=1.5,bty="n",
               legend=c("",gradient.var$legend$label.legend), 
               pt.bg=c("white",gradient.var$legend$col),
               col=c("white",gradient.var$legend$border),
               x.intersp = 0.5,y.intersp=0.8,
               cex=use.par$cex.axis,title=clist$name.v,
               xjust=0,title.adj =0.5,title.font = 2)    
      }
    } # closes legend
  }
  
  # x not num and y not num 
  if(!xlist$isnum & !ylist$isnum){
    tab<-data.frame(prop.table(table((xlist$V.f),ylist$V.f)))
    tab<-tab[tab$Freq>0,]
    tab$FreqP<-(tab$Freq)/(max(tab$Freq))
    tab$xnum<-as.numeric(tab$Var1)
    tab$ynum<-as.numeric(tab$Var2)
    
    mypar<-use.par$mar
    mypar[2]<-1.5+max(ceiling(strwidth(levels(ylist$V.f),units="inc")*5))
    par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
        mgp=use.par$mgp,cex=use.par$cex,
        cex.axis=use.par$cex.axis) 
    
    plot(x=tab$xnum,y=tab$ynum,xlim=c(0.5,max(tab$xnum)+0.5),
         ylim=c(0.5,max(tab$ynum)+0.5),main=tit.use,
         xlab="",ylab="",cex=(0.5+9.5*tab$FreqP),pch=21,
         bg=use.color,axes = FALSE)
    axis(1, at=1:length(levels(tab$Var1)), 
         labels=levels(tab$Var1))
    axis(2, at=1:length(levels(tab$Var2)), 
         labels=levels(tab$Var2),las=1)
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  
}
plt.xy.boxplot<-function(xlist,ylist,bw = TRUE,color=NULL,name.x,
                         name.y,adj.breaks = TRUE,switch.xy = FALSE,
                         use.par=NULL){
  pos.tit2<-2.6
  
  use.color<-build.colors(n=1,bw,color,bw.default="grey",
                          col.default="skyblue")
  if(xlist$class=="interval" | xlist$class=="breaks"){
    xlist$isnum <- FALSE  }
  if(ylist$class=="interval" | ylist$class=="breaks"){
    ylist$isnum <- FALSE  }
  
  # two numeric variables
  if(xlist$isnum & ylist$isnum & !switch.xy){
    use.max.y<-pretty_max(ylist$V)
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis)
    
    if(!adj.breaks){
      lev.x<-unique(xlist$V)
      lev.x<-lev.x[order(lev.x)]
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",col=use.color,at=lev.x)    }
    if(adj.breaks){
      lev.x<-unique(xlist$V)
      lev.x<-lev.x[order(lev.x)]
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",at=lev.x,axes = FALSE,col=use.color)    
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = pos.tit2,las=0)
  }
  if(xlist$isnum & ylist$isnum & switch.xy){
    use.max.y<-pretty_max(ylist$V)
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    if(!adj.breaks){
      lev.y<-unique(ylist$V)
      lev.y<-lev.y[order(lev.y)]
      boxplot(xlist$V~ylist$V,main=paste0(name.x," | ",name.y),
              xlab="",ylab="",col=use.color,horizontal = TRUE,at=lev.y)    }
    if(adj.breaks){
      lev.y<-unique(ylist$V)
      lev.y<-lev.y[order(lev.y)]
      boxplot(xlist$V~ylist$V,main=paste0(name.x," | ",name.y),
              xlab="",ylab="",axes = FALSE,col=use.color,horizontal = TRUE,
              at=lev.y)    
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = pos.tit2,las=0)
  }
  if(xlist$isnum & !ylist$isnum){
    y.num<-as.numeric(ylist$V.f)
    mypar<-use.par$mar
    mypar[2]<-1.5+max(ceiling(strwidth(levels(ylist$V.f),units="inc")*5))
    par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    boxplot(xlist$V~y.num,main=paste0(name.x," | ",name.y),
            xlab="",ylab="",horizontal = TRUE,axes = FALSE,col=use.color)    
    axis(2, at=1:length(levels(ylist$V.f)), labels=levels(ylist$V.f),
         cex.axis=use.par$cex.axis,las=1)
    if(!adj.breaks){axis(1) }
    if(adj.breaks){
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  if(!xlist$isnum & ylist$isnum){
    x.num<-as.numeric(xlist$V.f)
    use.max.y<-pretty_max(ylist$V)
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    boxplot(ylist$V~x.num,main=paste0(name.y," | ",name.x),
            xlab="",ylab="",axes = FALSE,col=use.color)    
    axis(1, at=1:length(levels(xlist$V.f)), labels=levels(xlist$V.f))
    if(!adj.breaks){axis(2, las=mylas)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = pos.tit2,las=0)
  }
}

# Functions to create and plot summaries  ------
# Function to obtain summaries by (modified 202406)
build.summaries<-function(x,ListBy,
                               stats,digits=2,f.digits=4,
                          force.digits=FALSE,use.scientific=FALSE){
  # create the by list
  list.by<-list()
  if(length(ListBy)>=1){
    for(k in 1:length(ListBy)){
      if(ListBy[[k]]$Bylist$class=="standard"){
        list.by[[ListBy[[k]]$name]]<-ListBy[[k]]$by
      }
      if(ListBy[[k]]$Bylist$class=="breaks"){
        list.by[[ListBy[[k]]$name]]<-ListBy[[k]]$Bylist$V.f
      }
      if(ListBy[[k]]$Bylist$class=="interval"){
        list.by[[ListBy[[k]]$name]]<-ListBy[[k]]$Bylist$V.f
      }
    }
  }
  if(length(list.by)==0){list.by$toberemoved<-rep(1,length(x))}
  
  # build the function to calculate stats
  pip<-function(x,stats,digits=digits,f.digits=f.digits){
    use.f <- FALSE
    if(is.factor(x)){use.f <- TRUE}
    x.na<-sum(is.na(x));  x<-na.omit(x)
    cs<-c("n"=length(x),"n.a"=x.na)
    
    # functions for modes and their properties
    f.mode <- function(x,use.f){
      tab.x<-tabulate(match(x, unique(x)))
      mode.s<-(unique(x)[which.max(tab.x)])
      if(use.f){mode.s<-as.character(mode.s)}
      return(mode.s)}
    f.n_mode <- function(x,use.f){
      tab.x<-tabulate(match(x, unique(x)))
      n.m<-sum(tab.x==max(tab.x))
      return(n.m)}
    f.p_mode <- function(x,use.f,f.digits=f.digits){
      p.m<-max(prop.table(tabulate(match(x, unique(x)))))
      p.m<-round(p.m,f.digits)
      return(p.m)}
    # functions for quartiles, adjusted for factors
    f.q<-function(x,f = FALSE,p){
      if(!f){qq<-quantile(x,p)} else {qq<-as.character(quantile(x,type=3,p))}
      return(qq)}
    
    cso<-NULL
    for(k in tolower(stats)){
      if(k=="mode"){cso<-c(cso,f.mode(x,use.f))}
      if(k=="n.modes"){cso<-c(cso,f.n_mode(x,use.f))}
      if(k=="mode%"){cso<-c(cso,f.p_mode(x,use.f,f.digits=f.digits))}
      if(k=="min"){cso<-c(cso,f.q(x,f=use.f,p=0))}
      if(k=="median"){cso<-c(cso,f.q(x,f=use.f,p=0.5))}
      if(k=="q1"){cso<-c(cso,f.q(x,f=use.f,p=0.25))}
      if(k=="q2"){cso<-c(cso,f.q(x,f=use.f,p=0.5))}
      if(k=="q3"){cso<-c(cso,f.q(x,f=use.f,p=0.75))}
      if(k=="mean"){cso<-c(cso,mean(x))}
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
                 FUN=function(x) pip(x,stats,digits=100,
                                     f.digits=100))
  out<-data.frame(out[,1:length(list.by)],out$x,check.names = FALSE)
  names(out)[1:length(list.by)]<-names(list.by)
  for(k in c("n","n.a","n.modes")){
    if(k %in% colnames(out)){
      out[,k]<-round(as.numeric(out[,k]),0)}  }
  for(k in c("mode%")){
    if(k %in% colnames(out)){
      out[,k]<-as.numeric(out[,k])
    }
  }
  if(is.numeric(x) | is.logical(x)){
    for(k in stats[!(stats %in% c("n","n.a","n.modes","mode%"))]){
      if(k %in% colnames(out)){
        out[,k]<-as.numeric(out[,k])
      } 
    }
  }
  out$toberemoved<-NULL
  
  # Create version to print
  out.print<-out
  for(k in c("mode%")){
    if(k %in% colnames(out)){
      out.print[,k]<-round(out[,k],f.digits)
      min.out<-round(min(out[,k],na.rm=T),f.digits)
      if(min.out==0 & !force.digits){
        out.print[,k]<-format(out[,k],digits=1,
                              scientific=use.scientific)
      }
    }
  }
  if(is.numeric(x) | is.logical(x)){
    for(k in stats[!(stats %in% c("n","n.a","n.modes","mode%"))]){
      if(k %in% colnames(out)){
        out.print[,k]<-round(out[,k],digits)
        min.out<-round(min(abs(out[,k]),na.rm=T),digits)
        if(min.out==0 & !force.digits){
          out.print[,k]<-format(abs(out[,k]),digits=1,
                                scientific=use.scientific)
          is.neg<-!is.na(out[,k]) & out[,k]<0
          if(sum(is.neg>0)){
            out.print[is.neg,k]<-paste0("-",out.print[is.neg,k])
          }
        }  
      }
    }
  }
  myout<-list(tab=out,tab.print=out.print,ListBy=ListBy,list.by=list.by)
}


# Function to build plots of summaries (added 202406)
#' @importFrom stats qt
build.summary.plt<-function(out,name.x,
                            list.stats,list.tit,
                            ListBy,
                            stats,plot.type,
                            bw = FALSE,color=NULL,
                            legend = TRUE,
                            conf.level=0.95,
                            adj.breaks=TRUE,msg.p,
                            type.print){
  Warn.list<-as.list("\nWarning:") 
  pardef <- par(no.readonly = TRUE)
  on.exit(par(pardef))
  list.by<-ListBy
  
  par(mar=c(3.5,3.7,3,2.1),tck=(-0.01),tcl=NA,las=1,
      mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  par.my<-list(mar=c(3.5,3.7,3,2.1),tck=(-0.01),tcl=NA,las=1,
               mgp=c(3, 0.3, 0),cex=0.88,cex.axis=0.88)
  pos.tit2<-2.6
  if(stats %in% c("mean","median") & length(list.by)==1){
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))
    mylas<-1
    use.max.y<-max(round(out[[stats]],0))
    use.max.y<-max(pretty(out[[stats]]))
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){mylas=1} else{mylas=0}}
    par(las=mylas)
    if(plot.type=="bars"){
      use.color<-build.colors(n=1,bw,color,bw.default="grey",
                              col.default="skyblue")
      if(is.numeric(out[[names(list.by)]])){
        Warn.list<-c(Warn.list,"With 'bars' the by-var is treated as a factor")}
      if(!adj.breaks){
        barplot(out[[stats]],names.arg=out[[names(list.by)]],
                col=use.color,main=tit.stats)}
      if(adj.breaks){
        barplot(out[[stats]],names.arg=out[[names(list.by)]],
                col=use.color,main=tit.stats,axes=FALSE)
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        use.labs<-format(aty, scientific = FALSE)
        axis(2, at=aty, labels=use.labs)
      }
      box()
    }
    if(plot.type=="lines" | plot.type=="points"){
      use.color<-build.colors(n=1,bw,color,bw.default="black",
                              col.default="black")
      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],
                    levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}
      
      plot(y=out[[stats]],x=x.n,col=use.color,
           main=tit.stats,
           axes = FALSE,xlab="",ylab="",lwd=2)
      points(y=out[[stats]],x=x.n,pch=16,col=use.color)
      
      if(plot.type=="lines"){
        lines(y=out[[stats]],x=x.n,col=use.color,lwd=2)
      }
      box()
      if(!adj.breaks){
        axis(2)
        axis(1,at=x.n,labels=x.c)}
      if(adj.breaks){
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        use.labs<-format(aty, scientific = FALSE)
        axis(2, at=aty, labels=use.labs)
        use.labs<-format(x.c,scientific=FALSE)
        axis(1,at=x.n,labels=use.labs)
      }
    }
    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = pos.tit2,las=0)
    
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      cat("\n")}
    par(pardef)
    return(out)
  }
  if(stats %in% c("mean","median") & length(list.by)==2){
    tit.stats<-paste0(name.x," | ",
                      names(list.by)[1]," by ",names(list.by)[2])
    use.by2<-unique(out[[names(list.by)[2]]])
    use.color<-build.colors(n=length(use.by2),bw,color)
    names(use.color)<-use.by2
    
    if(is.numeric(out[[names(list.by)[1]]])){
      x.c<-x.n<-out[[names(list.by)[1]]] }
    if(!is.numeric(out[[names(list.by)[1]]])){
      lev.use<-out[duplicated(out[[names(list.by)[1]]])==FALSE,]
      x.c<-factor(out[[names(list.by)[1]]],
                  levels=lev.use[[names(list.by)[1]]])
      x.n<-as.numeric(x.c)}
    
    mymin.y<-min(out[[stats]],na.rm = TRUE);  
    mymax.y<-max(out[[stats]],na.rm = TRUE)
    mymin.x<-min(x.n,na.rm = TRUE); 
    mymax.x<-max(x.n,na.rm = TRUE)
    
    use.max.y<-max(pretty(mymax.y))
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){mylas=1} else{mylas=0}}
    par(las=mylas)
    
    if(legend){
      by2.legend<-use.by2
      col.legend<-use.color
      leg.text<-max((strwidth(paste0(use.by2,"aaA"),units="fig")))
      leg.tit<-(strwidth(names(list.by)[2],units="fig"))
      length.leg<-max(leg.text,leg.tit)
      prop.leg<-min(length.leg,0.5)
      mypar<-par.my$mar
      mypar[4]<-0.2
      layout(matrix(c(1,2), nrow=1, byrow = TRUE),
             widths = c(1-prop.leg,prop.leg))
      par(mar=mypar,tck=par.my$tck,tcl=par.my$tcl,las=mylas,
          mgp=par.my$mgp,cex=par.my$cex,cex.axis=par.my$cex.axis)
    }
    plot(c(mymin.x,mymax.x),c(mymin.y,mymax.y),axes = FALSE,
         type="n",xlim=c(mymin.x,mymax.x),ylab="",xlab="",
         main=tit.stats)
    for(k in 1:length(use.by2)){
      sel<-out[[names(list.by)[2]]]==use.by2[k]
      points(x.n[sel],out[[stats]][sel],type="l",col=use.color[k],lwd=2)
      points(x.n[sel],out[[stats]][sel],pch=16,col=use.color[k])}
    box()
    
    if(!adj.breaks){
      axis(2)
      axis(1,at=x.n,labels=x.c)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      use.labs<-format(aty, scientific = FALSE)
      axis(2, at=aty, labels=use.labs)
      use.labs<-format(x.c,scientific=FALSE)
      axis(1,at=x.n,labels=use.labs)
    }
    mtext(side = 1, names(list.by)[1], line = 2)
    mtext(side = 2, list.tit, line = pos.tit2,las=0)
    
    if(legend){
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=par.my$tck,tcl=par.my$tcl,
          mgp=par.my$mgp,cex=par.my$cex,
          cex.axis=par.my$cex.axis) 
      plot(1:10,1:10,type="n",axes = FALSE,)
      legend("topleft", legend=by2.legend, 
             pch=21,pt.bg=col.legend,pt.cex=1,bty="n",
             x.intersp = 0.5,
             cex=par.my$cex.axis,title=names(list.by)[2],
             xjust=0,title.adj =0.5,title.font = 2)    
    }
    
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      cat("\n")}
    par(pardef)
    return(out)
  }
  
  if(stats %in% c("ci.mean") & length(list.by)==1){
    myout<-out
    t.q<-0
    t.q[out$n>1]<-stats::qt((0.5+conf.level/2),df=(out$n[out$n>1] -1))
    myout[,paste0("low.",conf.level*100)]<-out$mean-t.q*(out$sd/sqrt(out$n))
    myout[,paste0("up.",conf.level*100)]<-out$mean+t.q*(out$sd/sqrt(out$n))
    out$marg.err<-t.q*(out$sd/sqrt(out$n))
    out$marg.err[is.na(out$marg.err)]<-0
    sel<-out$marg.err>0
    
    if(!any(sel)){
      Warn.list<-c(Warn.list,"CI could not be calculated for by-groups with one case only")}
    
    mymax<-max(out$mean+out$marg.err)
    use.max.y<-max(pretty(mymax))
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){mylas=1} else{mylas=0}}
    par(las=mylas)
    
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))
    
    if(plot.type=="bars"){
      use.color<-build.colors(n=1,bw,color,bw.default="grey",
                              col.default="skyblue")
      if(is.numeric(out[[names(list.by)]])){
        Warn.list<-c(Warn.list,"With 'bars' the by-var is treated as a factor")}
      
      mymin=min(0,min(out$mean-out$marg.err))
      orig.plt<-barplot(out$mean,names.arg=out[[names(list.by)]],
                        col=use.color,main=tit.stats,
                        ylim=c(mymin,mymax),plot = FALSE)
      
      if(!adj.breaks){
        barplot(out$mean,names.arg=out[[names(list.by)]],
                col=use.color,main=tit.stats,ylim=c(mymin,mymax))
      }
      if(adj.breaks){
        barplot(out$mean,names.arg=out[[names(list.by)]],
                col=use.color,main=tit.stats,ylim=c(mymin,mymax),
                axes=FALSE)
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        use.labs<-format(aty, scientific = FALSE)
        use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
        axis(2, at=aty, labels=use.labs)
      }
      points(y=out$mean[sel],x=orig.plt[sel],pch=16,col="black")
      arrows(x0=orig.plt[sel], y0=(out$mean[sel]-out$marg.err[sel]),
             x1=orig.plt[sel], y1=(out$mean[sel]+out$marg.err[sel]),
             code=3, angle=90, col="black",lwd=1.5,
             length=min(0.1,(0.05*15/length(out$mean))))
      if(min(out$mean,na.rm = TRUE)<0){
        segments(x0=0,y0=0,x1=max(orig.plt),y1=0)}
      box()
    }
    if(plot.type=="lines" | plot.type=="points"){
      use.color<-build.colors(n=1,bw,color,bw.default="black",
                              col.default="black")
      if(is.numeric(out[[names(list.by)]])){
        x.c<-x.n<-out[[names(list.by)]] }
      if(!is.numeric(out[[names(list.by)]])){
        x.c<-factor(out[[names(list.by)]],levels=out[[names(list.by)]])
        x.n<-as.numeric(x.c)}
      mymin=min(out$mean-out$marg.err,na.rm = TRUE)
      
      plot(y=out$mean,x=x.n,type="n",col=use.color,main=tit.stats,
           axes = FALSE,xlab="",ylab="",lwd=2,ylim=c(mymin,mymax))
      points(y=out$mean[sel],x=x.n[sel],pch=16,col=use.color)
      points(y=out$mean[!sel],x=x.n[!sel],pch=8,col=use.color)
      if(plot.type=="lines"){
        lines(y=out$mean,x=x.n,col=use.color,lwd=2)
      }
      arrows(x0=x.n[sel], y0=(out$mean[sel]-out$marg.err[sel]),
             x1=x.n[sel], y1=(out$mean[sel]+out$marg.err[sel]),
             code=3, angle=90, col="black",lwd=1.5,
             length=min(0.1,(0.05*15/length(out$mean))))
      box()
      
      if(!adj.breaks){
        axis(2)
        axis(1,at=x.n,labels=x.c)}
      if(adj.breaks){
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        use.labs<-format(aty, scientific = FALSE)
        axis(2, at=aty, labels=use.labs)
        use.labs<-format(x.c,scientific=FALSE)
        axis(1,at=x.n,labels=use.labs)
      }
    }
    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = pos.tit2,las=0)
    
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      cat("\n")}
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
    use.max.y<-max(pretty(mymax.y))
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){mylas=1} else{mylas=0}}
    par(las=mylas)
    
    tit.stats<-paste0(list.tit," of ",name.x," | ",names(list.by))
    use.color<-build.colors(n=length(pctiles),bw,color,
                            bw.default="grey",
                            col.default="rgb")
    names(use.color)[1:length(pctiles)]<-pctiles
    if(legend){
      pct.legend<-pctiles
      col.legend<-use.color
      if(stats=="percentiles"){
        pctles.sel<-pctiles %in% c("min",paste0("p",seq(10,90,by=10)),"max")
        pct.legend<-pctiles[pctles.sel]
        col.legend<-use.color[pctles.sel]    }
      
      leg.text<-max((strwidth(paste0(pct.legend,"aaA"),units="fig")))
      leg.tit<-(strwidth("Quantiles",units="fig"))
      length.leg<-max(leg.text,leg.tit)
      prop.leg<-min(length.leg,0.5)
      mypar<-par.my$mar
      mypar[4]<-0.2
      layout(matrix(c(1,2), nrow=1, byrow = TRUE),
             widths = c(1-prop.leg,prop.leg))
      par(mar=mypar,tck=par.my$tck,tcl=par.my$tcl,las=mylas,
          mgp=par.my$mgp,cex=par.my$cex,cex.axis=par.my$cex.axis)
    }
    
    plot(c(mymin.x,mymax.x),c(mymin.y,mymax.y),axes = FALSE,
         type="n",xlim=c(mymin.x,mymax.x),ylab="",xlab="",
         main=tit.stats)
    for(k in pctiles){
      points(x.n,out[[k]],type="l",col=use.color[k],lwd=2)
      points(x.n,out[[k]],pch=16,col=use.color[k])
    }
    box()
    if(!adj.breaks){
      axis(2)
      axis(1,at=x.n,labels=x.c)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      use.labs<-format(aty, scientific = FALSE)
      axis(2, at=aty, labels=use.labs)
      use.labs<-format(x.c,scientific=FALSE)
      axis(1,at=x.n,labels=use.labs)
    }
    mtext(side = 1, names(list.by), line = 2)
    mtext(side = 2, list.tit, line = pos.tit2,las=0)
    if(legend){
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=par.my$tck,tcl=par.my$tcl,
          mgp=par.my$mgp,cex=par.my$cex,
          cex.axis=par.my$cex.axis) 
      plot(1:10,1:10,type="n",axes = FALSE,)
      legend("topleft", legend=pct.legend, 
             pch=21,pt.bg=col.legend,pt.cex=1,bty="n",
             x.intersp = 0.5,
             cex=par.my$cex.axis,title="Quantiles",
             xjust=0,title.adj =0.5,title.font = 2)    
    }
    
    if(msg.p$warn){
      if(length(Warn.list)>1){
        my.p.list(Warn.list[!duplicated(Warn.list)],
                  type.print=type.print)  }
      cat("\n")}
    par(pardef)
    return(out)
  } # closes quantiles
}

# Functions specific for inference ------
## General utilities -----

# function to work with rmarkdown - not used in 
#   standard output (added 202406)
.do_gen_symbols<-function(type.print="cat"){
  if(type.print=="cat"){
    USE.MU<-"\u03BC"
    USE.SIGMA<-"\u03C3"
    USE.GE<-"\u2265"
    USE.GT<-">"
    USE.LE<-"\u2264"
    USE.LT<-"<"
    USE.NEQ<-"\u2260"
    USE.EQ<-"="
    USE.MINUS<-"-"
  }
  if(type.print=="print"){
    USE.MU<-"MY_MU.RP"
    USE.SIGMA<-"MY_SIGMA.RP"
    USE.GE<-"MY_GE.RP"
    USE.GT<-"MY_GT.RP"
    USE.LE<-"MY_LE.RP"
    USE.LT<-"MY_LT.RP"
    USE.NEQ<-"MY_NE.RP"
    USE.EQ<-"MY_EQ.RP"
    USE.MINUS<-"MY_MINUS.RP"
  }
  list(mu=USE.MU,sigma=USE.SIGMA,ge=USE.GE,le=USE.LE,lt=USE.LT,gt=USE.GT,
       neq=USE.NEQ,eq=USE.EQ,minus=USE.MINUS)
}
#   function to format printout 
ci.print<-function(out,digits,force.digits=FALSE,use.scientific=FALSE){
  op.sci<-getOption("scipen")
  out.print<-out
  out.print<-round(out,digits)
  if(!force.digits){
    for(k in colnames(out)){
      use.values<-out[,k]
      min.out<-round(min(abs(out[,k]),na.rm=T),digits)
      if(min.out==0){
        out.print[,k]<-format(abs(out[,k]),digits=1,
                              scientific=use.scientific)
        is.neg<-!is.na(out[,k]) & out[,k]<0
        if(sum(is.neg>0)){
          out.print[is.neg,k]<-paste0("-",out.print[is.neg,k])
        }
      }
      if(is.numeric(out.print[,k])){
        out.print[,k]<-as.character(out.print[,k])
      }
    }
  }
  if(!use.scientific){options(scipen=10)}
  out.print.p<-as.matrix(out.print)
  out.print.p[is.na(out)]<-""
  output<-out.print.p
}


## Functions to test parameters specifications -----
chkpar.conf<-function(value,err.list=NULL){
  if(!is.list(err.list)){err.list<-list()}
  ok.value<-(complete.cases(value))
  if(sum(ok.value)==0){
    err.list<-c(err.list,paste0("All elements of 'conf.level' are missing!"))
  }  else {
    if(class(value)[1] != "numeric"){
      err.list<-c(err.list,paste0("'conf.level' cannot be a ",class(value)[1]))
    } else {
      value<-value[ok.value]
      if(length(value)>1){
        err.list<-c(err.list,paste0("'conf.level' should be a single numeric value, and not a vector"))
      } else if(length(value)==1){
        if(value >= 1L | value <= 0L){
          err.list<-c(err.list,"'conf.level' should be a number between 0 and 1")}
      }
    }
  }
  out<-err.list
}

chkpar.sigma<-function(value,err.list=NULL,onlyone = TRUE){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.null(value)){
    what<-deparse1(substitute(value))
    ok.value<-(complete.cases(value))
    if(sum(ok.value)==0){
      err.list<-c(err.list,paste0("All the elements of '",what,"' are missing!"))
    }  else {
      if(class(value)[1] != "numeric"){
        err.list<-c(err.list,paste0("'",what,"' cannot be a ",class(value)[1]))
      } else {
        value<-value[ok.value]
        if(onlyone){
          if(length(value)>1){ 
            err.list<-c(err.list,paste0("'",what,"' should be a single numeric value, and not a vector"))
          } else {
            if(value<=0){err.list<-c(err.list,paste0("'",what,"' must be greater than 0!"))}
          }
        }
        if(!onlyone){
          if(length(value)>2){ 
            err.list<-c(err.list,paste0("'",what,"' cannot have more than 2 values"))
          } else{
            if(any(value<=0)){
              err.list<-c(err.list,paste0("All the values in '",what,"' must be greater than 0!"))}
          }
        }
      }
    }
  }
  out<-err.list
}

chkpar.success<-function(value,x,err.list=NULL){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.null(value)){
    what.x<-(deparse1(substitute(x)))
    what.s<-(deparse1(substitute(value)))
    ok.value<-(complete.cases(value))
    if(sum(ok.value)==0){
      err.list<-c(err.list,paste0("All the elements of '",what.s,"' are missing!"))
    }  else {
      if(!(class(value)[1] %in% c("numeric","character","logical"))){
        err.list<-c(err.list,paste0("'",what.s,"' cannot be a ",class(value)[1]))
      } else {
        value<-value[ok.value]
        if(length(value)>1){
          err.list<-c(err.list,paste0("'",what.s,"' must be a single value, and not a vector"))
        } else if(length(value)==1){
          if(!is.null(x) && !(value %in% unique(x))){
            err.list<-c(err.list,paste0("'",what.s,"' must be one of the values taken by '",
                                        what.x,"'"))
          }
        }
      }
    }
  }
  out<-err.list
  return(out)
}

chkcon.diff<-function(type,type.s=NULL,err.list=NULL,warn.list=NULL,x=NULL,
                      y,by,data,name.y,name.by,name.data,sigma.d=NULL,
                      sigma.x=NULL,sigma.y=NULL,sigma.by=NULL){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  if(type=="prop"){type.s<-"independent"}
  exist.y<-exist.by <- FALSE
  vec.y<-vec.by<-NULL
  if(isTRUE(missing(y)) && isTRUE(missing(by))){
    if(type.s=="independent" | type.s=="none"){
      err.list<-c(err.list,"Arguments 'y' or 'by' must be specified")
    } else if(type.s=="paired"){
      err.list<-c(err.list,"Argument 'y' must be specified")
    }
  } else if(isFALSE(missing(y)) & isFALSE(missing(by))){
    err.list<-c(err.list,"Either 'y' or 'by' should be specified, but not both")
  } else if(isFALSE(missing(y)) & isTRUE(missing(by))){
    if(type=="mean"){
      check.y<-chk.data(y,data,name.data,name.y,
                        num = TRUE,missing = TRUE,err.list=err.list)}
    if(type=="prop"){
      check.y<-chk.data(y,data,name.data,name.y,
                        missing = TRUE,err.list=err.list)}
    err.list<-check.y$err.list
    exist.y<-check.y$exist.x; 
    vec.y<-check.y$vec.x
  } else if(isTRUE(missing(y)) & isFALSE(missing(by))){
    if(type.s=="paired"){
      err.list<-c(err.list,paste0("'by' not allowed for paired samples",
                                  "\n   ","   -> Build two row-matching vectors and use 'x' and 'y'"))
    }
    if(type.s=="independent"){
      check.by<-chk.data(by,data,name.data,name.by,
                         missing = TRUE,err.list=err.list)
      err.list<-check.by$err.list
      exist.by<-check.by$exist.x
      vec.by<-check.by$vec.x
    }
  }
  if(exist.by){
    n.by<-length(na.omit(unique(vec.by)))
    if(n.by>2){
      err.list<-c(err.list,"'by' should be a vector with 2 levels only!")
      exist.by <- FALSE} 
    if(!is.null(x) && length(x) != length(vec.by)){
      err.list<-c(err.list,"'x' and 'by' should have the same length")
      exist.by <- FALSE}
  }
  if(exist.y && !is.null(x)){
    if(type.s=="paired" & length(x) != length(vec.y)){
      err.list<-c(err.list,"'x' and 'y' should have the same length when samples are paired")
      exist.y <- FALSE}
    if(type.s=="paired" && length(x) == length(vec.y) && 
       sum(!(is.na(x)) & !(is.na(vec.y)))<2){
      err.list<-c(err.list,"Cases with non missing values in paired samples should be at least 2")
    }
  }
  
  # Check variances
  if(type=="mean"){
    if(type.s=="paired"){
      if(is.null(sigma.d) & length(c(sigma.by,sigma.x,sigma.y))>0){
        err.list<-c(err.list,("For paired samples, use 'sigma.d' to specify known variance"))
      }
      if(!is.null(sigma.d) & length(c(sigma.by,sigma.x,sigma.y))>0){
        err.list<-c(err.list,"Variances both for paired and independent types specified!")
      }
    }
    if(type.s=="independent"){
      if(!is.null(sigma.d) & length(c(sigma.by,sigma.x,sigma.y))==0){
        err.list<-c(err.list,"For independent samples, use 'sigma.x', 'sigma.y', or 'sigma.by' to specify known variances")
      }
      if(!is.null(sigma.d) & length(c(sigma.by,sigma.x,sigma.y))>0){
        err.list<-c(err.list,"Variances both for paired and independent types specified!")
      }
      if(is.null(sigma.d) & length(c(sigma.y,sigma.x))>0 & !is.null(sigma.by)){
        err.list<-c(err.list,"Variances both for 'y' and for 'by' specified!")
      }
      if(exist.by && is.null(sigma.d) &&
         is.null(sigma.by) && (!is.null(sigma.x) | !is.null(sigma.y))){
        err.list<-c(err.list,"With 'by', use 'sigma.by' to specify known variances")
      }
      if(exist.by && length(c(sigma.d,sigma.x,sigma.y))==0 && 
         !is.null(sigma.by) && length(sigma.by)==2 && 
         sum(names(sigma.by) %in% as.character(unique(na.omit(vec.by))))<2){
        err.list<-c(err.list,"Values in 'sigma.by' should have as names the two values of 'by'")
      }
      if(exist.y && is.null(sigma.d) && !is.null(sigma.by) &&
         is.null(sigma.x) & is.null(sigma.y)){
        err.list<-c(err.list,"With 'y', use 'sigma.x' and 'sigma.y' to specify known variances")
      }
      
    }
  }
  out<-list(exist.by=exist.by,exist.y=exist.y,y=vec.y,by=vec.by,err.list=err.list)
  return(out)
}

## Functions to obtain ci/tests -----
#specific function, one parameter 
ci.mean.known<-function(x,sigma,conf.level = 0.95,
                        digits=2,force.digits=FALSE,
                        use.scientific=FALSE,type.print="cat"){
  my.p.list(paste0("Confidence interval for the mean",
                   "\nConfidence level: ",conf.level,"\nVariance: known"),
            type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  m.x<-mean(x)
  s.x <- sigma
  se.m<-s.x/sqrt(n.x)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  z.q<-qnorm(0.5+conf.level/2)
  out<-t(as.matrix(c(n.x,m.x,s.x,se.m,m.x+(c(-1,1)*(se.m*z.q)))))
  colnames(out)<-c("n","xbar","sigma_X","SE","Lower","Upper")
  
  # modified 
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                         use.scientific=use.scientific)
  print(as.data.frame(out.print.p),
        row.names=FALSE,quote = FALSE,
        right = TRUE)
  output=data.frame(out,check.names = FALSE)
}

hyp.mean.known<-function(x,sigma=1,
                         mu0=0,alternative="two.sided",
                         digits=2,force.digits=force.digits,
                         use.scientific=use.scientific,
                         type.print="cat"){
  my.p.list(paste0("Test hypotheses on mean","\nVariance: known"),
            type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  m.x<-mean(x)
  s.x <- sigma
  se.m<-s.x/sqrt(n.x)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  z <- (m.x - mu0) / se.m
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  
  symb.use<-.do_gen_symbols(type.print)
  tit.null<-switch(alternative,
                   two.sided=paste0(symb.use$mu," ",symb.use$eq," ",mu0),
                   less=paste0(symb.use$mu," ",symb.use$eq," ",mu0,
                               " or ",symb.use$mu," ",symb.use$ge," ",mu0),
                   greater=paste0(symb.use$mu," ",symb.use$eq," ",mu0,
                                  " or ",symb.use$mu," ",symb.use$le," ",mu0))
  tit.alt<-switch(alternative,
                  two.sided=paste0(symb.use$mu," ",symb.use$neq," ",mu0),
                  less=paste0(symb.use$mu," ",symb.use$lt," ",mu0),
                  greater=paste0(symb.use$mu," ",symb.use$gt," ",mu0))
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=n.x,xbar=m.x,sigma_X=s.x,
                  SE=se.m,stat=z,"p-value"=p.z,
                  check.names = FALSE)
  out.print.p<-ci.print(out,digits,
                        force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),row.names=FALSE,
        quote = FALSE,
        right = TRUE)
  output=data.frame(out,check.names = FALSE)
} 

ci.mean.unknown<-function(x,conf.level = 0.95,digits=2,
                          force.digits=FALSE,use.scientific=FALSE,
                          type.print="cat"){
  my.p.list(paste0("Confidence interval for the mean",
                   "\nConfidence level: ",conf.level,"\nVariance: unknown"),
            type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  m.x<-mean(x)
  s.x<-sd(x) ; se.m<-s.x/sqrt(n.x)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  z.q<-qnorm(0.5+conf.level/2)
  t.q<-qt((0.5+conf.level/2),df=(n.x-1))
  out<-rbind(c(n.x,m.x,s.x,se.m,m.x+(c(-1,1)*(se.m*z.q))),
             c(n.x,m.x,s.x,se.m,m.x+(c(-1,1)*(se.m*t.q))))
  colnames(out)<-c("n","xbar","s_X","se","Lower","Upper")
  rownames(out)<-c("Normal.Approx","Student-t")

  # modified 
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(as.data.frame(out.print.p),quote = FALSE,
        right = TRUE)
  output=data.frame(out,check.names = FALSE)
}

hyp.mean.unknown<-function(x,mu0=0,alternative="two.sided",
                           digits=2,force.digits=force.digits,
                           use.scientific=use.scientific,
                           type.print="cat"){
  my.p.list(paste0("Test hypotheses on mean",
                   "\nVariance: unknown"),type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  m.x<-mean(x)
  s.x<-sd(x) ; 
  se.m<-s.x/sqrt(n.x)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  z <- (m.x - mu0) / se.m
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  p.t<-switch(alternative,
              two.sided=2*pt(-abs(z),df=(n.x-1)),
              less=pt(z,df=(n.x-1)),
              greater=1-pt(z,df=(n.x-1))) 
  
  symb.use<-.do_gen_symbols(type.print)
  tit.null<-switch(alternative,
                   two.sided=paste0(symb.use$mu," ",symb.use$eq," ",mu0),
                   less=paste0(symb.use$mu," ",symb.use$eq," ",mu0,
                               " or ",symb.use$mu," ",symb.use$ge," ",mu0),
                   greater=paste0(symb.use$mu," ",symb.use$eq," ",mu0,
                                  " or ",symb.use$mu," ",symb.use$le," ",mu0))
  tit.alt<-switch(alternative,
                  two.sided=paste0(symb.use$mu," ",symb.use$neq," ",mu0),
                  less=paste0(symb.use$mu," ",symb.use$lt," ",mu0),
                  greater=paste0(symb.use$mu," ",symb.use$gt," ",mu0))
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=c(n.x,n.x),
                  xbar=c(m.x,m.x),
                  s_X=c(s.x,s.x),
                  se=c(se.m,se.m),
                  stat=c(z,z),
                  "p-value"=c(p.z,p.t),
                  check.names = FALSE)
  rownames(out)<-c("Normal.Approx","Student-t")
  
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),quote = FALSE,
        right = TRUE)
  output=data.frame(out,check.names = FALSE)
}

ci.prop<-function(x,success,conf.level = 0.95, 
                  digits = 2,force.digits=FALSE,
                  use.scientific=FALSE,type.print="cat"){
  my.p.list(paste0("Confidence interval for the proportion",
                   "\nConfidence level: ",conf.level),type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  if(is.null(success)){x.success<-sum(x)} else{x.success<-sum(x==success)}
  p.x<-x.success/n.x
  s.x<-sqrt(p.x*(1-p.x))
  se.p<-s.x/sqrt(n.x)
  z.q<-qnorm(0.5+conf.level/2)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  out<-t(as.matrix(c(n.x,p.x,s.x,se.p,p.x+(c(-1,1)*(se.p*z.q)))))
  colnames(out)<-c("n","phat","s_X","se","Lower","Upper")
  #out[2:length(out)]<-round(out[2:length(out)],digits)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(data.frame(out.print.p,check.names = FALSE),
        row.names = FALSE)
  output=data.frame(out,check.names = FALSE)
}

hyp.prop<-function(x,success,p0=0.5,alternative="two.sided",
                   digits=2,force.digits=FALSE,
                   use.scientific=FALSE,type.print="cat"){
  my.p.list(paste0("Test hypotheses on proportion"),
            type.print=type.print)
  n.or<-length(x)
  x<-na.omit(x)
  n.x<-length(x)
  if(is.null(success)){x.success<-sum(x)} else{x.success<-sum(x==success)}
  p.x<-x.success/n.x
  s.x<-sqrt(p0*(1-p0))
  se.p<-s.x/sqrt(n.x)
  if(n.x<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.x),
                     " obs with NA on 'x' removed"),
              type.print=type.print)
  }
  z <- (p.x - p0) / se.p
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  
  symb.use<-.do_gen_symbols(type.print)
  tit.null<-switch(alternative,
                   two.sided=paste0("p ",symb.use$eq," ",p0),
                   less=paste0("p ",symb.use$eq," ",p0,
                               " or ","p ",symb.use$ge," ",p0),
                   greater=paste0("p ",symb.use$eq," ",p0,
                                  " or ","p ",symb.use$le," ",p0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("p ",symb.use$neq," ",p0),
                  less=paste0("p ",symb.use$lt," ",p0),
                  greater=paste0("p ",symb.use$gt," ",p0))
  
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=n.x,phat=p.x,s_X=s.x,
                  se=se.p,
                  stat=z,"p-value"=p.z,
                  check.names = FALSE)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),row.names=FALSE,
        quote = FALSE,right = TRUE)
  output=data.frame(out,check.names = FALSE)
}

#ok
ci.diff.paired_known<-function(x,y,names.xy,sigma.d,
                               conf.level = 0.95,digits = 2,
                               force.digits=FALSE,
                               use.scientific=FALSE,
                               type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  
  my.p.list(paste0("Confidence interval for ",use.diff,
                   "\nSamples: paired", "\nConfidence level: ",
                   conf.level,"\nVariance: known",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print=type.print)
  both.in<-!(is.na(x)) & !(is.na(y))
  n.or<-length(x)
  n.xy<-length(x[both.in])
  x<-x[both.in]
  y<-y[both.in]
  m.diff<-mean(x-y)
  s.diff<-sigma.d  
  se.diff<-s.diff/sqrt(n.xy)
  z.q<-qnorm(0.5+conf.level/2)
  if(n.xy<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.xy),
                     " obs with NA on 'x,y' removed"),
              type.print=type.print)
  }
  out<-t(as.matrix(c(n.xy,mean(x),mean(y),
                     m.diff,s.diff,se.diff,m.diff+(c(-1,1)*(se.diff*z.q)))))
  colnames(out)<-c("n","xbar","ybar","dbar=xbar-ybar","sigma_D","SE",
                   "Lower","Upper")
  #out[2:length(out)]<-round(out[2:length(out)],digits)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(data.frame(out.print.p,check.names = FALSE),row.names = FALSE)
  output=data.frame(out,check.names = FALSE)
}

#ok
hyp.diff.paired_known<-function(x,y,mdiff0=0,names.xy,sigma.d,
                                alternative="two.sided",
                                digits = 2,
                                force.digits=FALSE,
                                use.scientific=FALSE,
                                type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  my.p.list(paste0("Test hypotheses on ",use.diff,
                   "\nSamples: paired", 
                   "\nVariance: known",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print=type.print)
  
  both.in<-!(is.na(x)) & !(is.na(y))
  n.or<-length(x)
  n.xy<-length(x[both.in])
  x<-x[both.in]
  y<-y[both.in]
  m.diff<-mean(x-y)
  s.diff<-sigma.d  
  se.diff<-s.diff/sqrt(n.xy)
  if(n.xy<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.xy),
                     " obs with NA on 'x,y' removed"),
              type.print=type.print)
  }
  z <- (m.diff - mdiff0) / se.diff
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  
  tit.null<-switch(alternative,
                   two.sided=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0),
                   less=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                               " or (",use.diff,") ",symb.use$ge," ",mdiff0),
                   greater=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                                  " or (",use.diff,") ",symb.use$le," ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(",use.diff,") ",symb.use$neq," ",mdiff0),
                  less=paste0("(",use.diff,") ",symb.use$lt," ",mdiff0),
                  greater=paste0("(",use.diff,") ",symb.use$gt," ",mdiff0))
  
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  out<-data.frame(n=n.xy,xbar=mean(x),
                  ybar=mean(y),
                  "dbar=xbar-ybar"=m.diff,
                  "sigma_D"=s.diff,
                  "SE"=se.diff,stat=z,
                  "p-value"=p.z,
                  check.names = FALSE)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),row.names=FALSE,
        quote = FALSE,right = TRUE)
  output=data.frame(out,check.names = FALSE)
}#ok

#ok
ci.diff.paired_unknown<-function(x,y,names.xy,
                                 conf.level = 0.95,
                                 digits = 2,
                                 force.digits=FALSE,
                                 use.scientific=FALSE,
                                 type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  
  my.p.list(paste0("Confidence interval for ",use.diff,
                   "\nSamples: paired", "\nConfidence level: ",conf.level,
                   "\nVariance: unknown",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print=type.print)
  
  both.in<-!(is.na(x)) & !(is.na(y))
  n.or<-length(x)
  n.xy<-length(x[both.in])
  x<-x[both.in]
  y<-y[both.in]
  z.q<-qnorm(0.5+conf.level/2)
  t.q<-qt((0.5+conf.level/2),df=(n.xy-1))
  m.diff<-mean(x-y)
  s.diff<-sd(x-y) ; 
  se.diff<-s.diff/sqrt(n.xy)
  if(n.xy<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.xy),
                     " obs with NA on 'x,y' removed"),
              type.print=type.print)
  }
  out<-rbind(c(n.xy,mean(x),mean(y),m.diff,s.diff,
               se.diff,m.diff+(c(-1,1)*(se.diff*z.q))),
             c(n.xy,mean(x),mean(y),m.diff,s.diff,
               se.diff,m.diff+(c(-1,1)*(se.diff*t.q))))
  colnames(out)<-c("n","xbar","ybar","dbar=xbar-ybar","s_D","se",
                   "Lower","Upper")
  rownames(out)<-c("Normal.Approx","Student-t")
  # out[,2:ncol(out)]<-round(out[,2:ncol(out)],digits)
  #modified
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(as.data.frame(out.print.p),quote = FALSE,
        right = TRUE)
  #print(data.frame(out,check.names = FALSE))
  output=data.frame(out,check.names = FALSE)
}#ok

hyp.diff.paired_unknown<-function(x,y,mdiff0=0,names.xy,
                                  alternative="two.sided",
                                  digits = 2,
                                  force.digits=FALSE,
                                  use.scientific=FALSE,
                                  type.print="cat"){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  my.p.list(paste0("Test hypotheses on ",use.diff,
                   "\nSamples: paired","\nVariance: unknown",
                   "\n   x = ",names.xy["x"],"\n   y = ",names.xy["y"]),
            type.print=type.print)
  both.in<-!(is.na(x)) & !(is.na(y))
  n.or<-length(x)
  n.xy<-length(x[both.in])
  x<-x[both.in]
  y<-y[both.in]
  m.diff<-mean(x-y)
  s.diff<-sd(x-y) ; 
  se.diff<-s.diff/sqrt(n.xy)
  if(n.xy<n.or){
    my.p.list(paste0("\n Warning: ",(n.or-n.xy),
                     " obs with NA on 'x,y' removed"),
              type.print=type.print)
  }
  z <- (m.diff - mdiff0) / se.diff
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  p.t<-switch(alternative,
              two.sided=2*pt(-abs(z),df=(n.xy-1)),
              less=pt(z,df=(n.xy-1)),
              greater=1-pt(z,df=(n.xy-1))) 
  
  tit.null<-switch(alternative,
                   two.sided=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0),
                   less=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                               " or (",use.diff,") ",symb.use$ge," ",mdiff0),
                   greater=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                                  " or (",use.diff,") ",symb.use$le," ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(",use.diff,") ",symb.use$neq," ",mdiff0),
                  less=paste0("(",use.diff,") ",symb.use$lt," ",mdiff0),
                  greater=paste0("(",use.diff,") ",symb.use$gt," ",mdiff0))
  
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=c(n.xy,n.xy),
                  "xbar"=c(mean(x),mean(x)),
                  "ybar"=c(mean(y),mean(y)),
                  "dbar=xbar-ybar"=c(m.diff,m.diff),
                  "s_D"=c(s.diff,s.diff),
                  "se"=c(se.diff,se.diff),
                  stat=c(z,z),
                  "p-value"=c(p.z,p.t),
                  check.names = FALSE)
  rownames(out)<-c("Normal.Approx","Student-t")
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),
        quote = FALSE,right = TRUE)
  output=data.frame(out,check.names = FALSE)
}#ok


#ok
ci.diff.indep_known<-function(x,y,names.xy,sigma.x,sigma.y,
                              conf.level = 0.95,digits = 2,
                              force.digits=FALSE,
                              use.scientific=FALSE,
                              type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  
  my.p.list(paste0("Confidence interval for ",use.diff,
                   "\nSamples: independent",
                   "\nConfidence level: ",conf.level,
                   "\nVariances: known",
                   "\n   x = ",names.xy["x"],"\n   y = ",names.xy["y"]),
            type.print=type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  z.q<-qnorm(0.5+conf.level/2)
  m.diff<-mean(x)-mean(y)
  se.diff <- sqrt((sigma.x^2 / n.x) + (sigma.y^2 / n.y))
  
  msg.warn<-NULL
  if(n.x<n.or.x){msg.warn<-paste0((n.or.x-n.x),
                                  " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y)," obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),
              type.print=type.print)
  }
  
  out<-rbind(c(n.x,n.y,mean(x),mean(y),m.diff,sigma.x,sigma.y,
               se.diff,m.diff+(c(-1,1)*(se.diff*z.q))))
  colnames(out)<-c("n_x","n_y","xbar","ybar","xbar-ybar",
                   "sigma_X","sigma_Y","SE","Lower","Upper")
  #out[3:length(out)]<-round(out[3:length(out)],digits)
  # modified
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(as.data.frame(out.print.p),row.names=FALSE,quote = FALSE,
        right = TRUE)
  output=data.frame(out,check.names = FALSE)
}#ok


#ok
hyp.diff.indep_known<-function(x,y,mdiff0=0,names.xy,
                               sigma.x,sigma.y,
                               alternative="two.sided",
                               digits = 2,force.digits=FALSE,
                               use.scientific=FALSE,
                               type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  my.p.list(paste0("Test hypotheses on ",use.diff,
                   "\nSamples: independent","\nVariances: known",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print=type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  m.diff<-mean(x)-mean(y)
  se.diff <- sqrt((sigma.x^2 / n.x) + (sigma.y^2 / n.y))
  msg.warn<-NULL
  if(n.x<n.or.x){msg.warn<-paste0((n.or.x-n.x),
                                  " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y),
                     " obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),type.print=type.print)
  }
  
  z <- (m.diff - mdiff0) / se.diff
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  tit.null<-switch(alternative,
                   two.sided=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0),
                   less=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                               " or (",use.diff,") ",symb.use$ge," ",mdiff0),
                   greater=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                                  " or (",use.diff,") ",symb.use$le," ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(",use.diff,") ",symb.use$neq," ",mdiff0),
                  less=paste0("(",use.diff,") ",symb.use$lt," ",mdiff0),
                  greater=paste0("(",use.diff,") ",symb.use$gt," ",mdiff0))
  
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n_x=n.x,n_y=n.y,
                  xbar=mean(x),
                  ybar=mean(y),
                  "xbar-ybar"=m.diff,
                  "sigma_X"=sigma.x,
                  "sigma_Y"=sigma.y,
                  "SE"=se.diff,
                  stat=z,
                  "p-value"=p.z,
                  check.names = FALSE)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(as.data.frame(out.print.p),row.names=FALSE,
        quote = FALSE,right = TRUE)
  output=data.frame(out,check.names = FALSE)
}#ok

#ok
ci.diff.indep_unknown<-function(x,y,names.xy,conf.level = 0.95,
                                digits = 2,type.print="cat",
                                var.test = FALSE,
                                force.digits=FALSE,
                                use.scientific=FALSE){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  
  my.p.list(paste0("Confidence interval for ",use.diff,
                   "\nSamples: independent", 
                   "\nConfidence level: ",conf.level,
                   "\nVariances: unknown",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print = type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  m.diff<-mean(x)-mean(y)
  s2.x<-var(x)
  s2.y<-var(y)
  z.q<-qnorm(0.5+conf.level/2)
  df.equal<-n.x+n.y- 2L
  df.unequal<-((s2.x/n.x + s2.y/n.y)^2)/( ((s2.x/n.x)^2 /(n.x-1))+((s2.y/n.y)^2 / (n.y-1)))
  teq.q<-qt((0.5+conf.level/2),df=(df.equal))
  tdiff.q<-qt((0.5+conf.level/2),df=(df.unequal))
  s.equal<-sqrt(((n.x-1)*s2.x + (n.y - 1)*s2.y) / (n.x+n.y-2))
  se.equal<-s.equal*sqrt((1/n.x)+(1/n.y))
  se.unequal<-sqrt(s2.x/n.x + s2.y /n.y)
  
  msg.warn<-NULL
  if(n.x<n.or.x){
    msg.warn<-paste0((n.or.x-n.x),
                     " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y),
                     " obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),type.print=type.print)
  }
  
  out.eq<-rbind(c(n.x,n.y,mean(x),mean(y),
                  m.diff,sqrt(s2.x),sqrt(s2.y),
                  se.equal,m.diff+(c(-1,1)*(se.equal*z.q))),
                c(n.x,n.y,mean(x),mean(y),m.diff,
                  sqrt(s2.x),sqrt(s2.y),
                  se.equal,m.diff+(c(-1,1)*(se.equal*teq.q))))
  out.diff<-rbind(c(n.x,n.y,mean(x),mean(y),
                    m.diff,sqrt(s2.x),sqrt(s2.y),
                    se.unequal,m.diff+(c(-1,1)*(se.unequal*z.q))),
                  c(n.x,n.y,mean(x),mean(y),
                    m.diff,sqrt(s2.x),sqrt(s2.y),
                    se.unequal,
                    m.diff+(c(-1,1)*(se.unequal*tdiff.q))))
  #out.eq[3:length(out.eq)]<-round(out.eq[3:length(out.eq)],digits)
  #out.diff[3:length(out.diff)]<-round(out.diff[3:length(out.diff)],digits)
  colnames(out.eq)<-c("n_x","n_y","xbar","ybar","xbar-ybar",
                      "s_X","s_Y","se",
                      "Lower","Upper")
  colnames(out.diff)<-colnames(out.eq)
  rownames(out.eq)<-rownames(out.diff)<-c("Normal.Approx","Student-t")
  
  # added
  out.print.p.EQ<-ci.print(out.eq,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p.DIFF<-ci.print(out.diff,digits,force.digits=force.digits,
                           use.scientific=use.scientific)
  my.p.list(paste0("\n Unknown variances assumed to be equal"),
            type.print=type.print)
  #print(data.frame(out.eq,check.names = FALSE))
  print(as.data.frame(out.print.p.EQ),quote = FALSE,
        right = TRUE)
  my.p.list(paste0("\n Unknown variances assumed to be different"),
            type.print=type.print)
  #print(data.frame(out.diff,check.names = FALSE))
  print(as.data.frame(out.print.p.DIFF),quote = FALSE,
        right = TRUE)
  
  rownames(out.eq)<-rownames(out.diff)<-c()
  out.final=data.frame(Variances=c("Equal","Equal","Different","Different"),
                    Distrib=c("Normal.Approx","Student-t","Normal.Approx","Student-t"),
                    rbind(out.eq,out.diff),check.names = FALSE)
  #if(!var.test){return(output)}
  if(var.test){
    out.var<-hyp.diff.var(x,y,type="levene",
                          digits=digits,force.digits=force.digits,
                          use.scientific=use.scientific,
                          print.n=FALSE,
                          type.print=type.print)
    out.final<-list(test.means=out.final,test.vars=out.var)
  }
  output<-out.final
}#ok

#ok
hyp.diff.indep_unknown<-function(x,y,mdiff0=0,names.xy,
                                 alternative="two.sided",
                                 digits = 2,
                                 var.test = FALSE,
                                 force.digits=FALSE,
                                 use.scientific=FALSE,
                                 type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  use.diff<-paste0(symb.use$mu,"_x"," ",symb.use$minus," ",
                   symb.use$mu,"_y")
  my.p.list(paste0("Test hypotheses on ",use.diff,
                   "\nSamples: independent",
                   "\nVariances: unknown",
                   "\n   x = ",names.xy["x"],
                   "\n   y = ",names.xy["y"]),type.print=type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  m.diff<-mean(x)-mean(y)
  s2.x<-var(x)
  s2.y<-var(y)
  s.equal<-sqrt(((n.x-1)*s2.x + (n.y - 1)*s2.y) / (n.x+n.y-2))
  se.equal<-s.equal*sqrt((1/n.x)+(1/n.y))
  se.unequal<-sqrt(s2.x/n.x + s2.y /n.y)
  
  msg.warn<-NULL
  if(n.x<n.or.x){msg.warn<-paste0((n.or.x-n.x),
                                  " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y),
                     " obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),type.print=type.print)
  }
  
  z.eq<-(m.diff-mdiff0)/se.equal
  z.uneq<-(m.diff-mdiff0)/se.unequal
  p.zeq<-switch(alternative,
                two.sided=2*pnorm(-abs(z.eq)),
                less=pnorm(z.eq),
                greater=1-pnorm(z.eq))
  p.zuneq<-switch(alternative,
                  two.sided=2*pnorm(-abs(z.uneq)),
                  less=pnorm(z.uneq),
                  greater=1-pnorm(z.uneq))
  
  df.equal<-n.x+n.y- 2L
  df.unequal<-((s2.x/n.x + s2.y/n.y)^2)/( ((s2.x/n.x)^2 /(n.x-1))+((s2.y/n.y)^2 / (n.y-1)))
  p.teq<-switch(alternative,
                two.sided=2*pt(-abs(z.eq),df=df.equal),
                less=pt(z.eq,df=df.equal),
                greater=1-pt(z.eq,df=df.equal)) 
  p.tuneq<-switch(alternative,
                  two.sided=2*pt(-abs(z.uneq),df=df.unequal),
                  less=pt(z.uneq,df=df.unequal),
                  greater=1-pt(z.uneq,df=df.unequal)) 
  
  tit.null<-switch(alternative,
                   two.sided=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0),
                   less=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                               " or (",use.diff,") ",symb.use$ge," ",mdiff0),
                   greater=paste0("(",use.diff,") ",symb.use$eq," ",mdiff0,
                                  " or (",use.diff,") ",symb.use$le," ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(",use.diff,") ",symb.use$neq," ",mdiff0),
                  less=paste0("(",use.diff,") ",symb.use$lt," ",mdiff0),
                  greater=paste0("(",use.diff,") ",symb.use$gt," ",mdiff0))
  
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out.eq<-data.frame(n_x=c(n.x,n.x),n_y=c(n.y,n.y),
                     "xbar"=c(mean(x),mean(x)),
                     "ybar"=c(mean(y),mean(y)),
                     "xbar-ybar"=c(m.diff,m.diff),
                     "s_X"=c(sqrt(s2.x),sqrt(s2.x)),
                     "s_Y"=c(sqrt(s2.y),sqrt(s2.y)),
                     "se"=c(se.equal,se.equal),
                     stat=c(z.eq,z.eq),
                     "p-value"=c(p.zeq,p.teq),
                     check.names = FALSE)
  out.diff<-data.frame(n_x=c(n.x,n.x),n_y=c(n.y,n.y),
                       "xbar"=c(mean(x),mean(x)),
                       "ybar"=c(mean(y),mean(y)),
                       "xbar-ybar"=c(m.diff,m.diff),
                       "s_X"=c(sqrt(s2.x),sqrt(s2.x)),
                       "s_Y"=c(sqrt(s2.y),sqrt(s2.y)),
                       "se"=c(se.unequal,se.unequal),
                       stat=c(z.uneq,z.uneq),
                       "p-value"=c(p.zuneq,p.tuneq),
                       check.names = FALSE)
  rownames(out.eq)<-rownames(out.diff)<-c("Normal.Approx","Student-t")
  
  out.print.p.EQ<-ci.print(out.eq,digits,
                           force.digits=force.digits,
                           use.scientific=use.scientific)
  out.print.p.EQ[,"p-value"][out.eq[,"p-value"]<0.0001]<-"<0.0001"
  out.print.p.DIFF<-ci.print(out.diff,digits,
                           force.digits=force.digits,
                           use.scientific=use.scientific)
  out.print.p.DIFF[,"p-value"][out.diff[,"p-value"]<0.0001]<-"<0.0001"

  my.p.list(paste0("\n Unknown variances assumed to be equal"),
            type.print=type.print)
  print(as.data.frame(out.print.p.EQ),
        quote = FALSE,right = TRUE)
  my.p.list(paste0("\n Unknown variances assumed to be different"),
            type.print=type.print)
  print(as.data.frame(out.print.p.DIFF),
        quote = FALSE,right = TRUE)
  
  rownames(out.eq)<-rownames(out.diff)<-c()
  out.final=data.frame(Variances=c("Equal","Equal","Different","Different"),
                       Distrib=c("Normal.Approx","Student-t","Normal.Approx","Student-t"),
                       rbind(out.eq,out.diff),
                       check.names = FALSE)
  
  if(var.test){
    out.var<-hyp.diff.var(x,y,type="levene",
                          digits=digits,force.digits=force.digits,
                          use.scientific=use.scientific,
                          print.n=FALSE,
                          type.print=type.print)
    out.final<-list(test.means=out.final,test.vars=out.var)
  }
  output<-out.final
}#ok

#ok
hyp.diff.var<-function(x,y,type="levene",digits=2,
                       force.digits=FALSE,
                       use.scientific=FALSE,
                       print.n=TRUE,
                       type.print="cat"){
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  
  if(type=="levene"){
    s2.x<-var(x) ; s2.y<-var(y)
    z_x<-abs(x-median(x))
    z_y<-abs(y-median(y))
    num.stat<-length(x)*(mean(z_x)-mean(c(z_x,z_y)))^2 + 
      length(y)*(mean(z_y)-mean(c(z_x,z_y)))^2
    den.stat<-(length(x)-1)*var(z_x) + (length(y)-1)*var(z_y)
    stat<-(length(x)+length(y)-2)*num.stat/den.stat
    p.stat<-1-pf(stat,df1=1,df2=(length(x)+length(y)-2))
    
    symb.use<-.do_gen_symbols(type.print)
    my.p.list(paste0("\nLevene test for homogeneity of variance",
                     "\n Null hypothesis        H0: ",symb.use$sigma,"2_x ",symb.use$eq," ",symb.use$sigma,"2_y",
                     "\n Alternative hypothesis H1: ",symb.use$sigma,"2_x ",symb.use$neq," ",symb.use$sigma,"2_y"),
              type.print=type.print)
    if(print.n){
      msg.warn<-NULL
      if(n.x<n.or.x){msg.warn<-paste0((n.or.x-n.x),
                                      " obs with NA removed from 'x'")}
      if(n.y<n.or.y){
        if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
        msg.warn<-paste0(msg.warn,(n.or.y-n.y)," obs with NA removed from 'y'")}
      if(!is.null(msg.warn)){
        my.p.list(paste0("\n Warning: ",msg.warn),
                  type.print=type.print)
      }
      
      out.var<-data.frame("n_x" = n.x, "n_y"=n.y,
                          "s2_x"=s2.x,"s2_y"=s2.y,"F-stat"=stat,
                          "df1"=1,"df2"=(length(x)+length(y)-2),
                          "p-value"=p.stat,check.names = FALSE)
    }
    if(!print.n){
      out.var<-data.frame("s2_x"=s2.x,"s2_y"=s2.y,"F-stat"=stat,
                        "df1"=1,"df2"=(length(x)+length(y)-2),
                        "p-value"=p.stat,check.names = FALSE)
    }
    out.print.p<-ci.print(out.var,digits,force.digits=force.digits,
                          use.scientific=use.scientific)
    out.print.p[,"p-value"][out.var[,"p-value"]<0.0001]<-"<0.0001"
    print(as.data.frame(out.print.p),row.names=FALSE,quote = FALSE,
          right = TRUE)
    output<-(out.var)
  }
}#ok

#ok
ci.diff.prop<-function(x,y,names.xy,success.x=NULL,success.y=NULL,
                       conf.level =  0.95, digits = 2,
                       force.digits=FALSE,
                       use.scientific=FALSE,type.print="cat" ){
  symb.use<-.do_gen_symbols(type.print)
  my.p.list(paste0("Confidence interval for p_x ",symb.use$minus," p_y",
                   "\nConfidence level: ",conf.level),
            type.print=type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  
  if(is.null(success.x)){
    x.success<-sum(x)
    if(is.numeric(x)){tit.x<-paste0("  x:",names.xy["x"]," = 1")}
    if(is.logical(x)){tit.x<-paste0("  x:",names.xy["x"]," = TRUE")}
  } else {
    x.success<-sum(x==success.x)
    tit.x<-paste0("  x:",names.xy["x"]," = ",success.x)
  }
  if(names.xy["name.by"]!="NONE"){
    tit.x<-paste0(tit.x," | ",names.xy["name.by"]," = ",names.xy["lev1"])
  }
  if(is.null(success.y)){
    y.success<-sum(y)
    if(is.numeric(y)){tit.y<-paste0("  y:",names.xy["y"]," = 1")}
    if(is.logical(y)){tit.y<-paste0("  y:",names.xy["y"]," = TRUE")}
  } else {
    y.success<-sum(y==success.y)
    tit.y<-paste0("  y:",names.xy["y"]," = ",success.y)
  }
  if(names.xy["name.by"]!="NONE"){
    tit.y<-paste0(tit.y," | ",names.xy["name.by"]," = ",names.xy["lev2"])
  }
  my.p.list(paste0(tit.x,paste0("\n",tit.y)),type.print=type.print)
  
  p.x<-x.success/n.x
  p.y<-y.success/n.y
  p.diff<-p.x-p.y
  s.x<-sqrt(p.x*(1-p.x))
  s.y<-sqrt(p.y*(1-p.y))
  se.xy<-sqrt((p.x*(1-p.x)/n.x)+(p.y*(1-p.y)/n.y))
  z.q<-qnorm(0.5+conf.level/2)
  
  msg.warn<-NULL
  if(n.x<n.or.x){
    msg.warn<-paste0((n.or.x-n.x),
                     " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y),
                     " obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),type.print=type.print)
  }
  
  out<-t(as.matrix(c(n.x,n.y,p.x,p.y,
                     p.diff,s.x,s.y,se.xy,
                     p.diff+(c(-1,1)*(se.xy*z.q)))))
  colnames(out)<-c("n_x","n_y","phat_x","phat_y","phat_x-phat_y",
                   "s_X","s_Y","se",
                   "Lower","Upper")
  #out[3:length(out)]<-round(out[3:length(out)],digits)
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  print(data.frame(out.print.p,check.names = FALSE),row.names = FALSE)
  #print(data.frame(out,check.names = FALSE),row.names = FALSE)
  output=data.frame(out,check.names = FALSE)
}#ok

#ok
hyp.diff.prop<-function(x,y,names.xy,pdiff0=0,
                        success.x=NULL,
                        success.y=NULL,
                        alternative="two.sided", 
                        digits = 2,force.digits=FALSE,
                        use.scientific=FALSE,
                        type.print="cat"){
  symb.use<-.do_gen_symbols(type.print)
  my.p.list(paste0("Test hypotheses on p_x ",symb.use$minus," p_y"),
            type.print=type.print)
  n.or.x<-length(x)
  n.or.y<-length(y)
  x<-na.omit(x)
  y<-na.omit(y)
  n.x<-length(x)
  n.y<-length(y)
  
  if(is.null(success.x)){
    x.success<-sum(x)
    if(is.numeric(x)){tit.x<-paste0("  x:",names.xy["x"]," = 1")}
    if(is.logical(x)){tit.x<-paste0("  x:",names.xy["x"]," = TRUE")}
  } else {
    x.success<-sum(x==success.x)
    tit.x<-paste0("  x:",names.xy["x"]," = ",success.x)
  }
  if(names.xy["name.by"]!="NONE"){
    tit.x<-paste0(tit.x," | ",names.xy["name.by"]," = ",
                  names.xy["lev1"])
  }
  if(is.null(success.y)){
    y.success<-sum(y)
    if(is.numeric(x)){tit.y<-paste0("  y:",names.xy["y"]," = 1")}
    if(is.logical(x)){tit.y<-paste0("  y:",names.xy["y"]," = TRUE")}
  } else {
    y.success<-sum(y==success.y)
    tit.y<-paste0("  y:",names.xy["y"]," = ",success.y)
  }
  if(names.xy["name.by"]!="NONE"){
    tit.y<-paste0(tit.y," | ",names.xy["name.by"]," = ",
                  names.xy["lev2"])
  }
  my.p.list(paste0(tit.x,paste0("\n",tit.y)),type.print=type.print)
  
  p.x<-x.success/n.x
  p.y<-y.success/n.y
  p.diff<-p.x-p.y
  s.x<-sqrt(p.x*(1-p.x))
  s.y<-sqrt(p.y*(1-p.y))
  se.xy<-sqrt((p.x*(1-p.x)/n.x)+(p.y*(1-p.y)/n.y))
  
  if(pdiff0==0){
    p_xy<-(x.success+y.success)/(n.x+n.y)
    se.xy<-sqrt((p_xy*(1-p_xy)/n.x)+(p_xy*(1-p_xy)/n.y))
  }
  
  msg.warn<-NULL
  if(n.x<n.or.x){
    msg.warn<-paste0((n.or.x-n.x),
                     " obs with NA removed from 'x'")}
  if(n.y<n.or.y){
    if(!is.null(msg.warn)){msg.warn<-paste0(msg.warn," & ")}
    msg.warn<-paste0(msg.warn,(n.or.y-n.y),
                     " obs with NA removed from 'y'")}
  if(!is.null(msg.warn)){
    my.p.list(paste0("\n Warning: ",msg.warn),type.print=type.print)
  }
  
  z<-(p.diff-pdiff0)/se.xy
  p.z<-switch(alternative,
              two.sided=2*pnorm(-abs(z)),
              less=pnorm(z),
              greater=1-pnorm(z))
  
  tit.null<-switch(alternative,
                   two.sided=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$eq," ",pdiff0),
                   less=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$eq," ",pdiff0,
                               " or ","(p_x ",symb.use$minus," p_y) ",symb.use$ge," ",pdiff0),
                   greater=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$eq," ",pdiff0,
                                  " or ","(p_x ",symb.use$minus," p_y) ",symb.use$le," ",pdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$neq," ",pdiff0),
                  less=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$lt," ",pdiff0),
                  greater=paste0("(p_x ",symb.use$minus," p_y) ",symb.use$gt," ",pdiff0))
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  out<-data.frame("n_x"=n.x,
                  "n_y"=n.y,
                  "phat_x"=p.x,
                  "phat_y"=p.y,
                  "phat_x-phat_y"=p.diff,
                  "s_X"=s.x,
                  "s_Y"=s.y,
                  "se"=se.xy,
                  stat=z,
                  "p-value"=p.z,
                  check.names = FALSE)
  if(pdiff0==0){
    colnames(out)[8]<-"se_0"
  }
  out.print.p<-ci.print(out,digits,force.digits=force.digits,
                        use.scientific=use.scientific)
  out.print.p[,"p-value"][out[,"p-value"]<0.0001]<-"<0.0001"
  print(data.frame(out.print.p,check.names = FALSE),row.names = FALSE)
  output=data.frame(out,check.names = FALSE)
}#ok



