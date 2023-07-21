# Functions specific for inference ------

chkpar.conf<-function(value,err.list=NULL){
  if(is.list(err.list)==F){err.list<-list()}
  ok.value<-(complete.cases(value))
  if(sum(ok.value)==0){
    err.list<-c(err.list,paste0("All elements of 'conf.level' are missing!"))
  }  else {
    if(class(value)[1] != "numeric"){
      err.list<-c(err.list,paste0("'conf.level' cannot be a ",class(value)[1]))
    } else {
      value<-value[ok.value==T]
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

chkpar.sigma<-function(value,err.list=NULL,onlyone=T){
  if(is.list(err.list)==F){err.list<-list()}
  if(!is.null(value)){
    what<-deparse1(substitute(value))
    ok.value<-(complete.cases(value))
    if(sum(ok.value)==0){
      err.list<-c(err.list,paste0("All the elements of '",what,"' are missing!"))
    }  else {
      if(class(value)[1] != "numeric"){
        err.list<-c(err.list,paste0("'",what,"' cannot be a ",class(value)[1]))
      } else {
        value<-value[ok.value==T]
        if(onlyone==T){
          if(length(value)>1){ 
            err.list<-c(err.list,paste0("'",what,"' should be a single numeric value, and not a vector"))
          } else {
            if(value<=0){err.list<-c(err.list,paste0("'",what,"' must be greater than 0!"))}
          }
        }
        if(onlyone==F){
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
  if(is.list(err.list)==F){err.list<-list()}
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
        value<-value[ok.value==T]
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
  if(is.list(err.list)==F){err.list<-list()}
  if(is.list(warn.list)==F){warn.list<-list()}
  if(type=="prop"){type.s<-"independent"}
  exist.y<-exist.by<-F
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
                        num=T,missing=T,err.list=err.list)}
    if(type=="prop"){
      check.y<-chk.data(y,data,name.data,name.y,
                        missing=T,err.list=err.list)}
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
                         missing=T,err.list=err.list)
      err.list<-check.by$err.list
      exist.by<-check.by$exist.x
      vec.by<-check.by$vec.x
    }
  }
  if(exist.by==T){
    n.by<-length(na.omit(unique(vec.by)))
    if(n.by>2){
      err.list<-c(err.list,"'by' should be a vector with 2 levels only!")
      exist.by<-F} 
    if(!is.null(x) && length(x) != length(vec.by)){
      err.list<-c(err.list,"'x' and 'by' should have the same length")
      exist.by<-F}
  }
  if(exist.y==T && !is.null(x)){
    if(type.s=="paired" & length(x) != length(vec.y)){
      err.list<-c(err.list,"'x' and 'y' should have the same length when samples are paired")
      exist.y<-F}
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
      if(exist.by==T && is.null(sigma.d) &&
         is.null(sigma.by) && (!is.null(sigma.x) | !is.null(sigma.y))){
        err.list<-c(err.list,"With 'by', use 'sigma.by' to specify known variances")
      }
      if(exist.by==T && length(c(sigma.d,sigma.x,sigma.y))==0 && 
         !is.null(sigma.by) && length(sigma.by)==2 && 
         sum(names(sigma.by) %in% as.character(unique(na.omit(vec.by))))<2){
        err.list<-c(err.list,"Values in 'sigma.by' should have as names the two values of 'by'")
      }
      if(exist.y==T && is.null(sigma.d) && !is.null(sigma.by) &&
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
                        digits=2,type.print="cat"){
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
  colnames(out)<-c("n","xbar","sigma","SE","Lower","Upper")
  out[2:length(out)]<-round(out[2:length(out)],digits)
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}

hyp.mean.known<-function(x,sigma=1,mu0=0,alternative="two.sided",
                         digits=2,type.print="cat"){
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
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                   two.sided=paste0("\u03BC = ",mu0),
                   less=paste0("\u03BC = ",mu0," or \u03BC \u2265 ",mu0),
                   greater=paste0("\u03BC = ",mu0," or \u03BC \u2264 ",mu0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("\u03BC \u2260 ",mu0),
                  less=paste0("\u03BC < ",mu0),
                  greater=paste0("\u03BC > ",mu0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("\u03BC = ",mu0),
                     less=paste0("\u03BC = ",mu0," or \u03BC >= ",mu0),
                     greater=paste0("\u03BC = ",mu0," or \u03BC <= ",mu0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("\u03BC != ",mu0),
                    less=paste0("\u03BC < ",mu0),
                    greater=paste0("\u03BC > ",mu0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
      paste0(" Alternative hypothesis H1: ",tit.alt)),
      type.print=type.print)
  out<-data.frame(n=n.x,xbar=round(m.x,digits),sigma=round(s.x,digits),SE=round(se.m,digits),
                  Stat=round(z,digits),
                  "p.value"=round(p.z,4),"p-value"=as.character(round(p.z,4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
} 

ci.mean.unknown<-function(x,conf.level = 0.95,digits=2,
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
  colnames(out)<-c("n","xbar","sd","SE","Lower","Upper")
  rownames(out)<-c("Normal.Approx","Student-t")
  out[,2:ncol(out)]<-round(out[,2:ncol(out)],digits)
  print(data.frame(out,check.names=FALSE))
  output=data.frame(out,check.names=FALSE)
}

hyp.mean.unknown<-function(x,mu0=0,alternative="two.sided",
                           digits=2,type.print="cat"){
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
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                     two.sided=paste0("\u03BC = ",mu0),
                     less=paste0("\u03BC = ",mu0," or \u03BC \u2265 ",mu0),
                     greater=paste0("\u03BC = ",mu0," or \u03BC \u2264 ",mu0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("\u03BC \u2260 ",mu0),
                    less=paste0("\u03BC < ",mu0),
                    greater=paste0("\u03BC > ",mu0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("\u03BC = ",mu0),
                     less=paste0("\u03BC = ",mu0," or \u03BC >= ",mu0),
                     greater=paste0("\u03BC = ",mu0," or \u03BC <= ",mu0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("\u03BC != ",mu0),
                    less=paste0("\u03BC < ",mu0),
                    greater=paste0("\u03BC > ",mu0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=c(n.x,n.x),xbar=round(c(m.x,m.x),digits),sd=round(c(s.x,s.x),digits),
                  SE=round(c(se.m,se.m),digits),
                  Stat=round(c(z,z),digits),"p.value"=round(c(p.z,p.t),4),
                  "p-value"=as.character(round(c(p.z,p.t),4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  rownames(out)<-c("Normal.Approx","Student-t")
  print(data.frame(out,check.names=FALSE))
  output=data.frame(out,check.names=FALSE)
}

ci.prop<-function(x,success,conf.level = 0.95, 
                  digits = 2,type.print="cat"){
  my.p.list(paste0("Confidence interval for the proportion",
             "\nConfidence level: ",conf.level))
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
  #tochk<-n.x*p.x*(1-p.x)
  out<-t(as.matrix(c(n.x,p.x,s.x,se.p,p.x+(c(-1,1)*(se.p*z.q)))))
  colnames(out)<-c("n","pbar","sd","SE","Lower","Upper")
  out[2:length(out)]<-round(out[2:length(out)],digits)
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}

hyp.prop<-function(x,success,p0=0.5,alternative="two.sided",
                   digits=2,type.print="cat"){
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
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                   two.sided=paste0("p = ",p0),
                   less=paste0("p = ",p0," or p \u2265 ",p0),
                   greater=paste0("p = ",p0," or p \u2264 ",p0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("p \u2260 ",p0),
                  less=paste0("p < ",p0),
                  greater=paste0("p > ",p0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("p = ",p0),
                     less=paste0("p = ",p0," or p >= ",p0),
                     greater=paste0("p = ",p0," or p <= ",p0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("p != ",p0),
                    less=paste0("p < ",p0),
                    greater=paste0("p > ",p0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=n.x,pbar=round(p.x,digits),SE=round(se.p,digits),
                  Stat=round(z,digits),"p.value"=round(p.z,4),
                  "p-value"=as.character(round(p.z,4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}

#ok
ci.diff.paired_known<-function(x,y,names.xy,sigma.d,
                               conf.level = 0.95,
                               digits = 2,type.print="cat" ){
  my.p.list(paste0("Confidence interval for \u03BC_x-\u03BC_y",
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
  colnames(out)<-c("n","xbar","ybar","d=xbar-ybar","sigma_d","SE",
                   "Lower","Upper")
  out[2:length(out)]<-round(out[2:length(out)],digits)
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}

#ok
hyp.diff.paired_known<-function(x,y,mdiff0=0,names.xy,sigma.d,
                                alternative="two.sided",
                                digits = 2,type.print="cat" ){
  my.p.list(paste0("Test hypotheses on \u03BC_x-\u03BC_y",
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
  if(type.print=="cat"){
  tit.null<-switch(alternative,
                   two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                   less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                               " or (\u03BC_x-\u03BC_y) \u2265 ",mdiff0),
                   greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                  " or (\u03BC_x-\u03BC_y) \u2264 ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(\u03BC_x-\u03BC_y) \u2260 ",mdiff0),
                  less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                  greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                     less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                 " or (\u03BC_x-\u03BC_y) >= ",mdiff0),
                     greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                    " or (\u03BC_x-\u03BC_y) <= ",mdiff0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("(\u03BC_x-\u03BC_y) != ",mdiff0),
                    less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                    greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  out<-data.frame(n=n.xy,xbar=round(mean(x),digits),
                  ybar=round(mean(y),digits),
                  "d=xbar-ybar"=round(m.diff,digits),
                  "sigma_d"=round(s.diff,digits),
                  "SE"=round(se.diff,digits),Stat=round(z,digits),
                  "p.value"=round(p.z,4),"p-value"=as.character(round(p.z,4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}#ok

#ok
ci.diff.paired_unknown<-function(x,y,names.xy,conf.level = 0.95,
                                 digits = 2,type.print="cat" ){
  my.p.list(paste0("Confidence interval for \u03BC_x-\u03BC_y",
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
  colnames(out)<-c("n","xbar","ybar","d=xbar-ybar","sd_d","SE",
                   "Lower","Upper")
  rownames(out)<-c("Normal.Approx","Student-t")
  out[,2:ncol(out)]<-round(out[,2:ncol(out)],digits)
  print(data.frame(out,check.names=FALSE))
  output=data.frame(out,check.names=FALSE)
}#ok

hyp.diff.paired_unknown<-function(x,y,mdiff0=0,names.xy,
                                  alternative="two.sided",digits = 2,
                                  type.print="cat"){
  my.p.list(paste0("Test hypotheses on \u03BC_x-\u03BC_y",
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
  
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                   two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                   less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                               " or (\u03BC_x-\u03BC_y) \u2265 ",mdiff0),
                   greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                  " or (\u03BC_x-\u03BC_y) \u2264 ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(\u03BC_x-\u03BC_y) \u2260 ",mdiff0),
                  less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                  greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                     less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                 " or (\u03BC_x-\u03BC_y) >= ",mdiff0),
                     greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                    " or (\u03BC_x-\u03BC_y) <= ",mdiff0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("(\u03BC_x-\u03BC_y) != ",mdiff0),
                    less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                    greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n=c(n.xy,n.xy),
                  "xbar"=round(c(mean(x),mean(x)),digits),
                  "ybar"=round(c(mean(y),mean(y)),digits),
                  "d=xbar-ybar"=round(c(m.diff,m.diff),digits),
                  "sd_d"=round(c(s.diff,s.diff),digits),
                  SE=round(c(se.diff,se.diff),digits),
                  Stat=round(c(z,z),digits),
                  "p.value"=round(c(p.z,p.t),4),
                  "p-value"=as.character(round(c(p.z,p.t),4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  rownames(out)<-c("Normal.Approx","Student-t")
  print(data.frame(out,check.names=FALSE))
  output=data.frame(out,check.names=FALSE)
}#ok


#ok
ci.diff.indep_known<-function(x,y,names.xy,sigma.x,sigma.y,
                              conf.level = 0.95,
                              digits = 2,type.print="cat" ){
  my.p.list(paste0("Confidence interval for \u03BC_x-\u03BC_y",
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
  colnames(out)<-c("n_x","n_y","xbar","ybar","d=xbar-ybar",
                   "sigma_x","sigma_y","SE","Lower","Upper")
  out[3:length(out)]<-round(out[3:length(out)],digits)
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}#ok


#ok
hyp.diff.indep_known<-function(x,y,mdiff0=0,names.xy,sigma.x,sigma.y,
                               alternative="two.sided",
                               digits = 2,type.print="cat" ){
  my.p.list(paste0("Test hypotheses on \u03BC_x-\u03BC_y",
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
  if(type.print=="cat"){
  tit.null<-switch(alternative,
                   two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                   less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                               " or (\u03BC_x-\u03BC_y) \u2265 ",mdiff0),
                   greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                  " or (\u03BC_x-\u03BC_y) \u2264 ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(\u03BC_x-\u03BC_y) \u2260 ",mdiff0),
                  less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                  greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                     less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                 " or (\u03BC_x-\u03BC_y) >= ",mdiff0),
                     greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                    " or (\u03BC_x-\u03BC_y) <= ",mdiff0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("(\u03BC_x-\u03BC_y) != ",mdiff0),
                    less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                    greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame(n_x=n.x,n_y=n.y,
                  xbar=round(mean(x),digits),
                  ybar=round(mean(y),digits),
                  "d=xbar-ybar"=round(m.diff,digits),
                  "sigma_x"=round(sigma.x,digits),
                  "sigma_y"=round(sigma.y,digits),
                  "SE"=round(se.diff,digits),
                  Stat=round(z,digits),
                  "p.value"=round(p.z,4),"p-value"=as.character(round(p.z,4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}#ok

#ok
ci.diff.indep_unknown<-function(x,y,names.xy,conf.level = 0.95,
                                digits = 2,type.print="cat",
                                var.test=F){
  my.p.list(paste0("Confidence interval for \u03BC_x-\u03BC_y",
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
  out.eq[3:length(out.eq)]<-round(out.eq[3:length(out.eq)],digits)
  out.diff[3:length(out.diff)]<-round(out.diff[3:length(out.diff)],digits)
  colnames(out.eq)<-c("n_x","n_y","xbar","ybar","d=xbar-ybar",
                      "sd_x","sd_y","SE",
                      "Lower","Upper")
  colnames(out.diff)<-colnames(out.eq)
  rownames(out.eq)<-rownames(out.diff)<-c("Normal.Approx","Student-t")
  
  my.p.list(paste0("\n Unknown variances assumed to be equal"),
            type.print=type.print)
  print(data.frame(out.eq,check.names=FALSE))
  my.p.list(paste0("\n Unknown variances assumed to be different"),
            type.print=type.print)
  print(data.frame(out.diff,check.names=FALSE))
  
  rownames(out.eq)<-rownames(out.diff)<-c()
  output=data.frame(Variances=c("Equal","Equal","Different","Different"),
                    Distrib=c("Normal.Approx","Student-t","Normal.Approx","Student-t"),
                    rbind(out.eq,out.diff),check.names=FALSE)
  if(var.test==F){return(output)}
  if(var.test==T){
    out.var<-hyp.diff.var(x,y,type="levene",
                          digits=digits,type.print=type.print)
    out<-list(test.means=output,test.vars=out.var)
    return(out)
  }
}#ok

#ok
hyp.diff.var<-function(x,y,type="levene",digits=2,
                       type.print="cat"){
  x<-na.omit(x)
  y<-na.omit(y)
  if(type=="levene"){
    s2.x<-var(x) ; s2.y<-var(y)
    z_x<-abs(x-median(x))
    z_y<-abs(y-median(y))
    num.stat<-length(x)*(mean(z_x)-mean(c(z_x,z_y)))^2 + 
      length(y)*(mean(z_y)-mean(c(z_x,z_y)))^2
    den.stat<-(length(x)-1)*var(z_x) + (length(y)-1)*var(z_y)
    stat<-(length(x)+length(y)-2)*num.stat/den.stat
    p.stat<-1-pf(stat,df1=1,df2=(length(x)+length(y)-2))
    out.var<-data.frame("var_x"=round(s2.x,digits),
                        "var_y"=round(s2.y,digits),
                        "F-Stat"=round(stat,digits),
                        "df1"=1,"df2"=(length(x)+length(y)-2),
                        "p.value"=round(p.stat,4),
                        "p-value"=as.character(round(p.stat,4)),
                        check.names=FALSE)
    out.var[["p-value"]][out.var[["p.value"]]<0.0001]<-"<0.0001"
    out.var[["p.value"]]<-NULL
    
    if(type.print=="cat"){
      my.p.list(paste0("\nLevene's test for homogeneity of variance",
                     "\n Null hypothesis        H0: \u03C32_x = \u03C32_y",
                     "\n Alternative hypothesis H1: \u03C32_x \u2260 \u03C32_y"),
              type.print=type.print)
    }
    if(type.print=="print"){
      my.p.list(paste0("\nLevene's test for homogeneity of variance",
                       "\n Null hypothesis        H0: \u03C32_x = \u03C32_y",
                       "\n Alternative hypothesis H1: \u03C32_x != \u03C32_y"),
                type.print=type.print)
    }
    print(data.frame(out.var,check.names=FALSE))
    output<-data.frame(out.var,check.names=FALSE)
  }
}#ok

#ok
hyp.diff.indep_unknown<-function(x,y,mdiff0=0,names.xy,
                                 alternative="two.sided",
                                 digits = 2,var.test=F,
                                 type.print="cat" ){
  my.p.list(paste0("Test hypotheses on \u03BC_x-\u03BC_y", 
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
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                   two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                   less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                               " or (\u03BC_x-\u03BC_y) \u2265 ",mdiff0),
                   greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                  " or (\u03BC_x-\u03BC_y) \u2264 ",mdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(\u03BC_x-\u03BC_y) \u2260 ",mdiff0),
                  less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                  greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0),
                     less=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                 " or (\u03BC_x-\u03BC_y) >= ",mdiff0),
                     greater=paste0("(\u03BC_x-\u03BC_y) = ",mdiff0,
                                    " or (\u03BC_x-\u03BC_y) <= ",mdiff0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("(\u03BC_x-\u03BC_y) != ",mdiff0),
                    less=paste0("(\u03BC_x-\u03BC_y) < ",mdiff0),
                    greater=paste0("(\u03BC_x-\u03BC_y) > ",mdiff0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out.eq<-data.frame(n_x=c(n.x,n.x),n_y=c(n.y,n.y),
                     "xbar"=round(c(mean(x),mean(x)),digits),
                     "ybar"=round(c(mean(y),mean(y)),digits),
                     "d=xbar-ybar"=round(c(m.diff,m.diff),digits),
                     "sd_x"=round(c(sqrt(s2.x),sqrt(s2.x)),digits),
                     "sd_y"=round(c(sqrt(s2.y),sqrt(s2.y)),digits),
                     SE=round(c(se.equal,se.equal),digits),
                     Stat=round(c(z.eq,z.eq),digits),
                     "p.value"=round(c(p.zeq,p.teq),4),
                     "p-value"=as.character(round(c(p.zeq,p.teq),4)),
                     check.names=FALSE)
  out.diff<-data.frame(n_x=c(n.x,n.x),n_y=c(n.y,n.y),
                       "xbar"=round(c(mean(x),mean(x)),digits),
                       "ybar"=round(c(mean(y),mean(y)),digits),
                       "d=xbar-ybar"=round(c(m.diff,m.diff),digits),
                       "sd_x"=round(c(sqrt(s2.x),sqrt(s2.x)),digits),
                       "sd_y"=round(c(sqrt(s2.y),sqrt(s2.y)),digits),
                       SE=round(c(se.unequal,se.unequal),digits),
                       Stat=round(c(z.uneq,z.uneq),digits),
                       "p.value"=round(c(p.zuneq,p.tuneq),4),
                       "p-value"=as.character(round(c(p.zuneq,p.tuneq),4)),
                       check.names=FALSE)
  
  out.eq[["p-value"]][out.eq[["p.value"]]<0.0001]<-"<0.0001"
  out.eq[["p.value"]]<-NULL
  out.diff[["p-value"]][out.diff[["p.value"]]<0.0001]<-"<0.0001"
  out.diff[["p.value"]]<-NULL
  rownames(out.eq)<-rownames(out.diff)<-c("Normal.Approx","Student-t")
  
  my.p.list(paste0("\n Unknown variances assumed to be equal"),
            type.print=type.print)
  print(data.frame(out.eq,check.names=FALSE))
  my.p.list(paste0("\n Unknown variances assumed to be different"),
            type.print=type.print)
  print(data.frame(out.diff,check.names=FALSE))
  
  rownames(out.eq)<-rownames(out.diff)<-c()
  out.means=data.frame(Variances=c("Equal","Equal","Different","Different"),
                       Distrib=c("Normal.Approx","Student-t","Normal.Approx","Student-t"),
                       rbind(out.eq,out.diff),check.names=FALSE)
  if(var.test==F){return(out.means)}
  if(var.test==T){
    out.var<-hyp.diff.var(x,y,type="levene",
                          digits=digits,type.print=type.print)
    out<-list(test.means=out.means,test.vars=out.var)
    return(out)
  }
}#ok

#ok
ci.diff.prop<-function(x,y,names.xy,success.x=NULL,success.y=NULL,
                       conf.level =  0.95, digits = 2,
                       type.print="cat"){
  my.p.list(paste0("Confidence interval for p_x-p_y",
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
  colnames(out)<-c("n_x","n_y","pbar_x","pbar_y","d=pbar_x-pbar_y",
                   "sd_x","sd_y","SE",
                   "Lower","Upper")
  out[3:length(out)]<-round(out[3:length(out)],digits)
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}#ok

#ok
hyp.diff.prop<-function(x,y,names.xy,pdiff0=0,success.x=NULL,
                        success.y=NULL,
                        alternative="two.sided", 
                        digits = 2,type.print="cat"){
  my.p.list(paste0("Test hypotheses on p_x-p_y"),
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
  if(type.print=="cat"){
    tit.null<-switch(alternative,
                   two.sided=paste0("(p_x-p_y) = ",pdiff0),
                   less=paste0("(p_x-p_y) = ",pdiff0,
                               " or (p_x-p_y) \u2265 ",pdiff0),
                   greater=paste0("(p_x-p_y) = ",pdiff0,
                                  " or (p_x-p_y) \u2264 ",pdiff0))
  tit.alt<-switch(alternative,
                  two.sided=paste0("(p_x-p_y) \u2260 ",pdiff0),
                  less=paste0("(p_x-p_y) < ",pdiff0),
                  greater=paste0("(p_x-p_y) > ",pdiff0))
  }
  if(type.print=="print"){
    tit.null<-switch(alternative,
                     two.sided=paste0("(p_x-p_y) = ",pdiff0),
                     less=paste0("(p_x-p_y) = ",pdiff0,
                                 " or (p_x-p_y) >= ",pdiff0),
                     greater=paste0("(p_x-p_y) = ",pdiff0,
                                    " or (p_x-p_y) <= ",pdiff0))
    tit.alt<-switch(alternative,
                    two.sided=paste0("(p_x-p_y) != ",pdiff0),
                    less=paste0("(p_x-p_y) < ",pdiff0),
                    greater=paste0("(p_x-p_y) > ",pdiff0))
  }
  my.p.list(c(paste0(" Null hypothesis        H0: ",tit.null),
              paste0(" Alternative hypothesis H1: ",tit.alt)),
            type.print=type.print)
  
  out<-data.frame("n_x"=n.x,"n_y"=n.y,
                  "pbar_x"=round(p.x,digits),
                  "pbar_y"=round(p.y,digits),
                  "d=pbar_x-pbar_y"=round(p.diff,digits),
                  "sd_y"=round(s.y,digits),
                  "sd_x"=round(s.x,digits),
                  "SE"=round(se.xy,digits),
                  Stat=round(z,digits),"p.value"=round(p.z,4),
                  "p-value"=as.character(round(p.z,4)),
                  check.names=FALSE)
  out[["p-value"]][out[["p.value"]]<0.0001]<-"<0.0001"
  out[["p.value"]]<-NULL
  print(data.frame(out,check.names=FALSE),row.names = F)
  output=data.frame(out,check.names=FALSE)
}#ok
