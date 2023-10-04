# Functions specific for tables and plots -----
pretty.max<-function(tab,type){
  if(isTRUE(missing(type))){
    use.max.y<-max(pretty(max(tab,na.rm = TRUE)))
  } else {
    if(type=="Counts" & max(tab)>100){use.max.y<-max(pretty(max(tab)))}
    if(type=="Percentages" | (type=="Counts" & max(tab)<=100)){
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

chkcon.des<-function(type=NULL,err.list=NULL,warn.list=NULL,x=NULL,
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
                                  "\n   "," -> to force the procedure transform 'by' into a factor"))
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
                                  "\n   "," -> to force the procedure transform 'by' into a factor"))
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
  #return(out)
}

## Function to extract endpoints for a variable classified in intervals
chk.interval<-function(obs.x,consistency = FALSE,warn.list=NULL,
                       err.list=NULL,list.print=NULL,suffix = FALSE){
  if(!is.list(warn.list)){warn.list<-list()}
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(list.print)){list.print<-list()}
  name.x<-deparse1(substitute(obs.x))
  
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
    #list.print[[length(list.print)+1]]<-paste0("Intervals endpoints detected and used for '",name.x,"'\n")
    list.print[[length(list.print)+1]]<-paste0("Intervals endpoints detected and used for '",name.x,"'")
    if(consistency){
      df.ends.d<-as.matrix(t(df.ends[,c("Low","Up")]))
      colnames(df.ends.d)<-df.ends$Obs
      #list.print[[length(list.print)+1]]<-df.ends.d
      list.print[[length(list.print)+1]]<-data.frame(df.ends.d,check.names = F)
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

## Function to classify a veriable into intervals
chk.breaks<-function(x,breaks,adj.breaks,consistency = TRUE,
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
    # changed
    r.min<-min(x,na.rm = TRUE)
    r.max<-max(x,na.rm = TRUE)
    r.by<-(r.max-r.min)/breaks
    brk<-seq(r.min,r.max,by=r.by)
    i.x<-cut(x,breaks=brk,include.lowest = TRUE,right = FALSE)
    levels(i.x)<-paste0(rep("[",length(brk)-1),
                        round(brk[-length(brk)],5),",",round(brk[-1],5),
                        c(rep(")",length(brk)-2),"]"))
    breaks<-brk
    
    #end changed
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


build.Xlist<-function(x,breaks,interval,adj.breaks,consistency = FALSE,
                      err.list=NULL,warn.list=NULL,list.print=NULL,suffix = FALSE){
  if(!is.list(err.list)){err.list<-list()}
  if(!is.list(warn.list)){warn.list<-list()}
  if(!is.list(list.print)){list.print<-list()}
  
  name.x<-deparse1(substitute(x))
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
      check.breaks<-chk.breaks(x=x,breaks=breaks,adj.breaks=adj.breaks,
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
      check.int<-chk.interval(obs.x=x,consistency=consistency,
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

# Function to arrange multiple plots in a 
# graphical window
#' @importFrom grDevices n2mfrow
find.layout<-function(n.plots,nrows=NULL,ncols=NULL,square = TRUE,
                      err.list=NULL,warn.list=NULL){
  dim.grid<-NULL
  if(!is.list(err.list)){err.list<-list()}
  if(is.null(nrows) & is.null(ncols)){
    if(!square){dim.grid<-grDevices::n2mfrow(n.plots)}
    if(square){
      r.grid<-ceiling(sqrt(n.plots))
      c.grid<-ceiling(n.plots/r.grid)
      dim.grid<-c(r.grid,c.grid)}
  } else if(!(is.null(nrows)) & !(is.null(ncols))){
    if(nrows<=0 | ncols<=0){
      err.list<-c(err.list,"'nrows' and 'ncols' should be higher than 0")
    } else {
      dim.grid<-c(nrows,ncols)
      if(dim.grid[1]*dim.grid[2] < n.plots){
        warn.list<-c(warn.list,paste0("Using the specified 'nrows' and 'ncols' the required plots",
                                      "\n  ","    will be arranged in multiple output windows"))
      }}
  } else if(!(is.null(nrows))){dim.grid<-c(nrows,ceiling(n.plots/nrows))
  } else if(!(is.null(ncols))){dim.grid<-c(ceiling(n.plots/ncols),ncols)}
  out<-list(dim.grid=dim.grid,err.list=err.list,warn.list=warn.list)
}

## Functions to obtain different plots ------
# the single functions to obtain specific plots
#' @importFrom grDevices gray.colors
#' @importFrom grDevices rainbow
plt.x.pie<-function(tab,bw = TRUE,color=NULL,name.x,freq){
  if(bw){use.color <- grDevices::gray.colors(length(tab))} 
  if(!bw & is.null(color)){use.color <- grDevices::rainbow(length(tab))} 
  if(!(is.null(color))){use.color<-color
  if(length(use.color)<length(tab)){
    use.color <- grDevices::rainbow(length(tab))}}
  mai.p<-par("mai")
  par(mai=c(0,0,0.1,0))
  pie(tab,col=use.color,clockwise = TRUE)
  mtext(side=3,paste0("Pie chart: ",name.x),font=2,
        cex=par("cex.main"),line=-1)
  par(mai=mai.p)
}
plt.x.bars<-function(tab,bw = TRUE,color=NULL,name.x,freq){
  if(bw){use.color <- "grey"} 
  if(!bw & is.null(color)){use.color <- "skyblue"} 
  if(!(is.null(color))){use.color<-color}
  use.max.y<-pretty.max(tab,freq)
  if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  barplot(tab,col=use.color,main=paste0("Bar plot: ",name.x),
          ylim=c(0,use.max.y))
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = 2.3,las=0)
}
plt.x.spike<-function(xlist,color=NULL,name.x,freq){
  use.color<-"black"
    if(!(is.null(color))){use.color<-color[1]}
  tab<-xlist$tab.x
  use.max.y<-pretty.max(tab,freq)
  if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  par(las=mylas)
  if(xlist$isnum & xlist$class=="standard"){
    x.values<-as.numeric(names(tab))
    plot(x.values,y=as.vector(tab),type="p",main=paste0("Spike plot: ",name.x),
         ylim=c(0,use.max.y),xlab="",ylab="",lwd=2,pch=16,
         xlim=c(min(x.values),max(x.values))) 
    lines(tab)
  } else {
    x.values<-factor(names(tab),levels=names(tab))
    plot(tab,main=paste0("Spike plot: ",name.x),type="p",pch=16,
         ylim=c(0,use.max.y),xlab="",ylab="",lwd=2) 
    lines(tab)
  }
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = 2.3,las=0)
}


plt.x.cum.OLD<-function(xlist,color=NULL,name.x,freq,adj.breaks){
  use.color<-c("black")
  if(!(is.null(color))){use.color<-color[1]}
  
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
    use.max.y<-pretty.max(Cum.freq,freq)
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    par(las=mylas)
    if(!adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),col=use.color)
      points(x=x.values,y=Cum.freq,pch=16,col=use.color)}
    if(adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),col=use.color,axes = FALSE)
      points(x=x.values,y=Cum.freq,pch=16,col=use.color)
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      axis(2, at=aty, labels=format(aty, scientific = FALSE), 
           hadj=0.9)#, cex.axis=0.88)
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      axis(1, at=atx, labels=format(atx, scientific = FALSE), 
           hadj=0.9)#, cex.axis=0.88)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = 2.3,las=0)
  }
  if(xlist$class=="standard"){
    tab<-xlist$tab.x
    Cum.freq=as.numeric(cumsum(tab))
    use.max.y<-pretty.max(Cum.freq,freq)
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    par(las=mylas)
    
    if(!xlist$isnum){
      plot(x=factor(names(tab),levels=names(tab)),y=Cum.freq,type="l",
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
        plot(x=c(min(x.values),x.values),y=c(0,Cum.freq),type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,col=use.color)
        points(x=x.values,y=Cum.freq,pch=16,col=use.color)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col=use.color,lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lwd =1.5)
      }
      if(adj.breaks){
        plot(x=c(min(x.values),x.values),y=c(0,Cum.freq),type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,
             axes = FALSE,col=use.color)
        points(x=x.values,y=Cum.freq,pch=16,col=use.color)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col=use.color,lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lwd =1.5)
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
        p.xaxp<-par("xaxp")
        atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
        axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      }
      box()
    }
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = 2.3,las=0)
  }
}

plt.x.cum<-function(xlist,color=NULL,name.x,freq,adj.breaks){
  use.color<-c("black")
  if(!(is.null(color))){use.color<-color[1]}
  
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
    # changed:
    #use.max.y<-pretty.max(Cum.freq,freq)
    use.max.y<-switch(freq,Counts=pretty.max(Cum.freq,freq),
                Proportions=as.integer(1),
                Percentages=as.integer(100))
    #end changed
    #changed
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    mylas<-1
    if(freq=="Counts"){
      #if(max(nchar(as.character(use.max.y)))>5){mylas=0}
      if(!adj.breaks){
        if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
      if(adj.breaks){
        if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
          mylas=1} else{mylas=0}}
    }
    #end changed
    par(las=mylas)
    if(!adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),col=use.color)
      points(x=x.values,y=Cum.freq,pch=16,col=use.color)}
    if(adj.breaks){
      plot(x=x.values,y=Cum.freq,type="l",main=paste0("Ogive: ",name.x),
           xlab="",ylab="",lwd=2,ylim=c(0,use.max.y),col=use.color,axes = FALSE)
      points(x=x.values,y=Cum.freq,pch=16,col=use.color)
      
      #changed
      # p.yaxp<-par("yaxp")
      # aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      # axis(2, at=aty, labels=format(aty, scientific = FALSE), 
      #      hadj=0.9)#, cex.axis=0.88)
      if(freq!="Counts"){
        p.yaxp<-par("yaxp")
        aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        axis(2, at=aty)
      }
      if(freq=="Counts"){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      axis(2, at=aty, labels=format(aty, scientific = FALSE)) 
            #hadj=0.9)#, cex.axis=0.88)
      }
      #end changed
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      #changed
      #axis(1, at=atx, labels=format(atx, scientific = FALSE), 
      #     hadj=0.9)#, cex.axis=0.88)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = 2.3,las=0)
  }
  if(xlist$class=="standard"){
    tab<-xlist$tab.x
    Cum.freq=as.numeric(cumsum(tab))
    # changed:
    #use.max.y<-pretty.max(Cum.freq,freq)
    use.max.y<-switch(freq,Counts=pretty.max(Cum.freq,freq),
                      Proportions=as.integer(1),
                      Percentages=as.integer(100))
    #end changed
    #changed
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    mylas<-1
    if(freq=="Counts"){
      if(max(nchar(as.character(use.max.y)))>5){mylas=0}
    }
    #end changed
    par(las=mylas)
    if(!xlist$isnum){
      plot(x=factor(names(tab),levels=names(tab)),y=Cum.freq,type="l",
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
        plot(x=c(min(x.values),x.values),y=c(0,Cum.freq),type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,col=use.color)
        points(x=x.values,y=Cum.freq,pch=16,col=use.color)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col=use.color,lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lwd =1.5)
      }
      if(adj.breaks){
        plot(x=c(min(x.values),x.values),y=c(0,Cum.freq),type="s",
             xlab="",ylab="",lwd=1.5,
             main=paste0("Cumulative freq: ",name.x),
             ylim=c(0,use.max.y),xlim=use.xlim,
             axes = FALSE,col=use.color)
        points(x=x.values,y=Cum.freq,pch=16,col=use.color)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = max(use.x.values)+dr, y1 = 0,
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lty =2, lwd =1.5)
        segments(x0=min(use.x.values)-dr, y0=0, 
                 x1 = min(x.values), y1 = 0,
                 col=use.color,lwd =1.5)
        segments(x0=max(x.values), y0=max(Cum.freq), 
                 x1 = max(use.x.values)+dr, y1 = max(Cum.freq),
                 col=use.color,lwd =1.5)
        #changed
        # p.yaxp<-par("yaxp")
        # aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
        # axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
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
        #end changed
        
        p.xaxp<-par("xaxp")
        atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
        #changed
#        axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
        use.labs<-format(atx, scientific = FALSE)
        use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
        axis(1, at=atx, labels=use.labs)
        #end changed
      }
      box()
    }
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, paste0("Cumulative ",freq), line = 2.3,las=0)
  }
}

plt.x.hist<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  if(bw){use.color <- "grey"} 
  if(!bw & is.null(color)){use.color <- "skyblue"} 
  if(!(is.null(color))){use.color<-color}
  
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
  use.max.y<-pretty.max(h$density,freq)
  # changed
  #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  if(!adj.breaks){
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
  if(adj.breaks){
    if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
      mylas=1} else{mylas=0}}
  #end changed
  
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
    # changed
    #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
    axis(2, at=aty, labels=format(aty, scientific = FALSE))
    #end changed
    p.xaxp<-par("xaxp")
    atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
    #changed
    # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
    use.labs<-format(atx, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(1, at=atx, labels=use.labs)
    #end changed
  }
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = 2.3,las=0)
}

plt.x.density<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  use.color<-c("black")
  if(!(is.null(color))){use.color<-color[1]}
  
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
  use.max.y<-pretty.max(d$y,freq)
  # changed
  #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
  if(!adj.breaks){
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
  if(adj.breaks){
    if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
      mylas=1} else{mylas=0}}
  #end changed

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
    # changed
    #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
    axis(2, at=aty, labels=format(aty, scientific = FALSE))
    #end changed
    
    p.xaxp<-par("xaxp")
    atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
    #changed
    # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
    use.labs<-format(atx, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(1, at=atx, labels=use.labs)
    #end changed
    }
  box()
  mtext(side = 1, name.x, line = 2)
  mtext(side = 2, freq, line = 2.3,las=0)
}

plt.x.boxplot<-function(xlist,bw = TRUE,color=NULL,name.x,freq,adj.breaks){
  if(bw){use.color <- "grey"} 
  if(!bw & is.null(color)){use.color <- "skyblue"} 
  if(!(is.null(color))){use.color<-color}
  
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
    #changed
    #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
    use.labs<-format(aty, scientific = FALSE)
    use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
    axis(2, at=aty, labels=use.labs)
    box()
  }
  mtext(side = 2, name.x, line = 2.3,las=0)
}  

#' @importFrom grDevices gray.colors
#' @importFrom grDevices rainbow
plt.xy.crossbars<-function(tab,bw = TRUE,color=NULL,name.x,name.y,freq,legend,
                           beside = FALSE,use.tit=NULL,switch.xy = FALSE,
                           use.par=NULL){
  if(bw){use.color <- grDevices::gray.colors(nrow(tab))} 
  if(!bw & is.null(color)){use.color <- grDevices::rainbow((nrow(tab)))} 
  if(!(is.null(color))){
    use.color<-color
    if(length(use.color)<nrow(tab)){
      use.color <- grDevices::rainbow(nrow(tab))}
  }
  names(use.color)<-rownames(tab)
  if(beside){
    bar.space=c(0,1)
    use.max.y<-pretty.max(tab,freq)}
  if(!beside){
    bar.space<-0.2
    use.max.y<-pretty.max(max(apply(tab,2,sum,na.rm = TRUE)),freq)
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
      mtext(side = 2, freq, line = 2.3,las=0)
    }
    if(legend){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
      leg.text<-max((strwidth(paste0(rownames(tab),"aA"),units="fig")))
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
      mtext(side = 2, freq, line = 2.5, las=0)
      
      leg.par<-rep(0,4)
      leg.par[3]<-mypar[3]+0.1
      par(mar=leg.par,tck=use.par$tck,tcl=use.par$tcl,
          mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
      plot(1:10,1:10,type="n",axes = FALSE)
      legend("topleft", legend=rownames(tab), 
             pch=21,pt.bg=use.color,pt.cex=1,bty="n",x.intersp = 0.5,
             cex=use.par$cex.axis,title=name.y,xjust=0,title.adj = 0)    
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
      leg.text<-max((strwidth(paste0(rownames(tab),"aA"),units="fig")))
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
             cex=use.par$cex.axis,title=name.x,xjust=0,title.adj=0)    
    }
  }
}

plt.xy.scatter<-function(xlist,ylist,bw = TRUE,color=NULL,name.x,name.y,
                         adj.breaks = TRUE,fitline = FALSE,use.par=NULL){
  if(xlist$class=="interval" | xlist$class=="breaks"){
    xlist$isnum <- FALSE  }
  if(ylist$class=="interval" | ylist$class=="breaks"){
    ylist$isnum <- FALSE  }
  use.color2<-"black"
  use.color1 <- "grey"  
  if(!bw & is.null(color)){use.color1 <- "skyblue"} 
  if(!(is.null(color))){use.color1<-color}
  
  # two numeric variables
  if(xlist$isnum & ylist$isnum){
    use.max.x<-pretty.max(xlist$V)
    use.max.y<-pretty.max(ylist$V)
    #changed
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    #end changed
    
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    if(!adj.breaks){
      plot(x=xlist$V,y=ylist$V,main=paste0("Scatter: ",name.x," , ",name.y),
           xlab="",ylab="",pch=16,col=use.color2)
      if(fitline){
        abline(lm(ylist$V ~ xlist$V),col="red")}
    }
    if(adj.breaks){
      plot(x=xlist$V,y=ylist$V,main=paste0("Scatter: ",name.x," , ",name.y),
           xlab="",ylab="",pch=16,col=use.color2,axes = FALSE)
      if(fitline){
        abline(lm(ylist$V ~ xlist$V),col="red")}
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      # changed
      #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      #end changed
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      #changed
      # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
  }
  if(!xlist$isnum & !ylist$isnum){
    tab<-data.frame(prop.table(table((xlist$V.f),ylist$V.f)))
    tab<-tab[tab$Freq>0,]
    #chenged
    #tab$FreqP<-(tab$Freq-min(tab$Freq))/(max(tab$Freq)-min(tab$Freq))
    tab$FreqP<-(tab$Freq)/(max(tab$Freq))
    tab$xnum<-as.numeric(tab$Var1)
    tab$ynum<-as.numeric(tab$Var2)
    #if(max(nchar(levels(ylist$V.f)))<=5){mylas=1} else{mylas=0}
    mypar<-use.par$mar
    mypar[2]<-1.5+max(ceiling(strwidth(levels(ylist$V.f),units="inc")*5))
    par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    plot(x=tab$xnum,y=tab$ynum,xlim=c(0.5,max(tab$xnum)+0.5),
         ylim=c(0.5,max(tab$ynum)+0.5),
         main=paste0("Scatter: ",name.x," , ",name.y),
         xlab="",ylab="",cex=(0.5+9.5*tab$FreqP),pch=21,
         bg=use.color1[1],axes = FALSE)
    axis(1, at=1:length(levels(tab$Var1)), labels=levels(tab$Var1))
    axis(2, at=1:length(levels(tab$Var2)), labels=levels(tab$Var2),
         las=1)
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  if(xlist$isnum & !ylist$isnum){
    y.num<-as.numeric(ylist$V.f)
    #if(max(nchar(levels(ylist$V.f)))<=5){mylas=1} else{mylas=0}
    mypar<-use.par$mar
    mypar[2]<-1.5+max(ceiling(strwidth(levels(ylist$V.f),units="inc")*5))
    par(mar=mypar,tck=use.par$tck,tcl=use.par$tcl,las=1,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    plot(x=xlist$V,y=y.num,xlab="",ylab="",
         main=paste0("Scatter: ",name.x," , ",name.y),
         pch=16,bg=use.color2[1],axes = FALSE)
    axis(2, at=1:length(levels(ylist$V.f)), labels=levels(ylist$V.f),
         las=1)
    if(!adj.breaks){axis(1) }
    if(adj.breaks){
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      #changed
      # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  if(!xlist$isnum & ylist$isnum){
    x.num<-as.numeric(xlist$V.f)
    use.max.y<-pretty.max(ylist$V)
    #changed
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    #end changed
    
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    plot(x=x.num,y=ylist$V,xlab="",ylab="",
         main=paste0("Scatter: ",name.x," , ",name.y),
         pch=16,bg=use.color2[1],axes = FALSE)
    axis(1, at=1:length(levels(xlist$V.f)), labels=levels(xlist$V.f))
    if(!adj.breaks){axis(2, las=mylas)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      # changed
      #axis(2, at=aty, labels=format(aty, scientific = FALSE), 
      #     hadj=0.9,las=mylas)
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
  }
}

plt.xy.boxplot.old<-function(xlist,ylist,bw = TRUE,color=NULL,name.x,name.y,
                         adj.breaks = TRUE,switch.xy = FALSE,use.par=NULL){
  if(bw){use.color <- "grey"} 
  if(!bw & is.null(color)){use.color <- "skyblue"} 
  if(!(is.null(color))){use.color<-color}
  
  if(xlist$class=="interval" | xlist$class=="breaks"){
    xlist$isnum <- FALSE  }
  if(ylist$class=="interval" | ylist$class=="breaks"){
    ylist$isnum <- FALSE  }
  
  # two numeric variables
  if(xlist$isnum & ylist$isnum & !switch.xy){
    use.max.x<-pretty.max(xlist$V)
    use.max.y<-pretty.max(ylist$V)
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    if(!adj.breaks){
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",col=use.color)    }
    if(adj.breaks){
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",axes = FALSE,col=use.color)    
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
    
  }
  if(xlist$isnum & ylist$isnum & switch.xy){
    use.max.x<-pretty.max(xlist$V)
    use.max.y<-pretty.max(ylist$V)
    
    if(max(nchar(as.character(use.max.x)))<=5){mylas=1} else{mylas=0}
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    if(!adj.breaks){
      boxplot(xlist$V~ylist$V,main=paste0(name.x," | ",name.y),
              xlab="",ylab="",col=use.color,horizontal = TRUE)    }
    if(adj.breaks){
      boxplot(xlist$V~ylist$V,main=paste0(name.x," | ",name.y),
              xlab="",ylab="",axes = FALSE,col=use.color,horizontal = TRUE)    
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
    
  }
  if(xlist$isnum & !ylist$isnum){
    use.max.x<-pretty.max(xlist$V)
    y.num<-as.numeric(ylist$V.f)
    #if(max(nchar(levels(ylist$V.f)))<=5){mylas=1} else{mylas=0}
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
      axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  if(!xlist$isnum & ylist$isnum){
    x.num<-as.numeric(xlist$V.f)
    use.max.y<-pretty.max(ylist$V)
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    boxplot(ylist$V~x.num,main=paste0(name.y," | ",name.x),
            xlab="",ylab="",axes = FALSE,col=use.color)    
    axis(1, at=1:length(levels(xlist$V.f)), labels=levels(xlist$V.f))
    if(!adj.breaks){axis(2, las=mylas)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      axis(2, at=aty, labels=format(aty, scientific = FALSE), 
           hadj=0.9,las=mylas)
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
  }
}

#' @importFrom grDevices rainbow
plt.xy.boxplot<-function(xlist,ylist,bw = TRUE,color=NULL,name.x,name.y,
                         adj.breaks = TRUE,switch.xy = FALSE,use.par=NULL){
  if(bw){use.color <- "grey"} 
  if(!bw & is.null(color)){use.color <- "skyblue"} 
  if(!(is.null(color))){use.color<-color}
  
  if(xlist$class=="interval" | xlist$class=="breaks"){
    xlist$isnum <- FALSE  }
  if(ylist$class=="interval" | ylist$class=="breaks"){
    ylist$isnum <- FALSE  }
  
  # two numeric variables
  if(xlist$isnum & ylist$isnum & !switch.xy){
    use.max.x<-pretty.max(xlist$V)
    use.max.y<-pretty.max(ylist$V)
    #changed
    if(!adj.breaks){
    if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    #end changed
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis)
    
    if(!adj.breaks){
      lev.x<-unique(xlist$V)
      lev.x<-lev.x[order(lev.x)]
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",col=use.color,
              at=lev.x)    }
    if(adj.breaks){
      lev.x<-unique(xlist$V)
      lev.x<-lev.x[order(lev.x)]
      boxplot(ylist$V~xlist$V,main=paste0(name.y," | ",name.x),
              xlab="",ylab="",at=lev.x,axes = FALSE,col=use.color)    
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      # changed
      #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      #end changed
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      #changed
      # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
    
  }
  if(xlist$isnum & ylist$isnum & switch.xy){
    use.max.x<-pretty.max(xlist$V)
    use.max.y<-pretty.max(ylist$V)
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
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
      # changed
      #axis(2, at=aty, labels=format(aty, scientific = FALSE),hadj=0.9)
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      #end changed
      p.xaxp<-par("xaxp")
      atx<-seq(p.xaxp[1],p.xaxp[2],(p.xaxp[2]-p.xaxp[1])/p.xaxp[3])
      #changed
      # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
    
  }
  if(xlist$isnum & !ylist$isnum){
    use.max.x<-pretty.max(xlist$V)
    y.num<-as.numeric(ylist$V.f)
    # if(!adj.breaks){
    #   if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    # if(adj.breaks){
    #   if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
    #     mylas=1} else{mylas=0}}
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
      #changed
      # axis(1, at=atx, labels=format(atx, scientific = FALSE),hadj=0.9)
      use.labs<-format(atx, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(1, at=atx, labels=use.labs)
      #end changed
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = mypar[2]-1,las=0)
  }
  if(!xlist$isnum & ylist$isnum){
    x.num<-as.numeric(xlist$V.f)
    use.max.y<-pretty.max(ylist$V)
    if(!adj.breaks){
      if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}}
    if(adj.breaks){
      if(max(nchar(as.character(format(use.max.y,scientific = FALSE))))<=5){
        mylas=1} else{mylas=0}}
    #if(max(nchar(as.character(use.max.y)))<=5){mylas=1} else{mylas=0}
    
    par(mar=use.par$mar,tck=use.par$tck,tcl=use.par$tcl,las=mylas,
        mgp=use.par$mgp,cex=use.par$cex,cex.axis=use.par$cex.axis) 
    boxplot(ylist$V~x.num,main=paste0(name.y," | ",name.x),
            xlab="",ylab="",axes = FALSE,col=use.color)    
    axis(1, at=1:length(levels(xlist$V.f)), labels=levels(xlist$V.f))
    if(!adj.breaks){axis(2, las=mylas)}
    if(adj.breaks){
      p.yaxp<-par("yaxp")
      aty<-seq(p.yaxp[1],p.yaxp[2],(p.yaxp[2]-p.yaxp[1])/p.yaxp[3])
      # changed
      #axis(2, at=aty, labels=format(aty, scientific = FALSE), 
      #     hadj=0.9,las=mylas)
      use.labs<-format(aty, scientific = FALSE)
      use.labs<-gsub("^\\s+|\\s+$", "", use.labs)
      axis(2, at=aty, labels=use.labs)
      #end changed
      
    }
    box()
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, name.y, line = 2.3,las=0)
  }
}

#' @importFrom grDevices rainbow
plt.xby.hist<-function(xlist,bylist,name.x,name.by,bw = TRUE,color=NULL,
                       dim.grid=NULL,square = TRUE,legend = TRUE){
  if(!legend){lgd<-NULL} else if(legend){lgd<-"topright"
  } else {lgd<-legend}
  
  n.plots<-length(levels(bylist$V.f)) 
  if(is.null(color)){use.color <- grDevices::rainbow(n.plots)} 
  if(!(is.null(color))){use.color<-color
  if(length(use.color)<n.plots){use.color <- grDevices::rainbow(n.plots)}
  }
  names(use.color)<-levels(bylist$V.f)
  
  # find the variables to analyze
  if(xlist$class=="interval"){
    check.int<-xlist$info.int$tab
    # Adjust to be sure there are no missing intervals
    all.ends<-unique(c(check.int$Low,check.int$Up))
    all.ends<-all.ends[order(all.ends)]
    intm.x<-factor(xlist$V.f,levels=check.int$Obs,
                   labels=((check.int$Low+check.int$Up)/2))
    intm.x<-as.numeric(as.character(intm.x))
    x<-intm.x ; breaks<-all.ends
  } # close interval = TRUE
  
  if(xlist$class=="standard"){
    h<-hist(xlist$V,include.lowest = TRUE,right = FALSE,plot = FALSE)
    x<-xlist$V; breaks<-h$breaks}
  if(xlist$class=="breaks"){
    sel<-(!is.na(xlist$V) & 
            xlist$V>= min(xlist$breaks) & xlist$V<=max(xlist$breaks))
    by<-bylist$V.f[sel]
    x<-xlist$V[sel]
    breaks=xlist$breaks}
  
  max.density<-0
  List.yx<-list()
  for(k in levels(bylist$V.f)){
    x.by<-x[bylist$V.f ==k]
    h<-hist(x.by,breaks=breaks,include.lowest = TRUE,right = FALSE,plot = FALSE)
    max.density<-max(max.density,max(h$density))
    List.yx[[k]]<-x.by
  }
  
  use.max.y<-pretty.max(max.density,"Density")
  par(mfrow=dim.grid,oma=c(0,0,2,0),
      mar=c(1,2,2,0),tck=(-0.01),tcl=NA,mgp=c(0, 0.3, 0))
  hist(List.yx[[1]],freq = FALSE,include.lowest = TRUE,right = FALSE,
       breaks=breaks,main="",
       xlab="",ylab="",ylim=c(0,use.max.y),xlim=c(min(breaks),max(breaks)),
       col=use.color[names(List.yx)[1]])
  mtext(names(List.yx)[1],side=3,line=-0.5,font=2,cex=0.8)
  for(k in names(List.yx)[-1]){
    hist(List.yx[[k]],freq = FALSE,breaks=breaks,
         include.lowest = TRUE,right = FALSE,main="",
         xlab="",ylab="",ylim=c(0,use.max.y),xlim=c(min(breaks),max(breaks)),
         col=use.color[k])
    mtext(k,side=3,line=-0.5,font=2,cex=0.8)
  }
  mtext(paste0(name.x," | ",name.by), line=0, side=3, outer = TRUE,font=2)
}

#' @importFrom grDevices rainbow
plt.xby.dens<-function(xlist,bylist,name.x,name.by,bw = TRUE,color=NULL,
                       dim.grid=NULL,square = TRUE,overlay = FALSE,
                       legend = TRUE){
  if(!legend){lgd<-NULL} else if(legend){lgd<-"topright"
  } else {lgd<-legend}
  
  n.plots<-length(levels(bylist$V.f)) 
  if(is.null(color)){use.color <- grDevices::rainbow(n.plots)} 
  if(!(is.null(color))){
    use.color<-color
    if(length(use.color)<n.plots){use.color <- grDevices::rainbow(n.plots)}
  }
  names(use.color)<-levels(bylist$V.f)
  
  if(xlist$class=="interval"){
    check.int<-xlist$info.int$tab
    set.seed(100)
    x<-NULL
    for(k in 1:length(levels(xlist$V.f))){
      x<-c(x,runif(sum(xlist$V.f== levels(xlist$V.f)[k]),
                   check.int$Low[k],check.int$Up[k]))
    }
    by<-bylist$V.f
  } # close interval = TRUE
  if(xlist$class=="breaks"){
    sel<-(!is.na(xlist$V) & 
            xlist$V>= min(xlist$breaks) & xlist$V<=max(xlist$breaks))
    by<-bylist$V.f[sel]
    x<-xlist$V[sel]
    cat("\n   'breaks.x' ignored in density plots",file=stderr())
  }
  if(xlist$class=="standard"){
    by<-bylist$V.f
    x<-xlist$V
  }
  min.x<-max.x<-NULL
  max.density<-0
  List.yx<-list()
  for(k in levels(by)){
    c.x<-sum(by==k)
    if(c.x>0){
      x.by<-x[by==k]
      d<-density(x.by)
      min.x<-min(min.x,min(d$x))
      max.x<-max(max.x,max(d$x))
      max.density<-max(max.density,max(d$y))
      List.yx[[k]]<-d
    }
  }
  # ready to plot
  use.max.y<-pretty.max(max.density,"Density")
  
  # Overlapping plots:
  if(overlay){
    par(mar=c(3.5,3.5,4.1,2.1),tck=(-0.01),tcl=NA,las=0,
        mgp=c(3, 0.3, 0))
    plot(List.yx[[1]]$x,List.yx[[1]]$y,
         main=paste0(name.x," | ",name.by),
         type="l",xlab="",ylab="",ylim=c(0,use.max.y),xlim=c(min.x,max.x),
         col=use.color[names(List.yx)[1]],lwd=2)
    for(k in names(List.yx)[-1]){
      points(List.yx[[k]]$x,List.yx[[k]]$y,
             type="l",col=use.color[k],lwd=2)}
    mtext(side = 1, name.x, line = 2)
    mtext(side = 2, "Density", line = 2.5, las=0)
    if(!(is.null(lgd))){
      legend(lgd,names(List.yx),pt.bg=use.color[names(List.yx)],pch=22)
    }
  }
  if(!overlay){
    par(mfrow=dim.grid,oma=c(0,0,2,0),
        mar=c(1,2,2,0),tck=(-0.01),tcl=NA,mgp=c(0, 0.3, 0))
    plot(List.yx[[1]]$x,List.yx[[1]]$y,
         type="l",xlab="",ylab="",ylim=c(0,use.max.y),xlim=c(min.x,max.x),
         col=use.color[names(List.yx)[1]],lwd=2)
    mtext(names(List.yx)[1],side=3,line=0,font=2,cex=0.8)
    for(k in names(List.yx)[-1]){
      plot(List.yx[[k]]$x,List.yx[[k]]$y,
           type="l",xlab="",ylab="",ylim=c(0,use.max.y),xlim=c(min.x,max.x),
           col=use.color[k],lwd=2)
      mtext(k,side=3,font=2,cex=0.8)
    }
    mtext(paste0(name.x," | ",name.by), line=0, side=3, outer = TRUE,font=2)
    
  }
}

