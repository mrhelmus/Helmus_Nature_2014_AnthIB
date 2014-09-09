# Helmus et al. 2014 Nature Island Biogeography of the Anthropocene
# Code to produce Figure 2 in the main text: Past and Present SAR and SIR

#library for segmented regression
require(segmented)

#library for loading xlsx files
#library(xlsx) #use if want to use xlsx from Nature

#Read Data
#dat<-read.xlsx("Helmus_Data_Fig2.xlsx",sheetIndex=1,header=TRUE) #save data as csv and load if problems with xlsx (likely an outdated Java Runtime) 
dat<-read.csv("Helmus_Data_Fig2.csv",header=TRUE,as.is=TRUE)
rownames(dat)<-dat$bank
insitu<-dat$insitu.pa
dat$sr.ex<-dat$present.sr-dat$past.native.sr

#Start Plot Code

plot.sublines<-TRUE #Add the blue and red curves?

#pdf(file="Fig_2_Helmus_Nature2014.pdf", width=10, height=9.5) #!!Uncomment dev.off() at bottom of file
par(mfcol=c(2,2),cex=1.1,las=1,cex=1.6)
par(mar=c(0 ,4.1 ,4.1 ,0))

###################################
####SAR plots

##Past SAR Plot
  x<-log10(dat$a)
  y<-log(dat$past.native.sr+1)

  #Past SAR segmented regression
  lm.nat<-lm(y~x)
  seg.nat<-segmented.lm(lm.nat,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  
  #Check to make sure that the added scalar does not cause the nonlinear relationship
  x<-x[dat$past.native.sr>0]   #remove the three banks without native species
  y<-log(dat$past.native.sr[dat$past.native.sr>0])
  lm.nat<-lm(y~x)
  seg.nat<-segmented.lm(lm.nat,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  #Check OK! Estimated breakpoint not because of the added scalar
  
  #Revert data
  x<-log10(dat$a)
  y<-log(dat$past.native.sr+1)
  #Past SAR segmented regression
  lm.nat<-lm(y~x)
  seg.nat<-segmented.lm(lm.nat,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))

#Start plot
  plot(x,y,ylim=c(0,4.5),ylab="past richness",xlab="",xaxt="n",type="n",yaxt="n")
  axis(side=2,at=0:4,labels=c(expression(paste(italic(e)^{0})),expression(paste(italic(e)^{1})),expression(paste(italic(e)^{2}))
  ,expression(paste(italic(e)^{3})),expression(paste(italic(e)^{4}))))
  points(x[insitu==0],y[insitu==0],bg="white",pch=21,col="blue")
  points(x[insitu>0],y[insitu>0],bg="white",pch=21,col="red")
  plot(seg.nat,add=TRUE,lwd=3,rug=FALSE)
  r.s<-summary(seg.nat)$r.s #Rsquared in legend
  
  #plot breakpoint
  v<-confint(seg.nat)
  points(v$x[1],c(0),pch=8,cex=.7)
  arrows(v$x[2],c(0),v$x[3],c(0),angle=90,length=0.0007,code=3,lwd=.8)

  g<-cbind(x,y)
  g<-g[insitu==1,]
  xx<-g[,1]
  yy<-g[,2]

  fline <- function(object,col.="black",lwd.=1,lty.=1) {  #a pretty abline function
      # ``fline'' <--> fitted line.
      r <- range(object$model[,2])
      d <- data.frame(r)
      names(d) <- attr(object$terms,"term.labels")
      y <- predict(object,d)
      lines(r,y, col=col.,lty=lty.,lwd=lwd.)
  }

  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="red")}
  r.s<-c(r.s,summary(klm)$r.s)

  #banks wo in situ
  g<-cbind(x,y)
  rownames(g)<-rownames(dat)
  g<-g[insitu==0,]
  xx<-g[,1]
  yy<-g[,2]

  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="blue")}
  r.s<-c(r.s,summary(klm)$r.s)
  text(0.2,4.3,"a",cex=1.2)

##Present SAR
  #first check for a breakpoint
  x<-log10(dat$a)
  y<-log(dat$present.sr+1)
  lm.all<-lm(y~x)
  seg.all<-segmented.lm(lm.all,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  #Note that the model does not converge and breakpoint estimates not reliable.
  #Therefore, check for convergence without the added scalar 1
  #(only added because added 1 in past richness, as there are zeros).
  y<-log(dat$present.sr)
  lm.all<-lm(y~x)
  seg.all<-segmented.lm(lm.all,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  #Note that no breakpoints are estimated, conclusion: the nonlinear SAR has been eliminated.
  
  #Plot present SAR
  par(mar=c(4.1 ,4.1 ,0 ,0))
  y<-log(dat$present.sr+1) #add back in the scalar for plotting to keep y-axis in past and present SAR on same scale
  plot(x,y,ylim=c(0,4.5),ylab="present richness",xlab=expression(area ~ km^{2}),xaxt="n", type="n",yaxt="n")
  axis(side=1,at=0:5,labels=c(expression(paste("10"^{0})),expression(paste("10"^{1})),expression(paste("10"^{2}))
  ,expression(paste("10"^{3})),expression(paste("10"^{4})),expression(paste("10"^{5}))))
  axis(side=2,at=0:4,labels=c(expression(paste(italic(e)^{0})),expression(paste(italic(e)^{1})),expression(paste(italic(e)^{2}))
  ,expression(paste(italic(e)^{3})),expression(paste(italic(e)^{4}))))
  
  #Plot points
  bg.<-rep("white",length(insitu>0))
  bg.[apply(cbind(dat$sr.ex>0,insitu>0),1,all)]<-"red"
  bg.[apply(cbind(dat$sr.ex>0,insitu==0),1,all)]<-"blue"
  col.<-bg.
  col.[insitu>0]<-"red"
  col.[insitu==0]<-"blue"
  points(x,y,pch=21,bg=bg.,col=col.)
  
  ##Plot curves
  lm.all<-lm(y~x)
  fline(lm.all,lwd.=3)
  r.s<-c(r.s,summary(lm.all)$r.s)

  ###banks w in situ
  g<-cbind(x,y)
  g<-g[insitu==1,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="red")}
  r.s<-c(r.s,summary(klm)$r.s)

  ###banks wo in situ
  g<-cbind(x,y)
  g<-g[insitu==0,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="blue")}
  r.s<-c(r.s,summary(klm)$r.s)

  text(0.2,4.3,"b",cex=1.2)

  n.r.s<-c("s.f","s.w","s.o","c.f","c.w","c.o")

###################################
####SIR plots

  #Past SIR
  x<-rowMeans(cbind(dat$isolation.1,dat$isolation.2,dat$isolation.3))
  y<-log(dat$past.native.sr+1)
  lm.nat<-lm(y~x)
  seg.nat<-segmented.lm(lm.nat,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  #No breakpoint estimated, thus use linear model

  par(mar=c(0 ,0 ,4.1 ,4.1))
  plot(x,y,ylim=c(0,4.5),ylab="",xlab="", yaxt="n",xaxt="n",pch=21,bg="white",type="n")
  points(x[insitu==0],y[insitu==0],bg="white",pch=21,col="blue")
  points(x[insitu>0],y[insitu>0],bg="white",pch=21,col="red")

  #full
  fline(lm.nat,lty.=1,lwd.=3)
  r.s<-c(r.s,summary(lm.nat)$r.s)
  
  #with in situ
  g<-cbind(x,y)
  g<-g[insitu>0,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="red")}
  r.s<-c(r.s,summary(klm)$r.s)

  #wo in situ
  g<-cbind(x,y)
  g<-g[insitu==0,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="blue")}
  r.s<-c(r.s,summary(klm)$r.s)

  text(1.3,4.3,"c",cex=1.2)

  #Present SIR
  y<-log(dat$present.sr+1)
  lm.all<-lm(y~x)
  seg.all<-segmented.lm(lm.all,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))
  #No breakpoint estimated, use linear model
  
  #plot
  par(mar=c(4.1 ,0 ,0 ,4.1))
  plot(x,y,ylim=c(0,4.5),ylab="",xlab="isolation",yaxt="n",pch=21,bg="white",xaxt="n",type="n")
  axis(side=1,at=seq(-2,1.5,1),labels=seq(-2.0,1.5,1))
  bg.<-rep("white",length(insitu>0))
  bg.[apply(cbind(dat$sr.ex>0,insitu>0),1,all)]<-"red"
  bg.[apply(cbind(dat$sr.ex>0,insitu==0),1,all)]<-"blue"
  col.<-bg.
  col.[insitu>0]<-"red"
  col.[insitu==0]<-"blue"
  points(x[insitu>0],y[insitu>0],pch=21,bg=bg.)
  points(x,y,pch=21,bg=bg.,col=col.)
  
  #full
  fline(lm.all,lty.=1,lwd.=3)
  r.s<-c(r.s,summary(lm.all)$r.s)

  #banks w in situ  fit regression line
  g<-cbind(x,y)
  g<-g[insitu>0,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="red")}
  r.s<-c(r.s,summary(klm)$r.s)

  #banks wo in situ  fit regression line
  g<-cbind(x,y)
  g<-g[insitu==0,]
  xx<-g[,1]
  yy<-g[,2]
  if(plot.sublines){fline(klm<-lm(yy~xx),lty.=1,col.="blue")}
  r.s<-c(r.s,summary(klm)$r.s)

  text(1.3,4.3,"d",cex=1.2)
  n.r.s<-c(n.r.s,"s.f.i","s.w.i","s.o.i","c.f.i","c.w.i","c.o.i")

  #dev.off()
