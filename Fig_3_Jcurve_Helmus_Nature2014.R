# Helmus et al. 2014 Nature Island Biogeography of the Anthropocene
# Code to produce Figure 3 in the main text: exotic accumulation J-curve

#library for segmented regression
require(segmented)

#data
ecodat<-read.csv("Helmus_Data_Fig3.csv",header=TRUE,as.is=TRUE)

dy<-ecodat[,1]  #year
ac<-ecodat[,2]  #accumulation

#pdf("Fig_3_Helmus_Nature2014.pdf")  #!!Uncomment dev.off() at bottom of file
par(las=1,cex=1.35) #scaling of the plot
plot(dy,ac,ylab="",type="n",xlab="",xlim=c(1850,2012))#,xaxt="n")#,ylim=c(-.2,38))

par(las=3)
mtext("Established exotic anole populations (cumulative)",side=2,line=2.5,cex=1.3)

par(las=1)
mtext("Documented year",side=1,line=2.5,cex=1.3)

#War arrows
arrows(x0=1945,x1=1945,y0=0,y1=1.9,code=1,length=0.08,angle=25,lwd=1.3)
text(1952,2.5,"WWII end",pos=2,cex=0.8)
text(1947,1.1,"(1945)",pos=2,cex=0.70)
arrows(x0=1991,x1=1991,y0=0,y1=3,code=1,length=0.08,angle=25,lwd=1.3)
text(1991,3.9,"Coldwar end",pos=3,cex=0.80)
text(1991,2.5,"(1991)",pos=3,cex=0.70)

#Check order of variables is correct
ii<-order(dy)
x<-dy[ii]
y<-1:length(dy)

#Segmented regression
lm.nat<-lm(y~x)
seg.nat<-segmented.lm(lm.nat,seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0,it.max=1000))

#Plot breakpoints
v<-confint(seg.nat)
points(v$x[,1],c(0,0),pch=8,cex=.8)
arrows(v$x[,2],c(0,0),v$x[,3],c(0,0),angle=90,length=0.0007,code=3)

#Plot data
cols<-rep("white",length(dy))
points(dy[ii],(1:length(dy)),bg=cols[ii],pch=21,cex=1)

#Plot curve
plot(seg.nat,add=TRUE,lwd=3,rug=FALSE)

#dev.off()
