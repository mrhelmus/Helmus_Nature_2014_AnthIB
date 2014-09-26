# Helmus et al. 2014 Nature Island Biogeography of the Anthropocene
# Code to produce Table 2 in the main text: Economic isolation

#library for loading xlsx files
#library(xlsx) #use if want to use xlsx from Nature
require(car)

#Read Data
#dat<-read.xlsx("Helmus_Data_Table2.xlsx",sheetIndex=1,header=TRUE) #save data as csv and load if problems with xlsx (likely an outdated Java Runtime) 
dat<-read.csv("Helmus_Data_Table2.csv",header=TRUE,as.is=TRUE)
rownames(dat)<-dat$bank

#########################Regressios#################################################################################

  #Exotic Species Richness
lm.dat.best<-step(lm(scale(log(exotic.sr+1))~scale((log(area.km2)))+scale(isolation.1)+scale(isolation.2)+scale(isolation.3) + scale(economic.isolation),data=dat))
per<-(Anova(lm.dat.best)$S)/sum(Anova(lm.dat.best)$S)  #Anova(lm.dat)
per<-rbind(per,Anova(lm.dat.best)$P)
per<-rbind(per,Anova(lm.dat.best)$S)
per.e<-rbind(per,coef(lm.dat.best))
rownames(per.e)<-c("per","p","ss","c")

  #Present Species Richness
lm.dat.best<-step(lm(scale(log(present.sr+1))~scale((log(area.km2)))+scale(isolation.1)+scale(isolation.2)+scale(isolation.3) + scale(economic.isolation),data=dat))
per.<-(Anova(lm.dat.best)$S)/sum(Anova(lm.dat.best)$S)  #Anova(lm.dat)
per.<-rbind(per.,Anova(lm.dat.best)$P)
per.<-rbind(per.,Anova(lm.dat.best)$S)
per.a<-rbind(per.,coef(lm.dat.best))
rownames(per.a)<-c("per","p","ss","c")

ee<-c(NA,NA,per.e["ss",],NA)
e.t<-sum(ee,na.rm=TRUE)
e.i<-sum(ee[3:4],na.rm=TRUE)
ee<-c(round(ee,2),round(100*e.i/e.t),round(100*ee[5]/e.t))
aa<-c(per.a["ss",1:2],NA,NA,per.a["ss",3:4])
a.t<-sum(aa,na.rm=TRUE)
aa<-c(round(aa,2),round(100*aa[1]/a.t),round(100*aa[2]/a.t),round(100*aa[5]/a.t))

tab<-rbind(ee,aa)
rownames(tab)<-c("exotic","present")
colnames(tab)<-c("area","isolation 1","isolation 2","isolation 3","economic isolation","residual","per.area","per.geoiso","per.ecoiso")
tab

