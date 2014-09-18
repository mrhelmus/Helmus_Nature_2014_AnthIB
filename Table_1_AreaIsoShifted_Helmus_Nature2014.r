# Helmus et al. 2014 Nature Island Biogeography of the Anthropocene
# Code to produce Table 1 in the main text: Shifting importance of area and isolation

#library for loading xlsx files
#library(xlsx) #use if want to use xlsx from Nature
require(car)

#Read Data
#dat<-read.xlsx("Helmus_Data_Table1.xlsx",sheetIndex=1,header=TRUE) #save data as csv and load if problems with xlsx (likely an outdated Java Runtime) 
dat<-read.csv("Helmus_Data_Table1.csv",header=TRUE,as.is=TRUE)
rownames(dat)<-dat$bank

#####################################################################################################################
#########################Regressios#################################################################################
##################################################################################################################

  SStable<-NULL
  Ptable<-NULL
  Ctable<-NULL

  lm.dat<-lm(scale(log(past.native.sr+1))~scale((log(area.km2)))+scale(isolation.1)+scale(isolation.2)+scale(isolation.3),data=dat)           #PAST / NATIVE SPECIES
  anova.sr.nat<-Anova(lm.dat)
  SStable<-rbind(SStable,anova.sr.nat$Sum)
  Ptable<-rbind(Ptable,anova.sr.nat$P)
  Ctable<-rbind(Ctable,coef(lm.dat))

  lm.dat<-lm(scale(log(exotic.sr+1))~scale((log(area.km2)))+scale(isolation.1)+scale(isolation.2)+scale(isolation.3),data=dat)               #EXOTIC SPECIES
  anova.sr.ex<-Anova(lm.dat)
  SStable<-rbind(SStable,anova.sr.ex$Sum)
  Ptable<-rbind(Ptable,anova.sr.ex$P)
  Ctable<-rbind(Ctable,coef(lm.dat))

  lm.dat<-lm(scale(log(present.sr+1))~scale((log(area.km2)))+scale(isolation.1)+scale(isolation.2)+scale(isolation.3),data=dat)               #PRESENT / EXOTIC + NATIVE SPECIES
  anova.sr.all<-Anova(lm.dat)
  SStable<-rbind(SStable,anova.sr.all$Sum)
  Ptable<-rbind(Ptable,anova.sr.all$P)
  Ctable<-rbind(Ctable,coef(lm.dat))

colnames(SStable)<-c("area","pc1","pc2","pc3","error")
rownames(SStable)<-rownames(Ptable)<-rownames(Ctable)<-c("sr.past","sr.ex","sr.present")
#SStable<-round(SStable,2)
  SStable<-cbind(SStable,(SStable[,"area"]/rowSums(SStable)))
  p<-rowSums(SStable[,grep("pc",colnames(SStable))])/rowSums(SStable)
  SStable<-cbind(SStable,p)


colnames(SStable)<-c("area","isolation_1","isolation_2","isolation_3","error","per.area","per.iso")

colnames(Ptable)<-c("area","isolation_1","isolation_2","isolation_3","error")

colnames(Ctable)<-c("intercept","area","isolation_1","isolation_2","isolation_3")

Ptable
Ctable
a.ch<-(SStable[3,"per.area"]-SStable[1,"per.area"])/SStable[1,"per.area"]
i.ch<-(SStable[3,"per.iso"]-SStable[1,"per.iso"])/SStable[1,"per.iso"]
SStable<-round(SStable,2)
SStable[,6:7]<-round(100*SStable[,6:7])
#Table 1
SStable

#past to present change
round(100*c(a.ch,i.ch))

