# Helmus M.R., Mahler D.L. & Losos J.B. (2014). Island biogeography of the Anthropocene. Nature, 513, 543-546.
# Code to produce estimates of geographic isolation

########################################################################################
#The MIT License (MIT)
#
#Copyright (c) 2014 Matthew R. Helmus
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.
#
#####################################################################################################################



#Function to make distances among banks  ####################################################################
geodetic.distance.matrix <- function (long,lat) {
NSITES <- length(lat)
R <- 6371

latitude <- lat
longitude <- long
latlong <- cbind(latitude, longitude)*pi/180
d <- matrix(nrow=NSITES, ncol=NSITES)
for(i in 1:(NSITES-1)) {
d[i,i] <- 1.0
	for (j in (i+1):NSITES) {
	d[i,j] <- sin(latlong[i,1]) * sin(latlong[j,1]) + 			cos(latlong[i,1]) * cos(latlong[j,1])	*				cos(abs(latlong[i,2] - latlong[j,2]))
	d[j,i] <- d[i,j]
	}
}
d[NSITES, NSITES] <- 1.0
d <- R*acos(d)
d
}

############################################################################
ll<-read.csv(file="Bank_lat_long.csv",header=TRUE,as.is=TRUE)
### NOTE THAT THE LONGITUDES ARE POSITIVE!!!

rownames(ll)<-ll[,1]
isdist<-geodetic.distance.matrix(ll[,3],ll[,2])
colnames(isdist)<-ll[,1]
rownames(isdist)<-ll[,1]
        iso.sa<-(sqrt(isdist[,c("NorthSA")]))
        iso.cuba<-(sqrt(isdist[,c("Cuba")]))
        iso.all<-rowSums(sqrt(isdist[,rownames(isdist)!="Yucatan"]))
        iso.origins<-rowSums(sqrt(isdist[,c("Cuba","NorthSA")]))

#Isolation PCA
dat<-data.frame(isolation.all=iso.all,isolation.sa=iso.sa,isolation.cuba=iso.cuba,isolation.origins=iso.origins)
dat<-dat[-40,]
iso.pca<-prcomp(dat,scale.=TRUE)

dat<-data.frame(dat,isolation.1=iso.pca$x[,1],isolation.2=iso.pca$x[,2],isolation.3=iso.pca$x[,3])
