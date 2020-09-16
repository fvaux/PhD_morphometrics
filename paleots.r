#BASED ON COMMANDS IN PALEOTS MANUALS
#MODIFIED WITH LIZZIE DALY, MICHAEL GEMMELL AND JAMES CRAMPTON

#clear all buffers
rm(list=ls())

#set working directory
setwd("C:/Users/fvaux/Documents/R/rdata")

#load library
library(paleoTS)

#######################################################################################
#INPUT FILES
#input format is a CSV with sample names, PC loadings for retained PCs, and centroid size, and the age of specimens (bins)
#E.G. SAMPLE 1, 0.0055, -0.0301, 0.2400, 0.8000, 15000000 
#Variables are scaled where centroid size is included
tt<-read.csv(file="FILE.csv", header=TRUE)

#######################################################################################
#PALEOTS ANALYSIS
###you can change the COMP or the vairable, which in this case are PCs and centroid size
#trait 1 (W)
mean1<-aggregate(tt$PC1, by=list(tt$age), mean) ### inserts the mean from the spreadsheet
var1<-aggregate(tt$PC1, by=list(tt$age), var) ##finds the variances from thre speadsheet
counts1<-table(tt$age) ### the counts via table function

age<-mean1$Group.1###selects the age, which is Group.1 and enters ae values
age
w<-as.paleoTS(mean1$x, var1$x, counts1, age) #having input the different values from the input file

#trait 2 (X)
mean2<-aggregate(tt$PC2, by=list(tt$age), mean) ### inserts the mean from the spreadsheet
var2<-aggregate(tt$PC2, by=list(tt$age), var) ##finds the variances from thre speadsheet
counts2<-table(tt$age) ### the counts via table function

age<-mean2$Group.1###selects the age, which is Group.1 and enters ae values
age
x<-as.paleoTS(mean2$x, var2$x, counts2, age) #having input the different values from the input file

#trait 3 (Y)
mean3<-aggregate(tt$PC3, by=list(tt$age), mean) ### inserts the mean from the spreadsheet
var3<-aggregate(tt$PC3, by=list(tt$age), var) ##finds the variances from thre speadsheet
counts3<-table(tt$age) ### the counts via table function


age<-mean3$Group.1###selects the age, which is Group.1 and enters ae values
age
y<-as.paleoTS(mean3$x, var3$x, counts3, age) #having input the different values from the input file

#trait 4 (Z)
mean4<-aggregate(tt$Centroid, by=list(tt$age), mean) ### inserts the mean from the spreadsheet
var4<-aggregate(tt$Centroid, by=list(tt$age), var) ##finds the variances from thre speadsheet
counts4<-table(tt$Centroid) ### the counts via table function

age<-mean4$Group.1###selects the age, which is Group.1 and enters ae values
age
z<-as.paleoTS(mean4$x, var4$x, counts4, age) #having input the different values from the input file

#can add further variables as required!

#######################################################################################
#PLOTTING
par(mfrow=c(2,2))
plot(w)
plot(x)
plot(y)
plot(z)

#MODEL FITTING
#######################################################################################
fit3models(w) #seeing which of the 3 modles fits best
fit3models(x) #seeing which of the 3 modles fits best
fit3models(y) #seeing which of the 3 modles fits best
fit3models(z) #seeing which of the 3 modles fits best


#Further analyses:
lynchD(y, gen.per.t=1e05)
#########################################################################################
#When computing the evolutionary rate of directed walk
GRW<-opt.GRW(y, pool = TRUE, cl = list(fnscale = -1), meth = "L-BFGS-B", hess = FALSE)
GRWparam<-GRW$parameters
((GRWparam[1]^2)*1e+05)+((GRWparam[2]^2)*1e+05)
#######################################################################################
#When computing the evolutionary rate of random walk
URW<-opt.URW(y, pool = TRUE, cl = list(fnscale=-1), meth = "L-BFGS-B", hess = FALSE)
URMparam<-URW$parameters
(URMparam[1]*1e+05)
#######################################################################################
#When computing the evolutionary rate of stasis
STAT<-opt.Stasis(y, pool = TRUE, cl = list(fnscale=-1), meth = "L-BFGS-B", hess = FALSE)
STATparam<-STAT$parameters
STATparam[2]*2