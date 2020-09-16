#CREDIT: ADAPTED FROM SCRIPT BY JAMES CRAMPTON
#Code to apply broken stick model to existing eigenvalues

#clear all buffers
rm(list=ls())

#set working directory
setwd("C:/Users/fvaux/Documents/R/rdata")

#load library
library(vegan)

#Load .csv file with listed PCs and eigen values
xx<-read.csv("INPUT_FILE.csv")
#xx is the eigenvalues
zz<-as.data.frame(bstick(50,tot.var=sum(xx$eigenvalues)))
# zz is the broken stick model
components<-seq(from=1, to=nrow(xx), by=1)
xx$comp<-components
components2<-seq(from=1, to=nrow(zz), by=1)
zz$comp<-components2
plot(xx$comp, xx$eigenvalues, type="h")
lines(zz$comp, zz[,1], col="red")