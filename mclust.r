#CREDIT: CODE ADAPTED FROM ORIGINAL MCLUST SCRIPTS
#MODIFIED WITH LIZZIE DALY, MICHAEL GEMMELL AND JAMES CRAMPTON

#clear all buffers
rm(list=ls())

#set working directory
setwd("C:/Users/fvaux/Documents/R/rdata")

library(mclust)

####PREPARATION...
###SCALING FILES WITH PC LOADINGS AND CENTROID SIZE
#Where centroid size is included, you can either ignore certain clustering models OR scale centroid size against the PC loading variables
filename<-read.table("INPUT_FILE.csv",header=T,sep=",",row.names="Id")
filename2<-scale(filename)
write.csv(filename2,file="scaled.csv",row.names = TRUE,quote=FALSE)

####RUNNING MCLUST
###INPUT FILES
filename<-read.table("INPUT1.csv",header=T,sep=",",row.names="Id") #INPUT FILE WITH ONLY PC LOADINGS
filename<-read.table("INPUT2.csv",header=T,sep=",",row.names="Id") #INPUT FILE WITH SCALED PC LOADINGS AND CENTROID SIZE


##STANDARD MCLUST
#run without specifying a clustering model
filenameMclust<-Mclust(filename)
#
#OR
#
#run and specify a particular clustering model
filenameMclust<-Mclust(filename, G=7, modelName="EEV")

#calculate probabilities
probabilities<-filenameMclust$z
write.csv(probabilities,file="probabilities.csv",row.names=TRUE,quote=FALSE) #writes probabilities file

#calculate summary
summary(filenameMclust,parameters=TRUE) #detailed summary including the estimated parameters
filename$CLUST<-filenameMclust$classification
write.csv(filename,file="extant7PCs.csv",row.names = TRUE,quote=FALSE) #writes csv file with data and cluster assignment for each individual
plot(filenameMclust,col=c("blue","red","green","darkorange1","purple","cyan","gold2","deeppink1","green4")) #can change colours if wanted
1
2
3
4
0
#!!! Four graphs are produced (1.BIC, 2.classification, 3.uncertainty, 4.density) TYPE 1, 2, 3, 4 TO PROGRESS AND TYPE 0 TO EXIT

#estimate BIC values
#MCLUST BIC
filenameBIC <- mclustBIC(filename[,-ncol(filename)])
filenamesummary<-summary(filenameBIC,data=filename)
filenamesummary #gives information on the best 3 BIC values
filenameBIC #table of BIC scores from the 10 different models across 9 components (clusters)
