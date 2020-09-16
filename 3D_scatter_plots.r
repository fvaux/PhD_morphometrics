#clear all buffers
rm(list=ls())

#set working directory
setwd("C:/Users/fvaux/Documents/R/rdata")

#load libraries
#library(ggplot2)
library(car)
#library(rgl)

#input file is .csv of PC loadings (selected number of axes) and centroid size, which have been scaled together using base function in R
mydata<-read.table("all-PCs12-cen-scal-3D.csv",header=T,sep=",")

#Make 3D scatter plot
#e.g. FIVE GROUPS
p1 <- mydata$PC1
p2 <- mydata$PC2
centroid <- mydata$Centroid
scatter3d(bg.col=c("white"), x = p1, y = p2, z = centroid, groups = mydata$genus1, labels = mydata$genus1, grid = FALSE, surface = FALSE, axis.ticks= TRUE, ellipsoid = TRUE, level = 0.5, point.col=c("blue","red","green","cyan","darkgoldenrod1"), surface.col=c("blue","red","green","cyan","darkgoldenrod1"))

#export 3D plot as SVG (in current orientation in rgl window) [large file size!]
rgl.postscript("3Dplot.svg","svg")

#export 3D plot as PDF (in current orientation in rgl window) [low resolution!]
rgl.postscript("3Dplot.pdf","pdf")