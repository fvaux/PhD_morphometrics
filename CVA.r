#CREDIT: ADAPTED FROM CODE BY JAMES CRAMPTON

#Canonical Variates Analysis
#Useful items are:   projection (data projected onto DF axis, centred on overall mean)
#                    Mahalan.dist (distance between means) -- has distances in upper half, and permutation probabilities in lower half (probs of that distance arising by chance alone)
#                    jackCV (jacknifed cross-validation correct percentage)
#                    MANOVA components - use "str(object)" to see structure of output and use "object$stats[5]" to output F statistic, say
#                    std.coef = within-group standardized canonical coefficients - i.e., coefficients that allow us to interpret loadings of variables on canonical/discriminant axis
#                    proportion of between-group variation explain by each axis (where multiple groups/axes) - coded as "Proportion of trace" in lda output
#Some of this code adapted from http://www.statmethods.net/advstats/discriminant.html
#other parts taken from Tamsin's course notes (esp. generating pooled, within-group standardized canonical coefficients)
#IMPORTANT NOTE ABOUT INPUT: CANNOT HAVE SYMBOLS LIKE '+' IN COLUMN NAMES; CANNOT HAVE LABEL FOR FIRST COLUMN (WHICH WILL BECOME CASE NAMES)

#clear all buffers
rm(list=ls())

#set working directory
setwd("C:/Users/fvaux/Documents/R/rdata")

#load library
library(MASS)

#Infile expected to have case names in column 1 and grouping variable column 2
infile <- "INFILE.csv"       
n.iter <- 1000 # number iterations for permutation test

In.data <- read.csv(file=infile, header=TRUE, row.names=1)
In.data$group <- as.factor(In.data$group)
CVA.data <- as.matrix(In.data[,-1])       #remove grouping variable from data (lda requires this)
n.groups <- nlevels(In.data$group)
group.names <- levels(In.data$group)

CVA <- lda(CVA.data, In.data$group)      #performs discriminant analysis by group; variables must be separated from grouping variable
meanproj <- CVA$mean%*%CVA$scaling    #projected means
overall.mean <- apply(meanproj,MARGIN=2,FUN=mean)
projection <- (CVA.data%*%CVA$scaling)
projection <- sweep(projection, 2, overall.mean) # projects data and centres on (i.e., subtracts) mean for each axis
#to project and centre can also use "predict" and "scale"
col.labels <- as.character(c("group",colnames(projection)))                                                  
projection <- as.data.frame(cbind(as.character(In.data$group), projection))  #stick grouping variable back in
colnames(projection) <- col.labels
#output appears as set of factors ... laborious to convert all back to numbers but I can find no way to avoid this
for (f in 2:ncol(projection)) {
  projection[,f] <- as.numeric(as.character(projection[,f]))
  }

#plot(CVA)     #produces summary plot of projections by CV axis, format changes depending on number groups/axes
########## compute Mahalanobis distances between group means
mahalan.dist <- as.matrix(dist(meanproj, upper=TRUE))
#Note: 'dist' defaults to Euclidean distance, but if one is using full dimentionality, then Euclidean distance is equal to Mahalanobis distance (Krzanowski 1988: 301, 376)
#Alternative ways of computing Mahal.dist - see long-hand at end, or use:
#'dist(predict(CVA, CVA$mean) $x)'  
#Where x is one of the quantities produced by predict, see '?predict.lda'


##########  Permutation test for Mahalanobis distances
# Note: this generates random membership permutation for just two focal groups,
# but recomputes Mahalanobis distances within space of full dataset and all groups
for (j in 1:n.groups) {         # then loop through each pair-wise group comparison 
    for (k in j:n.groups) {
      mahalan.dist.perm <- vector(mode="numeric")
      perm.group <- as.factor(as.numeric(In.data$group))    # create a new grouping factor, integer, for use in permutations 

      rows.j <- grep(levels(In.data$group)[j], In.data$group)
      rows.k <- grep(levels(In.data$group)[k], In.data$group)
      samp.size.j <- length(rows.j)
      samp.size.k <- length(rows.k)
      samp.jk <- c(rows.j, rows.k)
      for (m in 1:n.iter) {
          # create permutation for groups jk
          perm <- sample(samp.jk) # take random permutation of row indices in gp.jk
          perm.group[perm[1:samp.size.j]] <- j   # recodes 'new.gp' for two focal groups according to permutation
          perm.group[perm[samp.size.j+1:length(perm)]] <- k        

          CVA.perm <- lda(CVA.data, perm.group)      # performs discriminant analysis by group; variables must be separated from grouping variable
          meanproj.perm <- CVA.perm$mean%*%CVA.perm$scaling    # projected means
          mahalan.dist.perm[m] <- as.matrix(dist(meanproj.perm))[j,k]
          }

      # compute probability of observed value against permutation
      perm.dist <- ecdf(mahalan.dist.perm)    # note, 'perm.dist' is a function
      mahalan.prob <- perm.dist(mahalan.dist[j,k])
      mahalan.dist[k,j] <- 1-mahalan.prob
      }
}

##########Undertake jacknifed (leave-one-out) predictions and print percent correct assignment
CVA2 <- lda(CVA.data, In.data$group, CV=TRUE)    # CV = jacknifed cross-validation
ct <- table(In.data$group, CVA2$class)  # assess accuracy of the prediction
jackCV <- diag((prop.table(ct,1))*100)  # NOTE: dimension of prop.table is important - columns totals for CVA classification, rows total for original classification
print("jacknifed assignments; rows are original classification; columns are jacknife classification")
# to read this table look along rows and then up columns to see how originally classified are reclass. in jacknife;
# look down columns to see how jacknife classified were classified originally.
ct
print("jacknifed cross-validation % correct ")
jackCV