library(InformationValue)
library(Information)
library(ClustOfVar)
library(reshape2)
library(plyr)
library(dplyr)
library(klaR)
library(Gifi)
library(psych)
library(GPArotation)

# load data
df <- read.table("divorce.csv", header = TRUE, sep = ";")
vars <- c(names(df)[-(55)])


# PCA
# https://rdrr.io/rforge/Gifi/man/princals.html
fitord <- princals(df[,vars], ndim = 8)  ## ordinal PCA
fitord
summary(fitord,cutoff = 0.3)
d1 <- fitord$weights[,"D1"]
d2 <- which(d1>0.95)

# clustering
#https://www.rdocumentation.org/packages/ClustOfVar/versions/1.1/topics/hclustvar
#https://multithreaded.stitchfix.com/blog/2015/08/13/weight-of-evidence/
tree <- hclustvar(df[,!(names(df) %in% c("Class"))])
nvars <- length(tree[tree$height<0.065])
part_init<-cutreevar(tree,nvars)$cluster
kmeans<-kmeansvar(X.quanti=df[,!(names(df) %in% c("Class"))],init=part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster", "Variable")
clusters <- clusters[order(clusters$Cluster),]

