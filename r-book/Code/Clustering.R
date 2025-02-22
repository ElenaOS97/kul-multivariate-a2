library("MASS")
library("FNN")
library(fBasics)
library(ggdendro)
require(graphics)
library(psych)
library(cluster)
library(effects)
library(lattice)
library(HDclassif)
library(mclust)
library(clustvarsel)
library(GPArotation)

#load data
load("E:/RStudio/shopping.Rdata")

## standardize variables
shopping<-scale(shopping,center=TRUE,scale=TRUE)

#create train and test set
set.seed(0829539)
sel<-sample(1:487,size=250,replace=FALSE)
train<-shopping[sel,]
valid<-shopping[-sel,]

# function classify new points to nearest cluster centroid
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

prcomp.out<-prcomp(shopping)
#plot eigenvalues
plot(prcomp.out$sd^2,type="b")
#compute variance accounted for by each component
round(prcomp.out$sd^2/sum(prcomp.out$sd^2),3)
comp<-as.matrix(shopping)%*%prcomp.out$rotation

#exploratory factor analysis
faO<-fa(shopping,2,rotate="oblimin", fm="mle");
print(faO,cutoff=0);

# compute Euclidean distances
distEuc<-dist(shopping, method = "euclidean", diag = FALSE, upper = FALSE)

##  hierarchical clustering method of Ward on squared Euclidean distance
hiclust_ward<- hclust(distEuc, "ward.D2")

## plot dendrogram
par(pty="s")
plot(hiclust_ward,hang=-1)

# classification of students for solutions with 1-6 clusters
clustvar<-cutree(hiclust_ward, k=1:6)

# k-means with 1 clusters using centroid of Ward as starting point
nclust<-1
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean1<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean1$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean1$cluster==1,],col="black",pch=19)

# k-means with 2 clusters using centroid of Ward as starting point
nclust<-2
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean2<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean2$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean2$cluster==1,],col="black",pch=19)
points(comp[kmean2$cluster==2,],col="red",pch=19)

# k-means with 3 clusters using centroid of Ward as starting point
nclust<-3
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean3<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean3$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean3$cluster==1,],col="black",pch=19)
points(comp[kmean3$cluster==2,],col="red",pch=19)
points(comp[kmean3$cluster==3,],col="green",pch=19)

# k-means with 4 clusters using centroid of Ward as starting point
nclust<-4
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean4<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean4$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean4$cluster==1,],col="black",pch=19)
points(comp[kmean4$cluster==2,],col="red",pch=19)
points(comp[kmean4$cluster==3,],col="green",pch=19)
points(comp[kmean4$cluster==4,],col="blue",pch=19)


# k-means with 5 clusters using centroid of Ward as starting point
nclust<-5
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean5<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean5$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean5$cluster==1,],col="black",pch=19)
points(comp[kmean5$cluster==2,],col="red",pch=19)
points(comp[kmean5$cluster==3,],col="green",pch=19)
points(comp[kmean5$cluster==4,],col="blue",pch=19)
points(comp[kmean5$cluster==5,],col="yellow",pch=19)

# k-means with 6 clusters using centroid of Ward as starting point
nclust<-6
stat<-describeBy(shopping,clustvar[,nclust],mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(shopping))
kmean6<-kmeans(shopping,centers=hcenter,iter.max=200)

round(kmean6$centers,2)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[kmean6$cluster==1,],col="black",pch=19)
points(comp[kmean6$cluster==2,],col="red",pch=19)
points(comp[kmean6$cluster==3,],col="green",pch=19)
points(comp[kmean6$cluster==4,],col="blue",pch=19)
points(comp[kmean6$cluster==5,],col="yellow",pch=19)
points(comp[kmean6$cluster==6,],col="purple",pch=19)

##number of observations per cluster
kmean1$size
kmean2$size
kmean3$size
kmean4$size
kmean5$size
kmean6$size

## proportion of explained variance
plot(c(1:6),c(kmean1$betweenss/kmean1$totss,kmean2$betweenss/kmean2$totss,kmean3$betweenss/kmean3$totss,kmean4$betweenss/kmean4$totss,kmean5$betweenss/kmean5$totss,kmean6$betweenss/kmean6$totss))
lines(c(1:6),c(kmean1$betweenss/kmean1$totss,kmean2$betweenss/kmean2$totss,kmean3$betweenss/kmean3$totss,kmean4$betweenss/kmean4$totss,kmean5$betweenss/kmean5$totss,kmean6$betweenss/kmean6$totss))


##############################
## validate cluster solution
#############################

# cluster train data Ward + kmeans
disttrain<-dist(train, method = "euclidean", diag = FALSE, upper = FALSE)
wardtrain<- hclust(disttrain, "ward.D2")
nclust<-5
clustvar<-cutree(wardtrain, k=nclust)
stat<-describeBy(train,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(train))
kmeantrain<-kmeans(train,centers=hcenter,iter.max=200)

# cluster validation data Ward + kmeans
distvalid<-dist(valid, method = "euclidean", diag = FALSE, upper = FALSE)
wardvalid<- hclust(distvalid, "ward.D2")
nclust<-5
clustvar<-cutree(wardvalid, k=nclust)
stat<-describeBy(valid,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(valid))
kmeanvalid<-kmeans(valid,centers=hcenter,iter.max=200)


classif1<-clusters(valid, kmeantrain[["centers"]])
classif2<-kmeanvalid$cluster
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("4","2","5","3","1"))
mat<-table(cl1,cl2)
print(mat)

percent5=sum(diag(mat))/sum(mat)
round(percent5,2)

rand5<-adjustedRandIndex(cl1,cl2)
round(rand5,2)

#draw the stability curve considering different number of clusters
#5 clusters
# cluster train data Ward + kmeans
disttrain<-dist(train, method = "euclidean", diag = FALSE, upper = FALSE)
wardtrain<- hclust(disttrain, "ward.D2")
nclust<-6
clustvar<-cutree(wardtrain, k=nclust)
stat<-describeBy(train,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(train))
kmeantrain<-kmeans(train,centers=hcenter,iter.max=200)

# cluster validation data Ward + kmeans
distvalid<-dist(valid, method = "euclidean", diag = FALSE, upper = FALSE)
wardvalid<- hclust(distvalid, "ward.D2")
nclust<-6
clustvar<-cutree(wardvalid, k=nclust)
stat<-describeBy(valid,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(valid))
kmeanvalid<-kmeans(valid,centers=hcenter,iter.max=200)


classif1<-clusters(valid, kmeantrain[["centers"]])
classif2<-kmeanvalid$cluster
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("1","4","5","6","3","2"))
mat<-table(cl1,cl2)
print(mat)

percent6=sum(diag(mat))/sum(mat)

rand6<-adjustedRandIndex(cl1,cl2)

#4 clusters
# cluster train data Ward + kmeans
disttrain<-dist(train, method = "euclidean", diag = FALSE, upper = FALSE)
wardtrain<- hclust(disttrain, "ward.D2")
nclust<-4
clustvar<-cutree(wardtrain, k=nclust)
stat<-describeBy(train,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(train))
kmeantrain<-kmeans(train,centers=hcenter,iter.max=200)

# cluster validation data Ward + kmeans
distvalid<-dist(valid, method = "euclidean", diag = FALSE, upper = FALSE)
wardvalid<- hclust(distvalid, "ward.D2")
nclust<-4
clustvar<-cutree(wardvalid, k=nclust)
stat<-describeBy(valid,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(valid))
kmeanvalid<-kmeans(valid,centers=hcenter,iter.max=200)


classif1<-clusters(valid, kmeantrain[["centers"]])
classif2<-kmeanvalid$cluster
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("1","2","4","3"))
mat<-table(cl1,cl2)
print(mat)

percent4=sum(diag(mat))/sum(mat)

rand4<-adjustedRandIndex(cl1,cl2)

#3 clusters
# cluster train data Ward + kmeans
disttrain<-dist(train, method = "euclidean", diag = FALSE, upper = FALSE)
wardtrain<- hclust(disttrain, "ward.D2")
nclust<-3
clustvar<-cutree(wardtrain, k=nclust)
stat<-describeBy(train,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(train))
kmeantrain<-kmeans(train,centers=hcenter,iter.max=200)

# cluster validation data Ward + kmeans
distvalid<-dist(valid, method = "euclidean", diag = FALSE, upper = FALSE)
wardvalid<- hclust(distvalid, "ward.D2")
nclust<-3
clustvar<-cutree(wardvalid, k=nclust)
stat<-describeBy(valid,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(valid))
kmeanvalid<-kmeans(valid,centers=hcenter,iter.max=200)


classif1<-clusters(valid, kmeantrain[["centers"]])
classif2<-kmeanvalid$cluster
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("1","2","3"))
mat<-table(cl1,cl2)
print(mat)

percent3=sum(diag(mat))/sum(mat)

rand3<-adjustedRandIndex(cl1,cl2)

#2 clusters
# cluster train data Ward + kmeans
disttrain<-dist(train, method = "euclidean", diag = FALSE, upper = FALSE)
wardtrain<- hclust(disttrain, "ward.D2")
nclust<-2
clustvar<-cutree(wardtrain, k=nclust)
stat<-describeBy(train,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(train))
kmeantrain<-kmeans(train,centers=hcenter,iter.max=200)

# cluster validation data Ward + kmeans
distvalid<-dist(valid, method = "euclidean", diag = FALSE, upper = FALSE)
wardvalid<- hclust(distvalid, "ward.D2")
nclust<-2
clustvar<-cutree(wardvalid, k=nclust)
stat<-describeBy(valid,clustvar,mat=TRUE)
hcenter<-matrix(stat[,5],nrow=nclust)
rownames(hcenter)<-paste("c_",rep(1:nclust),sep="")
colnames(hcenter)<-c(colnames(valid))
kmeanvalid<-kmeans(valid,centers=hcenter,iter.max=200)


classif1<-clusters(valid, kmeantrain[["centers"]])
classif2<-kmeanvalid$cluster
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("1","2"))
mat<-table(cl1,cl2)
print(mat)

percent2=sum(diag(mat))/sum(mat)

rand2<-adjustedRandIndex(cl1,cl2)

#plot
plot(c(2:6), c(percent2,percent3,percent4,percent5,percent6))
lines(c(2:6), c(percent2,percent3,percent4,percent5,percent6))

#plot
plot(c(2:6), c(rand2,rand3,rand4,rand5,rand6))
lines(c(2:6), c(rand2,rand3,rand4,rand5,rand6))


########################
# analysis with HDclassif
########################

#number of clusters chosen by hddc
#use BIC to select the number of dimensions
set.seed(0829539)
hddc1.out<-hddc(shopping,K=1:6,model="all")
hddc1.out
plot(hddc1.out)


#fit all models with K=1
set.seed(0829539)
hddc1.out<-hddc(shopping,K=1,model="all")
hddc1.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc1.out$class==1,],col="black",pch=19)

#fit all models with K=2
set.seed(0829539)
hddc2.out<-hddc(shopping,K=2,model="all")
hddc2.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc2.out$class==1,],col="black",pch=19)
points(comp[hddc2.out$class==2,],col="red",pch=19)


#fit all models with K=3
set.seed(0829539)
hddc3.out<-hddc(shopping,K=3,model="all")
hddc3.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc3.out$class==1,],col="black",pch=19)
points(comp[hddc3.out$class==2,],col="red",pch=19)
points(comp[hddc3.out$class==3,],col="green",pch=19)

#fit all models with K=4
set.seed(0829539)
hddc4.out<-hddc(shopping,K=4,model="all")
hddc4.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc4.out$class==1,],col="black",pch=19)
points(comp[hddc4.out$class==2,],col="red",pch=19)
points(comp[hddc4.out$class==3,],col="green",pch=19)
points(comp[hddc4.out$class==4,],col="blue",pch=19)

#fit all models with K=5
set.seed(0829539)
hddc5.out<-hddc(shopping,K=5,model="all")
hddc5.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc5.out$class==1,],col="black",pch=19)
points(comp[hddc5.out$class==2,],col="red",pch=19)
points(comp[hddc5.out$class==3,],col="green",pch=19)
points(comp[hddc5.out$class==4,],col="blue",pch=19)
points(comp[hddc5.out$class==5,],col="yellow",pch=19)

#fit all models with K=6
set.seed(0829539)
hddc6.out<-hddc(shopping,K=6,model="all")
hddc6.out

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[hddc6.out$class==1,],col="black",pch=19)
points(comp[hddc6.out$class==2,],col="red",pch=19)
points(comp[hddc6.out$class==3,],col="green",pch=19)
points(comp[hddc6.out$class==4,],col="blue",pch=19)
points(comp[hddc6.out$class==5,],col="yellow",pch=19)
points(comp[hddc6.out$class==6,],col="purple",pch=19)


##############################
## validate cluster solution
#############################

# cluster train data hddc
set.seed(0829539)
hddcT.out<-hddc(train,K=5,model="all")
hddcT.out

# cluster validation data hddc
set.seed(0829539)
hddcV.out<-hddc(valid,K=5,model="all")
hddcV.out

classif1<-clusters(valid, hddcT.out[["mu"]])
classif2<-hddcV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("2","4","5","1","3"))
mat<-table(cl1,cl2)
print(mat)

percent5=sum(diag(mat))/sum(mat)
round(percent5,2)

rand5<-adjustedRandIndex(cl1,cl2)
round(rand5,2)

#plot the stability curve considering different number of clusters
#6 clusters
# cluster train data hddc
set.seed(0829539)
hddcT.out<-hddc(train,K=6,model="all")
hddcT.out

# cluster validation data hddc
set.seed(0829539)
hddcV.out<-hddc(valid,K=6,model="all")
hddcV.out

classif1<-clusters(valid, hddcT.out[["mu"]])
classif2<-hddcV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("4","5","3","2","1","6"))
mat<-table(cl1,cl2)
print(mat)

percent6=sum(diag(mat))/sum(mat)

rand6<-adjustedRandIndex(cl1,cl2)

#4 clusters
# cluster train data hddc
set.seed(0829539)
hddcT.out<-hddc(train,K=4,model="all")
hddcT.out

# cluster validation data hddc
set.seed(0829539)
hddcV.out<-hddc(valid,K=4,model="all")
hddcV.out

classif1<-clusters(valid, hddcT.out[["mu"]])
classif2<-hddcV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("4","2","3","1"))
mat<-table(cl1,cl2)
print(mat)

percent4=sum(diag(mat))/sum(mat)

rand4<-adjustedRandIndex(cl1,cl2)

#3 clusters
# cluster train data hddc
set.seed(0829539)
hddcT.out<-hddc(train,K=3,model="all")
hddcT.out

# cluster validation data hddc
set.seed(0829539)
hddcV.out<-hddc(valid,K=3,model="all")
hddcV.out

classif1<-clusters(valid, hddcT.out[["mu"]])
classif2<-hddcV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("3","2","1"))
mat<-table(cl1,cl2)
print(mat)

percent3=sum(diag(mat))/sum(mat)

rand3<-adjustedRandIndex(cl1,cl2)

#2 clusters
# cluster train data hddc
set.seed(0829539)
hddcT.out<-hddc(train,K=2,model="all")
hddcT.out

# cluster validation data hddc
set.seed(0829539)
hddcV.out<-hddc(valid,K=2,model="all")
hddcV.out

classif1<-clusters(valid, hddcT.out[["mu"]])
classif2<-hddcV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("2","1"))
mat<-table(cl1,cl2)
print(mat)

percent2=sum(diag(mat))/sum(mat)

rand2<-adjustedRandIndex(cl1,cl2)

#plot
plot(c(2:6), c(percent2,percent3,percent4,percent5,percent6))
lines(c(2:6), c(percent2,percent3,percent4,percent5,percent6))

#plot
plot(c(2:6), c(rand2,rand3,rand4,rand5,rand6))
lines(c(2:6), c(rand2,rand3,rand4,rand5,rand6))


########################
#analysis with Mclust
########################

#number of clusters chosen by Mclust
set.seed(0829539)
mclust1.out<-Mclust(shopping,G=1:6)
summary(mclust1.out)

#fit all models with G=1
set.seed(0829539)
mclust1.out<-Mclust(shopping,G=1)
summary(mclust1.out)

#plot clusters extracted with Mcluster in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust1.out$class==1,],col="black",pch=19)

#fit all models with G=2
set.seed(0829539)
mclust2.out<-Mclust(shopping,G=2)
summary(mclust2.out)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust2.out$class==1,],col="black",pch=19)
points(comp[mclust2.out$class==2,],col="red",pch=19)

#fit all models with G=3
set.seed(0829539)
mclust3.out<-Mclust(shopping,G=3)
summary(mclust3.out)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust3.out$class==1,],col="black",pch=19)
points(comp[mclust3.out$class==2,],col="red",pch=19)
points(comp[mclust3.out$class==3,],col="green",pch=19)

#fit all models with G=4
set.seed(0829539)
mclust4.out<-Mclust(shopping,G=4)
summary(mclust4.out)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust4.out$class==1,],col="black",pch=19)
points(comp[mclust4.out$class==2,],col="red",pch=19)
points(comp[mclust4.out$class==3,],col="green",pch=19)
points(comp[mclust4.out$class==4,],col="blue",pch=19)

#fit all models with G=5
set.seed(0829539)
mclust5.out<-Mclust(shopping,G=5)
summary(mclust5.out)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust5.out$class==1,],col="black",pch=19)
points(comp[mclust5.out$class==2,],col="red",pch=19)
points(comp[mclust5.out$class==3,],col="green",pch=19)
points(comp[mclust5.out$class==4,],col="blue",pch=19)
points(comp[mclust5.out$class==5,],col="yellow",pch=19)

#fit all models with G=6
set.seed(0829539)
mclust6.out<-Mclust(shopping,G=6)
summary(mclust6.out)

#plot clusters extracted with HDDC in space of first two principal components
plot(comp,main="derived clusters")
points(comp[mclust6.out$class==1,],col="black",pch=19)
points(comp[mclust6.out$class==2,],col="red",pch=19)
points(comp[mclust6.out$class==3,],col="green",pch=19)
points(comp[mclust6.out$class==4,],col="blue",pch=19)
points(comp[mclust6.out$class==5,],col="yellow",pch=19)
points(comp[mclust6.out$class==6,],col="purple",pch=19)

##############################
## validate cluster solution
#############################

# cluster train data Mclust
set.seed(0829539)
mclustT.out<-Mclust(train,G=4)
summary(mclustT.out)

# cluster validation data Mclust
set.seed(0829539)
mclustV.out<-Mclust(valid,G=4)
summary(mclustV.out)


classif1<-clusters(valid, t(as.matrix(mclustT.out$parameters$mean)))
classif2<-mclustV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("3","4","2","1"))
mat<-table(cl1,cl2)
print(mat)

percent4=sum(diag(mat))/sum(mat)
round(percent4,2)

rand4<-adjustedRandIndex(cl1,cl2)
round(rand4,2)

#plot the stability curve considering different number of clusters
#6 clusters
# cluster train data Mclust
set.seed(0829539)
mclustT.out<-Mclust(train,G=6)
summary(mclustT.out)

# cluster validation data Mclust
set.seed(0829539)
mclustV.out<-Mclust(valid,G=6)
summary(mclustV.out)


classif1<-clusters(valid, t(as.matrix(mclustT.out$parameters$mean)))
classif2<-mclustV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("6","5","4","2","1","3"))
mat<-table(cl1,cl2)
print(mat)

percent6=sum(diag(mat))/sum(mat)

rand6<-adjustedRandIndex(cl1,cl2)
  
#5 clusters 
# cluster train data Mclust
set.seed(0829539)
mclustT.out<-Mclust(train,G=5)
summary(mclustT.out)

# cluster validation data Mclust
set.seed(0829539)
mclustV.out<-Mclust(valid,G=5)
summary(mclustV.out)

classif1<-clusters(valid, t(as.matrix(mclustT.out$parameters$mean)))
classif2<-mclustV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("3","5","4","2","1"))
mat<-table(cl1,cl2)
print(mat)

percent5=sum(diag(mat))/sum(mat)

rand5<-adjustedRandIndex(cl1,cl2)

#3 clusters
# cluster train data Mclust
set.seed(0829539)
mclustT.out<-Mclust(train,G=3)
summary(mclustT.out)

# cluster validation data Mclust
set.seed(0829539)
mclustV.out<-Mclust(valid,G=3)
summary(mclustV.out)

classif1<-clusters(valid, t(as.matrix(mclustT.out$parameters$mean)))
classif2<-mclustV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("3","2","1"))
mat<-table(cl1,cl2)
print(mat)

percent3=sum(diag(mat))/sum(mat)

rand3<-adjustedRandIndex(cl1,cl2)

#2 clusters
# cluster train data Mclust
set.seed(0829539)
mclustT.out<-Mclust(train,G=2)
summary(mclustT.out)

# cluster validation data Mclust
set.seed(0829539)
mclustV.out<-Mclust(valid,G=2)
summary(mclustV.out)

classif1<-clusters(valid, t(as.matrix(mclustT.out$parameters$mean)))
classif2<-mclustV.out$class
table(classif1,classif2)

cl1<-as.factor(classif1)
cl2<-factor(classif2,levels=c("2","1"))
mat<-table(cl1,cl2)
print(mat)

percent2=sum(diag(mat))/sum(mat)

rand2<-adjustedRandIndex(cl1,cl2)

#plot
plot(c(2:6), c(percent2,percent3,percent4,percent5,percent6))
lines(c(2:6), c(percent2,percent3,percent4,percent5,percent6))

#plot
plot(c(2:6), c(rand2,rand3,rand4,rand5,rand6))
lines(c(2:6), c(rand2,rand3,rand4,rand5,rand6))