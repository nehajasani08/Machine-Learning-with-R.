
library(MASS)
dataset=na.omit(nfl2008_fga)
dataset.lda<-lda(qtr~togo+kicker+ydline,data=dataset)
dataset.lda



dataset1=cbind(dataset$togo,dataset$kicker,dataset$ydline)
fit<-princomp(na.omit(dataset1),cor=TRUE) #princomp doesnt deal with NA values
summary(fit)



loadings(fit)
plot(fit,type='lines')


k.means.fit<-kmeans(dataset1,6)
attributes(k.means.fit)



#centroid(arithmetic mean)
k.means.fit$centers

#clusters
k.means.fit$size



wssplot<-function(dataset1,nc=10,seed=1234){
  
  wss<-(nrow(dataset1)-1)*sum(apply(dataset1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    
    wss[i]<-sum(kmeans(dataset1,centers=i)$withinss)
    
  }
  
  plot(1:nc,wss,type='b',xlab='number of clusters',ylab='within groups sum of squares')
  
}



wssplot(dataset1,nc=10)



install.packages('CCA') #GGally packages

library(CCA)



X<-cbind(dataset$togo,dataset$kicker,dataset$ydline)
Y<-cbind(dataset$distance,dataset$homekick)

cor(X,Y)

