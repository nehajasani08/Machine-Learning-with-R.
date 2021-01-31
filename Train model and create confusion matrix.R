
'Using the dataset from http://data.princeton.edu/wws509/datasets/cuse.dat, 
Propose the best classifier using Naïve Bayes and logistic regression.
#Considering 'wantsMore' as the output variable:
  
Train the model using 70% of this dataset, Predict the test set using the trained model , Provide the confusion matrix and obtain the probability of correctness of predictions. 
Write the functional form of your proposed classifier'



link="http://data.princeton.edu/wws509/datasets/cuse.dat"
data=read.table(link,header=FALSE)
data

View(d)
x1 = data$V1
x2 = data$V2
x3 = data$V3
x4 = data$V4
y = data$V5

dataset <- na.omit(data.frame(x1,x2,x3,x4,y))
dataset

'a. training the dataset'

set.seed(123)
n=nrow(dataset)
indexes = sample(n,n*(70/100))
trainset = dataset[indexes,]
testset = dataset[-indexes,]
trainset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

trainset1= na.omit(trainset)
trainset.glm <-  glm(y ~ .,trainset,family = 'binomial')
summary(trainset.glm )

'training the test set'

res=predict(trainset.glm,testset, type="response")  # prediction

# convert phat to yhat 
predictedvalues=rep(0,nrow(testset))
predictedvalues[res>=0.5]=1  # probability of region being 1, if p<0.5 then region = 0
df=data.frame(testset$y,predictedvalues )
#View(df)
df

t=table( predictedvalues, actualvalues=testset[,5])
t
accuracy=mean(predictedvalues == testset[,5]) # correctness of prediction
accuracy
# Fitting Naive bayes to the Training set
#library(e1071)
trainset.naiveB <- naiveBayes(y~. ,trainset)
summary(trainset.naiveB)
res_nb=predict(trainset.naiveB, newdata = testset[-5])  # prediction

t_nb=table(testset$y, res_nb)
t_nb
accuracy_nb=mean(res_nb == testset[,5]) # correctness of prediction
accuracy_nb



vect=0
mc=1000
for (i in 1:mc){
  n=nrow(dataset)
  indexes = sample(n,n*(70/100))
  trainset = dataset[indexes,]
  testset = dataset[-indexes,]
  #fit the logistic regression
  trainset.glm <- glm(y ~.,trainset, family="binomial") # ~. shows that we include all ind. variables
  #predict phat 
  res=predict(trainset.glm,testset, type="response")  # prediction
  # convert phat to yhat 
  predictedvalues=rep(0,nrow(testset))
  predictedvalues[res>0.5]=1  # probability of Gender being 1, if p<0.5 then gender=0
  df=data.frame(testset$y,predictedvalues )
  #View(df)
  #confusion matrix
  table( predictedvalues, actualvalues=testset[,5])
  #accuracy 
  accuracy=mean(predictedvalues == testset[,5]) # correctness of prediction
  accuracy
  trainset.naiveB <- naiveBayes(y~. ,trainset)
  summary(trainset.naiveB)
  res_nb=predict(trainset.naiveB, newdata = testset[-5])  # prediction
  
  t_nb=table(testset$y, res_nb)
  t_nb
  accuracy_nb=mean(res_nb == testset[,5]) # correctness of prediction
  accuracy_nb
  #############################################################################
  # apply Monte carlo sampling in 1000 runs to stabilize the result. 
  p_accuracy=c(accuracy,accuracy_nb)
  vect=vect+(1/mc)*p_accuracy
  
}
vect



