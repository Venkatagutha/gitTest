data<-read.csv("Churn_modelling.csv")
install.packages("caTools")
install.packages("h2o")
library(caTools)
library(h2o)
# Considering theddependent variables
data<-data[4:14]

#Encoding categorical variables

data$Geography<-as.numeric(factor(data$Geography, 
                                     levels=c("France","Germany","Spain"),
                                     labels=c(1,2,3)))
data$Gender<-as.numeric(factor(data$Gender, 
                               levels=c("Female","Male"),
                               labels = c(1,2)))
#splitting the dataset into train and test
set.seed(123)
split<- sample.split(data$Exited, SplitRatio = 0.8)
train<- subset(data, split==T)
test<- subset(data,split==F)
#Feature scaling
# feature scaling is a must in ANN as it involves a lot of computation.
train[-11]<-scale(train[-11])
test[-11]<- scale(test[-11])
#H20 model- efficiency
#establish a connection
h2o.init(nthreads = -1)
classifier<- h2o.deeplearning(y="Exited", training_frame = as.h2o(train),
                              activation = "Rectifier",
                              hidden = c(6,6),
                              epochs = 100,
                              train_samples_per_iteration = -2)
# predicting the test results

prob<- predict(classifier, newdata=  as.h2o(test[-11]))
prob_y<- prob>0.5
prob_y<-as.vector(prob_y)
# building the confusion matrix
cm<- table(test[,11],prob_y)
