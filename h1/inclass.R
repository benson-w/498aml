# library(klaR)
library(caret)
data<-read.csv('pima-indians-diabetes.data.txt', header=FALSE)
x_matrix<-data[,-c(9)] # negative sign leaves everything except for the 9th column
y_value<-data[,9] # only leaves 9th column
trscore<-array(dim=10)
tescore<-array(dim=10)
for (wi in 1:100)
{
  x_test_index<-createDataPartition(y=y_value, p=.8, list=FALSE)
  train_x_samples <- x_matrix[x_test_index, ]
  train_y_samples <- y_value[x_test_index] # is y positive? 
  training_pos_flag <- train_y_samples > 0
  # select positive and negative samples
  positive_training_samples <- train_x_samples[training_pos_flag, ]
  negative_training_samples<-train_x_samples[!training_pos_flag,]
  x_test_data<-x_matrix[-x_test_index, ]
  y_test_data<-y_value[-x_test_index]
  # na.rm true to detect unknown values
  # calculate mean and standard deviation of training data
  positive_training_mean<-sapply(positive_training_samples, mean, na.rm=TRUE)
  negative_training_mean<-sapply(negative_training_samples, mean, na.rm=TRUE)
  positive_training_sd<-sapply(positive_training_samples, sd, na.rm=TRUE)
  negative_training_sd<-sapply(negative_training_samples, sd, na.rm=TRUE)
  
  # compute the distance from sample points to data, positive training samples
  ptroffsets<-t(t(train_x_samples)-positive_training_mean)
  ptrscales<-t(t(ptroffsets)/positive_training_sd)
  positive_training_cost_sum<--(1/2)*rowSums(apply(ptrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(positive_training_sd))
  # same thing but for negative samples
  ntroffsets<-t(t(train_x_samples)-negative_training_mean)
  ntrscales<-t(t(ntroffsets)/negative_training_sd)
  negative_training_cost_sum<--(1/2)*rowSums(apply(ntrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(negative_training_sd))
  
  # compare if one side is bigger than the other side
  lvwtr<-positive_training_cost_sum > negative_training_cost_sum
  gotrighttr<-lvwtr==train_y_samples
  trscore[wi]<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))
  pteoffsets<-t(t(x_test_data)-positive_training_mean)
  ptescales<-t(t(pteoffsets)/positive_training_sd)
  ptelogs<--(1/2)*rowSums(apply(ptescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(positive_training_sd))
  nteoffsets<-t(t(x_test_data)-negative_training_mean)
  ntescales<-t(t(nteoffsets)/negative_training_sd)
  ntelogs<--(1/2)*rowSums(apply(ntescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(negative_training_sd))
  lvwte<-ptelogs>ntelogs
  gotright<-lvwte==y_test_data
  tescore[wi]<-sum(gotright)/(sum(gotright)+sum(!gotright))
}

