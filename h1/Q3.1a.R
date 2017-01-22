library(caret)
data<-read.csv('pima-indians-diabetes.data.txt', header=FALSE)
x_matrix<-data[,-c(9)] # negative sign leaves everything except for the 9th column
y_value<-data[,9] # only leaves 9th column
training_score<-array(dim=10)
testing_score<-array(dim=10)
for (wi in 1:1000) {
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
  positive_training_mean<-sapply(positive_training_samples, mean, na.rm=FALSE)
  negative_training_mean<-sapply(negative_training_samples, mean, na.rm=FALSE)
  positive_training_sd<-sapply(positive_training_samples, sd, na.rm=FALSE)
  negative_training_sd<-sapply(negative_training_samples, sd, na.rm=FALSE)
  # compute the distance from sample points to data, positive training samples
  p_sample_mean_dif<-t(t(train_x_samples)-positive_training_mean)
  p_dif_over_sd<-t(t(p_sample_mean_dif)/positive_training_sd)
  positive_training_cost_sum<--(1/2)*rowSums(apply(p_dif_over_sd,c(1, 2), function(x)x^2), na.rm=FALSE)-sum(log(positive_training_sd))
  positive_training_cost_sum<-positive_training_cost_sum + log(221/615)
  # same thing but for negative samples
  n_sample_mean_dif<-t(t(train_x_samples)-negative_training_mean)
  n_dif_over_sd<-t(t(n_sample_mean_dif)/negative_training_sd)
  negative_training_cost_sum<--(1/2)*rowSums(apply(n_dif_over_sd,c(1, 2), function(x)x^2), na.rm=FALSE)-sum(log(negative_training_sd))
  negative_training_cost_sum<-negative_training_cost_sum  + log(221/615)
  # compare if one side is bigger than the other side
  positive_flag <- positive_training_cost_sum > negative_training_cost_sum
  gotrighttr <- positive_flag==train_y_samples
  # calculate error rate
  training_score[wi]<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))
  # use trained data to find test error rates, positive
  p_test_mean_dif<-t(t(x_test_data)-positive_training_mean)
  p_test_over_sd<-t(t(p_test_mean_dif)/positive_training_sd)
  positive_testing_error_cost<--(1/2)*rowSums(apply(p_test_over_sd,c(1, 2), function(x)x^2), na.rm=FALSE)-sum(log(positive_training_sd))
  positive_testing_error_cost <- positive_testing_error_cost + log(221/615)
  # use trained data to find test error rates, positive
  n_test_mean_dif<-t(t(x_test_data)-negative_training_mean)
  n_test_over_sd<-t(t(n_test_mean_dif)/negative_training_sd)
  negative_testing_error_cost<--(1/2)*rowSums(apply(n_test_over_sd,c(1, 2), function(x)x^2), na.rm=FALSE)-sum(log(negative_training_sd))
  negative_testing_error_cost <- negative_testing_error_cost + log(221/615)
  # Calculate error rate of the testing data
  correct_testing_indices<-positive_testing_error_cost>negative_testing_error_cost
  gotright<-correct_testing_indices==y_test_data
  testing_score[wi]<-sum(gotright)/(sum(gotright)+sum(!gotright))
}

cat("Average training correct rate over 1000 trials:")
print(mean(training_score))
cat("\nAverage testing correct rate over 1000 trials:")
print(mean(testing_score))
cat("\n")
