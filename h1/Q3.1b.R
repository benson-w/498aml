library(caret)

# Read data from csv, separate data into trait values as x matrix and label values as y_value
data<-read.csv('pima-indians-diabetes.data.txt', header=FALSE)
x_matrix<-data[,-c(9)] 
y_value<-data[,9]

# Arrays to store score for each of the 100 trials
training_score<-array(dim=100)
testing_score<-array(dim=100)

# For columns 3,4,6,8, replace values of 0s with NA
for (i in c(3,4,6,8)) { 
  has_zero_index <- x_matrix[, i]==0
  x_matrix[has_zero_index, i]=NA
}

# Run many times for cross-validation
for (wi in 1:100) {
  
  # Create a random test partition with 80 percent of the data
  x_test_index<-createDataPartition(y=y_value, p=.8, list=FALSE)
  
  # Using the partition created, for 
  train_x_samples <- x_matrix[x_test_index, ]
  train_y_samples <- y_value[x_test_index]
  training_pos_flag <- train_y_samples > 0 # Is y positive
  
  # select positive and negative samples
  positive_training_samples <- train_x_samples[training_pos_flag, ]
  negative_training_samples<-train_x_samples[!training_pos_flag,]
  x_test_data<-x_matrix[-x_test_index, ]
  y_test_data<-y_value[-x_test_index]
  
  # na.rm to take account of the NA parameters when calculating means and standard deviations
  # calculate mean and standard deviation of training data
  positive_training_mean<-sapply(positive_training_samples, mean, na.rm=TRUE)
  negative_training_mean<-sapply(negative_training_samples, mean, na.rm=TRUE)
  positive_training_sd<-sapply(positive_training_samples, sd, na.rm=TRUE)
  negative_training_sd<-sapply(negative_training_samples, sd, na.rm=TRUE)
  
  # For positive labels, look at the training data and compute the error or distance from the normal distribution
  p_sample_mean_dif<-t(t(train_x_samples)-positive_training_mean)
  p_dif_over_sd<-t(t(p_sample_mean_dif)/positive_training_sd)
  positive_training_cost_sum<--(1/2)*rowSums(apply(p_dif_over_sd,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(positive_training_sd))
  positive_training_cost_sum<-positive_training_cost_sum # + log(221/615)
  
  # For negative labels, look at the training data and compute the error or distance from the normal distribution
  n_sample_mean_dif<-t(t(train_x_samples)-negative_training_mean)
  n_dif_over_sd<-t(t(n_sample_mean_dif)/negative_training_sd)
  negative_training_cost_sum<--(1/2)*rowSums(apply(n_dif_over_sd,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(negative_training_sd))
  negative_training_cost_sum<-negative_training_cost_sum # + log(221/615)
  
  # Get the list of data where positive_training is larger
  positive_flag <- positive_training_cost_sum > negative_training_cost_sum
  gotrighttr <- positive_flag==train_y_samples

  # calculate error rate
  training_score[wi]<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))

  # use trained data to find test error rates, positive
  p_test_mean_dif<-t(t(x_test_data)-positive_training_mean)
  p_test_over_sd<-t(t(p_test_mean_dif)/positive_training_sd)
  positive_testing_error_cost<--(1/2)*rowSums(apply(p_test_over_sd,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(positive_training_sd))
  
  # use trained data to find test error rates, positive
  n_test_mean_dif<-t(t(x_test_data)-negative_training_mean)
  n_test_over_sd<-t(t(n_test_mean_dif)/negative_training_sd)
  negative_testing_error_cost<--(1/2)*rowSums(apply(n_test_over_sd,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(negative_training_sd))
  
  # Calculate error rate of the testing data
  correct_testing_indices<-positive_testing_error_cost>negative_testing_error_cost
  gotright<-correct_testing_indices==y_test_data
  testing_score[wi]<-sum(gotright)/(sum(gotright)+sum(!gotright))
}

# Run multiple experiments, print out training correct rate and testing correct rate
cat("Average training correct rate over 100 trials:")
print(mean(training_score))
cat("\nAverage testing correct rate over 100 trials:")
print(mean(testing_score))
cat("\n")
