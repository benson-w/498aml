# Load pima indians text file
Data<-read.csv('pima-indians-diabetes.data.txt', header=FALSE)
library(klaR)
library(caret)

# Separate Data into the traits (x)  and labels (y)
x_matrix<-Data[,-c(9)]
y_value<-as.factor(Data[,9])

# Randomly partition the data, and select the training data
random_partition_index<-createDataPartition(y=y_value, p=.8, list=FALSE)
train_x<-x_matrix[random_partition_index,]
train_y<-y_value[random_partition_index]

# Train the model with naive bayes
model<-train(train_x, train_y, 'nb', trControl=trainControl(method='cv', number=10))

# Using the training model, test against the data not used in the training data
teclasses<-predict(model,newdata=x_matrix[-random_partition_index,])

# Print the confusion matrix
print(confusionMatrix(data=teclasses, y_value[-random_partition_index]))
