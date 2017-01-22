Data<-read.csv('processed.cleveland.data.txt', header=FALSE)
library(klaR)
library(caret)

# If Data's final column is greater than 1, count it as one
has_zero_index <- Data[, 14] > 0
Data[has_zero_index, 14]=1

# Separate x entries and y labels
x_data<-Data[,-c(14)]
y_value<-as.factor(Data[,14])

# Return a partitioned training_x and training_y (.85 of the data set) to train on
training_data<-createDataPartition(y=y_value, p=.85, list=FALSE)
training_x<-x_data[training_data,]
training_y<-y_value[training_data]

# use naive bayes as a a training method with training_x and training_y as inputs, returns a model object
model<-train(training_x, training_y, 'nb', trControl=trainControl(method='cv', number=10))

# get test results value from model, and test it on the other .15 of the test data
testing_data = x_data[-training_data,]
test_res<-predict(model,newdata=testing_data)
print(confusionMatrix(data=test_res, y_value[-training_data]))