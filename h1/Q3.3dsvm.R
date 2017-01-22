Data<-read.csv('processed.cleveland.data.txt', header=FALSE)
library(klaR)
library(caret)
x_matrix<-Data[,-c(9)]
bigy<-as.factor(Data[,9])
wtd<-createDataPartition(y=y_data, p=.8, list=FALSE)
svm<-svmlight(x_matrix[wtd,], bigy[wtd], pathsvm='/Users/daf/Downloads/svm_light_osx.8.4_i7/')
labels<-predict(svm, x_matrix[-wtd,])
foo<-labels$class
sum(foo==bigy[-wtd])/(sum(foo==bigy[-wtd])+sum(!(foo==bigy[-wtd])))