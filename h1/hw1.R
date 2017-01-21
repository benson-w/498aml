library(e1071)
library(caret)
set.seed(11) # so we can replicate random results

D = read.csv("pima-indians-diabetes.data.txt", header=F)

splitfunction <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <-1:nrow(dataframe)
  trainindex <- createDataPartition(iris$V8, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  trainset <- dataframe[trainindex,]
  trainset
}

s


