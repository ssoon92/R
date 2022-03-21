datasets::iris
iris
idx<-createDataPartition(iris$Species,p=0.7,list=F)
idx
iris_train<-iris[idx,]
iris_test<-iris[-idx,]

#rpart
rpart.iris<-rpart(Species ~ ., data = iris_train)
rpart.iris
rpart.plot(rpart.iris)
rpart.pred<-predict(rpart.iris, newdata = iris_test,type = "class")
confusionMatrix(rpart.pred, data=iris_test$Species)

#naive bayes
bayes.iris<-naiveBayes(Species ~ ., data = iris_train )
bayes.pred<-predict(bayes.iris, newdata = iris_test, type = "class")
confusionMatrix(bayes.pred, iris_test$Species)

#random forest
rdf.iris<-randomForest(Species ~ ., data = iris_train, importance=T)
rdf.pred<-predict(rdf.iris, newdata = iris_test, type="response")
confusionMatrix(rdf.pred,iris_test$Species)
