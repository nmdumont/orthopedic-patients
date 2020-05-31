#Installation of packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("rpart", repos = "http://cran.us.r-project.org")

# Upload the data from my personal GitHub Repo
url<-"https://raw.githubusercontent.com/nmdumont/orthopedic-patients/master/column_3C_weka.csv"
data<-read.csv(url(url))
#Let's look at the data
head(data)
#We search for NA values
sum(is.na(data))
#No unknown data, hurray. Make sure how the data are organized :
str(data)
#We have 310 observations of 7 variables : 6 degrees of various bones and the indication of disease.
library(ggplot2)
#Let's see how each angle has an incidence over the defect
data%>%
  gather(angle, degree, -class)%>%
  ggplot(aes(class,degree, col=class))+
  geom_boxplot() +
  facet_wrap(~angle)
#It seems that spondylolisthesis angle is a good predictor for spondylosisthesis, seems legit !
#We do not have a clear dinstinction between hernia and normal for the various angles, except for sacral slope
#Maybe if we observe a certain range in sacral slope we could, thanks to another angle, determine if the patient has hernia or not ?

#I want to make some plots and seek for correlation
#Arbitrarily I select the first two variables to predict the "disease"
data%>% filter (class!="Spondylolisthesis")%>%
  ggplot(aes(sacral_slope,pelvic_tilt, col=class))+
  geom_point()
#all seem quite intricate for those two variables.
#Hernia and Normal are similar, but the pattern for spondylosthesis looks a little bit different 
#It is time to separate the dataset into test and training set.
set.seed(1985)
test_index <- createDataPartition(data$class, times = 1, p = 0.2, list = FALSE)    # create a 20% test set
test_set <- data[test_index,]
train_set <- data[-test_index,]
nrow(train_set)
#Now, trying to guess the disease :
guess <- sample(c("Hernia","Normal","Spondylolisthesis"), nrow(test_set), replace = TRUE)
mean(guess == test_set$class)
#accuracy_results<- tibble(method="", accuracy=)
#We train a glm model on all predictors. Does not work since only 2-class outcome for glm#
#train_glm_all <- train(class~sacral_slope+pelvic_tilt, method = "glm", data = train_set)
#glm_all_preds <- predict(train_glm_all, test_set)
#mean(glm_all_preds == test_set$class)
#logistic regression
fit_glm<-glm(class~sacral_slope+pelvic_tilt, data=train_set, family=binomial())
summary(fit_glm)
fit_glm$model

p_hat_glm<-predict(fit_glm,test_set)
y_hat_glm<-factor(ifelse(p_hat_glm>0.5,"Normal","Hernia"))
confusionMatrix(data=y_hat_glm, reference=test_set$class)

#Let's use regression/decision tree
library(rpart)
fit<-rpart(class~.,data=train_set)
plot(fit, margin=0.1)
text(fit,cex=0.75)
train_rpart <- train(class ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$class)
confusionMatrix(rpart_preds, test_set$class)
data_spondy<- factor(ifelse(train_set$degree_spondylolisthesis < 16.08,"Spondylolisthesis","Normal"))
mean(data_spondy==train_set$class)

#Random forest
train_rf <- train(class ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 150,
                  tuneGrid = data.frame(mtry = seq(1:7)))
plot(train_rf)
train_rf$bestTune
rf_preds <- predict(train_rf, test_set)
confusionMatrix(rf_preds,test_set$class)
mean(rf_preds == test_set$class)
varImp(train_rf$finalModel) 

#KNN 
knn_fit<-knn3(class~.,data=train_set,k=5)
confusionMatrix(predict(knn_fit,test_set,type="class"),test_set$class)$overall["Accuracy"]
train_knn <- train(class~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
confusionMatrix(predict(train_knn,test_set,type="raw"),test_set$class)$overall["Accuracy"]


ggplot(train_knn,highlight = TRUE)

#Cross-validation on KNN
set.seed(36)
train_knn_cv <- train(class ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune
ggplot(train_knn_cv,highlight=TRUE)
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$class)


#PCA
x<-train_set[,-7]%>%as.matrix()
d<-dist(x)
image(as.matrix(d),col=rev(RColorBrewer::brewer.pal(9,"RdBu")))
cor(x)
pca<-prcomp(x)
summary(pca)$importance[,1:2]
qplot(1:6,pca$sdev)
x_train<-pca$x[,1:3]
y<-factor(train_set$class)
fit<-knn3(x_train,y)
col_means<-colMeans(test_set[,-7])
z<-test_set[,-7]%>%as.matrix()
x_test<-sweep(z,2,colMeans(z,na.rm=TRUE))%*%pca$rotation
x_test<-x_test[,1:3]
y_hat<-predict(fit,x_test,type="class")
confusionMatrix(y_hat,factor(test_set$class))$overall["Accuracy"]

h<-hclust(d)
plot(h,cex=0.65)

cor(train_sansspo)
data.frame(pca$x[,1:2], Disease=train_set$class) %>%
  ggplot(aes(PC1,PC2,fill=Disease))+
  geom_point(cex=3,pch=21)
x_train<-pca$x[,1:3]
y<-factor(train_set$class)
fit<-knn3(x_train,y)

address<-"https://raw.githubusercontent.com/nmdumont/orthopedic-patients/master/column_2C_weka.csv"
data_2<-read.csv(url(address))
head(data_2)
#We search for NA values
sum(is.na(data_2))
#Let's look at the structure:
str(data_2)
levels(data_2$class)
str(unique(data_2))

data_2%>%
  gather(angle, degree, -class) %>%
  ggplot(aes(class,degree, col=class))+
  geom_boxplot() +
  facet_wrap(~angle)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
data_2%>% 
  ggplot(aes(sacral_slope,pelvic_tilt.numeric, col=class))+
  geom_point()
data_2%>% 
  ggplot(aes(sacral_slope,lumbar_lordosis_angle, col=class))+
  geom_point()
data_2%>% 
  ggplot(aes(pelvic_radius,lumbar_lordosis_angle, col=class))+
  geom_point()
set.seed(3)
test_index <- createDataPartition(data_2$class, times = 1, p = 0.2, list = FALSE)    # create a 20% test set
test_set_2 <- data_2[test_index,]
train_set_2 <- data_2[-test_index,]
nrow(train_set_2)

fit_rpart_2<-rpart(class~.,data=train_set_2)
plot(fit_rpart_2, margin=0.1)
text(fit_rpart_2,cex=0.65)
train_rpart_2 <- train(class ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set_2)
ggplot(train_rpart_2)
rpart_preds_2 <- predict(train_rpart_2, test_set_2)
confusionMatrix(rpart_preds_2, test_set_2$class)


train_knn_cv_2 <- train(class ~ .,
                        method = "knn",
                        data = train_set_2,
                        tuneGrid = data.frame(k = seq(3, 51, 2)),
                        trControl = trainControl(method = "cv", number = 10, p = 0.9))
knn_cv_preds_2 <- predict(train_knn_cv_2, test_set_2)

confusionMatrix(predict(train_knn_cv_2,test_set_2,type="raw"),test_set_2$class)$overall["Accuracy"]

x_2<-train_set_2[,-7]%>%as.matrix()
pca_2<-prcomp(x_2)
data.frame(pca_2$x[,1:2], Disease=train_set_2$class) %>%
  ggplot(aes(PC1,PC2,fill=Disease))+
  geom_point(cex=3,pch=21)
summary(pca_2)
x_train_2<-pca_2$x[,1:3]
y_2<-factor(train_set_2$class)
fit_2<-knn3(x_train_2,y_2)
z_2<-test_set_2[,-7]%>%as.matrix()
x_test_2<-sweep(z_2,2,colMeans(z_2,na.rm=TRUE))%*%pca_2$rotation
x_test_2<-x_test_2[,1:3]
y_hat_2<-predict(fit_2,x_test_2,type="class")
pca_2_acc<-confusionMatrix(y_hat_2,test_set_2$class)$overall["Accuracy"]
accuracy_results_2<-bind_rows(accuracy_results_2,tibble(Method="PCA ",Accuracy=pca_2_acc))
