library(ElemStatLearn)
library(AppliedPredictiveModeling)
library(caret)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(e1071)
library(dplyr)
library(tidyverse)
library(pls)
library(fastDummies)
library(nnet)

dataset <-  read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
dataset$classe = as.factor(dataset$classe)
dataset2 <- dataset[ , colSums(is.na(dataset)) ==0]
dataset3 <- dataset2[,-c(12:20,43:48,52:60,74:82)]
dataset4 <- dataset3[,-c(1,3:7)]


set.seed(1234)
in_D4_train = createDataPartition(y=dataset4$classe, p=0.60, list = FALSE)
D4_Train <- dataset4[in_D4_train,] 
D4_Test <- dataset4[-in_D4_train,]

preProcPCA <- preProcess(D4_Train, method = "pca",thresh = 0.95)
PredictPCA <- predict(preProcPCA, D4_Train) %>% 
  select(-c('user_name'))


mod_fit_rf <- train(classe ~ ., method = 'rf', data = PredictPCA)
rf_fit <- predict(mod_fit_rf, newdata = PredictPCA)
train_w_results <- PredictPCA
train_w_results$rf_fit = as.factor(rf_fit)
confusionMatrix(train_w_results$classe, train_w_results$rf_fit)


PredictPCT_Test <- predict(preProcPCA, D4_Test) %>% 
  select(-c('user_name'))
rf_test_fit <- predict(mod_fit_rf, newdata = PredictPCT_Test)
test_w_results <- PredictPCT_Test
test_w_results$rf_fit = as.factor(rf_test_fit)
confusionMatrix(train_w_results$classe, train_w_results$rf_fit)


test_for_quiz <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')
test_for_quiz_predict <- predict(preProcPCA, test_for_quiz)
quiz_predict <- predict(mod_fit_rf, newdata = test_for_quiz_predict)

forquiz_w_results <- test_for_quiz_predict
forquiz_w_results$quiz_predict <- quiz_predict
results <- subset(forquiz_w_results, select = c(X, quiz_predict))




predict_









confusionMatrix(PredictPCT_Test$classe, rf_test_fit)



# PredictPCA2 <- dummy_cols(PredictPCA, select_columns = 'user_name') %>% 
#   select( -c('user_name'))
rm(mod_fit_boost)
mod_fit_boost <- train(classe ~ ., method = 'gbm', data = PredictPCA)
mod_fit_boost_test <- predict(mod_fit_boost, newdata =  PredictPCA)
PredictPCA$estimate <- mod_fit_boost_test
confusionMatrix(PredictPCA$classe, PredictPCA$estimate)







mod_fit_lasso <- train(classe ~ ., method = 'lasso', data = PredictPCA)
rf_fit <- predict(mod_fit_rf, newdata = PredictPCA)
test <- PredictPCA
test$fit_rf = rf_fit
confusionMatrix(test$classe, test$fit_rf)


mod_fit_lda <- train(classe ~ ., method = 'lda', data = PredictPCA)
lda_fit <- predict(mod_fit_lda, newdata = PredictPCA)
test_lda <- PredictPCA
test_lda$fit_lda <- lda_fit
confusionMatrix(test_lda$classe, test_lda$fit_lda)

mfit <- multinom(classe ~  ., data = PredictPCA)
multinom_fit <- predict(mfit, type="probs", newdata=PredictPCA)
test_Multi <- PredictPCA
test_Multi$



confusionMatrix(PredictPCA$classe, rf_fit)

featurePlot(x = trainData[, 1:18], 
            y = trainData$Purchase, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))




in_D4_train_pca <- prcomp(D4_Train[,-c(1,54)], scale = TRUE)
# reverse sign
in_D4_train_pca$x <- -1*in_D4_train_pca$x
# Calculate variance explained by each component
var_explained = in_D4_train_pca$sdev^2 / sum(in_D4_train_pca$sdev^2)
qplot(c(1:52), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)



test <- D4_Train[,-c(1,54)]


df_scaled = dataset4 %>% group_by(classe) %>% mutate_if(is.numeric, scale) %>% ungroup()
df_scaled_dum = dummy_cols(df_scaled, select_columns = 'user_name') %>% 
  subset(select = -c(user_name))
df_scaled2 <- df_scaled %>% 
  subset(select = -c(classe, user_name))



dataset5 <- subset(df_scaled_dum, select = -c(classe))
prCompAnalysis <- prcomp(dataset5) 
summary(prCompAnalysis)


df_scaled_num <- data.frame(subset(df_scaled, select = -c(user_name, classe)))
# Do first PCA and it breaks out people very nicely by the user.  Need to scale variables within user
preProcPCA <- preProcess(dataset4, method = "pca", pcaComp = 10)
PredictPCA <- predict(preProcPCA, dataset4)

PredictPCA$user_name <- df_scaled$user_name

ggplot(PredictPCA, aes(x=PC1, y=PC2, color=user_name)) + geom_point()

df_scaled_dum_train = createDataPartition(y=df_scaled_dum$classe, p=0.2, list = FALSE)
train <- df_scaled_dum[df_scaled_dum_train,]

mod_fit_boost <- train(classe ~ ., method = 'gbm', data = PredictPCA, verbose=FALSE)
rm(mod_fit_boost)





dataset5 <- dataset4 %>% 
  group_by(user_name) %>% 
  scale()

test <- subset(dataset4, select = -c(user_name))
test2 <- scale(test)




bp <- ggplot(dataset3, aes(x=classe, y=yaw_arm)) + 
  geom_boxplot()
bp
 
boxplot()

lm_model <- lm(classe ~ roll_belt + yaw_belt, data = dataset3)
library(rattle)
model_rpart <- train(classe ~ ., method = 'rpart', data = dataset4)
fancyRpartPlot(model_rf$finalModel)
model_rf$results

model_rf <- train(classe ~ ., method = 'rf', data = dataset4, prox = TRUE)



model_rf <- train(classe ~ roll_belt + yaw_belt, method = '', data = dataset3)
dataset3$ra <- (dataset3$roll_arm) +180
test <- aov(classe ~ ra, data = dataset3)
test <- scale(dataset3$roll_belt)

test2 <- subset(dataset3,select = -c(classe))
test3 <- scale(test2)
str(test3)

summary(dataset3)

test <- dataset3 %>% 
  mutate(scaled = scale(roll_belt)) %>% 
  select(classe, scaled) %>% 
  group_by(classe) %>% 
  summarise(mean_values = mean(scaled))

sd_value = sd(test$mean_values)
range_value = abs(max(test$mean_values) - min(test$mean_values) )




df = data.frame(variable = 'test', sd = sd(test$mean), m)