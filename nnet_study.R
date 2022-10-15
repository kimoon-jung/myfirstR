library(nnet)
library(MASS)
library(caret)

install.packages("keras")
install.packages("tensorflow")
library(keras)
library(tensorflow)
install_keras()
install_tensorflow()

install.packages("h2o")
library(h2o)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

library(reshape)
library(reshape2)
library(NeuralNetTools)

w <- read.csv("d:/R/data/R-main/ch7-1.csv", header =TRUE)
head(w)
str(w) 

ind <- sample(1:nrow(w), nrow(w)*0.8, replace = F)
r_train <- w[ind,]
r_test <- w[-ind,]

cor(r_train)
plot(r_train)



r_nn <- nnet(weight~., data = r_train, size = 3, decay =5e-4,
             ,rang = 0.1, maxit = 500, linout = TRUE)

summary(r_nn)

test <- r_test
test$pred <- predict(r_nn, newdata = r_test)
head(test)
plot(test)


nor <- preProcess(w[,1:2], method = "range")
r_x_train <- predict(nor, r_train[,1:2])
summary(r_x_train)
r_x_test <- predict(nor, r_test[,1:2])
summary(r_x_test)

r_n_train <- cbind(r_x_train, r_train[,3])
names(r_n_train)[3] <- "weight"
r_n_test <- cbind(r_x_test, r_test[,3])
names(r_n_test)[3] <- "weight"
r_nn_s <- nnet(weight~., data = r_n_train, size = 3, decay = 5e-4,
               ,rang = 0.1, maxit = 500, linout = TRUE)
summary(r_nn_s)
plot.nnet(r_nn_s)

r_n_test$pred <- predict(r_nn_s, newdata = r_n_test)


R2(r_n_test$pred, r_n_test$weight)      # R-sq
RMSE(r_n_test$pred, r_n_test$weight)    # RMSE (root mean sq error)
MAE(r_n_test$pred, r_n_test$weight)     # MAE (mean absolute error)

garson(r_nn_s)
lekprofile(r_nn_s)


h2o.init()        # h2o library 기동
hf_r_train <- as.h2o(r_train)
hf_r_test <- as.h2o(r_test)
head(hf_r_train)
str(hf_r_train)
fit <- h2o.deeplearning(x = 1:2, y = 3, training_frame = hf_r_train,
                        hidden = c(3,3), epochs = 200,
                        standardize = TRUE)
summary(fit)

#plot.nnet(fit)
r_pred <- h2o.predict(fit, newdata = hf_r_test)
class(r_pred)
df_r_pred <- as.data.frame(r_pred)
class(df_r_pred)

R2(df_r_pred$predict, r_test$weight)      # R-sq
RMSE(df_r_pred$predict, r_test$weight)    # RMSE (root mean sq error)
MAE(df_r_pred$predict, r_test$weight)     # MAE (mean absolute error)



c_train <- read.csv("d:/R/data/R-main/ch6-2_train.csv", header = TRUE)
c_test <- read.csv("d:/R/data/R-main/ch6-2_test.csv", header = TRUE)

c_train$judge <- ifelse(c_train$breeds == "a", 0, 
                        ifelse(c_train$breeds == "b", 1, 2))
c_test$judge <- ifelse(c_test$breeds == "a", 0, 
                        ifelse(c_test$breeds == "b", 1, 2))

c_x_train <- c_train[,1:3]
c_y_train <- c_train[,4]
c_x_test <- c_test[,1:3]
c_y_test <- c_test[,4]

nor <- preProcess(c_x_train[,1:3], method = "range")
n_c_x_train <- predict(nor, c_x_train)
summary(n_c_x_train)
n_c_x_test <- predict(nor, c_x_test)
summary(n_c_x_test)



n_c_x_train <- as.matrix(n_c_x_train)
n_c_x_test <- as.matrix(n_c_x_test)
nu_c_y_train <- as.numeric(c_train[,5])
nu_c_y_test <- as.numeric(c_test[,5])
class(n_c_x_train)
class(nu_c_y_train)

o_c_y_train <- to_categorical(nu_c_y_train)
o_c_y_test <- to_categorical(nu_c_y_test)

model <- keras_model_sequential()

model %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = 3) %>% 
  layer_dense(units = 16, activation = 'relu') %>% 
  layer_dense(units = 3, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  n_c_x_train,
  o_c_y_train,
  epochs = 300,
  batch_size = 16,
  validation_split = 0.2
)

plot(history)

pred_mat <- model %>% predict(n_c_x_test)

head(pred_mat)

pred_mat_label <- levels(c_y_test)[max.col(pred_mat)]

head(pred_mat_label)

pred <- as.factor(pred_mat_label)

act <- as.factor(c_y_test)

confusionMatrix(pred, act)

##================================================================
## 신경망 시각화
##================================================================
# 시각화 R 코드 함수 다운로드


# 신경망 모형 시각화

plot.nnet(r_nn)

##================================================================
## 06. 신경망 이해
##================================================================

garson(r_nn)
lekprofile(r_nn)
