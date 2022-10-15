w <- read.csv("d:/R/data/R-main/ch7-1.csv", header =TRUE)
head(w)
str(w) 

ind <- sample(1:nrow(w), nrow(w)*0.8, replace = F)
r_train <- w[ind,]
r_test <- w[-ind,]

cor(r_train)
plot(r_train)

library(nnet)

r_nn <- nnet(weight~., data = r_train, size = 3, decay =5e-4,
             ,rang = 0.1, maxit = 500, linout = TRUE)

summary(r_nn)


##================================================================
## 신경망 시각화
##================================================================
# 시각화 R 코드 함수 다운로드

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# 신경망 모형 시각화
library(reshape2)
plot.nnet(r_nn)

##================================================================
## 06. 신경망 이해
##================================================================
library(NeuralNetTools)
garson(r_nn)
lekprofile(iris.nn)
