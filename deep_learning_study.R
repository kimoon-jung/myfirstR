# 맛보기

library(keras)
library(tensorflow)
mnist <- dataset_mnist()

train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>% 
  layer_dense(units = 10, activation = "softmax")

network %>% compile(
  optimizer = "rmsprop", 
  loss = "categorical_crossentropy", 
  metrics = c("accuracy")
)

train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images/255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images/255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

metrics <- network %>% evaluate(test_images, test_labels)
metrics
network %>% predict_classes(test_images[1:10,])

x <- c(12, 3, 6, 14, 10)
str(x)
dim(as.array(x))

length(dim(train_images))
dim(train_images)
typeof(train_images)
digit <- train_images[5,,]
plot(as.raster(digit, max = 255))

my_slice <- train_images[10:99,,]
dim(my_slice)

batch <- train_images[1:128,,]

layer_dense(units = 512, activation = "relu")
output = relu(dot(W, input) + b)

naive_relu <- function(x){
  for(i in 1:nrow(x))
    for(j in 1:ncol(x))
      x[i, j] <- max(x[i, j], 0)
  x
}

naive_add <- function(x, y){
  for(i in 1:nrow(x))
    for(i in 1:ncol(x))
      x[i, j] = x[i, j] + y[i, j]
}

z <- x + y
z <- pmax(z, 0)

sweep(x, 2, y, '+')

x <- array(round(runif(1000, 0, 9)), dim = c(64, 3, 32, 10))
y <- array(5, dim = c(32, 10))
z <- sweep(x, c(3, 4), y, pmax)

naive_vector_dot <- function(x, y){
  z <- 0
  for(i in 1:length(x))
    z <- z + x[[i]] * y[[i]]
  z
}

naive_matrix_vector_dot <- function(x, y){
  z <- rep(0, nrow(x))
  for(i in 1:nrow(x))
    for(j in 1:ncol(x))
      z[[i]] <- z[[i]] + x[[i, j]] * y[[j]]
  z
}

navie_matrix_vector_dot2 <- function(x, y){
  z <- rep(0, nrow(x))
  for(i in 1:nrow(x))
    z[[i]] <- naive_vector_dot(x[i,], y)
  z
}

naive_matrix_dot <- function(x, y){
  z <- matrix(0, nrow = nrow(x), ncol = ncol(y))
  for(i in 1:nrow(x))
    for(j in 1:ncol(y)){
      row_x <- x[i,]
      column_y <- y[,j]
      z[i, j] <- naive_vector_dot(row_x, column_y)
    }
  z
}

train_images <- array_reshape(train_images, c(60000, 28 * 28))

x <- matrix(c(0, 1,
              2, 3,
              4, 5),
            nrow = 3, ncol = 2, byrow = TRUE)
x <- array_reshape(x, dim = c(2, 3))
x
x <- matrix(0, nrow = 300, ncol = 20)
dim(x)
x <- -t(x)
x
dim(x)

A <- c(0.5, 1.0)
B <- c(1.0, 0.25)
C <- A + B 
C
