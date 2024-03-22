
# remotes::install_github("rstudio/tensorflow")
# reticulate::install_python('3.10:latest')
# 
# 
# library(tensorflow)
# install_tensorflow(envname = "r-tensorflow")
# 
# install.packages("keras")
# library(keras)
# install_keras()
# 
# 
# library(tensorflow)
# 
# tf$constant("Hello TensorFlow!")





# Libraries
# install.packages("keras","tensorflow")
library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)

# Data
data("BostonHousing")
data <- BostonHousing
str(data)

data %<>% mutate_if(is.factor, as.numeric)

# Neural Network Visualization
n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = data,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)
plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:13]
test <- data[ind==2, 1:13]
trainingtarget <- data[ind==1, 14]
testtarget <- data[ind==2, 14]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Create Model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)






# Tine model

# modif 1 
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model)
# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)



# modif 2 : ajout de hlayers
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.4)  %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3)  %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 1)

summary(model)
# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)


# modif 3 : optimizer = optimizer_rmsprop(learning_rate = 0.001)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.4)  %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3)  %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.2)  %>%
  layer_dense(units = 1)

summary(model)
# Compile
model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(learning_rate = 0.002),
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)




