library(keras)

set.seed(123)
x<-runif(10000,-5,+5)
y<-x ** 4 - 20 * x ** 2 +  10 * x + 4 + runif(10000,0,100)

plot(x,y)
train.ind<-sample(x=1:length(x), size=0.8*length(x))
x_train<-x[train.ind]
x_test<-x[-train.ind]
y_train<-y[train.ind]
y_test<-y[-train.ind]

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 12, activation = 'elu', input_shape = 1) %>% 
  layer_dense(units = 9, activation = 'elu') %>% 
  layer_dense(units = 6, activation = 'elu') %>% 
  layer_dense(units = 3, activation = 'elu') %>%
  layer_dense(units = 1, activation = 'linear')


model %>% compile(
  loss = 'mse',
  optimizer = optimizer_rmsprop(),
  metrics = c('mse')
)


history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 16, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)

predicted<-model %>% predict(x_test)

df<-data.frame(x=x_test,
           y=y_test,
           y.pred=predicted)

plot(x_test, y_test)
plot(x_test, predicted)

library(ggplot2)
ggplot(df) +
  geom_point(aes(x=x, y=y))+
  geom_line(aes(x=x,y=predicted), color="red")

get_weights(model)

names(model)
model$weights
