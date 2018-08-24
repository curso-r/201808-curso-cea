# Preparar base -----------------------------------------------------------

library(recipes)
library(ggplot2)

receita <- recipe(price ~ . , data = diamonds) %>%
  step_dummy(cut, color, clarity) %>%
  step_log(all_outcomes()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

prep <- prep(receita, data = diamonds)

matriz <- bake(prep, newdata = diamonds, composition = "matrix")
y <- matriz[, "price"]
X <- matriz[, -which(colnames(matriz) == "price")]
dim(X)

# Exemplo 1 ---------------------------------------------------------------

library(keras)

input <- layer_input(shape = 23)
output <- input %>% layer_dense(units = 1)

model <- keras_model(input, output)
model

model %>%
  compile(
    optimizer = "sgd", 
    loss = loss_mean_squared_error,
    metrics = metric_mean_squared_error
  )

model %>%
  fit(X, y, validation_split = 0.2)


# Exemplo Classif ---------------------------------------------------------

X <- runif(10000) %>% matrix(ncol = 10)
y_p <- 1/(1 + exp(-(0.5*X[,1] + 0.1*X[,3] + 0.5*X[,7])))
summary(y_p)
y <- rbinom(length(y_p), size = 1, y_p)

input <- layer_input(shape = 10)
output <- input %>%
  layer_dense(1, name = "w") %>%
  layer_activation("sigmoid")

model <- keras_model(input, output)
model

model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "sgd",
    metrics = "accuracy"
  )

model %>%
  fit(
    X, y,
    validation_split = 0.1
  )

get_layer(model, "w") %>% get_weights()
glm(y ~ ., data = as.data.frame(X), family = "binomial")


