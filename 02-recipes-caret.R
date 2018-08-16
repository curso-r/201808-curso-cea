
# Pacotes -----------------------------------------------------------------

library(recipes)
library(caret)
library(dplyr)


# REGRESSÃO LINEAR --------------------------------------------------------

# Dados -------------------------------------------------------------------

data(diamonds, package = "ggplot2")
glimpse(diamonds)

diamonds <- diamonds %>%
  mutate(
    cut = as.character(cut),
    color = as.character(color),
    clarity = as.character(clarity)
  )

# Criando uma base de 'novos diamantes'

id_new <- sample(nrow(diamonds), size = 1000)
new_diamonds <- diamonds[id_new,]
diamonds <- diamonds[-id_new,]

ggplot(diamonds, aes(x = log(price))) + 
  geom_histogram(bins = 20)

# Criando receita

receita <- recipe(price ~ . , data = diamonds) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors()) %>% 
  step_log(all_outcomes())

# Treinando o modelo

train_control <- trainControl(
  method = "cv", 
  number = 5
)

modelo <- train(
  receita, 
  diamonds, 
  method = "lm", 
  trControl = train_control
)

modelo
summary(modelo)
varImp(modelo)


# Fazendo previsões -------------------------------------------------------

prep <- prep(receita, diamonds)
new_diamonds_prep <- bake(prep, new_diamonds)

new_diamonds$valores_preditos <- predict(modelo, new_diamonds_prep)

ggplot(new_diamonds_prep, aes(x = valores_preditos, y = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")
# 
# 
# # GLM ---------------------------------------------------------------------
# 
# # Criando receita
# 
# receita <- recipe(price ~ . , data = diamonds) %>%
#   step_dummy(all_nominal(), -all_outcomes()) %>%
#   step_nzv(all_predictors()) %>%
#   step_corr(all_predictors())
# 
# # Treinando o modelo
# 
# modelo <- train(
#   receita, 
#   diamonds, 
#   method = "glm",
#   family = inverse.gaussian(),
#   trControl = train_control
# )
# 
# modelo
# summary(modelo)
# varImp(modelo)
# 
# # Fazendo previsões -------------------------------------------------------
# 
# prep <- prep(receita, diamonds)
# new_diamonds_prep <- bake(prep, new_diamonds)
# 
# new_diamonds$valores_preditos <- predict(modelo, new_diamonds_prep)
# 
# ggplot(new_diamonds_prep, aes(x = valores_preditos, y = price)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "blue")


