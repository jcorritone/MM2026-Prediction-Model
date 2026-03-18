library(tidyverse)

fit_model_03_ridge <- function(train_data_ridge) {

  model_data <- train_data_ridge %>%
    select(
      Outcome,
      SeedDiff,
      RidgeRatingDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + RidgeRatingDiff,
    data = model_data,
    family = binomial(link = "logit"))
}
