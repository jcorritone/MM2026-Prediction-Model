fit_model_02_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      OffEffDiff,
      DefEffDiff,
      AdjTempoDiff,
      SOSDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + I(SeedDiff^2) + OffEffDiff + I(OffEffDiff^2) + DefEffDiff +
    OffEffDiff:DefEffDiff + SOSDiff + I(SOSDiff^2),
    data = model_data,
    family = binomial(link = "logit")
  )
}
