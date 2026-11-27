library(tidyverse)
library(readxl)
library(gtsummary)
library(GGally)
library(naniar)
library(tidymodels)
library(car)
library(rpart.plot)
library(DescTools)
library(fairmodels)
library(DALEX)


# ---- Anàlisi de sensibilitat ----

# 1. Models
lm_mod_explainer <- create_explainer(lm_fit, data_mod, "Linear regression", 
                                     regression = TRUE)
glm_mod_explainer <- create_explainer(glm_fit, data_mod, "Logistic regression")
rf_mod_explainer <- create_explainer(rf_fit, data_mod, "Random forest")
dt_mod_explainer <- create_explainer(dt_fit, data_mod, "Decision tree")

# 2. Fairness
fobject_mod <- fairness_check(
  lm_mod_explainer, glm_mod_explainer, rf_mod_explainer, dt_mod_explainer, 
  protected = data_mod$Sex, 
  privileged = "M", 
  verbose = FALSE
)

plot_fairness_mod <- plot(fobject_mod)

# 3. Intervals de confiança
table_fair_mod_glm <- map_dfr(
  fobject_mod$groups_confusion_matrices$`Logistic regression`, 
  function(x) as_tibble(x[c("tp", "fp", "tn", "fn")]), 
  .id = "Sex"
) |> 
  mutate(
    Sex, 
    ACC_x = tp + tn, 
    ACC_n = tp + fp + tn + fn, 
    TPR_x = tp, 
    TPR_n = tp + fn, 
    FPR_x = fp, 
    FPR_n = fp + tn, 
    PPV_x = tp, 
    PPV_n = tp + fp, 
    STP_x = tp + fp, 
    STP_n = tp + fp + tn + fn, 
    .keep = "none"
  ) |> 
  pivot_longer(-Sex, names_to = c("metric", ".value"), names_sep = "_") |> 
  pivot_wider(names_from = Sex, values_from = c(x, n)) |> 
  rowwise() |> 
  mutate(
    p_F = round(x_F / n_F, 2), 
    p_M = round(x_M / n_M, 2), 
    prop_F = sprintf("%.2f (%d/%d)", p_F, x_F, n_F), 
    prop_M = sprintf("%.2f (%d/%d)", p_M, x_M, n_M), 
    d = BinomDiffCI(x_F, n_F, x_M, n_M, method = "score"), 
    r = BinomRatioCI(x_F, n_F, x_M, n_M, method = "koopman"), 
    dif = sprintf("%.2f (%.2f; %.2f)", d[, 1], d[, 2], d[, 3]), 
    ratio = sprintf("%.2f (%.2f; %.2f)", r[, 1], r[, 2], r[, 3]), 
    p_val = round(fisher.test(
      matrix(c(x_F, n_F - x_F, x_M, n_M - x_M), nrow = 2)
    )$p.value, 3)
  ) |> 
  select(metric, prop_F, prop_M, ratio)

table_fair_mod_rf <- map_dfr(
  fobject_mod$groups_confusion_matrices$`Random forest`, 
  function(x) as_tibble(x[c("tp", "fp", "tn", "fn")]), 
  .id = "Sex"
) |> 
  mutate(
    Sex, 
    ACC_x = tp + tn, 
    ACC_n = tp + fp + tn + fn, 
    TPR_x = tp, 
    TPR_n = tp + fn, 
    FPR_x = fp, 
    FPR_n = fp + tn, 
    PPV_x = tp, 
    PPV_n = tp + fp, 
    STP_x = tp + fp, 
    STP_n = tp + fp + tn + fn, 
    .keep = "none"
  ) |> 
  pivot_longer(-Sex, names_to = c("metric", ".value"), names_sep = "_") |> 
  pivot_wider(names_from = Sex, values_from = c(x, n)) |> 
  rowwise() |> 
  mutate(
    p_F = round(x_F / n_F, 2), 
    p_M = round(x_M / n_M, 2), 
    prop_F = sprintf("%.2f (%d/%d)", p_F, x_F, n_F), 
    prop_M = sprintf("%.2f (%d/%d)", p_M, x_M, n_M), 
    d = BinomDiffCI(x_F, n_F, x_M, n_M, method = "score"), 
    r = BinomRatioCI(x_F, n_F, x_M, n_M, method = "koopman"), 
    dif = sprintf("%.2f (%.2f; %.2f)", d[, 1], d[, 2], d[, 3]), 
    ratio = sprintf("%.2f (%.2f; %.2f)", r[, 1], r[, 2], r[, 3]), 
    p_val = round(fisher.test(
      matrix(c(x_F, n_F - x_F, x_M, n_M - x_M), nrow = 2)
    )$p.value, 3)
  ) |> 
  select(metric, prop_F, prop_M, ratio)