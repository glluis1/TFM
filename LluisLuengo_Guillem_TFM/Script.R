library(tidyverse)
library(readxl)
library(gtsummary)
library(GGally)
library(naniar)
library(tidymodels)


# Càrrega de dades
data <- read_excel("CARC_ database.xls") |> 
  
  # Conversió de caràcters a factors
  mutate(across(where(is.character), as.factor)) |> 
  
  # Gestió de registres erronis
  mutate(
    mets_in = if_else(row_number() == 174, 6.7, mets_in),
    `6MWT_out` = if_else(row_number() == 174, NA_real_, `6MWT_out`)
  ) |> 
  
  # Gestió de factors
  mutate(
    # Edat
    `Age group` = `Age group` |> 
      fct_relevel("<50 years", "51-60 years", "61-70 years", ">70 years") |> 
      fct_relabel(~ str_replace(., "years", "anys")),
    
    # Sexe
    Sex = fct_recode(Sex, "F" = "W"),
    
    # Diagnòstic
    `Diagnosis/intervention` = `Diagnosis/intervention` |> 
      fct_collapse("Others" = c("Intervention on large vessles", "Others")) |> 
      fct_recode(
        "Síndrome coronària aguda" = "ACS +/- PCI", 
        "Insuficiència cardíaca" = "Heart insufficiency", 
        "Cirurgia valvular" = "Heart valve surgery", 
        "Altres" = "Others"
      )
  )


# Creació de variables
data <- data |> 
  # Diferències en resultats clínics
  mutate(
    # Capacitat funcional
    dif_6MWT = `6MWT_out` - `6MWT_in`,
    dif_QIS_left = QIS_left_out - QIS_left_in,
    dif_QIS_right = QIS_right_out - QIS_right_in,
    dif_MSEC = MSEC_out - MSEC_in,
    dif_mets = mets_out - mets_in,
    
    # Salut mental i QdV
    dif_had_depression = had_depression_out - had_depression_in,
    dif_had_anxiety = had_anxiety_out - had_anxiety_in,
    dif_hqol_physical = heart_quality_physical_out - heart_quality_physical_in,
    dif_hqol_emotion = heart_quality_emotion_out - heart_quality_emotion_in,
    
    # Activitat física
    dif_baecke_work = baecke_work_out - baecke_work_in,
    dif_baecke_sport = baceke_sport_out - baecke_sport_in,
    dif_baecke_leisure = baecke_leisure_out - baecke_leisure_in, 
    
    # Millora en 6MWT segonc MCID
    imp_6MWT = factor(if_else(dif_6MWT >= 25, "Yes", "No"))
  )


# Anàlisi de normalitat
data |> 
  ggplot(aes(sample = dif_6MWT)) + 
  geom_qq() + 
  geom_qq_line(color = "red", linewidth = 1) + 
  facet_wrap(~ Sex)


# ---- 4.3 Modelització ----

# Llavor aleatòria
set.seed(827)

# Transformacions i selecció de variables
data_mod <- data |> 
  # Mitjana de QIS
  mutate(
    avg_QIS_in = (QIS_right_in + QIS_left_in) / 2
  ) |>
  
  # Filtratge de valors faltants en la resposta
  drop_na(dif_6MWT) |> 
  
  # Selecció de variables per als models
  select(`Age group`, Sex, `Diagnosis/intervention`, `Smoking entry`, 
         `6MWT_in`, avg_QIS_in, MSEC_in, mets_in, 
         had_depression_in, had_anxiety_in, 
         heart_quality_physical_in, heart_quality_emotion_in, 
         baecke_work_in, baecke_sport_in, baecke_leisure_in, 
         dif_6MWT, imp_6MWT)

# Partició del conjunt de dades
mod_split <- data_mod |> 
  # 3.1. Factor d'estratificació
  mutate(
    strata_var = interaction(Sex, imp_6MWT, drop = TRUE)
  ) |> 
  
  # 3.2. Objecte de partició
  initial_split(prop = 0.75, strata = strata_var)

# Conjunt d'entrenament
data_train <- mod_split |>
  training()

# Conjunt de test
data_test <- mod_split |>
  testing()


# ---- Regressió lineal ----

set.seed(827)

# Mètriques
mod_reg_metrics <- metric_set(rmse, rsq)

# Validació creuada
mod_reg_folds <- vfold_cv(data_train, v = 5, repeats = 5, strata = strata_var)

# Preprocessament
mod_reg_recipe <- recipe(dif_6MWT ~ ., data = data_train) |> 
  step_rm(imp_6MWT, strata_var) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())

# Model
lm_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# Flux de treball
lm_workflow <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(mod_reg_recipe)

# Mètriques d'entrenament
lm_metrics_train <- lm_workflow |> 
  fit_resamples(
    resamples = mod_reg_folds, 
    metrics = mod_reg_metrics, 
    seed = 827
  ) |> 
  collect_metrics() |> 
  mutate(model = "lm_full", split_set = "cv")

# Resultats
lm_fit <- lm_workflow |> 
  last_fit(split = mod_split, metrics = mod_reg_metrics)

# Mètriques de test
lm_metrics_test <- collect_metrics(lm_fit) |> 
  mutate(model = "lm_full", split_set = "test")

# Prediccions
lm_predictions <- collect_predictions(lm_fit) |> 
  mutate(model = "lm_full")


# ---- Stepwise selection ----

set.seed(827)

# Preprocessament
stepwise_prep <- prep(mod_reg_recipe, training = data_train)
stepwise_train <- bake(stepwise_prep, new_data = data_train)

# Models complet i nul
lm_full <- lm(dif_6MWT ~ ., data = stepwise_train)
lm_null <- lm(dif_6MWT ~ 1, data = stepwise_train)

# Selecció múltiple
stepwise_model <- stats::step(
  lm_full, 
  scope = list(lower = lm_null, upper = lm_full), 
  direction = "both", 
  trace = FALSE
)

stepwise_vars <- names(coef(stepwise_model))
stepwise_vars <- stepwise_vars[stepwise_vars != "(Intercept)"]
stepwise_vars <- gsub("`", "", stepwise_vars, fixed = TRUE)

# Preprocessament
mod_reg_recipe_stepwise <- mod_reg_recipe |> 
  step_select(dif_6MWT, all_of(stepwise_vars))

# Flux de treball
stepwise_workflow <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(mod_reg_recipe_stepwise)

# Mètriques d'entrenament
stepwise_metrics_train <- stepwise_workflow |> 
  fit_resamples(
    resamples = mod_reg_folds, 
    metrics = mod_reg_metrics, 
    seed = 827
  ) |> 
  collect_metrics() |> 
  mutate(model = "stepwise", split_set = "cv")

# Resultats
stepwise_fit <- stepwise_workflow |> 
  last_fit(split = mod_split, metrics = mod_reg_metrics)

# Mètriques de test
stepwise_metrics_test <- collect_metrics(stepwise_fit) |> 
  mutate(model = "stepwise", split_set = "test")

# Prediccions
stepwise_predictions <- collect_predictions(stepwise_fit) |> 
  mutate(model = "stepwise")

rm(lm_full, lm_null, stepwise_prep, stepwise_train)


# ---- LASSO ----

set.seed(827)

# Preprocessament
mod_reg_recipe_glmnet <- mod_reg_recipe |> 
  step_normalize(all_numeric_predictors())

# Model
lasso_model <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# Flux de treball
lasso_workflow <- workflow() |> 
  add_model(lasso_model) |> 
  add_recipe(mod_reg_recipe_glmnet)

# Definició de paràmetres
lasso_tune <- lasso_workflow |> 
  tune_grid(
    resamples = mod_reg_folds, 
    grid = grid_regular(penalty(range = c(-4, 2)), levels = 30), 
    metrics = mod_reg_metrics, 
    seed = 827
  )

# Millor model
lasso_best <- select_best(lasso_tune, metric = "rmse")

# Flux de treball actualitzat
lasso_workflow <- lasso_workflow |> 
  finalize_workflow(lasso_best)

# Mètriques d'entrenament
lasso_metrics_train <- lasso_tune |> 
  collect_metrics() |> 
  semi_join(lasso_best, by = "penalty") |> 
  mutate(model = "lasso", split_set = "cv")

# Resultats
lasso_fit <- lasso_workflow |> 
  last_fit(split = mod_split, metrics = mod_reg_metrics)

# Mètriques de test
lasso_metrics_test <- collect_metrics(lasso_fit) |> 
  mutate(model = "lasso", split_set = "test")

# Prediccions
lasso_predictions <- collect_predictions(lasso_fit) |> 
  mutate(model = "lasso")


# ---- Ridge ----

set.seed(827)

# Model
ridge_model <- linear_reg(penalty = tune(), mixture = 0) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# Flux de treball
ridge_workflow <- workflow() |> 
  add_model(ridge_model) |> 
  add_recipe(mod_reg_recipe_glmnet)

# Definició de paràmetres
ridge_tune <- ridge_workflow |> 
  tune_grid(
    resamples = mod_reg_folds, 
    grid = grid_regular(penalty(range = c(-4, 2)), levels = 30), 
    metrics = mod_reg_metrics, 
    seed = 827
  )

# Millor model
ridge_best <- select_best(ridge_tune, metric = "rmse")

# Flux de treball actualitzat
ridge_workflow <- ridge_workflow |> 
  finalize_workflow(ridge_best)

# Mètriques d'entrenament
ridge_metrics_train <- ridge_tune |> 
  collect_metrics() |> 
  semi_join(ridge_best, by = "penalty") |> 
  mutate(model = "ridge", split_set = "cv")

# Resultats
ridge_fit <- ridge_workflow |> 
  last_fit(split = mod_split, metrics = mod_reg_metrics)

# Mètriques de test
ridge_metrics_test <- collect_metrics(ridge_fit) |> 
  mutate(model = "ridge", split_set = "test")

# Prediccions
ridge_predictions <- collect_predictions(ridge_fit) |> 
  mutate(model = "ridge")


# ---- Comparació ----

# Entrenament
summary_cv <- bind_rows(
  lm_metrics_train, 
  stepwise_metrics_train, 
  lasso_metrics_train, 
  ridge_metrics_train
) |> 
  select(model, .metric, mean, std_err) |> 
  pivot_wider(
    names_from = .metric, 
    values_from = c(mean, std_err), 
    names_glue = "cv_{.metric}_{.value}"
  )

# Test
summary_test <- bind_rows(
  lm_metrics_test, 
  stepwise_metrics_test, 
  lasso_metrics_test, 
  ridge_metrics_test
) |> 
  select(model, .metric, .estimate) |> 
  pivot_wider(
    names_from = .metric, 
    values_from = .estimate, 
    names_glue = "test_{.metric}"
  )

# Nombre de predictors
lm_n_vars <- fit(lm_workflow, data = data_train) |> 
  tidy() |> 
  filter(term != "(Intercept)", !is.na(estimate)) |> 
  nrow()

stepwise_n_vars <- length(stepwise_vars)

lasso_n_vars <- fit(lasso_workflow, data = data_train) |> 
  tidy() |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  nrow()

ridge_n_vars <- fit(ridge_workflow, data = data_train) |> 
  tidy() |> 
  filter(term != "(Intercept)", estimate != 0) |> 
  nrow()

# Taula final
table_mod <- tibble(
  model = c("lm_full", "stepwise", "lasso", "ridge"), 
  n_vars = c(lm_n_vars, stepwise_n_vars, lasso_n_vars, ridge_n_vars), 
  penalty = c(NA_real_, NA_real_, lasso_best$penalty, ridge_best$penalty)
) |> 
  left_join(summary_cv, by = "model") |> 
  left_join(summary_test, by = "model") |> 
  arrange(test_rmse)
