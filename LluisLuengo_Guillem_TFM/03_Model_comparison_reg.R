library(tidyverse)
library(readxl)
library(tidymodels)


# ---- Càrrega de dades ----

data_03 <- read_excel("CARC_ database.xls") |> 
  
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


# ---- Creació de variables ----

data_03 <- data_03 |> 
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
    imp_6MWT = factor(
      if_else(dif_6MWT >= 25, "Yes", "No"), 
      levels = c("Yes", "No")
    )
  )


# ---- Preprocessament ----

set.seed(827)

# Transformacions i selecció de variables
data_03 <- data_03 |> 
  # Mitjana de QIS i factor d'estratificació
  mutate(
    avg_QIS_in = (QIS_right_in + QIS_left_in) / 2, 
    strata_var = interaction(Sex, imp_6MWT, drop = TRUE)
  ) |>
  
  # Filtratge de valors faltants en la resposta
  drop_na(dif_6MWT) |> 
  
  # Selecció de variables per als models
  select(
    `Age group`, Sex, `Diagnosis/intervention`, `Smoking entry`, `6MWT_in`, 
    avg_QIS_in, MSEC_in, mets_in, had_depression_in, had_anxiety_in, 
    heart_quality_physical_in, heart_quality_emotion_in, baecke_work_in, 
    baecke_sport_in, baecke_leisure_in, dif_6MWT, imp_6MWT, strata_var
  )

# Partició del conjunt de dades
split_03 <- data_03 |> 
  initial_split(prop = 0.75, strata = strata_var)

# Conjunt d'entrenament
train_03 <- split_03 |>
  training()

# Conjunt de test
test_03 <- split_03 |>
  testing()


# ---- Comparació de models ----

set.seed(827)

# Validació creuada
folds_03 <- vfold_cv(train_03, v = 5, repeats = 5, strata = strata_var)

# Preprocessament
lm_recipe_03 <- recipe(dif_6MWT ~ ., data = train_03) |> 
  step_rm(imp_6MWT, strata_var) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact(terms = ~ Sex_F:heart_quality_emotion_in)

glmnet_recipe_03 <- lm_recipe_03 |> 
  step_normalize(all_numeric_predictors())

# Stepwise selection
stepwise_train_03 <- prep(lm_recipe_03, training = train_03) |> 
  bake(new_data = NULL)

lm_full_03 <- lm(dif_6MWT ~ . + Sex_F:heart_quality_emotion_in, 
                 data = stepwise_train_03)
lm_null_03 <- lm(dif_6MWT ~ 1, data = stepwise_train_03)

# Selecció múltiple
stepwise_model_03 <- stats::step(
  lm_full_03, 
  scope = list(lower = lm_null_03, upper = lm_full_03), 
  direction = "both", 
  trace = FALSE
)

stepwise_vars_03 <- attr(terms(stepwise_model_03), "term.labels")

# Preprocessament
stepwise_recipe_03 <- recipe(
  dif_6MWT ~ `6MWT_in` + MSEC_in + mets_in + had_depression_in + 
    baecke_work_in + `Age group` + Sex + heart_quality_emotion_in, 
  data = train_03
) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact(terms = ~ Sex_F:heart_quality_emotion_in)

# Models
lm_model_03 <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

ridge_model_03 <- linear_reg(penalty = tune(), mixture = 0) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

lasso_model_03 <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# Fluxos de treball
workflow_03 <- workflow_set(
  preproc = list(full = lm_recipe_03, stepwise = stepwise_recipe_03), 
  models = list(full = lm_model_03, stepwise = lm_model_03), 
  cross = FALSE
)

workflow_tune_03 <- workflow_set(
  preproc = list(ridge = glmnet_recipe_03, lasso = glmnet_recipe_03), 
  models = list(ridge = ridge_model_03, lasso = lasso_model_03), 
  cross = FALSE
)

# Graella de penalitzacions
penalty_grid_03 <- grid_regular(penalty(range = c(-4, 2)), levels = 30)

# Mètriques d'entrenament
results_03 <- workflow_03 |> 
  workflow_map("fit_resamples", resamples = folds_03) |> 
  rank_results() |> 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err))

results_tune_03 <- workflow_tune_03 |> 
  workflow_map("tune_grid", resamples = folds_03, grid = penalty_grid_03) |> 
  rank_results() |> 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) |> 
  group_by(wflow_id) |> 
  slice_min(order_by = mean_rmse, n = 1, with_ties = FALSE) |> 
  ungroup()

results_03 <- results_03 |> 
  bind_rows(results_tune_03) |> 
  arrange(mean_rmse)


rm(list = ls(pattern = "_03$"))