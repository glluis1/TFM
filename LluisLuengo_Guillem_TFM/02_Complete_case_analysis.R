library(tidyverse)
library(readxl)
library(tidymodels)


# ---- Càrrega de dades ----

data_02 <- read_excel("CARC_ database.xls") |> 
  
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

data_02 <- data_02 |> 
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
data_02 <- data_02 |> 
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
split_02 <- data_02 |> 
  initial_split(prop = 0.75, strata = strata_var)

# Conjunt d'entrenament
train_02 <- split_02 |>
  training()

# Conjunt de test
test_02 <- split_02 |>
  testing()


# ---- Comparació de models ----

set.seed(827)

# Validació creuada
folds_02 <- vfold_cv(train_02, v = 5, repeats = 5, strata = strata_var)

# Preprocessament
lm_recipe_cc_02 <- recipe(dif_6MWT ~ ., data = train_02) |> 
  step_rm(imp_6MWT, strata_var) |> 
  step_dummy(all_nominal_predictors())

lm_recipe_imp_02 <- recipe(dif_6MWT ~ ., data = train_02) |> 
  step_rm(imp_6MWT, strata_var) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())

logreg_recipe_cc_02 <- recipe(imp_6MWT ~ ., data = train_02) |> 
  step_rm(dif_6MWT, strata_var) |> 
  step_dummy(all_nominal_predictors())

logreg_recipe_imp_02 <- recipe(imp_6MWT ~ ., data = train_02) |> 
  step_rm(dif_6MWT, strata_var) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())

# Model
lm_model_02 <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

logreg_model_02 <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

# Flux de treball
lm_workflow_02 <- workflow_set(
  preproc = list(cc = lm_recipe_cc_02, imp = lm_recipe_imp_02), 
  models = list(lm = lm_model_02)
)

logreg_workflow_02 <- workflow_set(
  preproc = list(cc = logreg_recipe_cc_02, imp = logreg_recipe_imp_02), 
  models = list(logreg = logreg_model_02)
)

# Mètriques d'entrenament
lm_results_02 <- lm_workflow_02 |> 
  workflow_map("fit_resamples", resamples = folds_02) |> 
  rank_results() |> 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err))

logreg_results_02 <- logreg_workflow_02 |> 
  workflow_map("fit_resamples", resamples = folds_02) |> 
  rank_results() |> 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err))


rm(list = ls(pattern = "_02$"))