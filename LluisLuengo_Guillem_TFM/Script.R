library(tidyverse)
library(readxl)
library(gtsummary)
library(naniar)


# Dades
data <- read_excel("CARC_ database.xls")

# Transformacions
data <- data |> 
  # Conversió a factors
  mutate(across(where(is.character), as.factor)) |> 
  
  # Gestió de nivells
  mutate(
    `Age group` = fct_relevel(
      `Age group`, c("<50 years", "51-60 years", "61-70 years", ">70 years")
    ),
    `Diagnosis/intervention` = fct_collapse(
      `Diagnosis/intervention`, 
      "Others" = c("Intervention on large vessles", "Others")
    )
  ) |> 
  
  # Gestió de registres erronis
  mutate(
    mets_in = if_else(row_number() == 174, 6.7, mets_in),
    `6MWT_out` = if_else(row_number() == 174, NA_real_, `6MWT_out`)
  )

# Resums estadístics
# summary(data)

# Caracterització mostra
data_in <- data |> 
  # Selecció de variables
  select(
    `Age group`, Sex, `Diagnosis/intervention`, `Smoking entry`, 
    `6MWT_in`, QIS_left_in, QIS_right_in, MSEC_in, mets_in, 
    had_depression_in, had_anxiety_in, 
    heart_quality_physical_in, heart_quality_emotion_in, 
    baecke_work_in, baecke_sport_in, baecke_leisure_in
  ) 
  
# Taula 1
data_in |> 
  tbl_summary(
    by = Sex,
    label = list(
      `6MWT_in` = "6MWT"
    ),
    statistic = list(
      c(
        `6MWT_in`, QIS_left_in, QIS_right_in, MSEC_in, mets_in
      ) ~ "{mean} ± {sd}"
    ),
    missing = "no"
  ) |> 
  add_p(
    test = list(c(
      `6MWT_in`, QIS_left_in, QIS_right_in, MSEC_in, mets_in
    ) ~"t.test")
  ) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() |> 
  bold_p()

# Resultats de la RHB
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
    dif_baecke_leisure = baecke_leisure_out - baecke_leisure_in
  ) |> 
  
  # Columna binària millora en 6MWT
  mutate(imp_6MWT = factor(if_else(dif_6MWT >= 25, 1, 0))) |> 
  
  # Columna mitjana QIS
  mutate(
    avg_QIS_in = (QIS_right_in + QIS_left_in) / 2,
    avg_QIS_out = (QIS_right_out + QIS_left_out) / 2
  )

# Resum resultats
data |> 
  # Selecció de variables
  select(Sex, starts_with("dif_"), imp_6MWT) |> 
  
  # Taula 2
  tbl_summary(
    by = Sex,
    label = list(
      imp_6MWT = "% millora >25m 6MWT"
    ),
    statistic = list(
      c(
        dif_6MWT, dif_QIS_left, dif_QIS_right, dif_MSEC, dif_mets
      ) ~ "{mean} ± {sd}"
    ),
    missing = "no"
  ) |> 
  add_p(
    test = list(c(
      dif_6MWT, dif_QIS_left, dif_QIS_right, dif_MSEC, dif_mets
    ) ~"t.test")
  ) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() |> 
  bold_p()

# Valors nuls
# data_in |> arrange(Sex) |> vis_miss()
# gg_miss_var(data_in, facet = Sex, show_pct = TRUE)

p <- gg_miss_fct(data_in, fct = Sex)

p + geom_text(
  data = p$data,
  aes(
    label = paste0(round(pct_miss, 1), "%"),
    color = pct_miss < 10
  ),
) +
  scale_color_manual(
    values = c("FALSE" = "black", "TRUE" = "white"), guide = "none"
  )

# data_in |> mcar_test()

data_in |> 
  mutate(across(-Sex, ~ factor(if_else(is.na(.), "Missing", "Observed"),
                               levels = c("Observed", "Missing")))) |> 
  tbl_summary(
    by = Sex,
    missing = "no",
    value = list(everything() ~ "Missing")
  ) |> 
  add_p() |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() |> 
  bold_p()


# data |> 
#   select(`6MWT_in`, `6MWT_out`, dif_6MWT) |> 
#   pivot_longer(cols = c(`6MWT_in`, `6MWT_out`, dif_6MWT)) |> 
#   ggplot(aes(x = name, y = value)) + 
#   geom_boxplot()