library(tidyverse)
library(readxl)
library(scales)


# ---- Càrrega de dades ----

data_01 <- read_excel("CARC_ database.xls") |> 
  
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

data_01 <- data_01 |> 
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


# ---- Anàlisi de normalitat ----

analyze_normality_01 <- function(data, var) {
  shapiro_df <- data |> 
    drop_na(all_of(c("Sex", var))) |> 
    summarise(
      test = list(shapiro.test(.data[[var]])), 
      .by = Sex
    ) |> 
    mutate(
      w = map_dbl(test, "statistic"), 
      p = map_dbl(test, "p.value"), 
      label = sprintf("W = %.2f\n%s", w, pvalue(p, add_p = TRUE))
    )
  
  data |> 
    ggplot(aes(sample = .data[[var]])) + 
    geom_qq() + 
    geom_qq_line(color = "firebrick", linewidth = 1) + 
    geom_text(
      data = shapiro_df, 
      aes(x = -Inf, y = Inf, label = label), 
      hjust = -0.1, 
      vjust = 1.2, 
      size = 3.5, 
      inherit.aes = FALSE
    ) + 
    facet_wrap(~ Sex) + 
    labs(title = var)
}

# Gràfics Q-Q per sexe
plot_norm_01 <- data_01 |> 
  select(
    ends_with("_in"), starts_with("dif_")
  ) |> 
  names() |> 
  set_names() |> 
  map(function(var) analyze_normality_01(data_01, var))


rm(list = ls(pattern = "_01$"))