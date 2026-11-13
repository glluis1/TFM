library(tidyverse)
library(readxl)
library(gtsummary)
library(GGally)
library(naniar)


data |> 
  mutate(
    avg_QIS_in = (QIS_right_in + QIS_left_in) / 2, 
    avg_QIS_out = (QIS_right_out + QIS_left_out) / 2
  )