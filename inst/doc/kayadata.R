## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(dplyr)
library(kayadata)

## ----initialize, echo=TRUE, eval=FALSE----------------------------------------
#  install.packages("devtools")
#  devtools::install_github("jonathan-g/kayadata")
#  library(kayadata)

## ----initialize-pacman, echo=TRUE, eval=FALSE---------------------------------
#  install.packages("pacman")
#  library(pacman)
#  p_load_gh("jonathan-g/kayadata")

## ----get-kaya-data, echo=TRUE-------------------------------------------------
mexico_data = get_kaya_data("Mexico") 
mexico_data %>% filter(year >= 1965) %>% 
  select(region:ef) %>%
  head()

## ----project-top-down, echo=TRUE----------------------------------------------
mexico_2050 = project_top_down("Mexico", 2050)
mexico_2050

## ----plot-kaya, echo=TRUE-----------------------------------------------------
us_kaya = get_kaya_data("United States")
plot_kaya(us_kaya, "ef", y_lab = "Carbon intensity of economy",
          start_year = 2000, stop_year = 2010, log_scale = TRUE,
          trend_line = TRUE)

## ----plot-kaya-world, echo=TRUE-----------------------------------------------
world_kaya = get_kaya_data("World")
plot_kaya(world_kaya, "P", start_year = 2000, stop_year = 2010, log_scale = FALSE,
          trend_line = FALSE)

## ----get-fuel-mix, echo=TRUE--------------------------------------------------
mexico_mix = get_fuel_mix("Mexico")
mexico_mix

## ----plot-fuel-mix, echo=TRUE-------------------------------------------------
plot_fuel_mix(mexico_mix)

