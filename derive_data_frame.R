library(tidyverse)
library(lubridate)
setwd("/home/rstudio/work/source_data/")
music <- read.csv("df_music.csv")
music <- music %>% mutate(Play = case_when(
  ms_played >= 30000 ~ TRUE,
  ms_played < 30000 ~ FALSE
))


