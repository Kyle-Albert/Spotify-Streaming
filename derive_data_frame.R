library(tidyverse)
library(lubridate)
music <- read.csv("/home/rstudio/work/source_data/df_music.csv")
music <- music %>% mutate(Play = case_when(
  ms_played >= 30000 ~ TRUE,
  ms_played < 30000 ~ FALSE
))

time_zone <- "America/New_York"
music$ts <- lubridate::with_tz(
  as.POSIXct(music$ts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
  tz = time_zone)

if(!dir.exists("/home/rstudio/work/derived_data/")){
  dir.create("/home/rstudio/work/derived_data")
}

save(music, file="/home/rstudio/work/derived_data/music.Rda")
