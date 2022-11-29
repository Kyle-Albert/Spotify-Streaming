library(tidyverse)
library(lubridate)
library(gridExtra)
#pdf(NULL)
load("/home/rstudio/work/derived_data/music.rda")

# Minutes per year
minutes_year <- music %>% 
  group_by(ts) %>% 
  group_by(date = floor_date(ts, "year")) %>%
  summarize(minutes = sum(ms_played) / 60000) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = minutes)) + 
  geom_col(aes(fill = minutes)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  labs(x= "Year", y= "Minutes") + 
  ggtitle("Minutes of Music Listening vs Year")

# Hour of Day
minutes_day <- music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%H", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = minutes)) + 
  geom_col(aes(fill = minutes)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  labs(x= "Hour", y= "Minutes") + 
  ggtitle("Minutes of Music Listening vs Hour of Day")

#Streams day of week
days_vec <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
minutes_week <- music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%a", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000) %>% 
  ggplot(aes(x = date, y = minutes)) + 
  geom_col(aes(fill = minutes)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  scale_x_discrete(limits=days_vec) +
  labs(x= "Day", y= "Minutes") + 
  ggtitle("Minutes of music listening vs Day of Week")


song_streams_year_df <- music %>%
  filter(Play == TRUE) %>%
  count(date = strftime(ts, "%Y", tz=time_zone)) %>%
  rename(streams = n)

song_streams_year_unique_df <- music %>%
  filter(Play == TRUE) %>%
  group_by(date = strftime(ts, "%Y", tz=time_zone)) %>%
  distinct(track_id) %>%
  count() %>%
  rename(unique_streams = n)

unique_streams_ratio <- inner_join(song_streams_year_unique_df, song_streams_year_df)
unique_streams_ratio$ratio <- unique_streams_ratio$unique_streams / unique_streams_ratio$streams

# Unique Song streams per month
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
unique_streams_month <- music %>%
  filter(Play == TRUE) %>%
  group_by(ts) %>%
  group_by(date = strftime(ts, "%m", tz=time_zone)) %>%
  distinct(distinct = track_id) %>%
  count(date, sort = TRUE) %>%
  ggplot(aes(x =date, y = n)) + 
  geom_col(aes(fill = n)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  scale_x_discrete(labels = months) +
  labs(x= "Month", y= "Songs Streamed") + 
  ggtitle("Unique Songs Streamed vs Month")


# Minutes per month
minutes_month_all_years <- music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%m", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = minutes)) + 
  geom_col(aes(fill = minutes)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  scale_x_discrete(labels = months) +
  labs(x= "Year", y= "Minutes") + 
  ggtitle("Minutes of Music Listening per Month")


# Plot each years minutes listening by month for 2014-2022
min_month <- list()
for(i in 1:9){
  intermediate <-  music %>%
    group_by(ts) %>%
    group_by(date = strftime(ts, "%m", tz=time_zone)) %>%
    filter(strftime(ts, "%Y", tz=time_zone) == (2013 + i)) %>%
    summarize(minutes = floor(sum(ms_played) / 60000))
  
  if(i == 2){
    intermediate <- intermediate %>% add_row(date="10", minutes=0) %>%
      add_row(date="11", minutes=0) %>%
      add_row(date="12", minutes=0)
  }
  
  if(i==3){
    intermediate <- intermediate %>% add_row(date="02", minutes=0) %>%
      add_row(date="04", minutes=0)
  }
  
  if(i==9){
    intermediate <- intermediate %>% add_row(date="10", minutes=0) %>%
      add_row(date="11", minutes=0) %>%
      add_row(date="12", minutes=0)
  }

  min_month[[i]] <- intermediate %>%
    arrange(date) %>% 
    ggplot(aes(x = date, y = minutes)) + 
    geom_col(fill="purple") +
    scale_x_discrete(labels = months) + 
    labs(x= "Month", y= "Minutes") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) + 
    ggtitle(as.character(2013 + i))
}
min_month_grid <- arrangeGrob(min_month[[1]], min_month[[2]], min_month[[3]], min_month[[4]],
             min_month[[5]], min_month[[6]], min_month[[7]], min_month[[8]],
             min_month[[9]], ncol = 3, nrow = 3)

if(!dir.exists("/home/rstudio/work/figures/")){
  dir.create("/home/rstudio/work/figures/")
}

# Save pngs
ggsave(filename = "/home/rstudio/work/figures/minutes_year.png",
       plot = minutes_year,
       width = 6,
       height = 4,
       units = "in")

ggsave(filename = "/home/rstudio/work/figures/minutes_day.png",
       plot = minutes_day,
       width = 6,
       height = 4,
       units = "in")

ggsave(filename = "/home/rstudio/work/figures/minutes_week.png",
       plot = minutes_week,
       width = 6,
       height = 4,
       units = "in")

ggsave(filename = "/home/rstudio/work/figures/song_streams_year.png",
       plot = song_streams_year_g,
       width = 11,
       height = 4,
       units = "in")

ggsave(filename = "/home/rstudio/work/figures/minutes_month_all_years.png",
       plot = minutes_month_all_years,
       width = 6,
       height = 4,
       units = "in")

ggsave(filename = "/home/rstudio/work/figures/min_month_grid.png",
       plot = min_month_grid,
       width = 10,
       height = 10,
       units = "in")
