library(tidyverse)
library(lubridate)
library(gridExtra)
library(gt)
library(magick)
pdf(NULL)
load("/home/rstudio/work/derived_data/music.rda")
load("/home/rstudio/work/derived_data/podcasts.rda")


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

# Ratio of unique song streams/year to song streams/year
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
unique_streams_ratio <- unique_streams_ratio[4:10,]
unique_streams_ratio_plot <-ggplot(data=unique_streams_ratio,
                                   aes(x=date, y=ratio, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Year",y="Ratio") +
  ggtitle("Ratio of Unique Song Plays to Total Song Plays per Year")


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



# Minutes per month
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

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

### Which Release Years did I listen to most music
music$release_year <- substring(music$release_date, 1, 4)
release_year <- music %>%
  filter(Play = TRUE) %>%
  group_by(release_year) %>%
  count()
release_year <- release_year[-62,] 
release_year_plot <- ggplot(data=release_year,
                            aes(x=release_year, y=n)) +
  geom_col(aes(fill = n)) +
  scale_fill_gradient(low = "gray", high = "purple") + 
  labs(x= "Year", y= "Songs") +
  scale_x_discrete(breaks=seq(1950, 2030, 5)) +
  ggtitle("Number of Songs played vs Release Year of Song")


##### Valence Calculations
music$total_valence <- music$ms_played * music$valence

## Valence per year
valence_year_min <- music %>% 
  group_by(ts) %>% 
  group_by(date = floor_date(ts, "year")) %>%
  summarize(minutes = sum(ms_played) / 60000)
valence_year_valence <- music %>% 
  group_by(ts) %>% 
  group_by(date = floor_date(ts, "year")) %>%
  summarize(valence = sum(total_valence))
valence_year <- inner_join(valence_year_min, valence_year_valence)
valence_year$avg_valence <- valence_year$valence / (valence_year$minutes * 60000)
valence_year_plot <- ggplot(data=valence_year,
                            aes(x=date, y=avg_valence, group=1)) +
  geom_line() +
  geom_point() +
  ylim(0.475, 0.625) +
  labs(x="Year",y="Valence") +
  ggtitle("Average Valence per Year")



## Valence per Month
valence_month_min <- music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%m", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000)
valence_month_valence <- music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%m", tz=time_zone)) %>%
  summarize(valence = sum(total_valence))
valence_month <- inner_join(valence_month_min, valence_month_valence)
valence_month$avg_valence <- valence_month$valence / (valence_month$minutes * 60000)
valence_month_plot <- ggplot(data=valence_month,
                            aes(x=date, y=avg_valence, group=1)) +
  geom_line() +
  geom_point() +
  ylim(0.475, 0.625) +
  scale_x_discrete(labels = months) +
  labs(x="Month",y="Valence") +
  ggtitle("Average Valence per Month")



## Valence per Day
valence_day <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%a", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            valence_total = sum(total_valence))
valence_day$valence_avg <- valence_day$valence_total/(valence_day$minutes*60000)
valence_day_plot <- ggplot(data=valence_day,
                          aes(x=date, y=valence_avg, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Day",y="Valence") +
  ylim(0.5, 0.6) +
  scale_x_discrete(limits=days_vec) +
  ggtitle("Average Valence per Day of Week")



## Valence per Hour
valence_hour <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%H", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            valence_total = sum(total_valence))
valence_hour$valence_avg <- valence_hour$valence_total/(valence_hour$minutes*60000)
valence_hour_plot <- ggplot(data=valence_hour,
                           aes(x=date, y=valence_avg, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Hour",y="Valence") +
  ggtitle("Average Valence per Hour")



##### Energy & Danceability vs Day of week and time of day
music$total_energy <- music$ms_played * music$energy
music$total_danceability <- music$ms_played * music$danceability

## Energy
energy_hour <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%H", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            energy_total = sum(total_energy))
energy_hour$energy_avg <- energy_hour$energy_total/(energy_hour$minutes*60000)
energy_hour_plot <- ggplot(data=energy_hour,
                           aes(x=date, y=energy_avg, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Hour",y="Energy") +
  ggtitle("Average Energy per Hour")


energy_day <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%a", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            energy_total = sum(total_energy))
energy_day$energy_avg <- energy_day$energy_total/(energy_day$minutes*60000)
energy_day_plot <- ggplot(data=energy_day,
                           aes(x=date, y=energy_avg, group=1)) +
  geom_line() +
  geom_point() +
  ylim(0.68, 0.72) +
  labs(x="Day",y="Energy") +
  scale_x_discrete(limits=days_vec) +
  ggtitle("Average Energy per Day of Week")



## Danceability
danceability_hour <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%H", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            energy_total = sum(total_danceability))
danceability_hour$danceability_avg <- danceability_hour$energy_total/
  (danceability_hour$minutes*60000)
danceability_hour_plot <- ggplot(data=danceability_hour,
                           aes(x=date, y=danceability_avg, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Hour",y="Danceability") +
  ggtitle("Average Danceability per Hour")


danceability_day <-  music %>% 
  group_by(ts) %>% 
  group_by(date = strftime(ts, "%a", tz=time_zone)) %>%
  summarize(minutes = sum(ms_played) / 60000,
            energy_total = sum(total_danceability))
danceability_day$danceability_avg <- danceability_day$energy_total/
  (danceability_day$minutes*60000)
danceability_day_plot <- ggplot(data=danceability_day,
                                 aes(x=date, y=danceability_avg, group=1)) +
  geom_line() +
  geom_point() +
  labs(x="Day",y="Danceability") +
  scale_x_discrete(limits=days_vec) +
  ylim(0.585, 0.605) +
  ggtitle("Average Danceability per Day of Week")



### Find top 10 most listened to Songs, Artists, and Albums
height_gt <- 120
## Songs
top_songs <- music %>%
  group_by(track_id) %>%
  summarize(total_ms_played=sum(ms_played))
distinct_tracks <- music %>%
  distinct(track_id, .keep_all=TRUE)
top_songs <- top_songs[order(top_songs$total_ms_played, decreasing = TRUE),]
top_songs <- top_songs[1:15,]
top_songs$min_played <- top_songs$total_ms_played/60000
top_songs <- left_join(top_songs, distinct_tracks)
top_songs <- top_songs[,-c(1:2,4:5,9:13,15,17:31,33:37)]
top_songs <- top_songs %>% mutate(Rank= row_number())
top_songs <- top_songs[,c(8,5, 1, 2, 3, 4, 6, 7)]
colnames(top_songs) <- c("Rank", "Cover Art", "Minutes Played", "Song",
                         "Artist", "Album", "Release Date", "Genre(s)")
top_songs$`Minutes Played` <- trunc(top_songs$`Minutes Played`)
top_songs_gt <- top_songs %>% gt() %>%
  tab_header(title= "Most Played Songs") %>%
  text_transform(locations = cells_body(columns="Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_songs$`Cover Art`,
                     height= height_gt)
                 }
  )



## Top Albums
top_albums <- music %>%
  group_by(master_metadata_album_album_name) %>%
  summarize(total_ms_played=sum(ms_played))
distinct_albums <- music %>%
  distinct(master_metadata_album_album_name, .keep_all=TRUE)
top_albums <- top_albums[order(top_albums$total_ms_played, decreasing = TRUE),]
top_albums <- top_albums[1:15,]
top_albums$min_played <- top_albums$total_ms_played/60000
top_albums <- left_join(top_albums, distinct_albums)
top_albums <- top_albums[, -c(2, 4:6, 8:13, 15, 17:31, 33:37)]
top_albums$min_played <- trunc(top_albums$min_played)
top_albums <- top_albums %>% mutate(Rank= row_number())
top_albums <- top_albums[,c(7,4,2,1,3,5,6)]
colnames(top_albums) <- c("Rank", "Cover Art", "Minutes Played","Album","Artist", "Release Date", "Genre(s)")
top_albums_gt <- top_albums %>% gt() %>%
  tab_header(title= "Most Played Albums") %>%
  text_transform(locations = cells_body(columns="Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_albums$`Cover Art`,
                     height= height_gt)
                 }
  )



## Artists
top_artists <- music %>%
  group_by(master_metadata_album_artist_name) %>%
  summarize(total_ms_played=sum(ms_played))
distinct_artists <- music %>%
  distinct(master_metadata_album_artist_name, .keep_all=TRUE)
top_artists <- top_artists[order(top_artists$total_ms_played, decreasing = TRUE),]
top_artists <- top_artists[1:15,]
top_artists$min_played <- top_artists$total_ms_played/60000
top_artists <- left_join(top_artists, distinct_artists)
top_artists <- top_artists[, -c(2, 4:13, 15:37)]
top_artists$min_played <- trunc(top_artists$min_played)
top_artists <- top_artists %>% mutate(Rank= row_number())
top_artists <- top_artists[,c(4,3,2,1)]
colnames(top_artists) <- c("Rank", "Cover Art", "Minutes Played", "Artist")
top_artists_gt <- top_artists %>% gt() %>%
  tab_header(title= "Most Played Artists") %>%
  text_transform(locations = cells_body(columns="Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_artists$`Cover Art`,
                     height= height_gt)
                 }
  )



### List of top thing from each year
top_year_summary <- data.frame()
for(i in 1:10){
intermediate_song <- music %>%
  group_by(ts) %>%
  group_by(date = strftime(ts, "%Y", tz=time_zone)) %>%
  filter(strftime(ts, "%Y", tz=time_zone) == (2023 - i)) %>%
  group_by(track_id) %>%
  summarize(Year = 2023 - i, Song_Minutes = floor(sum(ms_played) / 60000))
intermediate_song <- intermediate_song[order(intermediate_song$Song_Minutes, decreasing = TRUE),]
intermediate_song <- intermediate_song[1,]
intermediate_song <- left_join(intermediate_song, distinct_tracks)
intermediate_song <- intermediate_song[,-c(1,4:5,9:13,15:37)]
intermediate_song <- intermediate_song[,c(1,6,2,3,4,5)]
colnames(intermediate_song) <- c("Year", "Song Cover Art", "Song Minutes",
                                 "Song Name", "Song Artist", "Song Album")

intermediate_album <- music %>%
  group_by(ts) %>%
  group_by(date = strftime(ts, "%Y", tz=time_zone)) %>%
  filter(strftime(ts, "%Y", tz=time_zone) == (2023 - i)) %>%
  group_by(master_metadata_album_album_name) %>%
  summarize(Year = 2023 - i, Song_Minutes = floor(sum(ms_played) / 60000))
intermediate_album <- intermediate_album[order(intermediate_album$Song_Minutes, decreasing = TRUE),]
intermediate_album <- intermediate_album[1,]
intermediate_album <- left_join(intermediate_album, distinct_albums)
intermediate_album <- intermediate_album[,-c(4:6,8:13,15:37)]
intermediate_album <- intermediate_album[,c(2,5,3,1,4)]
colnames(intermediate_album) <- c("Year", "Album Cover Art", "Album Minutes",
                                 "Album", "Album Artist")


intermediate_artist <-  music %>%
  group_by(ts) %>%
  group_by(date = strftime(ts, "%Y", tz=time_zone)) %>%
  filter(strftime(ts, "%Y", tz=time_zone) == (2023 - i)) %>%
  group_by(master_metadata_album_artist_name) %>%
  summarize(Year = 2023 - i, Artist_Minutes = floor(sum(ms_played) / 60000))
intermediate_artist <- intermediate_artist[order(intermediate_artist$Artist_Minutes, decreasing = TRUE),]
intermediate_artist <- intermediate_artist[1,]
intermediate_artist <- left_join(intermediate_artist, distinct_artists)
intermediate_artist <- intermediate_artist[, -c(4:13, 15:37)]
intermediate_artist <- intermediate_artist[, c(2,4,3,1)]
colnames(intermediate_artist) <- c("Year", "Artist Cover Art",
                                   "Artist Minutes", "Artist")

intermediate1 <- left_join(intermediate_song, intermediate_album)
intermediate2 <- left_join(intermediate1, intermediate_artist)
top_year_summary <- rbind(top_year_summary, intermediate2)
}

top_year_summary_gt <- top_year_summary %>% gt() %>%
  tab_header(title= "Most Played Songs, Albums, and Artists by Year") %>%
  text_transform(locations = cells_body(columns="Song Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_year_summary$`Song Cover Art`,
                     height= height_gt)
                 }
  ) %>%
  text_transform(locations = cells_body(columns="Album Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_year_summary$`Album Cover Art`,
                     height= height_gt)
                 }
  ) %>%
  text_transform(locations = cells_body(columns="Artist Cover Art"),
                 fn = function(x){
                   web_image(
                     url = top_year_summary$`Artist Cover Art`,
                     height= height_gt)
                 }
  ) %>%
  tab_spanner(
    label="Songs",
    columns = c("Song Cover Art", "Song Minutes",
                "Song Name", "Song Artist", "Song Album")
  ) %>%
  tab_spanner(
    label="Albums",
    columns = c("Album Cover Art", "Album Minutes",
                "Album", "Album Artist")
  ) %>%
  tab_spanner(
    label="Artists",
    columns = c("Artist Cover Art",
                "Artist Minutes", "Artist")
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("left"),
      weight = px(2)),
    locations = cells_body(
      columns = c("Song Cover Art", "Album Cover Art", "Artist Cover Art")
    )
  )



### Podcasts
top_podcasts <- podcasts %>%
  group_by(episode_show_name) %>%
  summarize(total_ms_played=sum(ms_played))
top_podcasts <- top_podcasts[order(top_podcasts$total_ms_played, decreasing = TRUE),]
top_podcasts <- top_podcasts[1:15,]
top_podcasts$min_played <- top_podcasts$total_ms_played/60000
top_podcasts$day_played <- round(top_podcasts$min_played/(60*24), 1)
top_podcasts$min_played <- trunc(top_podcasts$min_played)
top_podcasts <- top_podcasts[, -2]
top_podcasts <- top_podcasts %>% mutate(Rank = row_number())
top_podcasts <- top_podcasts[, c(4, 1,3,2)]
colnames(top_podcasts) <- c("Rank","Podcast Name", "Days Played", "Minutes Played")
top_podcasts_gt <- top_podcasts %>% gt() %>% tab_header(title= "Most Played Podcasts")





if(!dir.exists("/home/rstudio/work/figures/")){
  dir.create("/home/rstudio/work/figures/")
}

if(!dir.exists("/home/rstudio/work/html/")){
  dir.create("/home/rstudio/work/html/")
}

# Save pngs of the plots
width <- 6
height <- 4
units <- "in"

ggsave(filename = "/home/rstudio/work/figures/minutes_year.png",
       plot = minutes_year,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/streams_ratio_year.png",
       plot = unique_streams_ratio_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/minutes_day.png",
       plot = minutes_day,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/minutes_week.png",
       plot = minutes_week,
       width = width,
       height = height,
       units = units)

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

ggsave(filename = "/home/rstudio/work/figures/release_year_plot.png",
       plot = release_year_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/valence_year_plot.png",
       plot = valence_year_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/valence_month_plot.png",
       plot = valence_month_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/valence_day_plot.png",
       plot = valence_day_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/valence_hour_plot.png",
       plot = valence_hour_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/energy_hour_plot.png",
       plot = energy_hour_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/energy_day_plot.png",
       plot = energy_day_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/danceability_hour_plot.png",
       plot = danceability_hour_plot,
       width = width,
       height = height,
       units = units)

ggsave(filename = "/home/rstudio/work/figures/danceability_day_plot.png",
       plot = danceability_day_plot,
       width = width,
       height = height,
       units = units)


top_songs_gt %>% gtsave(filename="/home/rstudio/work/html/top_songs_gt.html",
                        inline_css=TRUE)

top_albums_gt %>% gtsave(filename="/home/rstudio/work/html/top_albums_gt.html",
                        inline_css=TRUE)

top_artists_gt %>% gtsave(filename="/home/rstudio/work/html/top_artists_gt.html",
                         inline_css=TRUE)

top_year_summary_gt %>% gtsave(filename="/home/rstudio/work/html/top_year_summary_gt.html",
                          inline_css=TRUE)

top_podcasts_gt %>% gtsave(filename="/home/rstudio/work/html/top_podcasts_gt.html",
                          inline_css=TRUE)
