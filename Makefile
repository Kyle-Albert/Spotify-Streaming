.PHONY: clean

clean:
	rm -rf figures/
	rm -rf derived_data/
	rm -rf html/
	rm -f report.pdf


.created-dirs:
	mkdir -p figures
	mkdir -p html/
	mkdir -p derived_data
	mkdir -p report
	touch .created-dirs

derived_data/music.rda derived_data/podcasts.rda: .created-dirs\
 derive_data_frame.R\
 source_data/df_music.csv\
 source_data/df_podcasts.csv
	Rscript derive_data_frame.R

figures/minutes_year.png\
 figures/streams_ratio_year.png\
 figures/minutes_day.png\
 figures/minutes_week.png\
 figures/minutes_month_all_years.png\
 figures/min_month_grid.png\
 figures/release_year_plot.png\
 figures/valence_year_plot.png\
 figures/valence_month_plot.png\
 figures/valence_day_plot.png\
 figures/valence_hour_plot.png\
 figures/energy_hour_plot.png\
 figures/energy_day_plot.png\
 figures/danceability_hour_plot.png\
 figures/danceability_day_plot.png\
 html/top_songs.html\
 html/top_albums.html\
 html/top_artists.html\
 html/top_year_summary.html\
 html/top_podcasts.html: .created-dirs\
 music_analysis.R\
 derived_data/music.rda
	Rscript music_analysis.R

report.pdf: .created-dirs\
 report.Rmd\
 figures/minutes_year.png\
 figures/streams_ratio_year.png\
 figures/minutes_day.png\
 figures/minutes_week.png\
 figures/minutes_month_all_years.png\
 figures/min_month_grid.png\
 figures/release_year_plot.png\
 figures/valence_year_plot.png\
 figures/valence_month_plot.png\
 figures/valence_day_plot.png\
 figures/valence_hour_plot.png\
 figures/energy_hour_plot.png\
 figures/energy_day_plot.png\
 figures/danceability_hour_plot.png\
 figures/danceability_day_plot.png\
 html/top_songs.html\
 html/top_albums.html\
 html/top_artists.html\
 html/top_year_summary.html\
 html/top_podcasts.html
	R -e "rmarkdown::render(\"report.Rmd\", output_format=\"pdf_document\")"

