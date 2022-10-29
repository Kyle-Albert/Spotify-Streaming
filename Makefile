.PHONY: clean

clean:
	rm -rf figures/
	rm -rf derived_data/
	rm -f report.pdf


.created-dirs:
	mkdir -p figures
	mkdir -p derived_data
	mkdir -p report
	touch .created-dirs

derived_data/music.rda derived_data/podcasts.rda: .created-dirs\
 derive_data_frame.R\
 source_data/df_music.csv\
 source_data/df_podcasts.csv
	Rscript derive_data_frame.R

figures/min_month_grid.png\
 figures/minutes_day.png\
 figures/minutes_month_all_year.png\
 figures/minutes_week.png\
 figures/minutes_year.png\
 figures/song_streams_year.png: .created-dirs\
 music_analysis.R\
 derived_data/music.rda
	Rscript music_analysis.R

report.pdf: .created-dirs\
 report.Rmd\
 figures/min_month_grid.png\
 figures/minutes_day.png\
 figures/minutes_month_all_year.png\
 figures/minutes_week.png\
 figures/minutes_year.png\
 figures/song_streams_year.png
	R -e "rmarkdown::render(\"report.Rmd\", output_format=\"pdf_document\")"

