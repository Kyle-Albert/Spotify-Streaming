PHONY: clean

clean:
	rm figures/*
	rm derived_data/*
	rm logs/*

.created-dirs:
	mkdir -p figures
	touch .created-dirs

report.pdf: .created-dirs
