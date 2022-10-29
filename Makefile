.PHONY: clean

clean:
	rm -rf figures/
	rm -rf derived_data/


.created-dirs:
	mkdir -p figures
	mkdir -p derived_data
	touch .created-dirs


report.pdf: .created-dirs


