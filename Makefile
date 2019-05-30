.PHONY: all drake

all: drake

drake: analysis/data/derived_data/parameter-draws.csv
	./analysis/drake.R

analysis/data/derived_data/parameter-draws.csv: analysis/scripts/draw_params.R
	./analysis/scripts/draw_params.R
