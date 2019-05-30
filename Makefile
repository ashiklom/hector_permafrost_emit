.PHONY: all drake deploy

all: drake deploy

drake: analysis/data/derived_data/parameter-draws.csv
	./analysis/drake.R

analysis/data/derived_data/parameter-draws.csv: analysis/scripts/draw_params.R
	./analysis/scripts/draw_params.R

deploy:
	./analysis/scripts/deploy-paper.sh
