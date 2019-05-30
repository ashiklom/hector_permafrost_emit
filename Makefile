.PHONY: all drake deploy

all: drake

drake: analysis/data/derived_data/parameter-draws.csv
	./analysis/drake.R
	rm -f Rplots.pdf

analysis/data/derived_data/parameter-draws.csv: analysis/scripts/draw_params.R
	./analysis/scripts/draw_params.R

deploy:
	./analysis/scripts/deploy-paper.sh
