.PHONY: all drake deploy cluster

all: drake

drake:
	./analysis/drake.R make
	rm -f Rplots.pdf

cluster:
	./analysis/drake.R --all --cluster

# analysis/data/derived_data/parameter-draws.csv: analysis/scripts/draw_params.R
# 	./analysis/scripts/draw_params.R

# analysis/data/derived_data/biome-parameter-draws.csv: analysis/scripts/biome-draw-params.R
# 	./analysis/scripts/biome-draw-params.R

deploy:
	./analysis/scripts/deploy-paper.sh

paper: ./analysis/paper/paper.Rmd
	./analysis/scripts/getcitations.R
	Rscript -e "rmarkdown::render('analysis/paper/paper.Rmd')"
