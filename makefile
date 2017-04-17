RMDFILES = $(wildcard *.Rmd)

all: slides.html fdat_cpr_run.html

slides.html: $(RMDFILES) style.css template.html
	R --vanilla -e "rmarkdown::render('slides.Rmd')"

fdat_cpr_run.html: fdat_cpr_run.R
	Rscript --vanilla fdat_cpr_run.R

