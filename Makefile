
all: cv.html site

cv.html: cv.Rmd
	R -e "rmarkdown::render('cv.Rmd')"

.PHONY: site
site:
	quarto render
