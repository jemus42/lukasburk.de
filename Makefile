
all: cv.html cv.pdf site

rmd = cv.Rmd

cv.html: $(rmd)
	R -e "rmarkdown::render('$(rmd)')"

cv.pdf: $(rmd)
	Rscript -e "pagedown::chrome_print('$(rmd)')"

.PHONY: site
site:
	quarto render
