
all: cv site

rmd = cv.Rmd

cv.html: $(rmd)
	Rscript -e "rmarkdown::render('$(rmd)')"

cv.pdf: $(rmd)
	Rscript -e "pagedown::chrome_print('$(rmd)')"

.PHONY: cv
cv: cv.html cv.pdf

.PHONY: site
site: 
	quarto render
