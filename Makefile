
all: cv site

.PHONY: cv
cv: cv.qmd
	quarto render cv.qmd

.PHONY: site
site:
	quarto render
