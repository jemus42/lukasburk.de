
all: cv site

.PHONY: cv
cv: cv.qmd
	quarto render cv.qmd

# Quarto's `freeze: auto` only invalidates when the .qmd source changes,
# not external data files. Track external deps here so freezes get
# regenerated whenever the underlying data changes.
_freeze/talks/execute-results/html.json: talks.qmd talks.yml
	rm -f $@
	quarto render talks.qmd

_freeze/publications/execute-results/html.json: publications.qmd references.bib
	rm -f $@
	quarto render publications.qmd

.PHONY: talks
talks: _freeze/talks/execute-results/html.json

.PHONY: publications
publications: _freeze/publications/execute-results/html.json

.PHONY: site
site: _freeze/talks/execute-results/html.json _freeze/publications/execute-results/html.json
	quarto render
