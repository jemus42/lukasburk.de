
all: cv site

.PHONY: cv
cv: cv.qmd
	quarto render cv.qmd

# Quarto's `freeze: auto` only invalidates when the .qmd source changes,
# not external data files. Track talks.yml here so the freeze is
# regenerated whenever the talk list changes.
_freeze/talks/execute-results/html.json: talks.qmd talks.yml
	rm -f $@
	quarto render talks.qmd

.PHONY: talks
talks: _freeze/talks/execute-results/html.json

.PHONY: site
site: _freeze/talks/execute-results/html.json
	quarto render
