# vim set fileencoding=utf-8:

#---
# Makefile
# author: Daniel Mata <daniel.mata@flacso.edu.mx>
# date created: 17-12-2022
# license: (c) Daniel Mata, 2022, GPL v3
#---

SCRIPTS := scripts
INPUT := data
PYENV := env


download:
	Rscript --vanilla $(SCRIPTS)/get_cleandata.R;

env:
	python3 -m venv env; \
	source ./env/bin/activate;\
        pip install -r requirements.txt	

run:
	@echo "Running shiny app..."
	Rscript -e 'shiny::runApp("armas-de-fuego", port=4699)'

clean:
	@echo "Removing python $(PYENV)";
	rm -rf $(PYENV);
