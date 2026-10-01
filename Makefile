# Retail Customer Lifetime Value -----------------------------------------------
# `make help` lists every target.

R       := Rscript
PORT    := 8000

.DEFAULT_GOAL := help
.PHONY: help all test serve api data clean distclean figures check-session

help: ## Show this help
	@echo "Targets:"
	@sed -n 's/^\([a-zA-Z_-]*\):.*## \(.*\)/  \1\t\2/p' $(MAKEFILE_LIST) \
		| column -t -s "$$(printf '\t')"

all: ## Run the full analysis pipeline
	$(R) -e 'targets::tar_make()'

data: ## Download the source dataset only
	@mkdir -p data/raw
	@curl -sSL --retry 3 -o data/raw/uci-online-retail-ii.zip \
		"https://archive.ics.uci.edu/static/public/502/online+retail+ii.zip" \
		|| (echo "download failed; see R/01_data.R::fetch_data()" && exit 1)
	@# The URL serves a .zip containing the .xlsx; extract before renaming, or
	@# read_excel() fails with "Couldn't find '_rels/.rels'".
	@cd data/raw && unzip -o -q uci-online-retail-ii.zip && rm -f uci-online-retail-ii.zip
	@echo "dataset ready:" && ls -la data/raw/

test: ## Run the unit test suite
	$(R) -e 'testthat::test_dir("tests", reporter = "summary")'

figures: ## Regenerate figures and tables only
	$(R) -e 'targets::tar_make(fields = c("tables", "figures", "report"))'

serve: ## Launch the Shiny dashboard
	$(R) -e 'shiny::runApp("app", port = 8080, launch.browser = TRUE)'

api: ## Launch the plumber API on PORT=$(PORT)
	$(R) -e 'pr <- plumber::plumb("api/plumber.R"); pr$run(port = $(PORT))'

check-session: ## Print the package versions this pipeline was built against
	@$(R) -e 'ip <- installed.packages()[, "Version"]; \
		need <- c("targets","dplyr","tidyr","readxl","lubridate","ggplot2", \
		          "testthat","shiny","plumber","patchwork","rmarkdown"); \
		for (p in need) cat(sprintf("%-12s %s\n", p, \
			if (p %in% names(ip)) ip[[p]] else "NOT INSTALLED"))'

clean: ## Remove pipeline cache and generated figures
	rm -rf _targets figures
	@echo "cache and figures removed"

distclean: clean ## Also remove the downloaded dataset
	rm -rf data/raw
	@echo "dataset removed"
