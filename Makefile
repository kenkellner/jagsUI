NAME = $(shell grep 'Package:' DESCRIPTION | cut -d ' ' -f2)
VER = $(shell grep 'Version:' DESCRIPTION | cut -d ' ' -f2)

install:
	R CMD INSTALL .

build:
	cd ..; R CMD build $(NAME)

check:
	make build
	cd ..; R CMD check $(NAME)_$(VER).tar.gz

cran-check:
	make build
	cd ..; R CMD check --as-cran $(NAME)_$(VER).tar.gz

test:
	make install
	Rscript -e "Sys.setenv("AT_HOME" = "TRUE"); tinytest::test_package('jagsUI', color=FALSE, verbose=0)"

coverage:
	make install
	Rscript -e 'Sys.setenv("AT_HOME" = "TRUE"); covr::report(file="/tmp/jagsUI-report.html")'
	firefox /tmp/jagsUI-report.html

site:
	Rscript -e "pkgdown::build_site()"
	firefox docs/index.html

README:
	Rscript -e "knitr::knit('README.Rmd')"
	pandoc README.md -o README.html
	firefox README.html
	sleep 3
	rm README.html
