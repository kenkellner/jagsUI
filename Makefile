NAME = $(shell grep 'Package:' DESCRIPTION | cut -d ' ' -f2)
VER = $(shell grep 'Version:' DESCRIPTION | cut -d ' ' -f2)

install:
	R CMD INSTALL .

build:
	cd ..; R CMD build $(NAME)

#check: build
#	cd ..; R CMD check $(NAME)_$(VER).tar.gz
#rm -r ../$(NAME).Rcheck

check:
	Rscript -e "devtools::check()"

test:
	Rscript -e "devtools::test()"

plots:
	Rscript -e "vdiffr::manage_cases()"

coverage:
	Rscript -e \
		'Sys.setenv(NOT_CRAN="true"); covr::report(file="/tmp/jagsUI-report.html")'
	firefox /tmp/jagsUI-report.html
