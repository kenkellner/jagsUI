NAME = $(shell grep 'Package:' DESCRIPTION | cut -d ' ' -f2)
VER = $(shell grep 'Version:' DESCRIPTION | cut -d ' ' -f2)

install:
	R CMD INSTALL .

build:
	cd ..; R CMD build $(NAME)

check: build
	cd ..; R CMD check $(NAME)_$(VER).tar.gz
	rm -r ../$(NAME).Rcheck

test:
	Rscript -e "devtools::test()"

coverage:
	Rscript -e 'covr::report(file="/tmp/jagsUI-report.html")'
	firefox /tmp/jagsUI-report.html
