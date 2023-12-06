NAME = $(shell grep 'Package:' DESCRIPTION | cut -d ' ' -f2)
VER = $(shell grep 'Version:' DESCRIPTION | cut -d ' ' -f2)

install:
	R CMD INSTALL .

build:
	cd ..; R CMD build $(NAME)

check:
	make build
	cd ..; R CMD check $(NAME)_$(VER).tar.gz

test:
	make install
	Rscript -e "Sys.setenv("AT_HOME" = "TRUE"); tinytest::test_package('jagsUI')"

coverage:
	make install
	Rscript -e 'covr::report(file="/tmp/jagsUI-report.html")'
	firefox /tmp/jagsUI-report.html
