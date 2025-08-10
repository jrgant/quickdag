readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'
	rm README.html
