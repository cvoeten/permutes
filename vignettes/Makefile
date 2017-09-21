.PHONY all: permutes.pdf permutes.html

permutes.pdf:
	R -e "library(rmarkdown);render('permutes.Rmd',encoding='UTF-8','pdf_document')"

permutes.html:
	R -e "library(rmarkdown);render('permutes.Rmd',encoding='UTF-8','html_document')"
